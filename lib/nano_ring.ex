defmodule LWWElemSet do
  @moduledoc """
  Simple implementation of the state-based **CRDT** :
  Last Writer Win by element in a Two-Phase Set
  Write are done by simple unions of the 2 add and remove sets
  All reads are done through a reduce function and Enum module
  """
  defstruct add_set: MapSet.new, rem_set: MapSet.new
  def put(set,e), do:
    %{set| add_set: set.add_set|>MapSet.put({e,:erlang.timestamp()})}
  def delete(set,e), do:
    %{set| rem_set: set.rem_set|>MapSet.put({e,:erlang.timestamp()})}
  def union(%LWWElemSet{add_set: add_set1,rem_set: rem_set1},%LWWElemSet{add_set: add_set2,rem_set: rem_set2}), do:
    %LWWElemSet{add_set: MapSet.union(add_set1,add_set2),rem_set: MapSet.union(rem_set1,rem_set2)}

  defimpl Enumerable, for: LWWElemSet do
    def reduce({:lwwlists,[],_},{:cont,acc},_fun), do: {:done,acc}
    def reduce({:lwwlists,[{v_add,ts_add}|add_list],rem_list},{:cont,acc},fun) do
      case rem_list |> Enum.find(fn {v_rem,_}->v_rem==v_add end) do
        {_,ts_rem} when ts_rem > ts_add -> reduce({:lwwlists,add_list,rem_list},acc,fun)
        _ ->reduce({:lwwlists,add_list,rem_list},fun.(v_add,acc),fun)
      end
    end
    def reduce(%LWWElemSet{add_set: add_set,rem_set: rem_set},{:cont,acc},fun) do
      reduce({:lwwlists,
          add_set|>Enum.sort(&(&1 > &2))|>Enum.uniq_by(fn {v,_ts}->v end),
          rem_set|>Enum.sort(&(&1 > &2))|>Enum.uniq_by(fn {v,_ts}->v end)
        },acc,fun)
    end
    def reduce(_,{:halt,acc},_fun), do: {:halted, acc}
    def reduce(s,{:suspend,acc},fun), do: {:suspended, acc, &reduce(s, &1, fun)}
    def reduce(s,acc,fun), do: reduce(s,{:cont,acc},fun)
    def member?(_s,_e), do: { :error, __MODULE__ }
    def count(set), do: set |> Enum.count(fn(_)->true end)
  end
  defimpl Inspect, for: LWWElemSet do
    def inspect(set, opts), do: "%LWWElemSet{#{set |> Enum.map(&Inspect.Algebra.to_doc(&1,opts)) |> Enum.join(",")}}"
  end
  def member?(set,e), do: set |> Enum.member?(e)
  def to_list(set), do: set |> Enum.to_list
end

defmodule NanoRing do
  use GenServer
  defstruct node_set: %LWWElemSet{}, up_set: %LWWElemSet{}

  def start_link, do: :gen_server.start_link({:local,__MODULE__},__MODULE__,[],[])
  def init(_) do
    :erlang.send_after(1000,self(),:send_gossip)
    case File.read(ring_path()) do
        {:ok,bin} -> {:ok,%NanoRing{node_set: :erlang.binary_to_term(bin),up_set: :erlang.binary_to_term(bin)}}
        _ -> {:ok,%NanoRing{node_set: %LWWElemSet{} |> LWWElemSet.put(node()),up_set: %LWWElemSet{} |> LWWElemSet.put(node())} }
    end
  end

  def all, do: :gen_server.call(__MODULE__, :get_all)
  def up, do: :gen_server.call(__MODULE__, :get_up)

  def add_node(node) when is_binary(node), do: add_node(:"#{node}")
  def add_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, { :add_node, node })

  def del_node(node) when is_binary(node), do: del_node(:"#{node}")
  def del_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, { :del_node, node })

  defp update_ring(old_ring,new_ring) do
    case {old_ring.up_set|>Enum.reduce(MapSet.new,&MapSet.put(&2,&1)),new_ring.up_set|>Enum.reduce(MapSet.new,&MapSet.put(&2,&1))} do
      {unchanged,unchanged}->:nothingtodo
      {old_up_set,new_up_set}->:gen_event.notify(NanoRing.Events,{:new_up_set,old_up_set,new_up_set})
    end
    case {old_ring.node_set|>Enum.reduce(MapSet.new,&MapSet.put(&2,&1)),new_ring.node_set|>Enum.reduce(MapSet.new,&MapSet.put(&2,&1))} do
      {unchanged,unchanged}->:nothingtodo
      {old_node_set,new_node_set}->:gen_event.notify(NanoRing.Events,{:new_node_set,old_node_set,new_node_set})
    end
    if new_ring.node_set !== old_ring.node_set, do: File.write!(ring_path(),new_ring.node_set|>:erlang.term_to_binary)
    new_ring
  end

  def handle_info(:send_gossip, %NanoRing{node_set: node_set,up_set: up_set}=ring) do
    :erlang.send_after(1000,self(),:send_gossip)
    if not LWWElemSet.member?(node_set,node()), do: :erlang.send_after(5000,self(),:halt_node)
    case up_set |> LWWElemSet.delete(node()) |> LWWElemSet.to_list do
      [] -> {:noreply,ring}
      active_nodes ->
        random_node = Enum.at(active_nodes,:rand.uniform(length(active_nodes))-1)
        ref = make_ref()
        :gen_server.cast({__MODULE__,random_node},{:reconcile,ring,self(),ref})
        receive do 
          {^ref,:is_up} ->{:noreply,ring} 
        after 
          100 -> {:noreply,update_ring(ring,%{ring|up_set: up_set|>LWWElemSet.delete(random_node)})}
        end
    end
  end
  def handle_info({_,:is_up},s), do: {:noreply,s} ## ignore up message from previous gossip
  def handle_info(:halt_node,s) do
    File.rm(ring_path())
    :init.stop()
    {:noreply,s}
  end

  def handle_cast({:reconcile,ring,from,ref},oldring) do
    send from, {ref,:is_up}
    new_up_set = LWWElemSet.union(ring.up_set,oldring.up_set)
    new_up_set = if LWWElemSet.member?(oldring.node_set,node(from)) and not LWWElemSet.member?(oldring.up_set,node(from)) do
       LWWElemSet.put(new_up_set,node(from))
    else
      new_up_set
    end
    {:noreply,update_ring(oldring,%NanoRing{up_set: new_up_set,node_set: LWWElemSet.union(ring.node_set,oldring.node_set)})}
  end
  def handle_cast({:add_node,n},%NanoRing{up_set: old_up_set,node_set: old_node_set}=ring) do
    case LWWElemSet.member?(old_node_set,n) do
      true -> {:noreply,ring}
      false -> {:noreply,update_ring(ring,%NanoRing{up_set: old_up_set|>LWWElemSet.put(n),node_set: old_node_set|>LWWElemSet.put(n)})}
    end
  end
  def handle_cast({:del_node,n},%NanoRing{up_set: old_up_set,node_set: old_node_set}=ring) do
    case LWWElemSet.member?(old_node_set,n) do
      false -> {:noreply,ring}
      true -> {:noreply,update_ring(ring,%NanoRing{up_set: old_up_set,node_set: old_node_set |> LWWElemSet.delete(n)})}
    end
  end

  def handle_call(:get_all,_from,ring), do: {:reply,ring.node_set,ring}
  def handle_call(:get_up,_from,ring), do: {:reply,ring.up_set,ring}

  defp ring_path, do:
      "#{:application.get_env(:nano_ring,:data_dir,"./data")}/ring"
end

defmodule NanoRing.App do
  use Application
  def start(_type,_args) do
    :supervisor.start_link(NanoRing.App.Sup,[])
  end
  defmodule Sup do
    use Supervisor
    def init([]) do
      supervise([
        worker(:gen_event,[{:local,NanoRing.Events}], id: NanoRing.Events),
        worker(NanoRing,[])
      ], strategy: :one_for_one)
    end
  end
end
