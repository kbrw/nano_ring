defrecord LWWElemSet, add_set: HashSet.new(), rem_set: HashSet.new()  do
  @moduledoc """
  Simple implementation of the state-based **CRDT** :
  Last Writer Win by element in a Two-Phase Set
  Write are done by simple unions of the 2 add and remove sets
  All reads are done through a reduce function and Enum module
  """
  def put(set,e), do:
    set.update_add_set fn(s)->s|>Set.put({e,:erlang.now()}) end
  def delete(set,e), do:
    set.update_rem_set fn(s)->s|>Set.put({e,:erlang.now()}) end
  def union(LWWElemSet[add_set: add_set1,rem_set: rem_set1],LWWElemSet[add_set: add_set2,rem_set: rem_set2]), do:
    LWWElemSet[add_set: Set.union(add_set1,add_set2),rem_set: Set.union(rem_set1,rem_set2)]

  defimpl Enumerable, for: LWWElemSet do
    def reduce({:lwwlists,[],_},acc,_fun), do: acc
    def reduce({:lwwlists,[{v_add,ts_add}|add_list],rem_list},acc,fun) do
      case rem_list |> Enum.find(fn {v_rem,_}->v_rem==v_add end) do
        {_,ts_rem} when ts_rem > ts_add -> reduce({:lwwlists,add_list,rem_list},acc,fun)
        _ ->reduce({:lwwlists,add_list,rem_list},fun.(v_add,acc),fun)
      end
    end
    def reduce(LWWElemSet[add_set: add_set,rem_set: rem_set],acc,fun) do
      reduce({:lwwlists,
          add_set|>Enum.sort(&(&1 > &2))|>Enum.uniq(fn {v,_ts}->v end),
          rem_set|>Enum.sort(&(&1 > &2))|>Enum.uniq(fn {v,_ts}->v end)
        },acc,fun)
    end
    def member?(set,e), do: set |> Enum.any?(&(&1==e))
    def count(set), do: set |> Enum.count(fn(_)->true end)
  end
  defimpl Inspect, for: LWWElemSet do
    def inspect(set, opts), do: "LWWSet[#{set |> Enum.map(&Kernel.inspect(&1,opts)) |> Enum.join(",")}]"
  end
  def member?(set,e), do: set |> Enum.member?(e)
  def to_list(set), do: set |> Enum.to_list
end

defmodule NanoRing do
  use GenServer.Behaviour
  defrecord Ring, node_set: LWWElemSet[], up_set: LWWElemSet[]

  def start_link, do: :gen_server.start_link({:local,__MODULE__},__MODULE__,[],[])
  def init(_) do
    :erlang.send_after(1000,self(),:send_gossip)
    case File.read(ring_path) do
        {:ok,bin} -> {:ok,Ring[node_set: binary_to_term(bin),up_set: binary_to_term(bin)]}
        _ -> {:ok,Ring[node_set: LWWElemSet[] |> LWWElemSet.put(node()),up_set: LWWElemSet[] |> LWWElemSet.put(node())] }
    end
  end

  def handle_info(:send_gossip, Ring[node_set: node_set,up_set: up_set]=ring) do
    :erlang.send_after(1000,self(),:send_gossip)
    if not Set.member?(node_set,node()), do: :erlang.send_after(5000,self(),:halt_node)
    case up_set |> Set.delete(node()) |> Set.to_list do
      [] -> {:noreply,ring}
      active_nodes ->
        random_node = Enum.at(active_nodes,:random.uniform(length(active_nodes))-1)
        ref = make_ref()
        :gen_server.cast({__MODULE__,random_node},{:reconcile,ring,self(),ref})
        receive do 
          {^ref,:is_up} ->{:noreply,ring} 
        after 
          1000 -> {:noreply,ring.up_set(up_set|>Set.delete(random_node))} 
        end
    end
  end
  def handle_info({_,:is_up},s), do: {:noreply,s} ## ignore up message from previous gossip
  def handle_info(:halt_node,s) do
    File.rm(ring_path)
    :init.stop()
    {:noreply,s}
  end

  def handle_cast({:reconcile,ring,from,ref},oldring) do
    from <- {ref,:is_up}
    new_up_set = Set.union(ring.up_set,oldring.up_set)
    if Set.member?(oldring.node_set,node(from)) and not Set.member?(oldring.up_set,node(from)), do:
       new_up_set = new_up_set |> Set.put(node(from))
    new_node_set = Set.union(ring.node_set,oldring.node_set)
    if new_node_set==oldring.node_set, do: File.write!(ring_path,new_node_set|>term_to_binary)
    {:noreply,Ring[up_set: new_up_set,node_set: new_node_set]}
  end
  def handle_cast({:node_down,n},ring) do # call when a call timeout
    {:noreply,ring.update_up_set(fn(s)->s |> Set.delete(n) end)}
  end
  def handle_cast({:add_node,n},Ring[up_set: old_up_set,node_set: old_node_set]=ring) do
    case old_node_set |> Set.member? n do
      true -> {:noreply,ring}
      false -> {:noreply,Ring[up_set: old_up_set |> Set.put(n) ,node_set: old_node_set |> Set.put(n)]}
    end
  end
  def handle_cast({:del_node,n},Ring[up_set: old_up_set,node_set: old_node_set]=ring) do
    case old_node_set |> Set.member? n do
      false -> {:noreply,ring}
      true -> {:noreply,Ring[up_set: old_up_set,node_set: old_node_set |> Set.delete(n)]}
    end
  end

  def handle_call(:get_all,_from,ring), do: {:reply,ring.node_set,ring}
  def handle_call(:get_up,_from,ring), do: {:reply,ring.up_set,ring}

  defp ring_path do
      "#{:application.get_env(:nano_ring,:data_dir,"./data")}/ring"
  end
end

defmodule NanoRing.App do
  use Application.Behaviour
  @moduledoc """
  The application launch only the ring manager
  """
  def start(_type,_args) do
    :supervisor.start_link(NanoRing.App.Sup,[])
  end
  defmodule Sup do
    use Supervisor.Behaviour
    def init([]) do
      supervise([
        worker(NanoRing,[])
      ], strategy: :one_for_one)
    end
  end
end
