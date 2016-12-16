defmodule NanoRing do
  use GenServer
  require Crdtex.Set
  require Crdtex.Map

  defstruct node_set: Crdtex.Set.new, up_set: MapSet.new, payload: Crdtex.Map.new, counter: 0

  def start_link, do: :gen_server.start_link({:local,__MODULE__},__MODULE__,[],[])

  def init(_) do
    :erlang.send_after(1000,self(),:send_gossip)
    case File.read(ring_path) do
      {:ok,bin} ->
        set = :erlang.binary_to_term(bin)
        monitor(get_set(set))
        {:ok,
         %NanoRing{
          # TODO: restore counter
          node_set: set,
          up_set: MapSet.new(get_set(set)),
          payload: Crdtex.Map.new,
          counter: 0}
        }
      _ ->
        set = Crdtex.Set.new
        {:ok, set} = add(set, {node(), 1}, node())
        {:ok,
         %NanoRing{
          node_set: set,
          up_set: MapSet.new([node()]),
          payload: Crdtex.Map.new,
          counter: 1}
        }
    end
  end

  defp monitor(list) do
    list = List.delete(list, node())
    Enum.each(list, fn(n) -> Node.monitor(n, :true) end)
  end


  def all, do: :gen_server.call(__MODULE__, :get_all)
  def up, do: :gen_server.call(__MODULE__, :get_up)

  def add_node(node) when is_binary(node), do: add_node(:"#{node}")
  def add_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, { :add_node, node })

  def del_node(node) when is_binary(node), do: del_node(:"#{node}")
  def del_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, { :del_node, node })

  defp update_ring(old_ring, new_ring) do
    merged_node_set = merge(old_ring.node_set, new_ring.node_set)
    merged_payload = merge(old_ring.payload, new_ring.payload)
    updated_counter = update_counter(merged_node_set, merged_payload, old_ring)

    up_set = notify_up_set(get_set(old_ring.node_set), get_set(merged_node_set),
    MapSet.to_list(old_ring.up_set) ++ new_ring.from_node)
    notify_node_set(old_ring.node_set, merged_node_set, new_ring.node_set)

    %NanoRing{node_set: merged_node_set, up_set: up_set,
      payload: merged_payload, counter: updated_counter}
  end

  defp update_counter(merged_node_set, merged_payload, old_ring) do
    old_nodes = old_ring.node_set
    old_payload = old_ring.payload
    case {merged_node_set, merged_payload} do
      {^old_nodes, ^old_payload} -> old_ring.counter
      _ -> old_ring.counter + 1
    end
  end

  defp notify_up_set(set, set, old_up) do
    MapSet.new(old_up)
  end
  defp notify_up_set(old_set, merged_set, old_up) do
    new_up = MapSet.difference(MapSet.new(merged_set), MapSet.new(old_set))
    Enum.each(new_up, fn(n) -> Node.monitor(n, :true) end)
    new_set = old_up ++ MapSet.to_list(new_up)
    :gen_event.notify(NanoRing.Events, {:new_up_set, old_up, new_set})
    MapSet.new(new_set)
  end

  defp notify_node_set(set, set, _) do
    :nothingtodo
  end
  defp notify_node_set(old_set, _, new_set) do
    :gen_event.notify(NanoRing.Events, {:new_node_set, get_set(old_set), get_set(new_set)})
      File.write!(ring_path, new_set |> :erlang.term_to_binary)
  end

  def handle_info(:send_gossip, %NanoRing{node_set: node_set} = ring) do
    :erlang.send_after(1000,self(),:send_gossip)
    if not contain?(node_set, node()) do
      :erlang.send_after(5000, self(), :halt_node)
    end
    case ring.up_set |> MapSet.delete(node()) |> MapSet.to_list do
      [] -> {:noreply,ring}
      active_nodes ->
        random_node =
          Enum.at(active_nodes, :rand.uniform(length(active_nodes)) - 1)
        :gen_server.cast({__MODULE__, random_node},
          {:reconcile, %{node_set: node_set, payload: ring.payload, from_node: [node()]}})
      {:noreply, ring}
    end
  end
  def handle_info({:nodedown, n}, %NanoRing{up_set: up_set} = ring) do
    {:noreply, %{ring | up_set: MapSet.delete(up_set, n)}}
  end
  def handle_info(:halt_node,s) do
	  # TODO: preserve counter
    File.rm(ring_path)
    :init.stop()
    {:noreply,s}
  end

  def handle_cast({:reconcile, gossip}, ring) do
    case gossip.from_node do
      [] -> :nothingtodo
      [n] ->
        case MapSet.member?(ring.up_set, n) do
          false -> :nothingtodo
          true ->
            # TODO add this info in the gossip to speedup the convergence of UPs
            Node.monitor(n, true)
        end
    end
    {:noreply, update_ring(ring, gossip)}
  end
  def handle_cast({:add_node, n}, %NanoRing{up_set: old_up_set, node_set: old_node_set, counter: counter} = ring) do
    case contain?(old_node_set, n) do
      true ->
        {:noreply, ring}
      false ->
        new_up_set = MapSet.put(old_up_set, n)
        {:ok, new_node_set} = add(old_node_set, {node(), counter + 1}, n)
        {:noreply,
         update_ring(%NanoRing{ring | up_set: new_up_set}, %{node_set: new_node_set, payload: ring.payload, from_node: []})}
    end
  end
  def handle_cast({:del_node,n},%NanoRing{up_set: old_up_set,node_set: old_node_set, counter: counter} = ring) do
    case contain?(old_node_set, n) do
      false ->
        {:noreply,ring}
      true ->
        new_up_set = MapSet.delete(old_up_set, n)
        {:ok, new_node_set} = delete(old_node_set, {node(), counter + 1}, n)
        {:noreply,
         update_ring(%NanoRing{ring | up_set: new_up_set}, %{node_set: new_node_set, payload: ring.payload, from_node: []})}
    end
  end

  def handle_call(:get_all,_from,ring) do
    {:reply, get_set(ring.node_set), ring}
  end
  def handle_call(:get_up,_from,ring) do
    {:reply, MapSet.to_list(ring.up_set), ring}
  end

  defp ring_path do
    "#{:application.get_env(:nano_ring,:data_dir,"./data")}/ring"
  end

  defp get_set(set), do: Crdtex.value(set)

  defp contain?(set, e), do: Crdtex.value(set, {:contains, e})

  defp add(set, actor, e), do: Crdtex.update(set, actor, {:add, e})

  defp delete(set, actor, e), do: Crdtex.update(set, actor, {:remove, e})

  defp merge(set1, set2), do: Crdtex.merge(set1, set2)
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
