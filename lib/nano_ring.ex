defmodule NanoRing do
  @moduledoc """
  Manages nodes ring
  """
  use GenServer

  defstruct node_set: %LWWElemSet{}, up_set: %LWWElemSet{}

  @typedoc "Structure to maintain nodes list (global, up)"
  @type t :: %__MODULE__{ node_set: LWWElemSet.t, up_set: LWWElemSet.t }

  @doc """
  Start nodes manager
  """
  @spec start_link() :: GenServer.on_start
  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Returns list of all nodes (up or not)
  """
  @spec all() :: LWWElemSet.t
  def all, do: :gen_server.call(__MODULE__, :get_all)

  @doc """
  Returns list of up nodes
  """
  @spec up() :: LWWElemSet.t
  def up, do: :gen_server.call(__MODULE__, :get_up)

  @doc """
  Add a node to the cluster
  """
  @spec add_node(atom | binary) :: :ok
  def add_node(node) when is_binary(node), do: add_node(:"#{node}")
  def add_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, {:add_node, node})

  @doc """
  Remove node from cluster
  """
  @spec del_node(atom | binary) :: :ok
  def del_node(node) when is_binary(node), do: del_node(:"#{node}")
  def del_node(node) when is_atom(node), do: :gen_server.cast(__MODULE__, {:del_node, node})

  ###
  ### GenServer callbacks
  ###
  @doc false
  def init(_) do
    :erlang.send_after(Application.get_env(:nano_ring, :gossip_after, 1000), self(), :send_gossip)
    s = case File.read(ring_path()) do
	  {:ok, bin} ->
	    %NanoRing{node_set: :erlang.binary_to_term(bin), up_set: :erlang.binary_to_term(bin)}
	  _ ->
	    %NanoRing{node_set: LWWElemSet.new([node()]), up_set: LWWElemSet.new([node()])}
	end
    {:ok, s}
  end

  @doc false
  def handle_info(:send_gossip, %NanoRing{node_set: node_set, up_set: up_set}=ring) do
    :erlang.send_after(Application.get_env(:nano_ring, :gossip_after, 1000), self(), :send_gossip)

    if not LWWElemSet.member?(node_set,node()), do: :erlang.send_after(5000, self(), :halt_node)

    case up_set |> LWWElemSet.delete(node()) |> LWWElemSet.to_list() do
      [] -> {:noreply, ring}
      active_nodes ->
        random_node = Enum.at(active_nodes, :rand.uniform(length(active_nodes)) - 1)
        ref = make_ref()
        :gen_server.cast({__MODULE__, random_node}, {:reconcile, ring, self(), ref})
        receive do
          {^ref, :is_up} -> {:noreply, ring}
        after Application.get_env(:nano_ring, :die_after, 100) ->
	        ring = update_ring(ring, %{ring | up_set: LWWElemSet.delete(up_set, random_node)})
	        {:noreply, ring}
        end
    end
  end
  def handle_info({_, :is_up}, s), do: {:noreply, s} ## ignore up message from previous gossip
  def handle_info(:halt_node, s) do
    File.rm(ring_path())
    :init.stop()
    {:noreply, s}
  end

  @doc false
  def handle_cast({:reconcile, ring, from, ref}, oldring) do
    send from, {ref, :is_up}

    new_up_set = LWWElemSet.union(ring.up_set, oldring.up_set)
    new_up_set = if LWWElemSet.member?(oldring.node_set, node(from)) and not LWWElemSet.member?(oldring.up_set, node(from)) do
       LWWElemSet.put(new_up_set, node(from))
    else
      new_up_set
    end

    ring = update_ring(oldring,
      %NanoRing{up_set: new_up_set, node_set: LWWElemSet.union(ring.node_set, oldring.node_set)})
    {:noreply, ring}
  end

  def handle_cast({:add_node, n}, %NanoRing{up_set: old_up_set, node_set: old_node_set}=ring) do
    case LWWElemSet.member?(old_node_set, n) do
      true ->
	      {:noreply, ring}
      false ->
        ring = update_ring(ring,
          %NanoRing{up_set: LWWElemSet.put(old_up_set, n), node_set: LWWElemSet.put(old_node_set, n)})
	      {:noreply, ring}
    end
  end

  def handle_cast({:del_node,n},%NanoRing{up_set: old_up_set, node_set: old_node_set}=ring) do
    case LWWElemSet.member?(old_node_set, n) do
      false ->
	      {:noreply, ring}
      true ->
	      ring = update_ring(ring, %NanoRing{up_set: old_up_set, node_set: LWWElemSet.delete(old_node_set, n)})
	      {:noreply, ring}
    end
  end

  @doc false
  def handle_call(:get_all, _from, ring), do: {:reply, ring.node_set, ring}
  def handle_call(:get_up, _from, ring), do: {:reply, ring.up_set, ring}

  ###
  ### Priv
  ###
  defp update_ring(%__MODULE__{}=old_ring, %__MODULE__{}=new_ring) do
    old_up_set = Enum.reduce(old_ring.up_set, MapSet.new(), &MapSet.put(&2, &1))
    new_up_set = Enum.reduce(new_ring.up_set, MapSet.new(), &MapSet.put(&2, &1))
    if old_up_set != new_up_set do
      :gen_event.notify(NanoRing.Events, {:new_up_set, old_up_set, new_up_set})
    end

    old_node_set = Enum.reduce(old_ring.node_set, MapSet.new(), &MapSet.put(&2, &1))
    new_node_set = Enum.reduce(new_ring.node_set, MapSet.new(), &MapSet.put(&2, &1))
    if old_node_set != new_node_set do
      :gen_event.notify(NanoRing.Events, {:new_node_set, old_node_set, new_node_set})
    end

    if new_ring.node_set !== old_ring.node_set do
      dump_state(new_ring)
    end

    new_ring
  end

  defp ring_path do
    Path.join Application.get_env(:nano_ring, :data_dir, "./data"), "ring"
  end

  defp dump_state(ring) do
    File.write!(ring_path(), :erlang.term_to_binary(ring.node_set))
  end
end
