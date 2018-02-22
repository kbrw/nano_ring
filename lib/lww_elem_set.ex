defmodule LWWElemSet do
  @moduledoc """
  Simple implementation of the state-based **CRDT** :
  Last Writer Win by element in a Two-Phase Set
  Write are done by simple unions of the 2 add and remove sets
  All reads are done through a reduce function and Enum module
  """
  
  defstruct add_set: MapSet.new(), rem_set: MapSet.new()

  @typedoc "Structure used to describe a Last-Write-Win element set"
  @type t :: %__MODULE__{}

  @doc """
  Add element to the set

  ## Examples

      iex> (s = LWWElemSet.put(LWWElemSet.put(%LWWElemSet{}, :ok), :ok); Enum.count(s))
      1

      iex> (s = LWWElemSet.put(LWWElemSet.put(%LWWElemSet{}, :one), :two); Enum.count(s))
      2
  """
  @spec put(t, any) :: t
  def put(%__MODULE__{ add_set: add_set }=set, e) do
    %{ set | add_set: MapSet.put(add_set, {e, :erlang.timestamp()})}
  end

  @doc """
  Remove element from set

  BUG: a delete right after a put may not remove the value. Check
  :erlang.timestamp precision issue ?

  ## Examples

      iex> (s = LWWElemSet.delete(LWWElemSet.put(%LWWElemSet{}, :ok), :ok); Enum.count(s))
      0

      iex> (s = LWWElemSet.delete(LWWElemSet.put(%LWWElemSet{}, :ok), :not_present); Enum.count(s))
      1
  """
  @spec delete(t, any) :: t
  def delete(%__MODULE__{ rem_set: rem_set }=set, e) do
    %{ set | rem_set: MapSet.put(rem_set, {e, :erlang.timestamp()})}
  end

  @doc """
  Returns a set containing all members of set1, and set2

  ## Examples

      iex> (s = LWWElemSet.union(LWWElemSet.put(%LWWElemSet{}, :one), LWWElemSet.put(%LWWElemSet{}, :two)); Enum.count(s))
      2

      iex> (s = LWWElemSet.union(LWWElemSet.put(%LWWElemSet{}, :one), LWWElemSet.put(%LWWElemSet{}, :one)); Enum.count(s))
      1
  """
  def union(%LWWElemSet{add_set: add_set1, rem_set: rem_set1}, %LWWElemSet{add_set: add_set2, rem_set: rem_set2}) do
    %LWWElemSet{add_set: MapSet.union(add_set1, add_set2), rem_set: MapSet.union(rem_set1, rem_set2)}
  end

  defimpl Enumerable, for: LWWElemSet do
    def reduce({:lwwlists, [], _}, {:cont, acc}, _fun), do: {:done, acc}
    def reduce({:lwwlists, [{v_add, ts_add} | add_list], rem_list}, {:cont, acc}, fun) do
      case rem_list |> Enum.find(fn {v_rem, _} -> v_rem==v_add end) do
        {_, ts_rem} when ts_rem > ts_add ->
	  reduce({:lwwlists, add_list, rem_list}, acc, fun)
	_ ->
	  reduce({:lwwlists, add_list, rem_list}, fun.(v_add, acc), fun)
      end
    end
    def reduce(%LWWElemSet{add_set: add_set, rem_set: rem_set}, {:cont, acc}, fun) do
      reduce({:lwwlists,
          add_set |> Enum.sort(&(&1 > &2)) |> Enum.uniq_by(fn {v, _ts} -> v end),
          rem_set |> Enum.sort(&(&1 > &2)) |> Enum.uniq_by(fn {v, _ts} -> v end)
        }, acc, fun)
    end
    def reduce(_, {:halt, acc}, _fun), do: {:halted, acc}
    def reduce(s, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(s, &1, fun)}
    def reduce(s, acc, fun), do: reduce(s, {:cont, acc}, fun)

    def member?(_s, _e), do: {:error, __MODULE__}

    def count(_set), do: {:error, __MODULE__}
  end

  defimpl Inspect, for: LWWElemSet do
    import Inspect.Algebra
    
    def inspect(set, opts) do
      concat(["%LWWElemSet<", to_doc(LWWElemSet.to_list(set), opts), ">"])
    end
  end
  
  defdelegate member?(set, e), to: Enum
  
  defdelegate to_list(set), to: Enum
end
