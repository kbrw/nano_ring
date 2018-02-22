defmodule LWWElemSet do
  @moduledoc """
  Simple implementation of the state-based **CRDT** :
  Last Writer Win by element in a Two-Phase Set
  Write are done by simple unions of the 2 add and remove sets
  All reads are done through a reduce function and Enum module
  """
  
  defstruct add_set: MapSet.new, rem_set: MapSet.new
  
  def put(set, e) do
    %{ set | add_set: set.add_set() |> MapSet.put({e, :erlang.timestamp()})}
  end
  
  def delete(set, e) do
    %{ set | rem_set: set.rem_set() |> MapSet.put({e, :erlang.timestamp()})}
  end
  
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
    
    def count(set), do: set |> Enum.count(fn(_) -> true end)
  end

  defimpl Inspect, for: LWWElemSet do
    def inspect(set, opts) do
      content = set |> Enum.map(&Inspect.Algebra.to_doc(&1, opts)) |> Enum.join(",")
      "%LWWElemSet{#{content}}"
    end
  end
  
  def member?(set, e), do: set |> Enum.member?(e)
  
  def to_list(set), do: set |> Enum.to_list
end
