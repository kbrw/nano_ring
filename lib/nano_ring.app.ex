defmodule NanoRing.App do
  @moduledoc """
  NanoRing application entry point
  """  
  use Application
  
  def start(_type,_args) do
    Supervisor.start_link([
      %{ id: NanoRing.Events, start: {:gen_event, :start_link, [{:local, NanoRing.Events}]} },
      %{ id: NanoRing, start: {NanoRing, :start_link, []} }
    ], strategy: :one_for_one)
  end
end
