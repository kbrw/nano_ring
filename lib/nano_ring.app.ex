defmodule NanoRing.App do
  @moduledoc """
  NanoRing application entry point
  """
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
