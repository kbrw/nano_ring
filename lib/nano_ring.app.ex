defmodule NanoRing.App do
  @moduledoc """
  NanoRing application entry point
  """  
  use Application
  
  def start(_type,_args) do
    Supervisor.start_link([
      NanoRing.Events,
      NanoRing
    ], strategy: :one_for_one)
  end
end
