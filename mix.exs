defmodule SaEautomation.Mixfile do
  use Mix.Project

  def project do
    [ app: :nano_ring,
      version: "0.0.3",
      elixir: "~> 1.2",
      deps: [{:crdtex, "0.0.1", git: "git://github.com/awetzel/crdtex"}]
    ]
  end

  def application do
    [ mod: { NanoRing.App,[] },
      applications: [:iex, :crdtex],
      env: [ data_dir: "./data" ] ]
  end
end
