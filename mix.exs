defmodule SaEautomation.Mixfile do
  use Mix.Project

  def project do
    [ app: :nano_ring,
      version: "0.0.3",
      elixir: ">= 1.3.0",
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  def application do
    [ mod: { NanoRing.App,[] },
      applications: [:iex],
      env: [ data_dir: "./data" ] ]
  end

  defp deps, do: [
    {:ex_doc, ">= 0.0.0", only: :dev}
  ]

  defp docs, do: [
    main: "readme",
    extras: [ "README.md" ],
    source_url: "https://gtihub.com/kbrw/nano_ring"
  ]

  defp package, do: [
    maintainers: [
      "Arnaud Wetzel <arnaud.wetzel@kbrw.fr>",
      "Jean Parpaillon <jparpaillon@kbrw.fr>"
    ],
    licenses: ["Apache License 2.0"],
    links: %{ "GitHub" => "https://github.com/kbrw/nano_ring" },
    source_url: "https://github.com/kbrw/nano_ring"
  ]
end
