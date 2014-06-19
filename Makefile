.SECONDARY:

all: deps nano_ring

deps: 
	@mix do deps.get

nano_ring:
	@mix compile

## single node dev
start: nano_ring
	@iex -S mix run

start_%: config/%.exs data/%
	@iex --name $*@127.0.0.1 -S mix run

## multiple node dev
NODES = dev1 dev2 dev3 dev4
multi_start: nano_ring
	@for n in $(NODES); do xterm -e "source ~/.zshrc; make start_$$n ; read" & done

data/%:
	mkdir -p "$@"
