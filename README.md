nano_ring
=========

NanoRing is a very very small Cluster management System in Elixir.

It uses a simple gossip protocol to share the ring state without a
master node. The 2 sets containing the ring state (all nodes and up
nodes) are made convergent with a LWW-element-set state-based CRDT
(http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf ).  The
LWW-element-set containing all nodes is persistent and written on
disk.

## How does it work ? ##

The state communication works using a gossip style protocol :

- each node has two sets : one containing all cluster nodes, and the
  other containing only up node.
- every second, a node choose a random node among up-nodes, send it
  its 2 sets.
- the chosen node updates its own ring merging the received ring.
- the chosen node look if the sending node is in its up-node set, if
not add it (because it knows that it has juste received a message
from it, so it is not down)
- the chosen node send an "ack" message to the sending node, so that
if this node did not receive the ack after a timeout, it can remove
the chosen node from its "up-set".

The whole cluster node set is persisted on-disk on each node, saved
on cluster change. The up node set is initialized with all nodes.

When a node is removed (cast `:del_node`), remove it from the
`node_set` but not from the `up_set` to allow the gossip propagates
the ring. When a node see that it is not in its current cluster,
delete its ring file and exit the node after 10 seconds, so that all
node will remove it from their `up_set`.

## How to test Nano Ring ##

First you can create 4 node launching either `make multi_start` if you use
xterm, it will launch 4 terms with erlang nodes named
`dev[1,2,3,4]@127.0.0.1` running `NanoRing.App`.

If you cannot use xterm, you can create your own 4 terminal and
launch `make start_dev[1-4]` on each one.

### Create a cluster ###

By default, each node is in a ring containing only itself. To join
another node, use for instance on `nano_ring_dev1`:

```elixir
NanoRing.add_node(:"nano_ring_dev2@127.0.0.1")
```

If you want to remove a node, use :

```elixir
NanoRing.del_node(:"nano_ring_dev2@127.0.0.1")
```

and wait 10 seconds.

In order to see the evolution of the cluster, you can see the whole
cluster with :

```elixir
NanoRing.get_all()
```

To observe the available nodes of the cluster, you can use :

```elixir
NanoRing.get_up()
```

## License

nano_ring source code is released under Apache 2 License.

Check the [LICENSE](LICENSE) file for more information.
