nano_ring
=========

NanoRing is a very very small Cluster management System in Elixir.

NanoRing needs only Elixir 1.2.x and crdtex. It uses a simple gossip protocol to
share the ring state and a payload without a master node. Both the set of
members of the cluster and the payload are CRDT.  all nodes set is made
convergent with an OR-set state-based CRDT
(http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf ).  The OR-set
containing all nodes is persistent and written on disk.

## How does it work ? ##

The state communication works using a gossip style protocol :

- each node has two sets : one containing all cluster nodes, and the
  other containing only up node.
- every second, a node choose a random node among up-nodes, send it
  its all_nodes set and its payload CRDT.
- the chosen node updates its own ring merging the received ring.
- the chosen node look if the sending node is in its up-node set, if
not add it (because it knows that it has juste received a message
from it, so it is not down)
- each node in the all_nodes set is monitored and added to the up_set
- when a node terminates {:nodedown, node} is received by each of the other
nodes which then remove it from the up_set

The whole cluster node set is persisted on-disk on each node, saved
on cluster change. The up node set is initialized with all nodes.

When a node is removed (cast`:del_node`), remove it from the
`node_set` but not from the `up_set` to allow the gossip propagates the
ring. When a node see that it is not in its current cluster, delete
its ring file and exit the node after 10 seconds, so that all node
will remove it from their `up_set`.

## How to test Nano Ring ##

First you can create 4 node launching either `make multi_start` if you use
xterm, it will launch 4 terms with erlang nodes named
`dev[1,2,3,4]@127.0.0.1` running `NanoRing.App`.

If you cannot use xterm, you can create your own 4 terminal and
launch `make start_dev[1-4]` on each one.

### Create a cluster ###

By default, each node is in a ring containing only itself. To join
another node, use for instance on `nano_ring_dev1`:

    :gen_server.cast(NanoRing,{:add_node,:"nano_ring_dev2@127.0.0.1"})
or more conveniently :
		Nanoring.add_node("nano_ring_dev2@127.0.0.1")

If you want to remove a node, use :

    :gen_server.cast(NanoRing,{:del_node,:"nano_ring_dev2@127.0.0.1"})
or more conveniently :
		Nanoring.del_node("nano_ring_dev2@127.0.0.1")

and wait 10 seconds.

In order to see the evolution of the cluster, you can see the whole
cluster with :

    :gen_server.call(NanoRing,:get_all)
or more conveniently :
		Nanoring.all

To observe the available nodes of the cluster, you can use :

    :gen_server.call(NanoRing,:get_up)
or more conveniently :
		Nanoring.up

