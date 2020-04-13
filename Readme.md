# Shokinin 20 - Food.

To run - this will install all the tools except hyperfine

```bash
./go install
./go run
```


## Tech

+ purescript
+ spago
+ spec (and quickcheck)
+ purescript-graph (correct and slow solution)
+ hyperfine - for benchmarking (```brew install hyperfine```)
+ deadcode elimination with rollup with purs-rollup-plugin

## Interesting bits

+ We reuse the MonadGen from quickcheck (Gen) in prod as well as test code
+ we abstract over console output with Teletype (a free monad)

## The graph algorithm

We model the office as graph, where nodes are locations ```(Int,Int)```, and an edge allows moves between locations.  Edges only exist along x-axis and y-axis directions.

             1 2 3
    1        X . .
    2        . . X
    3        X X X

is equivalent to:

    (2,1)->(2,2); (2,2)->(2,1); (2,1)->(3,1); (3,1)-> (2,1)
    (1,2)->(2,2); (2,2)->(1,2)

Then we find the strongly connected components (nodes in a component can reach each other via edges).  

In the above case, ```(1,2),(2,2),(2,1),(3,1)``` form a connected component.  

Note: In this implementation, we use a di-graph.  We model ```A->B``` AND ```B->A```, but since all edges are bi-directional and unweighted, that's quite wasteful.  In the above, we could just have had ```(1,2)<->(2,2)<->(2,1)<-(3,1)```

If our start position and any of the top edge are in the connected component, then there is path out of that office.  In our example above, while it _does_ have a path to the exit row, it doesn't contain the starting position, so no luck there.  We'd have to look in the other components.

## The backtracking algorithm:

The connected components approach considers all transitive pathways in the graph.  This is pretty efficient when there are few nodes, but fairly slow when there are many nodes.

WIP - Using [Backtracking, Interleaving, and Terminating Monad Transformers](http://okmij.org/ftp/papers/LogicT.pdf)

```LogicT``` is a monad transformer that supports the `ifthel` logical negation and `once` pruning primitives.  

[LogicT](https://github.com/mlang/purescript-logic/blob/master/src/Control/Monad/Logic/Class.purs) has implementations for Array, CatList and a few others.

This means that, instead of considering the graph as a whole, we can fairly search across the office.
