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
+ the frontier algorithm uses a tail recursive monad with Loop and Done primitive
+ the graph approach is based on an SCC algorithm

## Benchmarking:

    ./go benchmark
    ...
    Benchmark #1: node ./dist/app.js --strategy graph
    Time (mean ± σ):     22.507 s ±  0.158 s    [User: 22.859 s, System: 0.387 s]
    Range (min … max):   22.347 s … 22.704 s    4 runs
    
    Benchmark #2: node ./dist/app.js --strategy frontier
    Time (mean ± σ):      6.973 s ±  0.034 s    [User: 7.377 s, System: 0.166 s]
    Range (min … max):    6.943 s …  7.008 s    4 runs
    
    Summary
    'node ./dist/app.js --strategy frontier' ran
        3.23 ± 0.03 times faster than 'node ./dist/app.js --strategy graph'

## The frontier algorithm

We model the traversal a crest of increasing path length spreading in all directions from the start point:

    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    .....3....
    ....323...
    ...32123..

We have found a solution if, during each step:
    1) we add no new steps along the frontier (because we've filled the local area), 
    2) find a path to the exit

This approach is about 3x quicker than the graph algorithm, unoptimised, as it can exit quickly.


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

Note: LogicT approach consumes the whole stack super quickly.  I couldn't find a continuation implementation. Migrating from haskell was sufficientlly difficult.

Note 2: It's not obvious that we can prune whole search paths, and there isn't a way of keeping memory of previous searches.


# Part 2 - Errata

## Tech
+ haskell
+ stack
+ hedgehog/hspec
+ sitting along side the purescript - novel?  yes Useful? meh...

## Interesting bits

It's mostly a like for like, but there are thing that work in Purescript the don't work in Haskell, and vice versa.

### Recursion
Purescript:
```purescript  
    go v1 = tailRecM2 loop v1 (step v1)
        loop :: ViaFrontier Int 
                -> ViaFrontier Int
                -> Identity (Step { 
                    a :: ViaFrontier Int, 
                    b :: ViaFrontier Int} 
                    (ViaFrontier Boolean))
        loop v1 v2@(ViaFrontier (sol/\_)) = 
            pure $ case (stop v1 v2) of
            Just b -> Done (ViaFrontier (sol/\b))
            Nothing -> Loop { a : v2, b : step v2 }
```

Haskell:

```haskell
    go v1 = catMaybes $ zipWith loop (steps v1) (tail $ steps v1)
    steps :: ViaFrontier Int -> [ViaFrontier Int]
    steps v1 = v1 : steps (step v1)
    loop :: ViaFrontier Int 
            -> ViaFrontier Int
            -> Maybe (ViaFrontier Boolean)
    loop v1 v2@(ViaFrontier (sol,_)) = 
        (ViaFrontier.(sol,)) <$> stop v1 v2
```

### Lists/Arrays

Purescript really doesn't like you using Lists - most of the built in librarys work  with Arrays.  So there's a lof of List -> Array munging going on.

Since there aren't any patterns, and purescript doesn't optimise, the output is pretty slow.  Especially for folds/filters.  

Haskell's laziness is really nice here, as you can filter/fold - and the fusion wiull kind in, leaving you with just one traversal.

### IDE

I've been using VS code, and the Purescript LSP is ok.  It helps a bit, but it mostly works.

The haskell IDE extension is a bit fiddly, and error prone.  I think it neutral in value at the moment.  However, great minds are working on it, and I fully expect to see 2020 be a breakout year for the Haskell IDE experience.

### Parsers

Actually - Purescript's ```Text.Parsing``` and Haskells ```MegaParsec``` are pretty similar.  Megaparsec is super typeclassy, so full marks there.

### Generators

Haskel and Purescript have MonadGen type classes, mostly you can get the same sort of thing.  The eval parts are different - Hedghog (haskell, evalGen) gives you access to the rosetree, which is unexpected.

### Property based testing

I prefer the haskell/hedgehog choice of avoiding ```Arbitrary```.  Haskell has enough ways to manage namespaces, so coupling to Generators rather than newtypes is fine.

### Performance

Haskell wins by a country mile.  That's because it's nearly ALL statically eliminated.  Plus - the graph solution uses efficient Int indexing.  Totally different experience. 

If we shifted to a concurrency approach, haskell would probably beat Rust.  

### Effects

Purescript is niceer dev experience - the Free monads are slow but seem familiar.

I use polysemy in haskell - and I just don't the surface area very well, so it's mostly copy and paste.


### Setup

Haskell is a complete nightmare to setup.  It's getting better.  The IDE was sort of documented.  

No go script yet.  You could try and see how far you get...

```brew bundle``` then
```direnv allow``` then
```stack test``` then ...



