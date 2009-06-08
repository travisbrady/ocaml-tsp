#ocaml-tsp

A genetic algorithm for the solution of the traveling salesman problem
implemented in OCaml.

##Traveling Salesman Problem

Given a list of cities and their coordinates compute the shortest path passing
through all cities and returning to the origin.  The problem is NP-hard.

This code operates on tsp city data from [TSPLIB][http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/tsp/ "TSPLIB data"] with EDGE_WEIGHT_TYPE EUC_2D.

##The Genetic Algorithm

 1. Encoding:
    - simple path encoding ala `[1, 2, 3]`
 2. Crossover:
    - greedy crossover
 3. Mutation
    - two opt
 4. Selection
    - tournament

##Notes:

I wrote this for fun and as a way to learn a bit of OCaml.
Previously I'd written [a version in Haskell](http://github.com/travisbrady/shabonkie/tree/master "Shabonkie")
