# Extending Alga - Acyclic Graphs

### Representation of Acyclic Graphs

Please go through the [Alga paper](https://dl.acm.org/authorize?N46678) before proceeding as some of the terminology used is taken from the paper.

The acyclic graph has a very simple representation which requires the vertices to have an order defined on them.

Consider a set of graphs in which edges only exist from a lower ordered vertex to a higher ordered vertex. It is evident that no graph in this set has any cycle.

Now consider two operations on the set, `*` and `+` (connect and overlay),  
`+` has the same properties as mentioned in the paper  
`*` is slightly different, for any two acyclic graphs in the set, `g1 (v1, e1)` and `g2 (v2, e2)`, `g1 * g2 = g3 (v3, e3)` where `v3 = v1 *union* v2 and e3 = e1 *union* e2 *union* [(x, y) where y > x, x *belongs to* v1 and y *belongs to* v2]`  
Basically `g1 * g2` connects all the lower vertices in `g1` to higher counterparts in `g2`.
Note that this representation is algebrically anologous to a semi-ring.

### Modules and important functions/classes

#### module General.Graph

This module contains classes and a default instance of that class to represent a graph.

1. **class Graph**
    - This class is similar to one defined in the paper with an added function signature, `adjMap :: Map (Vertex g) [Vertex g]`. This is a simple canonical representation of a graph.

2. **data Relation**
    - This is similar to the `Relation` data type defined in the paper. A simple representation of a graph for this prototype.

#### module Acyclic.Graph

The most important module :-). This module contains the type `AcyclicRelation` which is the acyclic extension of the `Relation` data type defined in the paper.

1. **data AcyclicRelation**
    - This is an extension of the `Relation` data type which filters the improper edges in the `*` operation.

2. **unsafeConvertToAcyclic**
    - This function converts any type that has an instance of `Graph` to a graph of Acyclic type.
    - This is the most general type of transformation where one constructs the acyclic graph, but if the underlying type is known then one can create a faster transformation method.
    - This function, if implemented in the Alga library, should not be exported as humans can do all kind of crazy things.
    - Any function that results in an acyclic computation should be chained with this function to result in a graph which has the Acyclic type. For example, `scc' = unsafeConvertToAcyclic . scc`. `scc'` now returns a graph of Acyclic type.
    - This function topologically sorts the vertices, applies the new order and reconstructs the graph (Acyclic fashion).

#### module Acyclic.Util

Utility function for describing acyclic graphs, to be used in Acyclic.Graph.

1. **newtype SimpleOrder**
    - This is important to enforce a new order on already defined vertices.
    - This is a required type as any general graph, even though acyclic need not follow the property mentioned (edges from lower to higher order) and enforcing a strict usage of that property on a user is undesirable.

#### module Prototype

This is a simple prototype to show functionality. 

1. **graph**
    - This is treated as the result of an `scc` operation of the following graph, `A * B + B * D + D * B + D * C`. `B` and `D` form a strongly connected component and the the result of the scc is the following graph, `[[A], []] * [[B, D], [(B, D), (D, B)]] + [[B, D], [(B, D), (D, B)]] * [[C], []]` (Note that the vertices of the new graph are graphs).

2. **acyclicGraph**
    - This is the result of applying `unsafeConvertToAcyclic` to `graph`.


