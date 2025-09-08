MixedGraphs 1.1.9
-------------------------------------------------------------------------------

NEW FEATURES

 * Introduced `as.eList()` and `as.edgeMatrix()` functions to convert ordinary 
 objects into `eList` and `edgeMatrix` ones.

 * Modified `graphCr()` so that it deals with tabs, new lines and carriage 
 returns correctly

 * Modifed `morphEdges()` so that if two sets are provided, the edge type is
 directed, and no topological ordering is specified, then all entries in `A` 
 precede all entries in `B`.
 
 