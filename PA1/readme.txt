Rock Beom Kim rk5dy

I used Python first because of its convenient splicing method to store the start and end values into separate arrays and because object-oriented programming is significantly more comfortable for me than functional programming.
Also, the enforced tabbing allowed for easy legibility.
I used a brute-force methodology with a boolean table to represent the edges.
The while loop I used for the Python implementation was essentially the Kahn algorithm.
The final graph was stored in a list.
Ruby was more or less an exact clone of the Python implementation, with special caution for how the Ruby for loops is inclusive of the upper bound (hence the array.length - 1 used in the loops).
The difficulty of C stemmed from the use of arrays and multiple accumulators used to store the size/counter values and the need to allocate memory for the boolean table, the topologically sorted array of vertices and the constantly changing "set of no incoming edges" used in the Kahn algorithm on the Wikipedia page.
In my functional implementation (OCaml/Haskell), the edges were stored as a list of string tuples.
The function was recursive, with the empty edge list being the base case.
Otherwise, the function would take the vertices with no incoming edges, take the minimum element, get rid of all the edges from the minimum element.
The above repeats until there are no more outgoing edges from any of the vertices.
The reduction in problem stems from the filtering of edges and elements from the set of all starting vertex values.
The hardest language to use was by far Cool. Cool has no standard libraries, biggest issue being the lack of arrays (which was how I implemented topological sort in C).
In place of arrays, a linked list was used in Cool to store the nodes and their next values. Nodes were the equivalent of adjacency lists in the normal topological sort.
An iterator, pop, push and a getInstance function were implemented with an insertion sort to simulate the queue of nodes for the topological sort.
