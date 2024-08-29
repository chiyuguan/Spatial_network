## Purpose: This function converts spatial adjacency information into a format suitable for Stan.
# Inputs:
#   adjBUGS: Adjacency list.
#   numBUGS: Number of neighbors for each region.
# Outputs: A list containing:
#   N: Number of regions.
#   N_edges: Number of edges in the adjacency graph.
#   node1: A vector containing the starting node of each edge.
#   node2:  A vector containing the ending node of each edge.

mungeCARdata4stan = function(adjBUGS, numBUGS) {
  N = length(numBUGS);  # Number of regions
  nn = numBUGS;  # Number of neighbors for each region, vector
  N_edges = length(adjBUGS) / 2;  #**** Total number of edges in the adjacency list
  node1 = vector(mode="numeric", length=N_edges);  # Initialize node1 vector
  node2 = vector(mode="numeric", length=N_edges);  # Initialize node2 vector
  iAdj = 0;  # Initialize adjacency index
  iEdge = 0;  # Initialize edge index
  for (i in 1:N) {  # Loop over each region
    for (j in 1:nn[i]) {  # Loop over each neighbor
      iAdj = iAdj + 1;  # Increment adjacency index
      if (i < adjBUGS[iAdj]) {  # Check if the region index is less than the neighbor index to avoid duplicate edges
        iEdge = iEdge + 1;  # Increment edge index
        node1[iEdge] = i;  # Assign region index to node1
        node2[iEdge] = adjBUGS[iAdj];  # Assign neighbor index to node2
      }
    }
  }
  return (list("N"=N, "N_edges"=N_edges, "node1"=node1, "node2"=node2));  # Return the processed adjacency information
}
