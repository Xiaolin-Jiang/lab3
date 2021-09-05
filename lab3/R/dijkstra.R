#' The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph.
#' https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'
#' @param graph A dataframe with with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node A number.
#' @return The shortest path to every other node in \code{graph} from the \code{init_node} as a vector.

#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)

#' dijkstra(wiki_graph, 3)


dijkstra <- function(graph, init_node){
  if((is.data.frame(graph)==FALSE)||(is.numeric(init_node)==FALSE)){
    stop()
  }

  vertex_number = max(graph[,1])
  Length_matrix = matrix(10*max(graph[,3]), nrow = vertex_number, ncol = vertex_number )
  for (index in 1:length(graph[,1])){
    Length_matrix[graph[index,1], graph[index,2]] = graph[index,3]
  }
  Q = 1:vertex_number
  dist = rep(100*max(graph[,3]), vertex_number)
  prev = rep(0, vertex_number)
  dist[init_node] = 0

  while (length(Q)>1) {
    u_index = which.min(dist[Q])
    u = Q[u_index]
    Q = Q[-u_index]
    for (v in Q){
      alt = dist[u] + Length_matrix[u,v]
      if (alt<dist[v]){
        dist[v] = alt
        prev[v] = u
      }
    }
  }
  return(dist)
}
