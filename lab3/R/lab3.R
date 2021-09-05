

euclidean <- function(a,b){
  if(a<b){
    t = a
    a = b
    b = t
  }
  while (b != 0) {
    t = b
    b = a %% b
    a = t
  }
  return(a)
}

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


