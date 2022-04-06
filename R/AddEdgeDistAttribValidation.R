#' @import data.table igraph
#' @export

add_edge_dist_attrib<-function(graph){

  require(igraph)
  require(data.table)

  if(check_spatial_attribs(graph)){
    #Create distance matrix
    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)   #This can be sped up with GPU usage
    dist_mat2<-as.matrix(dist_mat)  #Store it in matrix format for creating a graph later
    dist_mat2<-apply(dist_mat2, 1:2, function(x){if(x!=0) x else 1})  #Since nodes may be located in the same place, the dist is 0. Convert 0 to 1 so that when it is multiplied with the adjacency matrix in a few steps, the output doesnt't lead to get 0s to removal of edges.
    dist_mat<-as.matrix(dist_mat) #Convert atomic vector to matrix

    weighted_adj_mat<-dist_mat2*as_adjacency_matrix(graph, sparse = F)
    graph<-graph_from_adjacency_matrix(weighted_adj_mat, mode=c("undirected"), diag = F, weighted = T)

    V(graph)$X<-tbl$X #Add X,Y of each node
    V(graph)$Y<-tbl$Y

    shortest_path_mat<-as.matrix(distances(graph, weights = NA))

    E(graph)$dist<-E(graph)$weight
    return(E(graph)$dist)

  }
  else{
    stop("Either the (X,Y) data for nodes is missing, or there is an issue with the (X,Y) data")
  }

}
