#' Validation Function
#'
#' This function adds a dist attribute to edges as a measure of the Euclidean distance of the connection
#' @param: igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#'
#' @keywords: check attributes
#' @references
#' @export
#' @examples
#' add_edge_dist_attrib()

require(maptools)
require(spatstat)
require(rgdal)
require(data.table)

add_edge_dist_attrib<-function(graph){
  if(check_spatial_attribs(graph)){
    #Create distance matrix
    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)   #This can be sped up with GPU usage
    dist_mat2<-as.matrix(dist_mat)  #Store it in matrix format for craeting graph later
    dist_mat2<-apply(dist_mat2, 1:2, function(x){if(x!=0) x else 1})  #Since nodes may be located in the same place, the dist is 0. Convert 0 to 1 so that when it is multiplied with the adjascency matrix in a few steps, you don't get 0s leading to removal of edges.
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
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}

