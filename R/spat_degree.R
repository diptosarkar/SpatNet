#' Calculate Spatial Degree
#'
#' This function allows you to express your love of Hello World
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.
#' @param alpha specifies alpha value at which degree is to be calculted
#' @param rank decides whether to return rank of nodes of raw values. Defaults to TRUE meaning ranks are to be calculated
#' @keywords: spatial clustering
#' @import data.table igraph
#' @export
#' @examples
#'

spat_degree<-function(graph, alpha, rank=TRUE){
  if(check_spatial_attribs(graph)){
    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)  #Can be sped up with GPU
    dist_mat2<-as.matrix(dist_mat)  #Store it in matrix format for craeting graph later
    dist_mat2<-apply(dist_mat2, 1:2, function(x){if(x!=0) x else 1})  #Since a multiple nodes can be in the same spot, the dist is 0. Convert 0 to 1 so that when it is multiplied with the adjascency matrix in a few steps, you don't get 0s leading to removal of edges.
    weighted_adj_mat<-dist_mat2*as_adjacency_matrix(graph, sparse = F)

    deg_dt<-data.table(vertex_name=V(graph)$name)

    deg_dt$degree_benefit<-apply(weighted_adj_mat, 1, sum)   #weighted adj mat has benefits
    #deg_dt$degree_benefit<-deg_dt$degree_benefit/1000 #In Km

    #Normalize between 0-1
    deg_dt$degree_benefit = (deg_dt$degree_benefit-min(deg_dt$degree_benefit))/(max(deg_dt$degree_benefit)-min(deg_dt$degree_benefit))

    weighted_adj_mat2<-apply(weighted_adj_mat, 1:2, function(x){if(x!=0) 1/x else x})  #Change to 1000/x for KM

    deg_dt$degree_cost<-apply(weighted_adj_mat2, 1, sum)
    #Normalize between 0-1
    deg_dt$degree_cost = (deg_dt$degree_cost-min(deg_dt$degree_cost))/(max(deg_dt$degree_cost)-min(deg_dt$degree_cost))

    V(graph)$degree<-degree(graph, normalized = T)
    V(graph)$deg_cost<-deg_dt$degree_cost
    V(graph)$deg_stren<-deg_dt$degree_benefit

    rm(deg_dt)
    if(alpha<0.0 | alpha>1.0)
    {
      stop("The value of alpha must be between [0,1]")
    }
    V(graph)$spat_deg<-alpha * (V(graph)$deg_cost) + (1-alpha) * (V(graph)$deg_stren)
    if(!rank){
      return(V(graph)$spat_deg) #return raw values
    }
    if(rank)
    {
      return(data.table::frank(-V(graph)$spat_deg, ties.method = 'average'))
    }

  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
