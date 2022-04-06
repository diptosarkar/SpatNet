#' @import igraph data.table crayon
#' @export

spat_degree<-function(graph, alpha = 0.5, alphaWarning, rank=TRUE){

  require(igraph)
  require(data.table)
  require(crayon)

  if(check_spatial_attribs(graph)){
    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)  #Can be sped up with GPU
    dist_mat2<-as.matrix(dist_mat)  #Store it in matrix format for creating a graph later
    dist_mat2<-apply(dist_mat2, 1:2, function(x){if(x!=0) x else 1})  #Since a multiple of nodes can be in the same spot, the dist is 0. Convert 0 to 1 so that when it is multiplied with the adjacency matrix in a few steps, the output doesnt't lead to get 0s to removal of edges.
    weighted_adj_mat<-dist_mat2*as_adjacency_matrix(graph, sparse = F)

    deg_dt<-data.table(vertex_name=V(graph)$name)

    deg_dt$degree_benefit<-apply(weighted_adj_mat, 1, sum)   #Weighted adjacency mat has benefits
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

    alphaWarning= cat(crayon::green$bold("Default alpha is set at 0.5\n"))

    if(alpha<0.0 | alpha>1.0)
    {
      stop("The value of alpha must be between [0,1]")
    }
    V(graph)$spat_deg<-alpha * (V(graph)$deg_cost) + (1-alpha) * (V(graph)$deg_stren)
    if(!rank){
      return(V(graph)$spat_deg) #Return raw values
    }
    if(rank)
    {
      return(data.table::frank(-V(graph)$spat_deg, ties.method = 'average'))
    }

  }
  else{
    stop("Either (X,Y) data for nodes is missing, or there is an issue with the (X,Y) data")
  }

}
