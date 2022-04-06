#' Calculate Spatial betweenness
#'
#' This function allows you calculate spatial betweenness of nodes at a particular alpha
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.
#' @param alpha specifies alpha value at which betweenness is to be calculted
#' @param rank decides whether to return rank of nodes of raw values. Defaults to TRUE meaning ranks are to be calculated
#' @keywords: spatial clustering
#' @import data.table igraph
#' @export
#' @examples
#'

spat_betweenness<-function(graph, alpha, rank=TRUE){
  if(check_spatial_attribs(graph)){
    E(graph)$dist<-add_edge_dist_attrib(graph)

    #Calculate betweenness
    V(graph)$betw_cost<-betweenness(graph, directed = F, weights = E(graph)$dist+1, normalized = F)  #+1 is done to avoid 0
    V(graph)$betw_stren<-betweenness(graph, directed = F, weights = mean(E(graph)$dist)/(E(graph)$dist+1), normalized = F)   #Can be 1/(E(graph)$dist+1) also

    #Normalize between 0 and 1
    V(graph)$betw_cost = (V(graph)$betw_cost-min(V(graph)$betw_cost))/(max(V(graph)$betw_cost)-min(V(graph)$betw_cost))
    V(graph)$betw_stren = (V(graph)$betw_stren-min(V(graph)$betw_stren))/(max(V(graph)$betw_stren)-min(V(graph)$betw_stren))

    if(alpha<0.0 | alpha>1.0)
    {
      stop("The value of alpha can be [0,1]. Check arguments from_alpha and to_alpha")
    }

    V(graph)$spat_betw<-alpha * (V(graph)$betw_cost) + (1-alpha) * (V(graph)$betw_stren)
    if(!rank){
      return(V(graph)$spat_betw) #return raw values
    }
    if(rank)
    {
      return(data.table::frank(-V(graph)$spat_betw, ties.method = 'average'))
    }
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
