#' Calculate Spatial closeness
#'
#' This function allows you calculate spatial closeness of nodes at a particular alpha
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.
#' @param alpha specifies alpha value at which closeness is to be calculted
#' @param rank decides whether to return rank of nodes of raw values. Defaults to TRUE meaning ranks are to be calculated
#' @keywords: spatial clustering
#' @import data.table igraph
#' @export
#' @examples
#'

spat_closeness<-function(graph, alpha, rank=TRUE){
  if(check_spatial_attribs(graph)){
    E(graph)$dist<-add_edge_dist_attrib(graph)

    #Calculate closenness
    V(graph)$close_cost<-closeness(graph, weights = E(graph)$dist+1, normalized = F)  #+1 is done to avoid 0
    V(graph)$close_stren<-closeness(graph, weights = mean(E(graph)$dist)/(E(graph)$dist+1), normalized = F)   #Can be 1/(E(graph)$dist+1) also

    #Normalize closeness 0 and 1
    V(graph)$close_cost = (V(graph)$close_cost-min(V(graph)$close_cost))/(max(V(graph)$close_cost)-min(V(graph)$close_cost))
    V(graph)$close_stren = (V(graph)$close_stren-min(V(graph)$close_stren))/(max(V(graph)$close_stren)-min(V(graph)$close_stren))

    if(alpha<0.0 | alpha>1.0)
    {
      stop("The value of alpha can be [0,1]. Check arguments from_alpha and to_alpha")
    }

    V(graph)$spat_close<-alpha * (V(graph)$close_cost) + (1-alpha) * (V(graph)$close_stren)
    if(!rank){
      return(V(graph)$spat_close) #return raw values
    }
    if(rank)
    {
      return(data.table::frank(-V(graph)$spat_close, ties.method = 'average'))
    }
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
