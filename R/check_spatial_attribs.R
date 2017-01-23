#' Validation Function
#'
#' This function checks whether the graph passsed as an argument has X and Y attributes
#' @param: igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#'
#' @keywords: spatial degree graph
#' @references
#' @export
#' @examples
#' spat_degree_change_graph()

library(igraph)

check_spatial_attribs<-function(graph){
  if("X" %in% list.vertex.attributes(graph) && "Y" %in% list.vertex.attributes(graph)){
    if(is.numeric(V(graph)$X) && is.numeric(V(graph)$Y)){
      return(TRUE)
    }
    else{
      stop("There seems to be a problem with (X,Y) data in node attributes")
      return(FALSE)
    }
  }
  else{
    return(FALSE)
    stop("(X,Y) attribute for nodes is missing")
  }
}
