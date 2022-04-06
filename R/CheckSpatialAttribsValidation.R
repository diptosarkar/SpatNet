#' @import data.table igraph
#' @export


check_spatial_attribs<-function(graph){

  require(igraph)
  require(data.table)

  if("X" %in% igraph::list.vertex.attributes(graph) & "Y" %in% igraph::list.vertex.attributes(graph)){
    if(is.numeric(V(graph)$X) && is.numeric(V(graph)$Y)){   #This returns TRUE if one is NA
      return(TRUE)
    }
    else{
      stop("There seems to be a problem with (X,Y) data in node attributes")
      return(FALSE)
    }
  }
  else{
    return(FALSE)
    stop("The (X,Y) attribute data for the nodes is missing")
  }
}
