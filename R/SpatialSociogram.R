#' @import igraph data.table
#' @export

spatial_sociogram<-function(graph, v_color='gray', v_lab_color="black", e_width=2){

  require(igraph)
  require(data.table)

  if(check_spatial_attribs(graph)){
    ##Spatial Plot
    layout_spatial<-as.matrix(data.table(x=V(graph)$X, y=V(graph)$Y))
    plot(graph, layout=layout_spatial, vertex.size = 7, vertex.label.cex=1.5, vertex.color=v_color, vertex.label.color=v_lab_color, edge.width=e_width)
  }
  else{
    stop("Either (X,Y) data for nodes is missing, or there is an error with the (X,Y) data")
  }
}
