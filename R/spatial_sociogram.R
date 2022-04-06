#' Plot Spatial Sociogram
#'
#' This function creates a sociogram where the relative position of the nodes with respect to each other are preserved
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#' @param v_color color of the vertices. Default=gray
#' @param v_lab_color color of the vertex labels. Default=black
#' @param e_width edge widths. Default=2
#'
#' @keywords: spatial sociogram
#' @import igraph data.table
#' @export
#' @examples
#'


spatial_sociogram<-function(graph, v_color='gray', v_lab_color="black", e_width=2){
  if(check_spatial_attribs(graph)){
    ##Spatial plot
    layout_spatial<-as.matrix(data.table(x=V(graph)$X, y=V(graph)$Y))
    plot(graph, layout=layout_spatial, vertex.size = 7, vertex.label.cex=1.5, vertex.color=v_color, vertex.label.color=v_lab_color, edge.width=e_width)
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }
}
