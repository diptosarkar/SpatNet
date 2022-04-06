#' This function creates a density plot for checking distance of connections
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#' @param bins number of bins for the histogram. Defaults to 30
#' @keywords: spatial clustering
#' @import ggplot2
#' @export

check_connection_distance_distribution<-function(graph, bins=30){
  if(check_spatial_attribs(graph)){
    E(graph)$dist<-add_edge_dist_attrib(graph)
    #E(graph)$dist<-E(graph)$dist/convFact  #Can include this to change from whatever units to desired unit by including argument. Similar to check_spatial_clustering()
    qplot(E(graph)$dist, geom = "blank", xlab = "Euclidean Distance of Connection [measurement units]", ylab = "Density")+theme_bw()+geom_histogram(aes(y = ..density..), bins=bins, alpha = 0.7, col="black")+geom_line(aes(y = ..density..), stat = 'density', col="red")
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
