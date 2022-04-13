#' @import ggplot2
#' @export

check_connection_distance_distribution<-function(graph, bins=30){

  require(ggplot2)

  if(check_spatial_attribs(graph)){
    E(graph)$dist<-add_edge_dist_attrib(graph)
    #E(graph)$dist<-E(graph)$dist/convFact  #Can include this to change from whatever units to desired unit by including argument. Similar to check_spatial_clustering()
    qplot(E(graph)$dist, geom = "blank", xlab = "Euclidean Distance of Connection [m]", ylab = "Density")+theme_bw()+geom_histogram(aes(y = ..density..), bins=bins, alpha = 0.7, col="black")+geom_line(aes(y = ..density..), stat = 'density', col="red")
  }
  else{
    stop("Either (X,Y) data for nodes if missing, or there is an issue with the (X,Y) data")
  }

}
