#' A trial Function
#'
#' This function creates a graph for iterative Ripley's K function to check for spatial clusetring of nodes
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#' @keywords: spatial clustering
#' @import data.table ggplot2
#' @importFrom spatstat ppp
#' @importFrom spatstat Kest
#' @export
#' @examples
#' check_spatial_clustering()


check_spatial_clustering<-function(graph){
  if(check_spatial_attribs(graph)){
    pts<-ppp(V(graph)$X, V(graph)$Y, window =  owin(xrange = c(0, max(V(graph)$X)), yrange = c(0, max(V(graph)$Y)) ) )
    K<-Kest(pts, correction = 'best')

    m2km = 1   #Can make this a argument for conversion from whatever units the data is to Km
    Kwide.df = data.frame(r = K$r*m2km, Kest = K$iso, Ktheo = K$theo)
    Klong.df = melt(Kwide.df, id = "r")
    p<-ggplot(Klong.df) + geom_line(aes(x = r, y = value, colour = variable)) +
      ylab(expression(paste(hat(K)))) +
      xlab("Distance (r)") + scale_color_discrete(name = "variable", breaks = c("Kest", "Ktheo"), labels = c("Observed", "Random")) + guides(color = guide_legend(title = NULL)) + theme_bw()

    plot(p)
    rm(K);rm(Klong.df);rm(Kwide.df);rm(m2km)
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
