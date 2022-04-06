#' @import data.table ggplot2 spatstat
#' @export

check_spatial_clustering<-function(graph){

  require(data.table)
  require(ggplot2)
  require(spatstat)

  if(check_spatial_attribs(graph)){
     pts<-ppp(V(graph)$X, V(graph)$Y, window =  owin(xrange = c(0, max(V(graph)$X)), yrange = c(0, max(V(graph)$Y)) ) )
    K<-Kest(pts, correction = 'best')

    Kwide.df = data.frame(r = K$r, Kest = K$iso, Ktheo = K$theo)
    Klong.df = reshape2::melt(Kwide.df, id = "r")
    p<-ggplot(Klong.df) + geom_line(aes(x = r, y = value, colour = variable)) +
      ylab(expression(paste(hat(K)))) +
      xlab("Distance (r)") + scale_color_discrete(name = "variable", breaks = c("Kest", "Ktheo"), labels = c("Observed", "Random")) + guides(color = guide_legend(title = NULL)) + theme_bw()

    plot(p)
    rm(K);rm(Klong.df);rm(Kwide.df)
  }
  else{
    stop("Either the (X,Y) data for the nodes is missing, or there is an issue with the (X,Y) data")
  }

}
