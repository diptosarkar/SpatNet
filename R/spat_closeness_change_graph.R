#' Graph Closeness Rank change
#'
#' This function creates the graph of spatial closeness rank change between alpha values. Seems to not work if modying only from_alpha or to_alpha
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#' @param tranparency specifies whether lines in the graph are highlighted according to the maximum rank changes. Defaults to TRUE
#' @param from_alpha specifies start alpha value of spatial closeness calculation. Defaults to 0
#' @param to_alpha specifies end alpha value of spatial closeness calculation. Defaults to 1
#' @param step species the interval at which the alpha values are to be calculated. Defaults to 0.05
#' @import igraph data.table ggplot2 directlabels ggbeeswarm
#' @keywords spatial betweenness graph
#' @export
#' @examples
#' spat_closeness_change_graph()

spat_closeness_change_graph<-function(graph, transparency = TRUE, from_alpha = 0, to_alpha = 1, step = 0.05){
  if(check_spatial_attribs(graph)){
    E(graph)$dist<-add_edge_dist_attrib(graph)
    #Calculate closeeenness
    V(graph)$close_cost<-closeness(graph, weights = E(graph)$dist+1, normalized = F)  #+1 is done to avoid 0
    V(graph)$close_stren<-closeness(graph, weights = mean(E(graph)$dist)/(E(graph)$dist+1), normalized = F)   #Can be 1/(E(graph)$dist+1) also

    #Normalize closeness 0 and 1
    V(graph)$close_cost = (V(graph)$close_cost-min(V(graph)$close_cost))/(max(V(graph)$close_cost)-min(V(graph)$close_cost))
    V(graph)$close_stren = (V(graph)$close_stren-min(V(graph)$close_stren))/(max(V(graph)$close_stren)-min(V(graph)$close_stren))


    V(graph)$degree<-degree(graph)

    if(from_alpha<0.0 | to_alpha>1.0)
    {
      stop("The value of alpha can be [0,1]. Check arguments from_alpha and to_alpha")
    }
    for_plot<-data.table(vertex_name=rep(V(graph)$name, length(seq(from_alpha,to_alpha,step))) ,alpha=rep(seq(from_alpha,to_alpha,step),vcount(graph)), close_cost=rep(V(graph)$close_cost, length(seq(from_alpha,to_alpha,step))), close_stren=rep(V(graph)$close_stren,length(seq(from_alpha,to_alpha,step))), deg=rep(V(graph)$degree,length(seq(from_alpha,to_alpha,step))))
    for_plot<-for_plot[,Bet_Comb := alpha * (close_cost) + (1-alpha) * (close_stren)]
    for_plot$vertex_name<-as.factor(for_plot$vertex_name)
    setkey(for_plot,vertex_name)


    #Create ranking of change in closeeenness value for vertices at each alpha value
    for_plot[,alpha_rank:=rank(-Bet_Comb, ties.method = 'average'), by=alpha]

    ##decision table to to find the maximum change in rank for each vertex across all alpha values for colour coding
    dec_dt<-for_plot[,.(min_rank_v=min(alpha_rank), max_rank_v=max(alpha_rank)), by=vertex_name]
    dec_dt[,change_in_rank:=abs(max_rank_v - min_rank_v)]

    #reflect the change in for_plot
    for_plot[,max_rank_change:=dec_dt$change_in_rank[match(for_plot$vertex_name, dec_dt$vertex_name)]]

    rm(dec_dt)
    if(transparency){
      #The following plot utilizes the max change in rank and highlights the vertices that have shown big changes
      plt<-ggplot(data=for_plot, aes(x=alpha, y=alpha_rank, group=vertex_name, color=max_rank_change))
      plt<-plt+geom_line()+geom_point()+geom_dl(aes(label = vertex_name), method = list(dl.combine("first.points", "last.points"), cex = 1.5))+theme_bw()+theme(legend.position="none")+xlab(expression(alpha))+ylab("Alpha Rank of Node's closeeenness")
      plt<-plt+scale_color_gradient2(low='white', high='black')  #The low does not seem to work, for desired color gradient change high
      print(plt)
    }
    if(!transparency){
      plt<-ggplot(data=for_plot, aes(x=alpha, y=alpha_rank, group=vertex_name))
      plt<-plt+geom_line()+geom_point()+geom_dl(aes(label = vertex_name), method = list(dl.combine("first.points", "last.points"), cex = 1.5))+theme_bw()+theme(legend.position="none")+xlab(expression(alpha))+ylab("Alpha Rank of Node's closeeenness")
      print(plt)
    }
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
