#' Graph Degree Rank change
#'
#' This function creates the graph of spatial degree rank change between alpha values. Seems to not work if modying only from_alpha or to_alpha
#' @param graph igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node
#' @param tranparency specifies whether lines in the graph are highlighted according to the maximum rank changes. Defaults to TRUE
#' @param from_alpha specifies start alpha value of spatial degree calculation. Defaults to 0
#' @param to_alpha specifies end alpha value of spatial degree calculation. Defaults to 1
#' @param step species the interval at which the alpha values are to be calculated. Defaults to 0.05
#' @import igraph data.table ggplot2 directlabels ggbeeswarm
#' @keywords spatial degree graph
#' @export
#' @examples
#' spat_degree_change_graph()



spat_degree_change_graph<-function(graph, transparency = TRUE, from_alpha = 0, to_alpha = 1, step = 0.05){
  if(check_spatial_attribs(graph)){
    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)  #Can be sped up with GPU
    dist_mat2<-as.matrix(dist_mat)  #Store it in matrix format for craeting graph later
    dist_mat2<-apply(dist_mat2, 1:2, function(x){if(x!=0) x else 1})  #Since a multiple nodes can be in the same spot, the dist is 0. Convert 0 to 1 so that when it is multiplied with the adjascency matrix in a few steps, you don't get 0s leading to removal of edges.
    weighted_adj_mat<-dist_mat2 * as_adjacency_matrix(graph, sparse = F)

    deg_dt<-data.table(vertex_name=V(graph)$name)

    deg_dt$degree_benefit<-apply(weighted_adj_mat, 1, sum)   #weighted adj mat has benefits
    #deg_dt$degree_benefit<-deg_dt$degree_benefit/1000 #In Km

    #Normalize between 0-1
    deg_dt$degree_benefit = (deg_dt$degree_benefit-min(deg_dt$degree_benefit))/(max(deg_dt$degree_benefit)-min(deg_dt$degree_benefit))

    weighted_adj_mat2<-apply(weighted_adj_mat, 1:2, function(x){if(x!=0) 1/x else x})  #Change to 1000/x for KM

    deg_dt$degree_cost<-apply(weighted_adj_mat2, 1, sum)

    #Normalize between 0-1
    deg_dt$degree_cost = (deg_dt$degree_cost-min(deg_dt$degree_cost))/(max(deg_dt$degree_cost)-min(deg_dt$degree_cost))

    V(graph)$degree<-degree(graph, normalized = T)
    V(graph)$deg_cost<-deg_dt$degree_cost
    V(graph)$deg_stren<-deg_dt$degree_benefit

    rm(deg_dt)

    if(from_alpha<0.0 | to_alpha>1.0)
    {
      stop("The value of alpha can be [0,1]. Check arguments from_alpha and to_alpha")
    }
    for_plot<-data.table(vertex_name=rep(V(graph)$name, length(seq(from_alpha,to_alpha,step))) ,alpha=rep(seq(from_alpha,to_alpha,step),vcount(graph)), deg_cost=rep(V(graph)$deg_cost, length(seq(from_alpha,to_alpha,step))), deg_stren=rep(V(graph)$deg_stren,length(seq(from_alpha,to_alpha,step))), deg=rep(V(graph)$degree,length(seq(from_alpha,to_alpha,step))))
    for_plot<-for_plot[,Deg_Comb := alpha * (deg_cost) + (1-alpha) * (deg_stren)]
    for_plot$vertex_name<-as.factor(for_plot$vertex_name)
    #for_plot$alpha<-as.factor(for_plot$alpha)
    setkey(for_plot,vertex_name)


    #Create ranking of change in betweenness value for vertices at each alpha value
    for_plot[,alpha_rank:=rank(-Deg_Comb, ties.method = 'average'), by=alpha]

    ##decision table to to find the maximum change in rank for each vertex across all alpha values for colour coding
    dec_dt<-for_plot[,.(min_rank_v=min(alpha_rank), max_rank_v=max(alpha_rank)), by=vertex_name]
    dec_dt[,change_in_rank:=abs(max_rank_v - min_rank_v)]

    #reflect the change in for_plot
    for_plot[,max_rank_change:=dec_dt$change_in_rank[match(for_plot$vertex_name, dec_dt$vertex_name)]]

    rm(dec_dt)
    #print(for_plot)

    if(transparency){
      #The following plot utilizes the max change in rank and highlights the vertices that have shown big changes
      plt<-ggplot(data=for_plot, aes(x=alpha, y=alpha_rank, group=vertex_name, color=max_rank_change))
      plt<-plt+geom_line()+geom_point()+geom_dl(aes(label = vertex_name), method = list(dl.combine("first.points", "last.points"), cex = 0.8))+theme_bw()+theme(legend.position="none")+xlab(expression(alpha))+ylab("Alpha Rank of Node's Degree")
      plt+scale_color_gradient2(low='blue', high='black')  #The low does not seem to work, for desired color gradient change high
    }
    else if(!transparency){
      plt<-ggplot(data=for_plot, aes(x=alpha, y=alpha_rank, group=vertex_name))
      plt<-plt+geom_line()+geom_point()+geom_dl(aes(label = vertex_name), method = list(dl.combine("first.points", "last.points"), cex = 0.8))+theme_bw()+theme(legend.position="none")+xlab(expression(alpha))+ylab("Alpha Rank of Node's Degree")
      plt
    }
  }
  else{
    stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }

}
