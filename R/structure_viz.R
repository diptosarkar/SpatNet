#' A trial Function
#'
#' This function allows you to express your love of Hello World
#' @param: igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.
#' @param: resolution specifies the grouping of points for numbering. Defaults to NA and numbering cannot be performed
#' @param: num specifies whether points should be aggregated and numbered. Requires resolution to be specified
#' @keywords: spatial clustering
#' @import data.table ggplot2
#' @export
#' @examples
#'


structure_viz<-function(graph, resolution=FALSE, num=FALSE){
  if(check_spatial_attribs(graph)){

    shortest_path_mat<-as.matrix(distances(graph, weights = NA))

    tbl<-data.table(ID=V(graph)$name, X=V(graph)$X, Y=V(graph)$Y)
    dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)   #This can be sped up using GPU
    dist_mat<-as.matrix(dist_mat) #Convert atomic vector to matrix

    dist_mat<-as.matrix(dist_mat[lower.tri(dist_mat)])  #Keep only lower triangular matrix
    shortest_path_mat<-as.matrix(shortest_path_mat[lower.tri(shortest_path_mat)])
    #The density plots work better if the entire matrix is used i.e. links are counted twice. Otherwise the kernels tend to merge or produce wierd shapes

    shortest_path_list<-as.data.table(melt(shortest_path_mat))
    names(shortest_path_list)[names(shortest_path_list)=="value"]<-"shortest_path"
    shortest_path_list<-shortest_path_list[Var1!=Var2]

    dist_list<-as.data.table(melt(dist_mat))
    names(dist_list)[names(dist_list)=="value"]<-"euc_dist"
    dist_list<-dist_list[Var1!=Var2]

    for_plot<-merge(shortest_path_list, dist_list, by=c("Var1", "Var2"))
    for_plot[,c("Var1", "Var2"):=NULL]  #We do not need var1 and var2 in subsequent steps

    rm(shortest_path_list); rm(dist_list)

    library(ggbeeswarm)
    commonTheme = list(labs(color="Density",fill="Density",
                            x="Euclidean Distance [measurement units]",
                            y="Shortest Path Distance"),
                       theme_bw(),
                       theme(legend.position="none",  #c(0,1),
                             legend.justification=c(0,1)))

    if(resolution==FALSE & num==FALSE){
      ggplot(data=for_plot,aes(x=euc_dist, y=shortest_path))+geom_quasirandom(alpha=0.4)+commonTheme  #geom_beeswarm
    }
    else if(num){
      if(resolution!=FALSE){
        ub<-max(for_plot$euc_dist)+10000;lb<-min(for_plot$euc_dist)
        analysis_scale<-resolution
        breaks = seq(lb, ub, by=analysis_scale)
        midpoints<-function(x, dp=2){
          lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
          upper <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
          return(round(lower+(upper-lower)/2, dp))
        }
        for_plot$euc_dist_classified<-midpoints(cut(for_plot$euc_dist,breaks, right = F))
        for_plot[,count:=.N, by=list(euc_dist_classified,shortest_path)]

        p<-ggplot(data=for_plot,aes(x=euc_dist_classified/1000, y=shortest_path, label=count))+geom_quasirandom(alpha=0.2)+geom_text()+commonTheme  #geom_beeswarm
        #p#+geom_vline(xintercept=max(dist_mat)/1000, linetype="longdash") #Line to denote spatial extent #This can be added later via an additional param
        print(p)

        rm(p);rm(breaks);rm(ub);rm(lb);rm(midpoints)

      }
      else{
        stop("Resolution must be specified for numbering to work")
      }
    }



  }
  else{
      stop("Either X and Y data for nodes if missing or there is something wrong with the X and Y data")
  }
}

