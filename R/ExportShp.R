#' @import igraph data.table maptools sp
#' @export

export_shp_SpatNet<-function(graph, projstr=CRS("+init=epsg:3395")){

  require(igraph)
  require(data.table)
  require(maptools)
  require(sp)

  if(check_spatial_attribs(graph)){
    ## Create SpatialPoints object containing coordinates
    xV <- SpatialPoints(cbind(V(graph)$X, V(graph)$Y), proj4string = projstr)
    xV <- SpatialPointsDataFrame(cbind(V(graph)$X, V(graph)$Y), data.frame(ID=seq(1:vcount(graph))), proj4string = projstr)

    ## Write vertices to a shapefile
    dir.create("shp")
    writePointsShape(xV, fn="shp/nodes")


    ## Create SpatialLinesDataFrame object describing edges
    edges <- get.edgelist(graph)
    edges <- as.data.table(cbind(edgeNum=1:nrow(edges), v1=edges[,1], v2=edges[,2]))
    lnlst<-c()
    for (i in 1:nrow(edges))
    {
      #print(edges[i]$v1)
      startNode<-edges[i]$v1
      startNode_X<-V(graph)[name==startNode]$X
      startNode_Y<-V(graph)[name==startNode]$Y

      endNode<-edges[i]$v2
      endNode_X<-V(graph)[name==endNode]$X
      endNode_Y<-V(graph)[name==endNode]$Y

      ln<-Lines(Line(cbind(c(startNode_X,endNode_X),c(startNode_Y,endNode_Y))), ID = as.character(edges[i]$edgeNum))
      lnlst<-c(lnlst,ln)

    }
    xE <- SpatialLinesDataFrame(SpatialLines(lnlst,proj4string = CRS("+init=epsg:3395")), data=data.frame(edgeNum=1:nrow(edges)))

    ## Write edges to a shapefile
    writeLinesShape(xE, fn="shp/edges")

  }else{
    stop("Either (X,Y) data for nodes is missing, or there is an error with the (X,Y) data")
  }
}
