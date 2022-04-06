#' @import igraph progress crayon data.table
#' @export

k_fulfillment <- function(g, k){

  require(igraph)
  require(progress)
  require(crayon)
  require(data.table)

  #Use distance matrix between every node pair in the network
  ##Its was created previously and stored in dist_mat2
  ##For the package, dist matrix was created out of the network in struct_viz.R

  tbl<-data.table(ID=V(g)$name, X=V(g)$X, Y=V(g)$Y)
  dist_mat<-dist(tbl[,2:3,with=F], method = "euclidean", upper = T, diag = T)   #This can be sped up using GPU
  dist_mat<-as.matrix(dist_mat) #Convert atomic vector to matrix
  #weighted_adj_mat<-as.matrix(get.adjacency(g, attr = NULL)*dist_mat)
  adj_mat<-get.adjacency(g, attr=NULL)
  k_fulfill_list=c()

  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = vcount(g))
  for(z in 1:vcount(g))
  {
    pb$tick()
    r<-dist_mat[z,]  #Extract row
    x<-sort(r, index.return=TRUE)$x
    x_i<-sort(r, index.return=TRUE)$ix  #This is the original index

    #Find nearest-k neighbors
    #print(x[2:(k+1)])  #Entry 1 is the node itself
    #print(x_i[2:(k+1)])

    #Check how many connections exist
    k_cons=0
    for(j in 2:(k+1)){
      #print(adj_mat[z,x_i[j]])
      if(adj_mat[z, x_i[j]]==1)  {
        k_cons=k_cons+1
      }
    }
    k_fullfil=k_cons/k
    #print(k_fullfil)
    k_fulfill_list<-append(k_fulfill_list, k_fullfil)
  }
  return(k_fulfill_list)
}
