#' @import igraph progress crayon data.table
#' @export

flatting_ratio <- function(g, niter=10000){

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

  alt_mat<-dist_mat  #This will hold the weighted spatial adjacency table for the alternate graph

  #For each node find degree
  deg_V<-degree(g)
  n<-vcount(g)

  g_alt<-graph.empty(n=vcount(g), directed = F)

  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = niter)
  for(z in 1:niter)
  {
    pb$tick()
    #cat("Iteration number: ", z, "\n")
    i<-sample(1:vcount(g), 1)  #Pick a random node
    #cat("iterating node no = ", i, "\n")
    g_int_deg<-degree(g_alt)
    r<-dist_mat[i,]  #Extract row
    ##cat("Current Row", r, "\n")
    x<-sort(r) #sort values in asc order
    ##cat("Sorted Row", x, "\n")
    pref_list2<-match(x, r)
    #cat("Original Preference Order", pref_list2, "\n")
    pref_list<-order(r)  #This takes care of ties
    #cat("Preference Order", pref_list, "\n")
    j=g_int_deg[i]  #Some connections may have already been made to this node. So, cannot start from 0
    #cat("Degree of node i before iteration start: ", j, "\n")
    #cat("Max Degree of node i: ", deg_V[i], "\n")
    pref_list<- pref_list [! pref_list %in% i]  #Remove the node itself from the preference list as we don't want self loops.
    #cat("Preference Order after delete:", pref_list, "\n")
    k=1
    while(j<deg_V[i])
    {
      #cat("Current deg of node i", j, "\n")
      #Attempt connection. Check degree
      while(k<=length(pref_list))
      {
        att_v<-pref_list[k]
        #cat("Currently trying connection with: ", att_v, "\n")
        #cat("Degree limit of attempted connection node: ", deg_V[att_v], "\n")
        #cat("Degree attempted connection node: ", g_int_deg[att_v], "\n")
        exist_connect<-are.connected(g_alt, att_v, i)  #TO stop multi-edges between nodes
        if(g_int_deg[att_v]<deg_V[att_v]  && exist_connect==FALSE)  #Thus conections are made if there are free degrees and an edge already does not exist between the 2 nodes
        {
          #cat("Connection successful!", "\n")
          #  Connect successfully
          g_alt<-add_edges(g_alt,c(i, att_v))
          k<-k+1  #Try the next prefered node in next iteration
          j<-j+1  #The degree of current node increases by 1  #This line may need to be connected
          #cat("j updated to", j, "\n")
          break
        }
        else
          #If not go to next item in list
        {
          #cat("Connection Unsuccessful!", "Try next...", "\n")
          k=k+1
        }
      }
      #cat("Firing the j update to show that degree has been updated", "\n")
      j=j+1
    }
    #print("\n\n\n")

  }

  #Jitter
  ##Add noise to to co-ods to stop overlap
  #V(g)$X<-jitter(V(g)$X, factor = 50)
  #V(g)$Y<-jitter(V(g)$Y, factor = 50)

  #Annotate new graph
  V(g_alt)$name<-V(g)$name
  V(g_alt)$X<-V(g)$X
  V(g_alt)$X<-V(g)$Y

  #Check degree constraint
  cat(blue("Diagonistics: \n"))
  cat("Edge count in original network: ", ecount(g))
  cat("\nEdge count in alternate network: ", ecount(g_alt), "\n")

  degree(g)
  degree(g_alt)

  #Add weights
  alt_mat<-as.matrix(get.adjacency(g_alt)*dist_mat)
  weighted_adj_mat<-as.matrix(get.adjacency(g, attr = NULL)*dist_mat)

  #Network flattening ratio
  network_flattening_ratio<-sum(rowSums(alt_mat))/sum(rowSums(weighted_adj_mat))
  return(network_flattening_ratio)  #We can argue the closer it is to 1, the more spatially efficient the network is
}
