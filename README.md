SpatNet
=====
Metrics and Visualizations for Spatial Social Networks
-----

The details of the metrics and visualizations can be found in the upcoming paper in IJGIS entitled "Metrics for Characterizing Network Structure and Node Importance in Spatial Social Networks".

The metrics and visualization provided this package are as follows:
1. Network Level
    * Socio-Spatial Network Schema (SS-NS)
    * FLattening Ratio (Upcoming)
2. Node Level
    * SS Tuning parameter for Modified Centrality Metrics, namely:
      + SS Degree
      + SS Betweenness
      + SS Closeness

Additionally, this package also contains (Upcoming) the two synthetically generated Spatial Social Networks (SSNs) used in the article. Namely,
1. Poisson Network : Nodes randomly distributed in geographic space, probability of edges decreasing exponentially with distance between the nodes
2. Clustered Network : Nodes clustered in geographic space, probability of edges decreasing exponentially with distance between the nodes

These networks mimic properties of SSNs consistently reported in literature and we recommend using these as test datasets for SSN metrics.

### Network Requirement
These metrics are designed for SSNs where the X, Y location of the nodes are attached as attributes of the nodes. There can be only one connected component in the graph.

The workings of the package are heavily dependent on the excellent igraph package. The networks thus should in the format of an igraph.

## Basic Usage
```
library(devtools)
install_github("diptosarkar/SpatNet") #ref='dev' can be added when installing from any other branch (e.g. dev)
library(SocialSpatialNetwork) #Load the package

#Assuming that g is an igraph where nodes have X and Y attributes in projected coordinates

spatial_sociogram(g) #Create spatial sociogram. The nodes will be anchored to their geographic X and Y coordinates.

V(g)$spat_bet<-spat_betweenness(g) #Add spatial Betweenness values at particular \alpha (defaults to 0.5).
spat_betweenness_change_graph(g) #Takes long time if number of nodes is high
#Same as above for degree and closeness

export_shp_SpatNet(g) #Export nodes and edges as shapefile
#If nodes are overlapping (i.e. have same X and Y coords), may want to addd jitter before exporting
V(g)$X<-jitter(V(g)$X, amount = 0)
V(g)$Y<-jitter(V(g)$Y, amount = 0)
spatial_sociogram(g)
export_shp_SpatNet(g)

library(reshape2) #This is needed to structure_viz to work. The package needs to be updated to remove requirement manually importing package here
structure_viz(g)
structure_viz(g, resolution = F, num=F) #TT and FF works. resolutions needs a number. Not sure whether Km or m


```

## Known issues:
Pleas have a look at the issue tracker for known issues. If you can fix them, please go ahead. Your help is much appreciated.
