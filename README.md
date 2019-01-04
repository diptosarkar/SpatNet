SpatNet
=====
Metrics and Visualizations for Spatial Social Networks
-----

The details of the metrics and visualizations can be found in the upcoming paper in IJGIS entitled "Metrics for Characterizing Network Structure and Node Importance in Spatial Social Networks".

The mterics and visualization provided this package are as follows:
1. Network Level
    * Socio-Spatial Network Schema (SS-NS)
    * FLattening Ratio (Upcoming)
2. Node Level
    * SS Tuning parameter for Modified Centality Metrics, namely:
      + SS Degree
      + SS Betweenness
      + SS Closeness

Additionally, this package also contains (Upcoming) the two synthetically generated Spatial Social Networks (SSNs) used in the article. Namely,
1. Poisson Network : Nodes randomly distributed in geographic space, probability of edges decresing exponentially with distance between the nodes
2. Clustered Network : Nodes clustered in geographic space, probability of edges decresing exponentially with distance between the nodes

These networks mimic properties of SSNs consistently reported in literature and we recommend using these as test datasets for SSN metrics.

### Network Requirement
These metrics are desined for SSNs where the X, Y location of the nodes are attached as attributes of the nodes.

The workings of the package are heavily dependednt on the excellent igraph package. The networks thus should in the format of an igraph.

### The documentation of this package is under active development to make it easier to use

## Basic Usage
UPCOMING

library(SpatNet)


## Known Issues
UPCOMING
