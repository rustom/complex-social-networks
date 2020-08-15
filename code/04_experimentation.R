##################################################
## Project: MDT Analysis
## Script purpose: Survey Analysis
## Date: 2019-01-25
## Author: Rustom Ichhaporia - rustomi2@illinois.edu
## Author: Diego Zara - dgomezara@u.northwestern.edu
##################################################

load("data_R/03_ergm.RData")
library(igraph)

edgelist <- invitations[,c("sender", "recipient")]
invitations_adjacency_matrix <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
write.csv(invitations_adjacency_matrix, file = "results/adjacencymatrix.csv")
