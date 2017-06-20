library(bnlearn)
library(igraph)

# Bayesian Networks --> Assignment 2
# Date: January - 2016
# Authors: Alejandro GonzĂˇlez Rogel
#           Athena Iakovidi
#           Juraj Susnjara
#
# Description: 
# This script will compare moralized output of MMHC algorithm, output of SI-HITON-PC and
# moralized version of our manually created graph made in Assignment 1.
#
# Firstly, script will create/calculate all necessary graphs, then it will plot them
# and show comparison of each two graphs.
# User is required to press ENTER after each comparison to proceed to next one.

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
my.data <- read.csv(my.data.path,sep=";")
my.data <- data.frame(lapply(my.data, factor))


nodes <- c("Unsecure", "Weapon", "Age", "Sports", "Alcohol", "Depression",
           "Grades","Fight","Sex","Bullied","Race")

nSize <- length(nodes)

# Moralized result of MMHC algorithm
mmhc.dag = moral(mmhc(my.data, restart=10, perturb=200))

# Result of SI-HITON-PC algorithm
si.hiton.pc.dag = si.hiton.pc(my.data, alpha = 0.05)

# Manually created graph from Assignment 1 (we moralized it this time in order to compare
# with other two graphs)
adj = matrix(0L, ncol = nSize, nrow = nSize,
             dimnames = list(nodes[1:nSize], nodes[1:nSize]))
adj["Unsecure", "Depression"] = 1L
adj["Unsecure", "Weapon"] = 1L
adj["Age", "Alcohol"] = 1L
adj["Age", "Weapon"] = 1L
adj["Sports", "Unsecure"] = 1L
adj["Sports", "Depression"] = 1L
adj["Alcohol", "Grades"] = 1L
adj["Alcohol", "Weapon"] = 1L
adj["Depression", "Alcohol"] = 1L
adj["Fight", "Grades"] = 1L
adj["Fight", "Weapon"] = 1L
adj["Sex", "Grades"] = 1L
adj["Sex", "Weapon"] = 1L
adj["Sex", "Fight"] = 1L
adj["Bullied", "Depression"] = 1L
adj["Bullied", "Unsecure"] = 1L
adj["Bullied", "Fight"] = 1L
adj["Race", "Grades"] = 1L
adj["Race", "Bullied"] = 1L
adj["Race", "Fight"] = 1L
adj["Grades", "Weapon"] = 1L
adj["Fight", "Unsecure"] = 1L
adj["Age", "Bullied"] = 1L
adj["Unsecure", "Alcohol"] = 1L
adj["Age", "Depression"] = 1L

org.dag = empty.graph(nodes)
amat(org.dag) = adj
org.dag = moral(org.dag)

###################### Compare SI-HITON-PC and MMHC ########################

par(mfrow = c(1,2))
cat("####### Comparing SI-HITON-PC and MMHC #######\n")
plot(mmhc.dag, main = "MMHC (Restart = 10 & Perturb=200)")
plot(si.hiton.pc.dag, main = "SI-HITON-PC (Alpha = 0.05)")

## Calculate number of connections in each graph
adj.mmhc = amat (mmhc.dag)
connections.mmhc = sum(adj.mmhc==1)/2 # Because matrixes are symetric
adj.si.hiton.pc = amat(si.hiton.pc.dag)
connections.si.hiton.pc = sum(adj.si.hiton.pc==1)/2 # Because matrixes are symetric

cat("Edges MMHC: ", connections.mmhc, "\nEdges SI-HITON-PC: ", connections.si.hiton.pc)

## Check how many edges are different
tmp = adj.mmhc + adj.si.hiton.pc
dissimilarities = sum(tmp==1)/2 # Because matrixes are symetric
cat("\nNumber of different edges between graphs: ", dissimilarities)

## Check for mean lenght distance between nodes
tmp1 = graph_from_adjacency_matrix(adj.mmhc, mode = "undirected")
tmp2 = graph_from_adjacency_matrix(adj.si.hiton.pc, mode = "undirected")
meanDist.mmhc = mean_distance(tmp1, directed=FALSE, unconnected = TRUE)
meanDist.si.hiton.pc = mean_distance(tmp2, directed=FALSE, unconnected = TRUE)

cat("\nMean length distance MMHC: ", meanDist.mmhc)
cat("\nMean length distance SI-HITON-PC: ", meanDist.si.hiton.pc)
readline(prompt="Press [enter] to continue")



###################### Compare MMHC and Manually created graph ########################

par(mfrow = c(1,2))
cat("####### Comparing MMHC and manually created graph #######\n")
plot(mmhc.dag, main = "MMHC (Restart = 10 & Perturb=200)")
plot(org.dag, main = "Manually created graph")

## Calculate number of connections in each graph
adj.mmhc = amat (mmhc.dag)
connections.mmhc = sum(adj.mmhc==1)/2 # Because matrixes are symetric
adj.org = amat(org.dag)
connections.org = sum(adj.org==1)/2 # Because matrixes are symetric

cat("Edges MMHC: ", connections.mmhc, "\nEdges Manual: ", connections.org)

## Check how many edges are different
tmp = adj.mmhc + adj.org
dissimilarities = sum(tmp==1)/2 # Because matrixes are symetric
cat("\nNumber of different edges between graphs: ", dissimilarities)

## Check for mean lenght distance between nodes
tmp1 = graph_from_adjacency_matrix(adj.mmhc, mode = "undirected")
tmp2 = graph_from_adjacency_matrix(adj.org, mode = "undirected")
meanDist.mmhc = mean_distance(tmp1, directed=FALSE, unconnected = TRUE)
meanDist.org = mean_distance(tmp2, directed=FALSE, unconnected = TRUE)

cat("\nMean length distance MMHC: ", meanDist.mmhc)
cat("\nMean length distance Manual: ", meanDist.org)
readline(prompt="Press [enter] to continue")



###################### Compare SI-HITON-PC and Manually created graph ########################

par(mfrow = c(1,2))
cat("####### Comparing MMHC and manually created graph #######\n")
plot(si.hiton.pc.dag, main = "SI-HITON-PC (Restart = 10 & Perturb=200)")
plot(org.dag, main = "Manually created graph")

cat("Edges SI-HITON-PC: ", connections.si.hiton.pc, "\nEdges Manual: ", connections.org)

## Check how many edges are different
tmp = adj.si.hiton.pc + adj.org
dissimilarities = sum(tmp==1)/2 # Because matrixes are symetric
cat("\nNumber of different edges between graphs: ", dissimilarities)

cat("\nMean length distance SI-HITON-PC: ", meanDist.si.hiton.pc)
cat("\nMean length distance Manual: ", meanDist.org)

