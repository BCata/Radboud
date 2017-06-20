library(bnlearn)
library(igraph)

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
my.data <- read.csv(my.data.path,sep=";")
my.data <- data.frame(lapply(my.data, factor))


nodes <- c("Unsecure", "Weapon", "Age", "Sports", "Alcohol", "Depression",
           "Grades","Fight","Sex","Bullied","Race")

nSize <- length(nodes)

#tabu search (score-based)
#default scoring using BIC (Bayesian Information Criterion)
#if scoring used in algorithm is set to log-likelihood than results are overconnected networks but with
#better loglikelihood score
counter <- 1
graphs <- c()
ntests <- c()
k <- c()
tabu.size <- c()
for(i in seq(1, 201, 5)) {
  mmhc.dag = mmhc(my.data, maximize.args=c(tabu=i))
  graphs[counter] <- mmhc.dag
  ntests[counter] <- mmhc.dag$learning$ntests
  k[counter] <- mmhc.dag$learning$args$k
  tabu.size[counter] <- i
  counter <- counter + 1
}

moral.mmhc = moral(mmhc.dag)

#SI-HITON-PC
si.hiton.pc.dag = si.hiton.pc(my.data)

plot(si.hiton.pc.dag)

#######################
######################

adj1 = amat(moral.mmhc)
connections1 = sum(adj1==1)/2 # Because matrixes are symetric
adj2 = amat(si.hiton.pc.dag)
connections2 = sum(adj1==1)/2 # Because matrixes are symetric
sim = 0

## Check how many edges are different

tmp = adj1 + adj2
dissimilarities = sum(tmp==1)/2 # Because matrixes are symetric

## Check how many edges are equal
dissimilarities = sum(tmp==2)/2 # Because matrixes are symetric


## Check for mean lenght distance between nodes

tmp1 = graph_from_adjacency_matrix(adj1, mode = "undirected")
tmp2 = graph_from_adjacency_matrix(adj2, mode = "undirected")

meanDist1 = mean_distance(tmp1, directed=FALSE, unconnected = TRUE)
meanDist2 = mean_distance(tmp2, directed=FALSE, unconnected = TRUE)


######################
######################

#compare graphs by ploting them side by side
par(mfrow = c(1,2))
plot(moral.mmhc, main = "MMHC (Moralized)")
plot(si.hiton.pc.dag, main = "Semi-interleaved HITON-PC")

# #our manualy created graph from assignment 1
# adj = matrix(0L, ncol = nSize, nrow = nSize,
#              dimnames = list(nodes[1:nSize], nodes[1:nSize]))
# 
# adj["Unsecure", "Depression"] = 1L
# adj["Unsecure", "Weapon"] = 1L
# adj["Age", "Alcohol"] = 1L
# adj["Age", "Weapon"] = 1L
# adj["Sports", "Unsecure"] = 1L
# adj["Sports", "Depression"] = 1L
# adj["Alcohol", "Grades"] = 1L
# adj["Alcohol", "Weapon"] = 1L
# adj["Depression", "Alcohol"] = 1L
# adj["Fight", "Grades"] = 1L
# adj["Fight", "Weapon"] = 1L
# adj["Sex", "Grades"] = 1L
# adj["Sex", "Weapon"] = 1L
# adj["Sex", "Fight"] = 1L
# adj["Bullied", "Depression"] = 1L
# adj["Bullied", "Unsecure"] = 1L
# adj["Bullied", "Fight"] = 1L
# adj["Race", "Grades"] = 1L
# adj["Race", "Bullied"] = 1L
# adj["Race", "Fight"] = 1L
# adj["Grades", "Weapon"] = 1L
# adj["Fight", "Unsecure"] = 1L
# adj["Age", "Bullied"] = 1L
# adj["Unsecure", "Alcohol"] = 1L
# adj["Age", "Depression"] = 1L
# 
# org.dag = empty.graph(nodes)
# amat(org.dag) = adj
# org.loglik.score = score(org.dag, my.data, type = "loglik")
# org.cv = bn.cv(my.data, org.dag , loss = "pred", k=10,
#               loss.args = list(target = "Weapon"))
# org.features <- attributes(org.cv)
# org.mean.cv <- org.features$mean
# 
# print("Original graph loglikelihood score:")
# print(org.loglik.score)
# print("Original graph expected loss mean:")
# print(org.mean.cv)
# print("")
# 
# #random network just for comparison
# 
# adj = matrix(0L, ncol = nSize, nrow = nSize,
#              dimnames = list(nodes[1:nSize], nodes[1:nSize]))
# 
# adj["Unsecure", "Weapon"] = 1L
# adj["Unsecure", "Sports"] = 1L
# adj["Age", "Alcohol"] = 1L
# adj["Age", "Grades"] = 1L
# adj["Sports", "Bullied"] = 1L
# adj["Sports", "Depression"] = 1L
# adj["Alcohol", "Grades"] = 1L
# adj["Alcohol", "Weapon"] = 1L
# adj["Depression", "Alcohol"] = 1L
# adj["Fight", "Grades"] = 1L
# adj["Fight", "Weapon"] = 1L
# adj["Sex", "Grades"] = 1L
# adj["Sex", "Alcohol"] = 1L
# adj["Sex", "Fight"] = 1L
# adj["Bullied", "Depression"] = 1L
# adj["Race", "Sports"] = 1L
# adj["Race", "Age"] = 1L
# 
# rand.dag = empty.graph(nodes)
# amat(rand.dag) = adj
# rand.loglik.score = score(rand.dag, my.data, type = "loglik")
# rand.cv = bn.cv(my.data, rand.dag , loss = "pred", k=10,
#                loss.args = list(target = "Weapon"))
# rand.features <- attributes(rand.cv)
# rand.mean.cv <- rand.features$mean
# 
# print("Random graph loglikelihood score:")
# print(rand.loglik.score)
# print("Random graph expected loss mean:")
# print(rand.mean.cv)
# print("")

