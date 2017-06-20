library(bnlearn)

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
my.data <- read.csv(my.data.path,sep=";")
my.data <- data.frame(lapply(my.data, factor))

#NOTE: possible to use prior information of data using arc whitelisting and blacklisting
#check this paper https://arxiv.org/pdf/0908.3817v2.pdf

#Some results of algorithms can be strange and needs to be changed, eg. hc algorithm results
#in network which contains Bullied -> Race connection which doesn't make sense

nodes <- c("Unsecure", "Weapon", "Age", "Sports", "Alcohol", "Depression",
           "Grades","Fight","Sex","Bullied","Race")

nSize <- length(nodes)

#hill-climbing (score-based)
#default scoring using BIC (Bayesian Information Criterion)
#if scoring used in algorithm is set to log-likelihood than results are overconnected networks but with
#better loglikelihood score
hc.dag = hc(my.data)
hc.loglik.score = score(hc.dag, my.data, type = "loglik")
hc.cv = bn.cv(my.data, hc.dag , loss = "pred", k=10,
                   loss.args = list(target = "Weapon"))
hc.features <- attributes(hc.cv)
hc.mean.cv <- hc.features$mean

print("HC loglikelihood score:")
print(hc.loglik.score)
print("HC expected loss mean:")
print(hc.mean.cv)
print("")

#tabu search (score-based)
tabu.dag = tabu(my.data)
tabu.loglik.score = score(tabu.dag, my.data, type = "loglik")
tabu.cv = bn.cv(my.data, tabu.dag , loss = "pred", k=10,
              loss.args = list(target = "Weapon"))
tabu.features <- attributes(tabu.cv)
tabu.mean.cv <- tabu.features$mean
print("Tabu search loglikelihood score:")
print(tabu.loglik.score)
print("Tabu expected loss mean:")
print(tabu.mean.cv)
print("")

#PC )
si.hiton.pc.dag = si.hiton.pc(my.data)
# si.hiton.pc.loglik.score = score(si.hiton.pc.dag, my.data, type = "loglik")
# si.hiton.pc.cv = bn.cv(my.data, si.hiton.pc.dag , loss = "pred", k=10,
#                loss.args = list(target = "Weapon"))
# si.hiton.pc.features <- attributes(si.hiton.pc.cv)
# si.hiton.pc.mean.cv <- si.hiton.pc.features$mean
# print("PC search loglikelihood score:")
# print(si.hiton.pc.loglik.score)
# print("Tabu expected loss mean:")
# print(si.hiton.pc.mean.cv)
print("")

#compare graphs by ploting them side by side
par(mfrow = c(1,2))
plot(hc.dag, main = "Hill-climbing")
plot(tabu.dag, main = "Tabu search")

#our manualy created graph from assignment 1
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
org.loglik.score = score(org.dag, my.data, type = "loglik")
org.cv = bn.cv(my.data, org.dag , loss = "pred", k=10,
              loss.args = list(target = "Weapon"))
org.features <- attributes(org.cv)
org.mean.cv <- org.features$mean

print("Original graph loglikelihood score:")
print(org.loglik.score)
print("Original graph expected loss mean:")
print(org.mean.cv)
print("")

#random network just for comparison

adj = matrix(0L, ncol = nSize, nrow = nSize,
             dimnames = list(nodes[1:nSize], nodes[1:nSize]))

adj["Unsecure", "Weapon"] = 1L
adj["Unsecure", "Sports"] = 1L
adj["Age", "Alcohol"] = 1L
adj["Age", "Grades"] = 1L
adj["Sports", "Bullied"] = 1L
adj["Sports", "Depression"] = 1L
adj["Alcohol", "Grades"] = 1L
adj["Alcohol", "Weapon"] = 1L
adj["Depression", "Alcohol"] = 1L
adj["Fight", "Grades"] = 1L
adj["Fight", "Weapon"] = 1L
adj["Sex", "Grades"] = 1L
adj["Sex", "Alcohol"] = 1L
adj["Sex", "Fight"] = 1L
adj["Bullied", "Depression"] = 1L
adj["Race", "Sports"] = 1L
adj["Race", "Age"] = 1L

rand.dag = empty.graph(nodes)
amat(rand.dag) = adj
rand.loglik.score = score(rand.dag, my.data, type = "loglik")
rand.cv = bn.cv(my.data, rand.dag , loss = "pred", k=10,
               loss.args = list(target = "Weapon"))
rand.features <- attributes(rand.cv)
rand.mean.cv <- rand.features$mean

print("Random graph loglikelihood score:")
print(rand.loglik.score)
print("Random graph expected loss mean:")
print(rand.mean.cv)
print("")

