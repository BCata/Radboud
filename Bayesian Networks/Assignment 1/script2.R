library (bnlearn)

# Bayesian Networks --> Assignment 1
# Date: December - 2016
# Authors: Alejandro González Rogel
#           Athena Iakovidi
#           Juraj Susnjara

# Get the data

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
my.data <- read.csv(my.data.path,sep=";")

tmp <- data.frame(lapply(my.data, factor))
str (tmp)

# Original network

# Creating the network

nodes <- c("Unsecure", "Weapon", "Age", "Sports", "Alcohol", "Depression",
           "Grades","Fight","Sex","Bullied","Race")

nSize <- length(nodes)

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

e = empty.graph(nodes)
amat(e) = adj
plot(e)

# Create prob. tables
fittedbn <- bn.fit(e, data = tmp)

# print(fittedbn$Unsecure) # For test porpuses.

# Test the network
result <- cpquery(fittedbn, event = (Weapon=="2"), evidence = ( Age=="5"))
str(result)

results.cv = bn.cv(tmp, e , loss = "pred", k=10,
                   loss.args = list(target = "Weapon"), debug = TRUE)

(features <- attributes(results.cv))
mean <- features$mean

# Switching loss to "pred-lw" and then adding "from = "NODES"" in the "loss.args"
# we can calculate the loss when using the nodes that we want. with the sentence
# above, we just use the parent nodes of Weapon.

# Overfitted network

# Creating the network
#
#nodes <- c("Unsecure", "Weapon", "Age", "Sports", "Alcohol", "Depression",
#           "Grades","Fight","Sex","Bullied","Race")
#
#nSize <- length(nodes)
#
#adj = matrix(0L, ncol = nSize, nrow = nSize,
#             dimnames = list(nodes[1:nSize], nodes[1:nSize]))
#
#adj["Unsecure", "Alcohol"] = 1L
#adj["Unsecure", "Weapon"] = 1L
#adj["Unsecure", "Grades"] = 1L
#adj["Unsecure", "Fight"] = 1L
#adj["Age", "Sports"] = 1L
#adj["Age", "Fight"] = 1L
#adj["Age", "Weapon"] = 1L
#adj["Age", "Bullied"] = 1L
#adj["Age", "Depression"] = 1L
#adj["Sports", "Fight"] = 1L
#adj["Sports", "Alcohol"] = 1L
#adj["Sports", "Grades"] = 1L
#adj["Sports", "Depression"] = 1L
#adj["Alcohol", "Grades"] = 1L
#adj["Alcohol", "Weapon"] = 1L
#adj["Alcohol", "Fight"] = 1L
#adj["Depression", "Alcohol"] = 1L
#adj["Depression", "Fight"] = 1L
#adj["Depression", "Grades"] = 1L
#adj["Depression", "Weapon"] = 1L
#adj["Fight", "Grades"] = 1L
#adj["Fight", "Weapon"] = 1L
#adj["Sex", "Grades"] = 1L
#adj["Sex", "Weapon"] = 1L
#adj["Sex", "Fight"] = 1L
#adj["Sex", "Age"] = 1L
#adj["Sex", "Bullied"] = 1L
#adj["Sex", "Sports"] = 1L
#adj["Sex", "Depression"] = 1L
#adj["Bullied", "Depression"] = 1L
#adj["Bullied", "Alcohol"] = 1L
#adj["Bullied", "Fight"] = 1L
#adj["Race", "Grades"] = 1L
#adj["Race", "Alcohol"] = 1L
#adj["Race", "Depression"] = 1L
#adj["Race", "Sports"] = 1L

#e = empty.graph(nodes)
#amat(e) = adj
#plot(e)

# Create prob. tables
#fittedbn <- bn.fit(e, data = tmp)

# print(fittedbn$Unsecure) # For test porpuses.

# Test the network
#result <- cpquery(fittedbn, event = (Weapon=="2"), evidence = ( Age=="1"))
#str(result)
