library (bnlearn)

# Bayesian Networks --> Assignment 1
# Date: December - 2016
# Authors: Alejandro González Rogel
#           Athena Iakovidi
#           Juraj Susnjara

######################################
# The following scripts instanciates the final network of our assignment
# for the course Bayesian Networks.
# A picture of the network and it's nodes can be found in the report.
#
# The fitted network has the name "fittedbn".
#
# After running the script, you will be able to run queries on the 
# bayesian network. The following are just a few examples on how to
# do it:
# 
# The query would have the following structure:
#
# result <- cpquery(fittedbn, event = (Weapon=="VALUE"), evidence = (AVAILABLE_DATA))
# Where "VALUE" should be "2" to predict the probability of a teenager carring
# a weapon to school and AVAILABLE_DATA should be a list of all the information
# available we have about the teenager we are testing the network against.
#
# EXAMPLES:
# result <- cpquery(fittedbn, event = (Weapon=="2"), evidence = (Sex=="1"))
# Probability of carrying a weapon by a female teenager.
# result = 0.0191
#
# result <- cpquery(fittedbn, event = (Weapon=="2"), evidence = (Sex=="1" & Unsecure=="2"))
# Probability of carrying a weapon by an unsecure female teenager
# result = 0.084


# 
# Load the data.
# Remember to keep this script in the same directory as the data (ProjectData.csv)
#

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
my.data <- read.csv(my.data.path,sep=";")
my.data <- data.frame(lapply(my.data, factor))

#
# Creating a network.
#
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
# NEW CONNECTIONS ADDED
adj["Fight", "Unsecure"] = 1L
adj["Age", "Bullied"] = 1L
adj["Unsecure", "Alcohol"] = 1L
adj["Age", "Depression"] = 1L

e = empty.graph(nodes)
amat(e) = adj
plot(e)

fittedbn <- bn.fit(e, data = my.data) #probability tables

print("The network has been created.")

