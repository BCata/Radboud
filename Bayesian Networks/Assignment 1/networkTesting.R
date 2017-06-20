library (dagitty) # if it doesn't work, first run: install.packages('dagitty', dependencies = TRUE)
library (bnlearn)

# Bayesian Networks --> Assignment 1
# Date: December - 2016
# Authors: Alejandro González Rogel
#           Athena Iakovidi
#           Juraj Susnjara
#
# Description: Code used for testing porpuses.
#     It contains both of the networks proposed in the report and all the
#     functions used for testing their behaviour.
#
#     This is NOT the function to run.

# Load the data

data.file <- "ProjectData.csv"
wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), data.file, sep="/")
my.data <- read.csv(my.data.path,sep=";")
cat("Data loaded correctly")

# #############################################
# Test the structure of the networks (using dagitty)
# #############################################

############### Original graph ###############

g <- 'dag {
Unsecure -> { Weapon Depression }
Age -> { Alcohol Weapon }
Sports -> { Depression Unsecure }
Alcohol -> { Weapon Grades }
Depression -> { Alcohol }
Grades -> { Weapon }
Fight -> { Grades Weapon }
Sex -> { Weapon Grades Fight }
Bullied -> { Depression Fight Unsecure }
Race -> { Fight Grades Bullied }
}'

plot (graphLayout (g) )

# List conditional independencies and perform a test on them
r <-  localTests(g, my.data)

# List problematic connections (with high p value and estimate).
significant = r$p.value<0.05
big = abs(r$estimate)>0.1
print(r[significant & big,][order(-abs(r[significant & big,]$estimate)),])
# plotLocalTestResults(r)

cat ("Finished test on the structure of the original network.")
readline(prompt="Press [enter] to continue")

############### New graph ###############

g <- 'dag {
Unsecure -> { Weapon Depression Alcohol}
Age -> { Alcohol Weapon Bullied}
Sports -> { Depression Unsecure }
Alcohol -> { Weapon Grades }
Depression -> { Alcohol }
Grades -> { Weapon }
Fight -> { Grades Weapon Alcohol Unsecure}
Sex -> { Weapon Grades Fight Depression}
Bullied -> { Depression Fight Unsecure}
Race -> { Fight Grades Bullied Unsecure}
}'

plot (graphLayout (g) )

# List conditional independencies and perform a regression to test on each
# of them.
r <-  localTests(g, my.data, conf.level = 0.95)
significant = r$p.value<0.05
big = abs(r$estimate)>0.1
print(r[significant & big,][order(-abs(r[significant & big,]$estimate)),])


cat ("Finished test on the structure of the proposed network.
     We will start testing the predictions of both networks once the key
     is pressed.")
readline(prompt="Press [enter] to continue")

# #############################################
# Test the predictions of our networks (using bnlearn)
# #############################################

# We need to change the structure of the data first
my.data <- data.frame(lapply(my.data, factor))
print("=========================================")
print("============== Dataset ================")
print("=========================================")

str (my.data)

############### Original graph ###############

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

results.cv = bn.cv(my.data, e , loss = "pred", k=10,
                   loss.args = list(target = "Weapon"), debug=TRUE)

(features <- attributes(results.cv))
mean.BN1 <- features$mean


############### New graph ###############

# "nodes" and "nSize" are the same as for the previos network

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

results.cv = bn.cv(my.data, e , loss = "pred", k=10,
                   loss.args = list(target = "Weapon"))

(features <- attributes(results.cv))
mean.BN2 <- features$mean

str("The mean classification error for the first network was: ")
str(mean.BN1)
str("The mean classification error for the second network was: ")
str(mean.BN2)

print("Tests executed correctly.")
