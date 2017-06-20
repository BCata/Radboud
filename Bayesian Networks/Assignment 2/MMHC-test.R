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


mmhc.dag.1 = moral(mmhc(my.data, restart=0, perturb=1))
mmhc.dag.2 = moral(mmhc(my.data, restart=5, perturb=1))
mmhc.dag.3 = moral(mmhc(my.data, restart=10, perturb=1))
mmhc.dag.4 = moral(mmhc(my.data, restart=20, perturb=1))


par(mfrow = c(2,2))
plot(mmhc.dag.1, main = "MMHC (Restart = 0)")
plot(mmhc.dag.2, main = "MMHC (Restart = 5)")
plot(mmhc.dag.3, main = "MMHC (Restart = 10)")
plot(mmhc.dag.4, main = "MMHC (Restart = 20)")


mmhc.dag.1 = moral(mmhc(my.data, restart=2, perturb=1))
mmhc.dag.2 = moral(mmhc(my.data, restart=2, perturb=2))
mmhc.dag.3 = moral(mmhc(my.data, restart=2, perturb=4))
mmhc.dag.4 = moral(mmhc(my.data, restart=2, perturb=8))


par(mfrow = c(2,2))
plot(mmhc.dag.1, main = "MMHC (Restart = 2 & Perturb=1)")
plot(mmhc.dag.2, main = "MMHC (Restart = 2 & Perturb=2)")
plot(mmhc.dag.3, main = "MMHC (Restart = 2 & Perturb=4)")
plot(mmhc.dag.4, main = "MMHC (Restart = 2 & Perturb=8)")


# mmhc.dag.1 = moral(mmhc(my.data, alpha = 0.5, restart=2, perturb=1))
# mmhc.dag.2 = moral(mmhc(my.data, alpha = 0.5, restart=2, perturb=2))
# mmhc.dag.3 = moral(mmhc(my.data, alpha = 0.5, restart=2, perturb=4))
# mmhc.dag.4 = moral(mmhc(my.data, alpha = 0.5, restart=2, perturb=8))
# 
# 
# par(mfrow = c(2,2))
# plot(mmhc.dag.1, main = "MMHC (Restart = 2 & Perturb=1)")
# plot(mmhc.dag.2, main = "MMHC (Restart = 2 & Perturb=2)")
# plot(mmhc.dag.3, main = "MMHC (Restart = 2 & Perturb=4)")
# plot(mmhc.dag.4, main = "MMHC (Restart = 2 & Perturb=8)")


mmhc.best.dag = moral(mmhc(my.data, restart=2, perturb=8))
plot(mmhc.best.dag, main = "MMHC (Restart = 1000 & Perturb=200)")

