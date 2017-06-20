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

si.hiton.pc.dag.1 = si.hiton.pc(my.data, test = "mi")

si.hiton.pc.dag.2 = si.hiton.pc(my.data, test = "mi-sh")

si.hiton.pc.dag.3 = si.hiton.pc(my.data, test = "mi-adf")

par(mfrow = c(1,3))
plot(si.hiton.pc.dag.1, main = "SI-HITON-PC (Chi squared)")
plot(si.hiton.pc.dag.2, main = "SI-HITON-PC (Shrinkage estimator for mutual information)")
plot(si.hiton.pc.dag.3, main = "SI-HITON-PC (Chi squared with adjusted degrees of freedom)")

si.hiton.pc.dag.4 = si.hiton.pc(my.data, alpha = 0.005)

si.hiton.pc.dag.1 = si.hiton.pc(my.data, alpha = 0.01)

si.hiton.pc.dag.2 = si.hiton.pc(my.data, alpha = 0.05)

si.hiton.pc.dag.3 = si.hiton.pc(my.data, alpha = 0.1)


par(mfrow = c(2,2))
plot(si.hiton.pc.dag.4, main = "SI-HITON-PC (Alpha = 0.005)")
plot(si.hiton.pc.dag.1, main = "SI-HITON-PC (Alpha = 0.01)")
plot(si.hiton.pc.dag.2, main = "SI-HITON-PC (Alpha = 0.05)")
plot(si.hiton.pc.dag.3, main = "SI-HITON-PC (Alpha = 0.1)")



