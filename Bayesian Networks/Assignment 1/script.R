library (dagitty) # if it doesn't work, first run: install.packages('dagitty', dependencies = TRUE)


# Load the data

wd <- getSrcDirectory(function(x) {x})
setwd(wd)
my.data.path <- paste(getwd(), "ProjectData.csv", sep="/")
# Please don't remove this line (wd doesn't work for me)
# my.data.path <- "D:/Dropbox/Bayesian networks - Assignment/Assignment 1/ProjectData.csv"
my.data <- read.csv(my.data.path,sep=";")

# Original graph

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

# List conditional independencies and perform a regression to test on each
# of them.
r <-  localTests(g, my.data)

# we want to reduce wrongly assumed independencies with big estimates. A big estimate means that a small 
# change in the input data causes a big change in the output data. So, we don't care about small 
# estimates (even when they are statistically significant, practically they are not important).
significant = r$p.value<0.05
big = abs(r$estimate)>0.1
print(r[significant & big,][order(-abs(r[significant & big,]$estimate)),])


# Fight-->Alcohol we have 2 wrong independecies for alcohol and fight (the biggest ones)
# Fight-->Unsecure takes care of 2 more wrong independencies (Alcohol _||_ Unsecure | Bullied, Depression AND Fight _||_ Unsecure | Bullied )
# Age -->Bullied because it makes sense, and takes care of NON-conditional independence Age _||_ Bullied 
# Sex --> Depression 
### now we see that the main problems are with alcohol and grades in relation to unsecure
# Unsecure-->Alcohol takes care of 5/7 wrong independencies
# Race --> Unsecure FOR 3INDEPENDECIES: Race _||_ Unsecure | Bullied, Fight && Grades _||_ Unsecure | Age, Alcohol, Bullied, Fight, Sex && Age _||_ Depression | Bullied, Fight, Sex 
### now we have only 4 wrong important independencies. Should we stop here?

# Improved graph
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


### if we were to continue
# Sex --> Unsecure (only 1)
# Age --> Depression (1)
# Age --> Fight (1)
# Unsecure --> Grades (1)

# overfitted(?) graph
g <- 'dag {
Unsecure -> { Weapon Depression Alcohol Grades}
Age -> { Alcohol Weapon Bullied Fight Depression}
Sports -> { Depression Unsecure }
Alcohol -> { Weapon Grades }
Depression -> { Alcohol }
Grades -> { Weapon }
Fight -> { Grades Weapon Alcohol Unsecure}
Sex -> { Weapon Grades Fight Depression Unsecure}
Bullied -> { Depression Fight Unsecure}
Race -> { Fight Grades Bullied Unsecure}
}'

plot (graphLayout (g) )

# List conditional independencies and perform a regression to test on each
# of them.
r <-  localTests(g, my.data, conf.level = 0.95)
plotLocalTestResults(r)

# check how many conflicts (TRUE = BAD)
print (r$p.value<0.05)

cat ("Press [enter] to continue")
line <- readline()


# Overfitted network

#g <- 'dag {
#Unsecure -> {Weapon Alcohol Grades Fight}
#Age -> {  Weapon Bullied Depression Fight Sports}
#Sports -> { Depression Fight Alcohol Grades}
#Alcohol -> { Fight Weapon Grades }
#Depression -> { Alcohol Fight Grades Weapon}
#Grades -> { }
#Fight -> { Grades Weapon }
#Sex -> { Weapon Grades Fight Age Bullied Sports Depression}
#Bullied -> { Depression  Fight Alcohol}
#Race -> { Grades Alcohol Depression Sports}
#}'
#
#plot (graphLayout (g) )
#
#
# List conditional independencies and perform a regression to test on each
# of them.
#r <-  localTests(g, my.data)
#
# check how many conflicts (TRUE = BAD)
#print (r$p.value<0.05)
#
#cat ("Press [enter] to continue")
#line <- readline()


