
###############
# Project Notes
###############



################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("arules")
install.packages("arulesViz")
install.packages("doMC")
library(caret)
library(corrplot)
library(doMC)
library(doParallel)
library(mlbench)
library(readr)
library(arules)
library(arulesViz)

#####################
# Parallel processing
#####################

#--- Set Cores ---#

detectCores() # detect number of cores
registerDoMC(cores = 4)  



##############
# Import data
##############



#### --- Load preprocessed datasets --- ####

Donations<- read.csv("~/Documents/Personal Files/Donations/assessment_data.csv", stringsAsFactors = FALSE)


################
# Evaluate data
################

#--- Dataset 1 ---#

#checking file
str(Donations)  
names(Donations)
head(Donations)
summary(Donations)

# plot
hist(Donations$donation_revenue)
plot(Donations$donation_revenue,type="b")

#create Regions table
Regions <-table(Donations$us_region)
#create bar chart of donations by US Region
barplot(Regions[order(Regions)])

#create States table
States <-table(Donations$state)
#create bar chart of donations by state
barplot(States[order(States)])


# removing integer data from Donations

Donations = Donations[,!(names(Donations) %in% c("age_group_n","age","donation_id","us_region_n","state_n","gender_n","donor_type_n","channel_n","campaign_n","donor_type_mismatch","dup_donor_type"))]

names(Donations)

summary(Donations)




#--- Create model: APRIORI for Market Analysis ---#

apriori(Donations) 
#creating rules by confidence so I can filter down later
RulesName<- apriori (Donations, parameter = list(conf = 0.15)) 
inspect(RulesName)
summary(RulesName)

#remove duplicates
(is.redundant(RulesName))
RulesName <- RulesName[!is.redundant(RulesName)]
inspect(sort( RulesName, by = "confidence"))



#--------Filte Rules Apriori-------#

inspect(sort( RulesName, by = "confidence"))
SouthRules <- subset(RulesName, items %in% "us_region=South")
WestRules <- subset(RulesName, items %in% "us_region=West")
MidwestRules <- subset(RulesName, items %in% "us_region=Midwest")
Recurring <- subset(RulesName, items %in% "donor_type=recurring")
Campaign <- subset(RulesName, items %in% "campaign_name")

#plot rules
plot(RulesName[1:20], method="graph", control=list(), measure = "count", shading = "confidence")



#--------Save file-------#




write(RulesName,
      file = "association_rules.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)


