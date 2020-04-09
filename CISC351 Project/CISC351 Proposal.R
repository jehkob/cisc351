library(dplyr)
data<-read.csv("C:/Users/Jakob/Documents/CISC351 Project/train_V2.csv")
to.keep<- c("duo","duo-fpp","solo","solo-fpp","squad","squad-fpp")
data <- filter(data, matchType %in% to.keep)
sample.size <- round(nrow(data) * 0.8)
data$TotalDistance<-data$swimDistance + data$rideDistance + data$walkDistance
train <- sample_n(data, sample.size)
sample_id <- as.numeric(rownames(train)) 
test <- data[-sample_id,]






library(dplyr)

#sample.size <- round(nrow(train) * 0.8)
#match.count <- train %>% group_by(matchType) %>% tally()
#ggplot(data=train, aes(x=factor(matchType))) +
#  geom_bar(stat="count")

#f.test <-filter(test, matchType %in% to.keep)


#train.sample <- sample_n(f.train, sample.size)

require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)
require(reshape)

#cor_mat = cor(train.sample[-c(1,2,3,16)],use="pairwise.complete.obs")
#melted_cor_mat = melt(cor_mat)
#uppertriangle = function(cor_mat){
#  cor_mat[lower.tri(cor_mat)] = NA
#  return(cor_mat)
#}
#uppertri = uppertriangle(cor_mat)

#corplotdata = melt(uppertri, na.rm = TRUE)
# For some reason, this melt ends up swapping a value into the upper triangle portion where it should be in the lower, I manually fix this:
#corplotdata[528,3] = corplotdata[506,3]
#corplotdata[506,3] = NA

#ggplot(data = corplotdata, aes(x=X1, y=X2, fill=value)) + 
#  geom_tile(color = "white") +
#  labs(x="", y="") +
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", na.value="white", 
 #                      midpoint = 0, limit = c(-1,1), space = "Lab", 
#                       name="Pearson\nCorrelation") +
 # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # This line of code organizes our text on the x-axis to actually be legible.
  
#  coord_fixed()

###Subsetting data by matchType 
solo<-c("solo","solo-fpp")
solo.train<-filter(train, matchType %in% solo)
solo.test<-filter(test,matchType %in% solo)
solo.train<-select(solo.train,-c(rideDistance,walkDistance,swimDistance,matchType,teamKills,assists, killPoints, winPoints,Id,groupId,matchId,revives,DBNOs))
solo.test<-select(solo.test,-c(rideDistance,walkDistance,swimDistance,matchType,teamKills,assists,killPoints,winPoints,Id,groupId,matchId,revives,DBNOs))
solo.sample <- round(nrow(solo.train) * 0.05)
set.seed(1)
solo.train <- sample_n(solo.train, solo.sample)
solotest.sample <- round(nrow(solo.test) * 0.05)
set.seed(1)
solo.test <- sample_n(solo.test, solotest.sample)

duo<- c("duo","duo-fpp")
duo.train<-filter(train,matchType %in% duo)
duo.test<-filter(test,matchType %in% duo)
duo.train<-select(duo.train,-c(matchType,killPoints,winPoints,Id,groupId,matchId,rideDistance,walkDistance,swimDistance))
duo.test<-select(duo.test,-c(matchType,killPoints,winPoints,Id,groupId,matchId,rideDistance,walkDistance,swimDistance))
duo.sample <- round(nrow(duo.train) * 0.05)
set.seed(1)
duo.train <- sample_n(duo.train, duo.sample)
duotest.sample <- round(nrow(duo.test) * 0.05)
set.seed(1)
duo.test <- sample_n(duo.test, duotest.sample)

squad<-c("squad","squad-fpp")
squad.train<-filter(train,matchType %in% squad)
squad.test<-filter(test,matchType %in% squad)
squad.train<-select(squad.train,-c(matchType,killPoints,winPoints,Id,groupId,matchId,rideDistance,walkDistance,swimDistance))
squad.test<-select(squad.test,-c(matchType,killPoints,winPoints,Id,groupId,matchId,rideDistance,walkDistance,swimDistance))
squad.sample <- round(nrow(squad.train) * 0.05)
set.seed(1)
squad.train <- sample_n(squad.train, squad.sample)
squadtest.sample <- round(nrow(squad.test) * 0.05)
set.seed(1)
squad.test <- sample_n(squad.test, squadtest.sample)
### Examine distribution among correlated variables.
library(cowplot)
library(ggplot2)

lm.solo <-lm(winPlacePerc~.,data=solo.train)
sm.solo<-summary(lm.solo)
lm.duo <-lm(winPlacePerc~.,data=duo.train)
sm.duo<-summary(lm.duo)
lm.squad <-lm(winPlacePerc~.,data=squad.train)
sm.squad<-summary(lm.squad)
