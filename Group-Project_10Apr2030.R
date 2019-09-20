## Data used taken from https://www.kaggle.com/yelp-dataset/yelp-dataset/data 
## The business & attributes datasets
library(tree)
library(dplyr)
library(data.table)
library(ISLR)
library(stringr)
library(stats)
?read.csv
Yelp<-read.csv("yelp_business.csv",header=T,na.strings="NA")
dim(Yelp)

YelpNew<- Yelp[which(Yelp$state=='QC'|Yelp$state=='ON'),]
# Filtered to only consider QC and ON
dim(YelpNew)

Yelp1<-YelpNew[,c(-3,-4,-7,-8,-9)]
# Removed neighborhood, address, postal_code, latitude, and longitude columns.
# We will be using city as a geographical reference
summary(Yelp1)

dim(Yelp1)
YelpA <-read.csv("yelp_business_attributes.csv",header=T,na.strings="Na")
# Loaded new dataset that includes business attributes

YelpN<- merge(Yelp1,YelpA, by.x = "business_id",by.y="business_id")
# Combined initial dataset with attributes dataset
dim(YelpN)

?count
as.data.frame(count(YelpN,categories, sort=TRUE))[1:25,]
Top25=as.data.frame(count(YelpN,name, sort=TRUE))[1:25,]
# Took most common occurances from both name and categories to be used in final dataset

summary(YelpN$categories)

Keyword=ifelse(str_detect(YelpN$categories,"Restaurant")|
              str_detect(YelpN$categories,"Food")|
              str_detect(YelpN$categories,"Sandwich")|
              str_detect(YelpN$categories,"Coffee")|
              str_detect(YelpN$categories,"Cafe"),"Yes","No")


Yelp6=data.frame(YelpN,Keyword)

No=which(Yelp6$Keyword=="No")
Yelp7a<-Yelp6[-No,]
# Removed all the data points that don't include the key words

Yelp7b <- count(Yelp7a,name, sort=TRUE)
Yelp7b <- subset(Yelp7b, n>=5)

Yelp7b$csum <-ave(Yelp7b$n, FUN=cumsum)

plot(Yelp7b$n,(Yelp7b$csum/max(Yelp7b$csum)),xlab="Number of restaurants by Brand",
     ylab="% of Cumulative total sample", xlim=rev(range(Yelp7b$n)))

Yelp7c <- Yelp7a[(Yelp7a$name%in%Yelp7b$name),]

summary(Yelp7c)
# Visually expected

YelpFinal <- Yelp7c[,c(1:8,13,14,27,54,75)]
# Final dataset with relevant datapoints
# Used columns with enough True or False results

class(YelpFinal)
typeof(YelpFinal)
str(YelpFinal)
View(YelpFinal)
dim(YelpFinal)
#
#Summary statistics
#
par(mfrow = c(1,1))

barplot(table(YelpFinal$stars),
              xlab="Average rating", main="Distribution of average ratings")
par(mfrow = c(3,1))
hist(YelpFinal$stars[which(YelpFinal$name %in% Top25[1:10,]$name)],
     xlab="Average rating of Top 10 brands", main="Distribution of TOp 10's average ratings")
hist(YelpFinal$stars[which(YelpFinal$name %in% Top25[11:25,]$name)],
     xlab="Average rating of Top 11-25 brands", main="Distribution of TOp 11-25's average ratings")
hist(YelpFinal$stars[-which(YelpFinal$name %in% Top25$name)],
     xlab="Average rating of Top 25 brands", main="Distribution of Non-Top 25's average ratings")

?plot
as.data.frame(count(YelpFinal,city, sort=TRUE))[1:10,] #most sampled cities
View(Top25)

#
#Use association rules to do a preliminary assessment of key variables
#
library(arules)
library(data.table)
library(arulesViz)
YelpAss = YelpFinal #create a copy of the Final yelp for association rules

YelpAss$stars<-discretize(YelpAss$stars,breaks=4) #use default 4 categories

YelpAss$review_count<- discretize(YelpAss$review_count,breaks=4) # use 5 breaks based on length limit

YelpAss$is_open<- as.factor(YelpAss$is_open)

YelpARM = as(YelpAss,"transactions")

rules=apriori(YelpARM)
summary(rules)

subsetRules = which(colSums(is.subset(rules,rules))>1)
length(subsetRules)
rules=rules[-subsetRules]
inspect(rules)
rules=rules[-c(1:4),]
# Remove redundant rules
library(arulesViz)

plot(rules)
inspect(rules)
# There are 0 significant rules that can be used due to large amount of missing data

# Test sub-sample of dataset to avoid using large franchises to potentially skew data
Yelp7c = Yelp7b[which(Yelp7b$n<=100),] # limit to franchise with fewer than 50 locations
YelpAss2 = YelpAss[which((YelpAss$name %in% Yelp7c$name)),]
YelpARM2 = as(YelpAss2,"transactions")

rules2=apriori(YelpARM2)
summary(rules2)

R2 = inspect(rules2)

subsetRules2 = which(colSums(is.subset(rules2,rules2))>1)
length(subsetRules2)
rules2=rules2[-subsetRules2]
rules2
inspect(rules2)
rules2=rules2[-c(1:4),]
# Remove redundant rules

plot(rules2)
inspect(rules2)
inspect(rules)
# There are 0 significant rules that can be used

#
# Apply tree to learn what variables  with categorical varables are most applicable to reviews
#and staying open
#
YelpTree = YelpFinal
?discretize
YelpTree$stars = discretize(YelpTree$stars,breaks=4,method="interval") #split ratings into 5 categories
YelpTree$nameNum = as.numeric(YelpTree$name) #generate unique numeric ID for name

par(mfrow = c(1,1))
barplot(table(YelpTree$stars),xlab="Average rating", main="Distribution of average ratings")
print(table(YelpTree$stars),xlab="Average rating", main="Distribution of average ratings")
par(mfrow = c(3,1))
barplot(table(YelpTree$stars[which(YelpTree$name %in% Top25[1:10,]$name)]),
        xlab="Average rating of Top 10 brands", main="Distribution of Top 10's average ratings")
barplot(table(YelpTree$stars[which(YelpTree$name %in% Top25$name)]),
     xlab="Average rating of Top 11-25 brands", main="Distribution of Top 11-25's average ratings")
barplot(table(YelpTree$stars[-which(YelpTree$name %in% Top25$name)]),
     xlab="Average rating of Top 25 brands", main="Distribution of Non-Top 25's average ratings")

View(YelpTree)
# test tree on decision leading to reviews
YelpStarTree = tree(stars ~ nameNum+as.numeric(city)+review_count+is_open+HappyHour
                    ,data=YelpTree)
summary(YelpStarTree)

par(mfrow = c(1,1))
plot(YelpStarTree)
text(YelpStarTree, pretty=0)
YelpStarTree
# Happy hour and large number of ratings are the major factors

# test tree on decision leading to reviews without top 25
YelpStarTree2 = tree(stars ~ review_count+as.numeric(city)+is_open+nameNum+HappyHour
                    , data=YelpTree[-which(YelpTree$name %in% Top25$name),])
summary(YelpStarTree2)

par(mfrow = c(1,1))
plot(YelpStarTree2)
text(YelpStarTree2, pretty=0)
YelpStarTree2
# Large number of ratings are the major factors

# Test fit using sample size
set.seed(20)
StarI = sample(seq_len(nrow(YelpTree)), size = 0.60 * nrow(YelpTree)) #index for 80% of data for training
YelpStartrain = YelpTree[StarI,] #Create training set
YelpStartest = YelpTree[-StarI,] #Create test set
YelpStarAct = YelpTree[-StarI,]$stars #actual star of restaurant

YelpStarTree2 = tree(stars ~ review_count+as.numeric(city)+nameNum+is_open+
                       HappyHour, data=YelpStartrain)
YelpStarPred = predict(YelpStarTree2, YelpStartest, type="class")

mean(YelpStarPred == YelpStarAct)
table(YelpStarPred, YelpStarAct)
#end of Tree test on ratings

#Test tree prediction without top 10/for 78788 for Mucho Burrito
YelpStar2test = YelpTree[-which(YelpTree$name %in% Top25[1:10,]$name),] #Create test set
YelpStar2train = YelpTree[which(YelpTree$name %in% Top25[1:10,]$name),] #Create train set
YelpStar2Act = YelpTree[-which(YelpTree$name %in% Top25[1:10,]$name),5] #actual star of 2 brands

YelpStarTree3 = tree(stars ~ review_count+as.numeric(city)+nameNum+is_open+
                       HappyHour, data=YelpStar2train)
YelpStar2Pred = predict(YelpStarTree3, YelpStar2test, type="class")

mean(YelpStar2Pred == YelpStar2Act)
table(YelpStar2Pred, YelpStar2Act)

#Test tree prediction for 78788 for Mucho Burrito
YelpStar3test = YelpTree[which(YelpTree$nameNum %in% c("78788")),] #Create test set
YelpStar3train = YelpTree[-which(YelpTree$nameNum %in% c("78788")),] #Create train set
YelpStar3Act = YelpTree[which(YelpTree$nameNum %in% c("78788")),5] #actual star of 2 brands

#BusinessParking_street+BusinessParking_validated+WheelchairAccessible+HappyHour+DogsAllowed
YelpStarTree4 = tree(stars ~ review_count+as.numeric(city)+nameNum+is_open+HappyHour
                       , data=YelpStar3train)
YelpStar3Pred = predict(YelpStarTree4, YelpStar3test, type="class")

mean(YelpStar3Pred == YelpStar3Act)
table(YelpStar3Pred, YelpStar3Act)

# applying statistical models (OLS & logit) to predict the review (i.e. stars) and survival
YelpLog = YelpFinal

YelpLog$nameNum = as.numeric(YelpLog$name) #generate unique numeric ID for name

YelpLS = lm(stars ~ name,data=YelpLog[which(YelpLog$is_open == "1"),]) #check name alone
summary(YelpLS)
YelpLS2 = lm(stars ~ review_count+name,data=YelpLog) #regress name and number of reviews 
YelpLS2 = lm(stars ~ review_count+name+city,data=YelpLog)
summary(YelpLS2)
#regress name and number of reviews with open surviving restaurants
YelpLS3 = lm(stars ~ review_count+name,data=YelpLog[which(YelpLog$is_open == "1"),])
summary(YelpLS3)

YelpLSTest = YelpLog[which(YelpLog$nameNum %in% c("74142")),] # Test stars for McDonald's
par(mfrow = c(1,1))
barplot(table(YelpLSTest$stars),xlab="Average rating", main="McDonald's average ratings")
#visually inspected the average rating of McDonald's

YelpLSTest = YelpLSTest[sample(nrow(YelpLSTest),nrow(YelpLSTest)*0.25,FALSE),] #randomly pick stores
YelpLSTrain = YelpLog[-which(YelpLog$business_id %in% YelpLSTest$business_id),]

YelpLSFit = lm(stars ~ review_count+name,data=YelpLSTrain)
print(summary(YelpLSFit))

# Plot results for visual inspections
par(mfrow = c(1,2))
plot(predict(YelpLSFit), residuals(YelpLSFit))
plot(predict(YelpLSFit), rstandard(YelpLSFit))
par(mfrow = c(1,1))
qqnorm(rstandard(YelpLSFit), ylab="Standardized Residuals",xlab="Normal Scores")
qqline(rstandard(YelpLSFit))

?predict
YelpLSPred=predict(YelpLSFit, YelpLSTest)
par(mfrow = c(2,1))
hist(YelpLSPred,main="Predicted Rating")
hist(YelpLSTest$stars,main="Actual Rating")
par(mfrow = c(1,1))
plot(YelpLSTest$stars,YelpLSPred,ylim=c(min(YelpLSPred)-0.005,max(YelpLSPred)+0.005),
     xlim=c(1,5), xlab="Actual rating",ylab="Predicted rating",
     main="Actual and Predicted MacDonalod's rating") #plot predicted against actual

?plot
#
# USe Logit model to predict if restaurant will survive
#
YelpLogit = glm(is_open ~ review_count+stars
              ,family=binomial("probit"),data=YelpLog)
summary(YelpLogit)
#amount of review is mroe important than the review itself
set.seed(25)
YelpLogTest = YelpLog[which(!(YelpLog$name %in% Top25$name) & YelpLog$is_open=="0"),]
YelpLogTest = YelpLogTest[1:100,] # sample 100 closed restaurants not in Top 25

YelpLogTrain = YelpLog[which(YelpLog$business_id %in% YelpLogTest$business_id),]

YelpLogFit = glm(is_open ~ stars+review_count
                ,family=binomial("probit"),data=YelpLogTrain)
print(summary(YelpLogFit))

YelpLogPred=predict.glm(YelpLogFit, YelpLogTrain)
YelpLogPred=ifelse(YelpLogPred>0.5,1,0) # predict if restaurant will close based on model

mean(YelpLogPred == YelpLogTest$is_open)
#1% success rate to predict restaurant closure