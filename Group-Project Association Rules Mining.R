## Data used taken from https://www.kaggle.com/yelp-dataset/yelp-dataset/data 
## The business & attributes datasets

library(tree)
library(dplyr)
library(data.table)
library(ISLR)
library(stringr)


Yelp<-read.csv("yelp_business.csv",header=T)




YelpNew<- Yelp[which(Yelp$state=='QC'|Yelp$state=='ON'),]
# Filtered to only consider QC and ON




Yelp1<-YelpNew[,c(-3,-4,-7,-8,-9)]
# Removed neighborhood, address, postal_code, latitude, and longitude columns.
# We will be using city as a geographical reference


summary(Yelp1)
YelpA <-read.csv("yelp_business_attributes.csv",header=T)
# Loaded new dataset that includes business attributes


YelpN<- merge(Yelp1,YelpA, by.x = "business_id",by.y="business_id")
# Combined initial dataset with attributes dataset

?count

as.data.frame(count(YelpN,categories, sort=TRUE))[1:25,]
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



library(arules)
library(data.table)
library(arulesViz)

str(YelpFinal)

YelpFinal$stars<-discretize(YelpFinal$stars)

YelpFinal$review_count<- discretize(YelpFinal$review_count)

YelpFinal$is_open<- as.factor(YelpFinal$is_open)

YelpARM = as(YelpFinal,"transactions")


rules=apriori(YelpARM)
summary(rules)

R = inspect(rules)

subsetRules = which(colSums(is.subset(rules,rules))>1)
length(subsetRules)
rules=rules[-subsetRules]
rules
inspect(rules)
rules=rules[-c(1:4),]
# Remove redundant rules
library(arulesViz)

plot(rules)
inspect(rules)
# There are 6 significant rules that can be used























