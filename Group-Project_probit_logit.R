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

class(YelpFinal)
typeof(YelpFinal)
str(YelpFinal)
View(YelpFinal)

# Apply tree to learn what variables  with categorical varables are most applicable to reviews and staying open
# applying statistical model (probit) to predict the review (i.e. stars) for 2-3 chains
?discretize
YelpFinal$stars = discretize(YelpFinal$stars,breaks=3) #split ratings into 5 categories
YelpFinal$nameNum = as.numeric(YelpFinal$name)
View(YelpFinal)
# test tree on decision leading to reviews
YelpStarTree = tree(stars ~ review_count+as.numeric(city)+as.numeric(name)+
                      BusinessParking_street+BusinessParking_validated+WheelchairAccessible+HappyHour+DogsAllowed
                    , data=YelpFinal)
summary(YelpStarTree)

par(mfrow = c(1,1))
plot(YelpStarTree)
text(YelpStarTree, pretty=0)
YelpStarTree
# Happy hour and large number of ratings are the major factors

# Test fit using sample size
set.seed(20)
StarI = sample(seq_len(nrow(YelpFinal)), size = 0.60 * nrow(YelpFinal)) #index for 80% of data for training
YelpStartrain = YelpFinal[StarI,] #Create training set
YelpStartest = YelpFinal[-StarI,] #Create test set
YelpStarAct = YelpFinal[-StarI,]$stars #actual star of restaurant

YelpStarTree2 = tree(stars ~ review_count+as.numeric(city)+as.numeric(name)+
                                      BusinessParking_street+BusinessParking_validated+WheelchairAccessible+
                                      HappyHour+DogsAllowed, data=YelpStartrain)
YelpStarPred = predict(YelpStarTree2, YelpStartest, type="class")

mean(YelpStarPred == YelpStarAct)
table(YelpStarPred, YelpStarAct)
#end of Tree test on ratings

#Test tree prediction for 78788 for Mucho Burrito, 102258
set.seed(20)
YelpStar2test = YelpFinal[which(YelpFinal$nameNum %in% c("102258")),] #Create test set
YelpStar2train = YelpFinal[which(!YelpFinal$nameNum %in% c("102258")),] #Create train set
YelpStar2Act = YelpFinal[which(YelpFinal$nameNum %in% c("102258")),5] #actual star of 2 brands

YelpStarTree3 = tree(stars ~ review_count+as.numeric(city)+as.numeric(name)+
                       BusinessParking_street+BusinessParking_validated+WheelchairAccessible+
                       HappyHour+DogsAllowed, data=YelpStar2train)
YelpStar2Pred = predict(YelpStarTree3, YelpStar2test, type="class")

mean(YelpStar2Pred == YelpStar2Act)
table(YelpStar2Pred, YelpStar2Act)

#Test tree prediction for 78788 for Mucho Burrito
set.seed(20)
YelpStar3test = YelpFinal[which(YelpFinal$nameNum %in% c("78788")),] #Create test set
YelpStar3train = YelpFinal[which(!YelpFinal$nameNum %in% c("78788")),] #Create train set
YelpStar3Act = YelpFinal[which(YelpFinal$nameNum %in% c("78788")),5] #actual star of 2 brands

YelpStarTree4 = tree(stars ~ review_count+as.numeric(city)+as.numeric(name)+
                       BusinessParking_street+BusinessParking_validated+WheelchairAccessible+
                       HappyHour+DogsAllowed, data=YelpStar3train)
YelpStar3Pred = predict(YelpStarTree4, YelpStar3test, type="class")

mean(YelpStar3Pred == YelpStar3Act)
table(YelpStar3Pred, YelpStar3Act)

