mydataset <- read.csv("D:/sample/r/Flight-Delay-prediction-master/Flight-Delay-prediction-master/flightscombined (Autosaved).csv") #import dataset
View(mydataset)
mydataset=mydataset[,c(-14:-18)] #removing unwanted variables
mydataset=na.omit(mydataset) #takes care of missing values
mydataset_cluster=data.frame(mydataset$ARR_DELAY_NEW) #convert to data frame 
names(mydataset_cluster)[1]="DELAY" 

# In order to find the ideal value of k for our kmeans clustering algorithm, we use the elbow method

wss <- (nrow(mydataset_cluster)-1)*sum(apply(mydataset_cluster,2,var)) # calculating within group sum of sqaures
for (i in 2:15) wss[i] <- sum(kmeans(mydataset_cluster,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",   #plotting wss against number of clusters to plotelbow graph
     pch=20, cex=2)


# By th elbow graph, it appears that number of clusters must be 6 
km=kmeans(mydataset_cluster,6) #apply k means algorithm with k=6
km$size
km$centers

plot(mydataset_cluster$DELAY,col=km$cluster+1,ylab="DELAY") #visulalize the clusters formed




library(randomForest)
library(caret)
AIR <- read.csv("D:/sample/r/Flight-Delay-prediction-master/Flight-Delay-prediction-master/flightscombined (Autosaved).csv")
View(AIR)
air=AIR[,c(2,4,5,6,9,10,13)] #defining predictors
View(air)
air=na.omit(air)
# Converting numerical attributes to categorical attributes
air$day=ifelse(air$DAY_OF_WEEK==1,"MONDAY",
               ifelse(air$DAY_OF_WEEK==2,"TUESDAY",
                      ifelse(air$DAY_OF_WEEK==3,"WEDNESDAY",
                             ifelse(air$DAY_OF_WEEK==4,"THURSDAY",
                                    ifelse(air$DAY_OF_WEEK==5,"FRIDAY",
                                           ifelse(air$DAY_OF_WEEK==6,"SATURDAY",
                                                  ifelse(air$DAY_OF_WEEK==7,"SUNDAY","NA")))))))
air$day=as.factor(air$day)
air$month=ifelse(air$MONTH==5,"MAY","DECEMBER")
air$month=as.factor(air$month)
air$distance=ifelse(air$DISTANCE<1118,"SHORT","LONG")
air$distance=as.factor(air$distance)
air=air[,c(-2,-3,-5,-7)] #removing unwanted numerical predictors
#randomising the data 
air_random=air[order(runif(nrow(air))),]  
air_random=air_random[,-4] #removing numerical distance attribute
View(air_random) 
# Splitting the data into training and testing sets 
# using 80% as training data and 20 % as test data
trainIndex=createDataPartition(air$DELAY,p=0.8,list=FALSE)
air_train=air_random[trainIndex,]
air_test=air_random[-trainIndex,]
View(air_train)
View(air_test)
#Applying the random forest algorithm on training data
modelrf=randomForest(air_train[,-3],air_train$DELAY)
#Checking accuracy of the fitted model
modelrf$err.rate
modelrf$confusion
modelrf$classes
#Applying predictive model on test data
predictrf=predict(modelrf,air_test[-3])
predictrf
# Checking accuracy of test data
Err=1-sum(predictrf==air_test$DELAY)/length(air_test$DELAY)
Err
T=table(Observed=air_test$DELAY,Predicted=predictrf)
T
