## Set the Working Directory and load the dataset

setwd("C:/Users/aritra.a.chatterjee/Desktop/Analytics_Raw Data/Boston")

boston=read.csv("boston.csv")

## Plot the prices of the real ewstatle using longitude and latitude

plot(boston$LON,boston$LAT)

## Point the plots of CHAS River

points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=20)

## Get the summary of Air Pollution in boston city, which is the variable NOX
## Plot the location on the map where the level of NOX is higher than the mean OF nox

plot(boston$LON,boston$LAT)

## IN other words where the pollution is lower

points(boston$LON[boston$NOX<=0.5547],boston$LAT[boston$NOX<=0.5547],col="green",pch=20)

## Plot points using ggmap and ggplot
## Create dataframe for above mean of Air Pollution

AirPollution_abovemean<-data.frame(boston$LON[boston$NOX>=0.5547],
                                   boston$LAT[boston$NOX>=0.5547],
                                   boston$NOX[c(boston$NOX>=0.5547)])
colnames(AirPollution_abovemean)=c("LON","LAT","NOX")
names(AirPollution_abovemean)

## Load the necessary Packages

library(ggmap)
library(ggplot2)

# Create the data frame for Air Pollution is above NOX mean

AirPollution_abovemean=data.frame(boston$LON[boston$NOX>=0.5547],
                                  boston$LAT[boston$NOX>=00.5547],
                                  boston$NOX[boston$NOX>=0.5547])
colnames(AirPollution_abovemean)=c("LON","LAT","NOX")

## Create the dataframe where the nox emission is lower than mean value

AirPollution_belowmean=data.frame(boston$LON[boston$NOX<=0.5547],
                                  boston$LAT[boston$NOX<=0.5547],
                                  boston$NOX[boston$NOX<=0.5547])

colnames(AirPollution_belowmean)=c("LON","LAT","NOX")

## Create the map

x=get_map("boston",maptype = "terrain")

## PLot the map and the points for above mean

ggmap(x)+geom_point(aes(x =LON, y = LAT,colour="red",size=NOX), 
                    data = AirPollution_abovemean, alpha = 0.3)

## Plot the map and the points where the points for below mean

ggmap(x)+geom_point(aes(x=LON,y=LAT,size=NOX),
                    data=AirPollution_belowmean,alpha=0.5,
                    colour="green")+geom_point(aes(x =LON, 
                                                   y = LAT,colour="red",size=NOX),
                                               data = AirPollution_abovemean, alpha = 0.3)

## Create the data frame with the lowest NOX emission level

AirPollution_minimumnox=data.frame(boston$LON[boston$NOX==min(boston$NOX)],
                                   boston$LAT[boston$NOX==min(boston$NOX)],
                                   boston$NOX[boston$NOX==min(boston$NOX)])
colnames(AirPollution_minimumnox)=c("LON","LAT","NOX")


## PLot the town with lowest NOX emission level

ggmap(x)+geom_point(aes(x=LON,y=LAT,size=NOX),
                    data=AirPollution_belowmean,alpha=0.5,
                    colour="#33FF00")+geom_point(aes(x =LON, 
                                                     y = LAT,size=NOX),
                                                 data = AirPollution_abovemean,colour="#FF3333",alpha = 0.3)+geom_point(
                                                   aes(x=LON,y=LAT),
                                                   data=AirPollution_minimumnox,colour="#FFFF00",alpha=1)

## Create the dataset for the prices

AirPollution_Abovemedprice<-data.frame(boston$LON[boston$MEDV>=21.24],
                                       boston$LAT[boston$MEDV>=21.24])
colnames(AirPollution_Abovemedprice)=c("LON","LAT")

## Create the dataset with the prices lower than average MEDV

AirPollution_belowmedprice<-data.frame(boston$LON[boston$MEDV<=21.24],boston$LAT
                                       [boston$MEDV<=21.24])
colnames(AirPollution_belowmedprice)=c("LON","LAT")

## Plot the town which have hoger prices than the MEDV.

ggmap(x)+geom_point(aes(x=LON,y=LAT,size=NOX),
                    data=AirPollution_belowmean,alpha=0.5,
                    colour="#33FF00")+geom_point(aes(x =LON, 
                                                     y = LAT,size=NOX),
                                                 data = AirPollution_abovemean,colour="#FF3333",alpha = 0.3)+geom_point(
                                                   aes(x=LON,y=LAT),
                                                   data=AirPollution_minimumnox,colour="#FFFF00",alpha=1)+geom_point(
                                                     aes(x=LON,y=LAT),data=AirPollution_Abovemedprice,colour="#330000")+geom_point(
                                                       aes(x=LON,y=LAT),data=AirPollution_belowmedprice,color="#FF33FF")

## Regression between the house price and Longitude and latitude respectively,

latlon=lm(MEDV~LAT+LON,data=boston)
summary(latlon)


## Relationship between NOX Emission and Prices

ggmap(x)+geom_point(aes(x =LON, 
                        y = LAT),
                    data = AirPollution_abovemean,colour="#FF3333",alpha = 0.3)+geom_point(
                      aes(x=LON,y=LAT),data=AirPollution_Abovemedprice,colour="#330000")

## Regression Tree

## Load the necessary packages

library(rpart.plot)
library(rpart)


## PLot the regresssion tree

latlonTree=rpart(MEDV~LAT+LON,data=boston,minbucket=50)
prp(latlonTree)


## Create the dataframe for the fitted value of regression tree

fittedvalues=predict(latlonTree)

latlondf=data.frame(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2])
colnames(latlondf)=c("LON","LAT")


## PLot the fitted values on the existing map to see the level of accuracy of the model

ggmap(x)+geom_point(aes(x =LON, 
                        y = LAT),
                    data = AirPollution_abovemean,colour="#FF3333",alpha = 0.3)+geom_point(
                      aes(x=LON,y=LAT),data=AirPollution_Abovemedprice,colour="#FFFF00",alpha=0.4)+
  geom_point(aes(x=LON,y=LAT),data=latlondf,colour="#003366",alpha=0.5)

## Load the caTools in order to split the data into training and testing set

library(caTools)
set.seed(123)
split=sample.split(boston$MEDV,SplitRatio = 0.7)
Train=subset(boston,split==TRUE)
Test=subset(boston,split==FALSE)

## Prepare the regression tree model
nonvars=c("TOWN","TRACT")
Train=Train[,!names(Train)%in%nonvars]
Test=Test[,!names(Train)%in%nonvars]
linreg=lm(MEDV~.,data=Train)
summary(linreg)

## Predict the values
linreg.predict=predict(linreg,newdata=Test)
lingreg.sse=sum((linreg.predict-Test$MEDV)^2)

## Create the regression tree with these variables,




