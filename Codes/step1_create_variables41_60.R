# work by Ellie

house<- read.csv('train.csv')
ellie<- house[,41:60]
ellie
df <- cbind(ellie[1:20], house[,81])

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)

#all na values 
map(ellie, ~sum(is.na(.)))
#there are na values in Fireplace Quality, Garage Type, Year built

#na means there's no fireplace
df$FireplaceQu[is.na(df$FireplaceQu)]<- 'No Fireplace'
#na means there's no garage
df$GarageType[is.na(df$GarageType)]<- 'No Garage'
#na means there's no garage
df$GarageYrBlt[is.na(df$GarageYrBlt)]<- 'No Garage'


#now look at our data again
map(df, ~sum(is.na(.)))

temp_df <- cor(na.omit(house[, sapply(house, is.numeric)]))
temp_df[abs(temp_df)<0.7] <- 0
corrplot(temp_df,type = "upper",
         number.cex = 0.5,method = 'square',tl.cex=0.6
)

#totromsabvgrd and grliv area are highly correlated
df
#Starting from column 41
#HeatingQC
str(df[,1])
plot(table(df[,1]))
newdf <- as.data.frame(df$HeatingQC)
names(newdf)[1]<- 'HeatingQC'
newdf$HeatingQC<- as.factor(newdf$HeatingQC)
str(newdf)

#42
#CentralAir
str(df[,2])
plot(table(df[,2]))
df %>% group_by(CentralAir) %>% summarize(n())
newdf$CentralAir <- ifelse(df$CentralAir=="Y", 1, 0)
data <- as.data.frame(df[,2])
colnames(data) <- 'CentralAir'
newdf %>% group_by(CentralAir) %>% summarize(n())
newdf

#43
#Electrical, only 1 missing value
#since standard is the most electrical system for a house, we will impute standard 
df %>% group_by(Electrical) %>% summarize(n())
df[1380,3]<- 'SBrkr'

str(df[,3])
plot(table(df[,3]))
newdf[,3] <- as.data.frame(df[,3])
names(newdf)[3] <- "ElectricalType"
newdf$ElectricalType
newdf$ElectricalType <- as.factor(newdf$ElectricalType)

#convert to dummies

#44
#X1stFlrSF
str(df[,4])
plot(table(df[,4]))
newdf[,4] <- as.data.frame(df[,4])
names(newdf)[4] <- "FirstFloorSqft"

#45
#X2ndFlrSF
str(df[,5])
boxplot(df[df$X2ndFlrSF!= 0,5])
newdf[,5] <- as.data.frame(df[,5])
names(newdf)[5] <- "SecondFloorSqft"

#46
#LowQualFinSF
str(df[,6])
plot(table(df[,6]))
#can remove this column most are zero 

#47
#GrLivArea
str(df[,7])
plot(table(df[,7]))
newdf[,6] <- as.data.frame(df[,7])
names(newdf)[6] <- "Abovegradeliving"

#48
#BsmtFullBath
str(df[,8])
plot(table(df[,8]))
newdf[,7] <- as.data.frame(df[,8])
names(newdf)[7] <- "BaseFullBath"

#49
#BsmtHalfBath
str(df[,9])
plot(table(df[,9]))
newdf[,8] <- as.data.frame(df[,9])
names(newdf)[8] <- "BaseHalfBath"

#50
#FullBath
str(df[,10])
plot(table(df[,10]))
newdf[,9] <- as.data.frame(df[,10])
names(newdf)[9] <- "FullBath"

#51
#HalfBath
str(df[,11])
plot(table(df[,11]))
newdf[,10] <- as.data.frame(df[,11])
names(newdf)[10] <- "HalfBath"

#52
#BedroomAbvGr
str(df[,12])
plot(table(df[,12]))
newdf[,11] <- as.data.frame(df[,12])
names(newdf)[11] <- "BedroomAbvGr"

#53
#KitchenAbvGr
str(df[,13])
plot(table(df[,13]))
newdf[,12] <- as.data.frame(df[,13])
names(newdf)[12] <- "KitchenAbvGr"

#54
#KitchenQual
str(df[,14])
plot(table(df[,14]))
newdf[,13] <- as.data.frame(df[,14])
names(newdf)[13] <- "KitchenQual"
newdf$KitchenQual <- as.factor(newdf$KitchenQual)

#55
#TotRmsAbvGrd
#this is highly correlated with GrLivArea so shouldn't include

#56
#Functional
str(df[,16])
plot(table(df[,16]))
df %>% group_by(Functional) %>% summarize(n())
df$Functional <- ifelse(df$Functional%in% c("Maj1",'Maj2','Min1', 'Min2', 'Mod', 'Sev'), 'Deduction', 'Typ')
df$Functional
newdf[,14] <- as.data.frame(df[,17])
names(newdf)[14] <- 'Functional'
newdf$Functional <- as.factor(newdf$Functional)
#typical, rest as deduction 

#57
#Fireplaces
str(df[,17])
plot(table(df[,17]))
newdf[,15] <- as.data.frame(df[,17])
names(newdf)[15] <- "Fireplaces"

#58
#FireplaceQu
#we can remove garage yr blt, fireplace quality as we only need to consider whether these features are available 

#59
#GarageType
str(df[,19])
plot(table(df[,19]))
newdf[,16] <- as.data.frame(df[,19])
names(newdf)[16] <- "GarageType"
newdf$GarageType <- as.factor(newdf$GarageType)
str(newdf)

#60
#GarageYrBlt
#we can remove garage yr blt, fireplace quality as we only need to consider whether these features are available 
write.csv(newdf, file='Elliespart.csv')
