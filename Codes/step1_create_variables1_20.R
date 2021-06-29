# work by Smitha
library(ggplot2)
library(corrplot)
library(dplyr)
library(patchwork)

#plots 
# - correlateion
# - Types dwelling, change the scientific notation.
# - neighnorhoods - count is size
# - Sale Price across rating 


house_data <- read.csv('train.csv')
head(house_data)
str(house_data)
dim(house_data)

#First lets check how the distribution of prediction price looks like 
mean_sprice <- round(mean(house_data$SalePrice)/1000,2)
ggplot(house_data,aes(x = SalePrice/1000)) + 
  geom_histogram(fill = 'sky blue') +
  geom_vline(aes(xintercept = mean_sprice),
             linetype='dashed',color='red') +
  annotate("text", x = mean_sprice * 1.3,
           y = mean_sprice * 1.7,
           label = paste("Mean =", mean_sprice),
           col = "red",
           size = 3)+
  labs(title = 'Sale Price', y = 'Count', x = 'Sale Price (1000 dollars)')+
  theme(
    plot.title = element_text(hjust = 0.5))

#imp
#We observe average sales price observed in the data is aroun 180.92K$
#This average might be not very representative, since the
#distribution is skewed towards right. i.e. There are few houses sold
#at very high price, which might have lead increased average price. 
####################################################################

#we observe below distribution of attribute type 
#Categorical:46
#Continous:28 
#Date:5

#############################################################################3
#lets check the correlation plot 
temp_df <- cor(na.omit(house_data[, sapply(house_data, is.numeric)]))
temp_df[abs(temp_df)<0.5] <- 0
corrplot(temp_df,type = "upper",
         number.cex = 0.7,method = 'square',tl.cex=0.6
)

#imp
#from the correlation plot, we can see that 
#TotalBsmtSF: Total square feet of basement area 
#1stFlrSF: First Floor square feet

#GrLivArea: Above grade (ground) living area square feet
#TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)

#GarageYrBlt: Year garage was built
#YearBuilt: Original construction date

#GarageCars: Size of garage in car capacity
#GarageArea: Size of garage in square feet

#GrLivArea: Above grade (ground) living area square feet
#sale price

#OverallQual: Rates the overall material and finish of the house
#sale price

#############################################################3
#MSSubClass

avrg_subclass <- house_data %>% group_by(MSSubClass) %>% summarise(average = mean(SalePrice),
                                                                   count = n())
avrg_subclass$MSSubClass <- as.factor(avrg_subclass$MSSubClass)
options(scipen=10000)
ggplot(avrg_subclass) + 
  geom_col(aes(x = MSSubClass, y = count), size = 1, color = "black", fill = "white") +
  geom_point(aes(x = MSSubClass, y = average/500), size = 1.5, color="red", group = 1) + 
  geom_line(aes(x = MSSubClass, y = average/500), size = 1, color="red", group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "average sales price($)"))+
  theme(axis.line.y.right = element_line(color = "red"),axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right  = element_text(color = "red")) +
  labs(title = 'Distribution of types of dwelling involved in sales', x = 'types of dwelling')

house_data$MSSubClass <- as.factor(house_data$MSSubClass)
#imp
#We majority of house involved in sale are - 1-STORY 1946 & NEWER ALL STYLES
#we see also see the average sale price for - 2-STORY 1946 & NEWER, is largest.

#######################################################################
#MSZoning
 
p1 <- ggplot(house_data)+geom_bar(aes(x =as.factor(MSZoning),y=(..count..)/sum(..count..)),fill='sky blue') +
  scale_y_continuous(labels=scales::percent) +
  labs(title ='Distribution of zones', y = 'percentage', x = 'Zones')+
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = 1,
           y = 0.6,
           label = paste('A:\tAgriculture\nC:\tCommercial\nFV:\tFloating Village Residential\nI:\tIndustrial\nRH:\t	Residential High Density\nRL:\tResidential Low Density\nRP:\tResidential Low Density Park \nRM:\t	Residential Medium Density'),
                               col = "black",size = 3,hjust = 0) 
p2 <- ggplot(house_data) +
geom_boxplot(aes(x = MSZoning,y = SalePrice)) +
  labs(title = 'Sales Price across zone', y = 'sales price', x = '')+
  theme(
    plot.title = element_text(hjust = 0.5))


p2+p1 +plot_layout(ncol=1)

#imp
#We observe sales price for Floating Village Residential(FV) is very high 

#####################################################################
###"LotFrontage"   
(sum(is.na(house_data$LotFrontage))/nrow(house_data))*100
#Aroung 18% of value are missing, also these feature is not correlated with dependent variable
#hence we wont consider it in our future analysis 
col_remove <- c('LotFrontage')

#######################################
#"LotArea"
ggplot(house_data) + geom_histogram(aes(LotArea))

#From the histogram we see that there exists outlier for this feautre, ther are around 3 entires
#which has extremely large value for lot area, lets remove these entires for futher analysis
#nrow(house_data[house_data$LotArea>150000,])
house_data <- house_data[house_data$LotArea<150000,]


#############################################################################################
#"Street"
ggplot(house_data) + geom_bar(aes(x = Street,y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent)
#We wont consider these attribute since it has more than 90% of the value with one category. 
col_remove <- c(col_remove,'Street')

#######################################################################################
#"Alley"        
sum(is.na(house_data$Alley))/nrow(house_data)

#Almost 93% of value is na, hence we will remove this attribute
col_remove <- c(col_remove,'Alley')

#########################################################################################3
#"LotShape"    
table(house_data$LotShape)

#Removing the variable as we observe relationship with atrribute Lot area
#Larger the lot, the lot is irregular. Hence we would consider that 
#attibute instead. (observed in excel pivot table)
col_remove <- c(col_remove,'LotShape')

##############################################################
#"LandContour" 
ggplot(house_data) + geom_bar(aes(x = LandContour,y=..count../sum(..count..)))

##################################################################
#"Utilities"
#two level present, but the other category appeasrs only once and rest of the value is for other
#category
table(house_data$Utilities)
col_remove <- c(col_remove,'Utilities')

############################################################################
#"LotConfig"    
ggplot(house_data) + geom_bar(aes(x = LotConfig,y=..count../sum(..count..)))

###########################################################################
#"LandSlope"   

temp <- house_data %>% group_by(LandSlope, LandContour) %>%
  summarise('cnt'=n())

ggplot(temp) + 
  geom_bar(aes(x = LandSlope, y=cnt,fill = LandContour), position = "dodge", stat="identity")

#since similar information in Landcontour
col_remove <- c(col_remove,'LandSlope')

#############################################################################  
#"Neighborhood" 

Neighborhood_list = c('Blmngtn', 'Blueste', 'BrDale', 'BrkSide', 'ClearCr', 'CollgCr', 'Crawfor', 'Edwards',
                      'Gilbert', 'IDOTRR', 'MeadowV', 'Mitchel', 'NAmes', 'NoRidge', 'NPkVill', 'NridgHt', 'NWAmes','OldTown',
                      'SWISU','Sawyer','SawyerW', 'Somerst', 'StoneBr', 'Timber','Veenker', 'Greens')
Latitude =c(42.0563761, 42.0218678, 42.052795, 42.024546, 42.0360959, 42.0214232, 42.028025, 42.0154024,
            42.1068177, 42.0204395, 41.997282, 41.9903084, 42.046618, 42.048164, 42.0258352, 42.0597732, 42.0457802,
            42.029046, 42.0266187, 42.0295218, 42.034611, 42.0508817, 42.0595539, 41.9999732, 42.0413042, 42.001350383364695)
Longitude =c(-93.6466598, -93.6702853, -93.6310097, -93.6545201, -93.6575849, -93.6584089, -93.6093286,
             -93.6875441, -93.6553512, -93.6243787, -93.6138098, -93.603242, -93.6362807, -93.6496766, -93.6613958,
             -93.65166, -93.6472075, -93.6165288, -93.6486541, -93.7102833, -93.7024257, -93.6485768, -93.6365891,
             -93.6518812, -93.6524905, -93.6446034587931)


for(ind in 1:length(Neighborhood_list)){
  house_data[house_data$Neighborhood==Neighborhood_list[ind],'Latitude'] = Latitude[ind]
  house_data[house_data$Neighborhood==Neighborhood_list[ind],'Longitude'] = Longitude[ind]
}


temp <- house_data %>% group_by(Neighborhood,Latitude,Longitude) %>% 
  summarise(Average_Sale_Price = mean(SalePrice),Total_Sale_Count = n()) 

#ggplot(temp,aes(x=Longitude,y=Latitude)) + 
#  geom_point(aes(size = Average_Sale_Price,color = Total_Sale_Count ),alpha=0.5) + 
#  geom_text(data=temp,aes(label=temp$Neighborhood),size=3,position = position_nudge(y = 0.004)
#  ) + ggtitle("Neighborhoods within Ames city") +
#  scale_fill_discrete(name = "New Legend Title")

ggplot(temp,aes(x=Longitude,y=Latitude)) + 
  geom_point(aes(color = Average_Sale_Price,size = Total_Sale_Count ),alpha=0.5) + 
  geom_text(data=temp,aes(label=temp$Neighborhood),size=3,position = position_nudge(y = 0.004)
  ) + ggtitle("Neighborhoods within Ames city") +
  scale_fill_discrete(name = "New Legend Title")
#imp

#############################################################################3
#"Condition1" 
ggplot(house_data) + geom_bar(aes(Condition1))

#skewed
col_remove <- c(col_remove,'Condition1')
#################################################################################  
#"Condition2"   
ggplot(house_data) + geom_bar(aes(Condition2))

col_remove <- c(col_remove,'Condition2')
###############################################################################3
#"BldgType"    
ggplot(house_data) + geom_bar(aes(BldgType))

#############################################################################
#"HouseStyle"   
ggplot(house_data) + geom_bar(aes(HouseStyle))
#since this information is in MSsubclass
col_remove <- c(col_remove,'HouseStyle')
#############################################################################
#"OverallQual"  
ggplot(house_data) + geom_bar(aes(as.factor(OverallQual)))

#imp
ggplot(house_data) + geom_boxplot(aes(x = as.factor(OverallQual), y = SalePrice),color='red') +
  labs(x= 'Rating of the overall material and finish of the house',title='Sale price across rating') +
  theme(
    plot.title = element_text(hjust = 0.5)) 

#############################################################################
#"OverallCond"  
ggplot(house_data) + geom_boxplot(aes(x = as.factor(OverallCond), y = SalePrice),color='red')
#############################################################################
#"YearBuilt"
sum(is.na(house_data$YearBuilt))
summary(house_data$YearBuilt)
house_data['house_age'] <- house_data$YrSold - house_data$YearBuilt  
ggplot(house_data) + geom_histogram(aes(YearBuilt)) 
col_remove <- c(col_remove,'YearBuilt')
#add column - how old
#################################################################
house_data_filtered <- house_data[colnames(house_data)[1:20]]
house_data_filtered <- house_data_filtered[,!(names(house_data_filtered) %in% col_remove)]
house_data_filtered <- merge(house_data_filtered, house_data[c('Id','house_age')], 
                             by.x = 'Id', by.y = 'Id', all.x = TRUE, all.y = TRUE)

write.csv(house_data_filtered,"var01_20.csv", row.names = FALSE)


#######################################################################
#### reading shortlisted attributes 
data_combined<- read.csv('cleaned_combined.csv')
head(data_combined)
#don't consider Id
#converting MSSubClass to factor variable 
data_combined['MSSubClass'] <- factor(data_combined[,'MSSubClass'])


#Creating training and test set in 80:20 ratio
train_ind <- sample(1:nrow(data_combined),0.8*nrow(data_combined))
train_df <- data_combined[train_ind,]
test_df <- data_combined[-train_ind,]

#writing the train and text to csv
write.csv(train_df,"train_final.csv", row.names = FALSE)
write.csv(test_df,"test_final.csv", row.names = FALSE)

#considering regression on all the attributes
all_reg_model <- lm(SalePrice~.-Id, data = train_df)

#summary
sum_all_reg_model <- summary(all_reg_model)
sum_all_reg_model

BldgTypeDuplex 
CentralAir 
Fireplaces

unique(train_df$Fireplaces)
corr(train_df)

