# work by Smitha

library(leaps)
#### reading shortlisted attributes 
data_combined<- read.csv('cleaned_combined.csv')

#since sale price  is skewed , hence taking log
data_combined$SalePrice <- log(data_combined$SalePrice)
head(data_combined)
#don't consider Id
#converting MSSubClass to factor variable 
data_combined['MSSubClass'] <- factor(data_combined[,'MSSubClass'])

#creating dummy variable for all the factor variable

factor_col <- c()
for(c in colnames(data_combined)){
  if(class(data_combined[,c])=='factor'){
    factor_col <- append(factor_col,c)
    temp <- model.matrix(~data_combined[,c]-1)
    colnames(temp) <- gsub("data_combined[, c]",c,colnames(temp),fixed = TRUE)
    data_combined <- cbind(data_combined,data.frame(temp)[-c(1)])
  }
}



data_combined <- data_combined[,!names(data_combined) %in% factor_col]
str(data_combined)


#column that created NA
#NeighborhoodBlueste, BldgTypeDuplex , ExterCondPo
#dropping them , since they are possibly linearly related to other variable, seen from lm coeff 
#linear_col <- c('BldgTypeDuplex','ExterCondPo','HeatingQCPo')
linear_col <- c('BldgTypeDuplex')
data_combined <- data_combined[,!names(data_combined) %in% linear_col]

#Creating training and test set in 80:20 ratio
set.seed(2134)
train_ind <- sample(1:nrow(data_combined),0.8*nrow(data_combined))
train_df <- data_combined[train_ind,]
test_df <- data_combined[-train_ind,]

#writing the train and text to csv
#write.csv(train_df,"train_final.csv", row.names = FALSE)
#write.csv(test_df,"test_final.csv", row.names = FALSE)


#considering regression on all the attributes
all_reg_model <- lm(SalePrice~.-Id, data = train_df)

#summary
sum_all_reg_model <- summary(all_reg_model)
sum_all_reg_model


all_train_rmse <- sum_all_reg_model$sigma
all_train_rmse
all_test_rmse <- sqrt(mean((test_df$SalePrice - predict(all_reg_model, test_df)) ^ 2))
all_test_rmse


#runnning feature selection 
reg_back <- regsubsets(SalePrice~.-Id, train_df, nvmax = ncol(train_df)-2,
                          method = "backward")
smry_reg_back <- summary(reg_back)

#Plotting Adjusted R squared
plot(smry_reg_back$adjr2, xlab = "Number of Variables", 
     ylab ="Adj R-squared")
maxar2 <- which.max(smry_reg_back$adjr2)
points(maxar2,smry_reg_back$adjr2[maxar2], col = "red", cex = 2, pch = 20)

#smry_reg_back

#running model on train dataset with best subset attributes obtained in part 5
best_model_back <- lm(SalePrice~.,data=train_df[append(names(coef(reg_back,maxar2))[-1],'SalePrice')])
summary(best_model_back)


back_train_rmse <- summary(best_model_back)$sigma
back_train_rmse

back_test_rmse <- sqrt(mean((test_df$SalePrice - predict(best_model_back, test_df)) ^ 2))
back_test_rmse


back_train_rmse <- sqrt(mean((exp(train_df$SalePrice) - exp(predict(best_model_back, train_df))) ^ 2))
back_train_rmse

back_test_rmse <- sqrt(mean((exp(test_df$SalePrice) - exp(predict(best_model_back, test_df))) ^ 2))
back_test_rmse

#the rmse observed from backward selected is having lower rmse and comparativley high rsqr
#write.csv(train_df[append(names(coef(reg_back,maxar2))[-1],'SalePrice')],"train_bck_Selected.csv", row.names = FALSE)
#write.csv(test_df[append(names(coef(reg_back,maxar2))[-1],'SalePrice')],"test_bck_Selected.csv", row.names = FALSE)


tempp <- read.csv('cleaned_combined.csv')
summary(tempp$SalePrice)
