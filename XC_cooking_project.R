#Import library
library(glmnet)

#Import data
data<-read.csv("C:/Users/Xavix/Documents/epi_r.csv/epi_r.csv", header=TRUE)

#Explore
dim(data)
summary(data)
sum(is.na(data))

#20052 rows
nrow(data)

#4188 rows contain at least 1 NA
sum(apply(data, 1, anyNA))
#Any nas seem to be in the nutritional information section
#This info is most likely important

#For now we will delete all rows with nas
new_data<-na.omit(data)
nrow(new_data)
#15864 rows now

#Add id column
new_data2<-cbind(1:nrow(new_data), new_data)
colnames(new_data2)<-c("ID", colnames(new_data))

#Don't need ID or title in model. Not useful. Will just create noise.
new_data3<-new_data2[,-c(1,2)]
# Predictor variables
x <- data.matrix(new_data3[,-1])
# Outcome variable
y <- new_data3$rating


#k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#Find ideal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#Plot of test MSE by lambda value
plot(cv_model) 

#Ideal model using ideal lambda value
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
#Coefficients
coef(best_model)
#Predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#Find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#Find R-Squared
rsq <- 1 - sse/sst
rsq

#Find MSE
mse<-mean((y- y_predicted)^2)
mse

#Find RMSE
rmse<-mean(sqrt((y- y_predicted)^2))
rmse


#Number of coefficients used in model (not eliminated): 284
length(coef(best_model)[abs(coef(best_model))>0])

#Most negative coefficient... "leftovers" (-3.38)
rownames(coef(best_model))[which(coef(best_model)==min(coef(best_model)))]

#Most positive coefficient... "chili" (1.43)
rownames(coef(best_model))[which(coef(best_model)==max(coef(best_model)[-1]))]
