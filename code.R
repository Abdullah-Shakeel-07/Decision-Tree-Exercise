# importing all libraries

# install.packages("rpart.plot")

library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


# loading dataset 
data = X14_donor_exercise  

# head of data
head(data)

# data types of columns
str(data)

# column names
colnames(data)

# sum of NULL values in data
sum(is.na(data))

# checking column wise NULL valeus

lapply(data,function(x) { length(which(is.na(x)))})

# -------------------------- Q 1 ------------------

# take a sample and plot the LIFETIME_GIFT_COUNT

# density plot of LIFETIME_GIFT_COUNT to check distribution of data 
d <- density(data$LIFETIME_GIFT_COUNT) # returns the density data
plot(d) # plots the results

# as from the graph it clearly indicate that distribution is right skewed


# -------------------------- Q 2 ------------------

# head of MEDIAN_HOME_VALUE
head(data$MEDIAN_HOME_VALUE)

# scatter plot of LIFETIME_GIFT_COUNT against MEDIAN_HOME_VALUE (as both values are numeric)
ggplot(data, aes(LIFETIME_GIFT_COUNT, MEDIAN_HOME_VALUE)) + geom_point(size = 2)

# -------------------------- Q 3 ------------------
# seed = 50
set.seed(50)

# copy data
dataset = data

# shuffle dataset for better performance 
shuffle_index <- sample(1:nrow(dataset))
head(shuffle_index)

# removing Nan from dataset
dataset = na.omit(dataset)

# train - test split
dt = sort(sample(nrow(dataset), nrow(dataset)*.7))
train<-dataset[dt,]
test<-dataset[-dt,]

# building model
fit <- rpart(TARGET_B~., data = train, method = 'class')

# ploting
rpart.plot(fit, extra = 106)

# predicting the values
predict_unseen <-predict(fit, test, type = 'class')

# table of prediction
table_mat <- table(test$TARGET_B, predict_unseen)
table_mat

# -------------------------- Q 4 ------------------
# calculaiting accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy of Decision Tree on test is', accuracy_Test))


# -------------------------- Q 5 ------------------

# In order to promote donation for the company, company should know more about the donor.
# According to decision tree roughly 60% of individual should be target as donor and from the
# above graph LIFETIME_GIFT_COUNT means that total no. of donation from individual to
# organization are more where the MEDIAN_HOME_VALUE is less.



# --------------------------Q 6 -------------------
# random Forest

# building the model
model <- randomForest(as.factor(TARGET_B)~., data = train, ntree = 500, mtry = 5, importance = TRUE)
model

# predicting 
predValid <- predict(model, test, type = "class")
acc = mean(predValid == test$TARGET_B)                    
print(paste('Accuracy of Random Forest on test is', acc))

