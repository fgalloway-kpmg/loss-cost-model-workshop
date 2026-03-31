
############################################################
# LOSS COST MODEL WORKSHOP SCRIPT
# "Let's Build a Loss Cost Model Together!"
############################################################



############################################################
#R Basics
############################################################
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf
# https://dplyr.tidyverse.org/


# Install Packages
install.packages("tidyverse")
library(tidyverse)

# Load Data
df <- read.csv("loss_cost_data.csv")

# Inspect Data
str(df)
summary(df)
head(df)
dim(df)


# Selecting columns
## Select age, vehicle_type, and annual_miles
head(select(df, age, vehicle_type, annual_miles), 10)
df %>% select(age, vehicle_type, annual_miles) %>% head(10)

## Select all columns except policy_id and total_loss
df %>% select(-policy_id, -total_loss) %>% head()

## Select all numeric columns
df %>% select(where(is.numeric)) %>% head()

## Select all character columns
df %>% select(where(is.character)) %>% head()


# Filter Data
## Filter to Trucks
df %>% filter(vehicle_type=='Truck') %>% head()

## Filter to Sedans with less than 10,000 annual miles
df %>% filter(vehicle_type=='Sedan', annual_miles < 10000) %>% head()

## Filter to records with credit scores greater than 700 or annual miles over 10,000
df %>% filter(credit_score > 700 | annual_miles > 10000) %>% head()

## Filter to missing values of age and vehicle age
df %>% filter(is.na(age), is.na(vehicle_age)) %>% head()


# Sort Data
## Sort based on annual miles
df %>% arrange(annual_miles) %>% head()

## Sort based on descending loss cost
df %>% arrange(desc(loss_cost)) %>% head()

## Sort based on age and descending vehicle age
df %>% arrange(age, desc(vehicle_age)) %>% head()


# Summarize Data
## What is the mean and median loss cost?
df$loss_cost %>% mean()
df$loss_cost %>% median()

## What are the min and max credit scores?
df$credit_score %>% min(na.rm = TRUE)
df$credit_score %>% max(na.rm = TRUE)

## What is the Mode of vehicle age and age?
install.packages("DescTools")
library(DescTools)

df$vehicle_age %>% Mode(na.rm = TRUE)
df$age %>% Mode(na.rm = TRUE)

## What is the average annual mileage by territory?
df %>% group_by(territory) %>% summarise(mean_annual_miles = mean(annual_miles,na.rm=TRUE))
df %>% group_by(territory) %>% summarise(median_annual_miles = median(annual_miles,na.rm=TRUE))


# Creating/deleting columns
## Create a column that squares vehicle age
df$age_squared <- df$age ** 2
df$age_squared <- NULL

df <- df %>% mutate(age_squared = age**2)
df$age_squared <- NULL

## Calculate frequency and severity
df$frequency <- df$claims / df$exposure
df$frequency <- NULL

df <- df %>% mutate(severity = total_loss / claims)
df$severity <- NULL



############################################################
# Exploratory Data Analysis (EDA)
############################################################
#https://rstudio.github.io/cheatsheets/html/data-visualization.html

# Histogram
ggplot(df, aes(x=loss_cost)) + geom_histogram(bins=50, fill = 'red', alpha = 0.5)
ggplot(df, aes(x=annual_miles)) + geom_histogram(bins=50, fill = 'red', alpha = 0.5)

# Barchart
ggplot(df, aes(x=territory)) + geom_bar(fill='blue', alpha=0.5)
ggplot(df, aes(x=vehicle_type)) + geom_bar(fill = 'orange', alpha=0.5, stat = 'count') + stat_count(geom="text", aes(label= ..count..))

# Scatterplot
ggplot(df, aes(x=credit_score, y=loss_cost)) + geom_point(alpha=.3) + geom_smooth() + ggtitle("Credit Score vs Loss Cost")
ggplot(df, aes(x=annual_miles, y=loss_cost)) + geom_point(alpha=.3) + geom_smooth() + ggtitle("Annual Miles vs Loss Cost")
ggplot(df, aes(x=annual_miles, y=credit_score)) + geom_point(aes(color=territory))

# Boxplot
ggplot(df, aes(x=territory, y=loss_cost)) + geom_boxplot()
ggplot(df, aes(x=vehicle_type, y=annual_miles)) + geom_boxplot()
ggplot(df, aes(x=vehicle_type, y=vehicle_age)) + geom_boxplot()
ggplot(df, aes(x=vehicle_type, y=age)) + geom_boxplot()

# Correlation Plot
install.packages("corrplot")
library(corrplot)

numeric_vars <- df %>% select(where(is.numeric)) %>% na.omit()
corr_matrix <- cor(numeric_vars)

corrplot(corr_matrix,
         method="color",
         type="upper"
         )



############################################################
# Data Modification
############################################################
# Check for duplicate records
df %>% duplicated()
df %>% filter(duplicated(df)) %>% dim()
df1 <- df %>% distinct()

# Check Factor levels
## Convert strings to factors
df1$territory <- as.factor(df1$territory)
df1$territory %>% levels()
ggplot(df1, aes(x=territory)) + geom_bar()
df1$territory <- relevel(df1$territory, ref = "Suburban")

df1$vehicle_type <- as.factor(df1$vehicle_type)
df1$vehicle_type %>% levels()
ggplot(df1, aes(x=vehicle_type)) + geom_bar()
df1$vehicle_type <- relevel(df1$vehicle_type, ref = "SUV")

##df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)

# Handle NA values
## Remove all records with NA
na.omit(df1) %>% dim()
df2 <- na.omit(df1)
summary(df2)

## Impute values
df2 <- df1
df2$age <- replace_na(df1$age, median(df2$age, na.rm = TRUE))
df2$vehicle_age <- replace_na(df2$vehicle_age, median(df2$vehicle_age, na.rm = TRUE))
df2$annual_miles <- replace_na(df2$annual_miles, median(df2$annual_miles, na.rm = TRUE))
df2$credit_score <- replace_na(df2$credit_score, median(df2$credit_score, na.rm = TRUE))

summary(df2)


############################################################
# Train/Test Split
############################################################
install.packages("caret")
library(caret)

trainIndex <- createDataPartition(df2$loss_cost,p=.7,list=FALSE)

train <- df2[trainIndex,]
test  <- df2[-trainIndex,]


############################################################
# Model Building
############################################################
# xgboost, Matrix, GGally, splines, pROC
# 
# set.seed(123)
install.packages("tweedie")
install.packages("statmod")

library(tweedie)
library(statmod)

tweedie_power <- tweedie_profile(
  loss_cost ~ 1,
  data = train
  )$xi.max

model_1 <- glm(loss_cost ~ . -policy_id -exposure -claims -total_loss,
               data=train,
               family=tweedie(var.power=tweedie_power, link.power = 0)
               )

summary(model_1)

termplot(model_1, partial.resid = TRUE, se = TRUE)

model_1 <- glm(loss_cost ~ . -policy_id -exposure -claims -total_loss -vehicle_age -territory -vehicle_type,
               data=train,
               family=tweedie(var.power=tweedie_power, link.power = 0)
               )

summary(model_1)

############################################################
# Transforming Predictor Variables
############################################################
# Vehicle Type - Alternate Groups
train$vehicle_group <- ifelse(train$vehicle_type %in% c("Sports","Truck"),"HighRisk", "Standard")

# Vehicle Age - Quadratic Term
train$vehicle_age_sq <- train$vehicle_age^2

# Vehicle Age - Binning
train$vehicle_age_band <- cut(
  train$vehicle_age,
  breaks = c(-Inf, 2, 5, 10, 15, Inf),
  labels = c("0-2", "3-5", "6-10", "11-15", "16+")
)

# Interaction
train$age_x_vehicle_age <- train$age * train$vehicle_age


model_2 <- glm(loss_cost ~ age + annual_miles + credit_score + vehicle_group + vehicle_age_sq,
               data=train,
               family=tweedie(var.power=tweedie_power, link.power = 0)
               )

summary(model_2)

model_2 <- glm(loss_cost ~ age + annual_miles + credit_score + vehicle_group,
               data=train,
               family=tweedie(var.power=tweedie_power, link.power = 0)
               )

summary(model_2)

############################################################
# Alternative GLM Distributions
############################################################

# Inverse Gaussian
inv_model <- glm(
  loss_cost ~ . -policy_id -total_loss -claims,
  data=train,
  family=inverse.gaussian(link="log")
)

summary(inv_model)

############################################################
# Model Validation
############################################################
# Score model on the Test data
model_1_pred <- predict(model_1,test,type="response")

# Vehicle Type - Alternate Groups
test$vehicle_group <- ifelse(test$vehicle_type %in% c("Sports","Truck"),"HighRisk", "Standard")

model_2_pred <- predict(model_2,test,type="response")

rmse_1 <- sqrt(mean((test$loss_cost - model_1_pred)^2))
rmse_2 <- sqrt(mean((test$loss_cost - model_2_pred)^2))

# Actual vs Predicted
plot(test$loss_cost, model_1_pred,
     xlab="Actual",
     ylab="Predicted")

abline(0,1,col="red")

plot(test$loss_cost, model_2_pred,
     xlab="Actual",
     ylab="Predicted")

abline(0,1,col="red")

############################################################
# Lift Chart
############################################################

test$pred_1 <- model_1_pred
test$pred_2 <- model_2_pred

test <- test %>% arrange(pred_1)

test$decile <- ntile(test$pred_1,10)

lift <- test %>%
  group_by(decile) %>%
  summarise(avg_loss = mean(loss_cost))

plot(lift$decile, lift$avg_loss, type="b")

############################################################
# Double Lift
############################################################

test$actual_rank <- ntile(test$loss_cost,10)

double_lift <- test %>%
  group_by(decile, actual_rank) %>%
  summarise(n=n())

print(double_lift)

############################################################
# Loss Ratio View
############################################################

lr <- test %>%
  group_by(decile) %>%
  summarise(
    actual = sum(loss_cost),
    predicted = sum(pred),
    ratio = actual / predicted
  )

print(lr)

############################################################
# Gini Index
############################################################

gini <- pROC::roc(test$loss_cost, test$pred)$auc * 2 - 1

cat("Model Gini:", gini)

############################################################
# 6 — Matrix for Boosting
############################################################

X_train <- model.matrix(loss_cost ~ . -1 -policy_id -total_loss -claims,train)
y_train <- train$loss_cost

X_test <- model.matrix(loss_cost ~ . -1 -policy_id -total_loss -claims,test)

############################################################
# 7 — XGBoost
############################################################

xgb_model <- xgboost(
  data=X_train,
  label=y_train,
  nrounds=150,
  max_depth=6,
  eta=.1,
  objective="reg:squarederror",
  verbose=0
)

xgb_pred <- predict(xgb_model,X_test)

rmse_xgb <- sqrt(mean((test$loss_cost - xgb_pred)^2))

cat("\nXGBoost RMSE:",rmse_xgb,"\n")

############################################################
# 8 — Compare
############################################################

results <- data.frame(Model=c("GLM","XGBoost"),
                      RMSE=c(rmse,rmse_xgb))

print(results)

############################################################
# 9 — Importance
############################################################

importance <- xgb.importance(model=xgb_model)
print(importance)
xgb.plot.importance(importance)

############################################################
# 10 — Optional Tuning
############################################################

grid <- expand.grid(
  nrounds=c(100,200),
  max_depth=c(4,6,8),
  eta=c(.05,.1,.3),
  gamma=0,
  colsample_bytree=.8,
  min_child_weight=1,
  subsample=.8
)

control <- trainControl(method="cv",number=3)

tuned <- train(
  x=X_train,
  y=y_train,
  method="xgbTree",
  trControl=control,
  tuneGrid=grid
)

print(tuned)

cat("\nWorkshop Complete!\n")
