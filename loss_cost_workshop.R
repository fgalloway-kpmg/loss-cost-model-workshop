
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
df <- read.csv("C:/Users/fgall/Desktop/CAS/2026 Spring Meeting/loss_cost_data.csv")

# Inspect Data
str(df)
summary(df)
head(df)
dim(df)


# Selecting columns
## Select named_insured_age, vehicle_age, and annual_miles
head(select(df, named_insured_age, vehicle_age, annual_miles), 10)
df %>% select(named_insured_age, vehicle_age, annual_miles) %>% head(10)

cols <- c('named_insured_age', 'vehicle_age', 'annual_miles')

df[, cols] %>% head(5)
subset(df, select = cols) %>% head(5)

## Select all columns except named_insured_age, vehicle_age, and annual_miles
df %>% select(-named_insured_age, -vehicle_age, -annual_miles) %>% head()

df[, !names(df) %in% cols] %>% head()
subset(df, select = -c(named_insured_age, vehicle_age, annual_miles)) %>% head(5)

## Select all numeric columns
df %>% select(where(is.numeric)) %>% head()

## Select all character columns
df %>% select(where(is.character)) %>% head()


# Filter Data
## Filter to Sedans
df %>% filter(vehicle_type=='Sedan') %>% head()

## Filter to Sedans with less than 15,000 annual_miles
df %>% filter(vehicle_type=='Sedan', annual_miles < 15000) %>% head()

## Filter to records with named_insured_age less than 24 or annual_miles over 15,000
df %>% filter(named_insured_age < 24 | annual_miles > 15000) %>% head()

## Filter to missing values of credit_score or annual_miles
df %>% filter(is.na(driver_tenure) | is.na(annual_miles)) %>% head()


# Sort Data
## Sort based on annual miles
df %>% arrange(annual_miles) %>% head()

## Sort based on descending collision_loss_cost
df %>% arrange(desc(collision_loss_cost)) %>% head()

## Sort based on named_insured_age and descending vehicle_age
df %>% arrange(named_insured_age, desc(vehicle_age)) %>% head()


# Summarize Data
## What is the mean and median loss cost?
df$collision_loss_cost %>% mean()
df$collision_loss_cost %>% median()

## What are the min and max annual miles?
df$annual_miles %>% min(na.rm = TRUE)
df$annual_miles %>% max(na.rm = TRUE)

## What is the Mode of vehicle age and named insured age?
install.packages("DescTools")
library(DescTools)

df$vehicle_age %>% Mode(na.rm = TRUE)
df$named_insured_age %>% Mode(na.rm = TRUE)

## What is the average annual mileage by territory?
df %>% group_by(territory) %>% summarise(avg_miles = mean(annual_miles,na.rm=TRUE))
df %>% group_by(territory) %>% summarise(median_miles = median(annual_miles,na.rm=TRUE))
df %>% group_by(territory, marital_status) %>% summarise(avg_miles = mean(annual_miles,na.rm=TRUE))


# Creating/deleting columns
## Calculate severity
df$severity <- if_else(df$collision_claim_count == 0, 0, df$collision_total_loss / df$collision_claim_count)

df <- df %>% mutate(severity = if_else(collision_claim_count == 0, 0, collision_total_loss / collision_claim_count))


############################################################
# Exploratory Data Analysis (EDA)
############################################################
#https://rstudio.github.io/cheatsheets/html/data-visualization.html

# Histogram
ggplot(filter(df, collision_loss_cost > 0), aes(x=collision_loss_cost)) + geom_histogram(bins=100, fill = 'red', alpha = 0.5)
ggplot(df, aes(x=annual_miles)) + geom_histogram(bins=50, fill = 'red', alpha = 0.5)

# Barchart
ggplot(df, aes(x=territory)) + geom_bar(fill='blue', alpha=0.5)
ggplot(df, aes(x=vehicle_type)) + geom_bar(fill = 'blue', alpha=0.5)
ggplot(df, aes(x=marital_status)) + geom_bar(fill='blue', alpha=0.5)
ggplot(df, aes(x=underwriting_company)) + geom_bar(fill='blue', alpha=0.5)

# Scatterplot
ggplot(filter(df, collision_loss_cost > 0), aes(x=annual_miles, y=collision_loss_cost)) + geom_point(alpha=.3) + geom_smooth()
ggplot(filter(df, collision_loss_cost > 0), aes(x=credit_score, y=collision_loss_cost)) + geom_point(alpha=.3) + geom_smooth()

# Boxplot
ggplot(filter(df, collision_loss_cost > 0), aes(x=territory, y=collision_loss_cost)) + geom_boxplot()
ggplot(filter(df, collision_loss_cost > 0), aes(x=vehicle_type, y=collision_loss_cost)) + geom_boxplot()


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

# Check Factor levels
## Convert strings to factors
df1 <- df
char_cols <- sapply(df1, is.character)
df1[char_cols] <- lapply(df1[char_cols], as.factor)

df1$youthful_driver <- as.factor(df1$youthful_driver)
df1$collision_deductible <- as.factor(df1$collision_deductible)

df1 %>% group_by(territory) %>% summarise(count = n())
df1 %>% group_by(vehicle_type) %>% summarise(count = n())
df1 %>% group_by(marital_status) %>% summarise(count = n())
df1 %>% group_by(underwriting_company) %>% summarise(count = n())
df1 %>% group_by(youthful_driver) %>% summarise(count = n())
df1 %>% group_by(collision_deductible) %>% summarise(count = n())

df1$territory <- relevel(df1$territory, ref = "Suburban")
df1$underwriting_company <- relevel(df1$underwriting_company, ref = "Standard")
df1$collision_deductible <- relevel(df1$collision_deductible, ref = "500")

# Handle NA values
## Remove all records with NA
sum(is.na(df1))

df2 <- na.omit(df1)
summary(df2)

## Impute values
df2 <- df1
df2$annual_miles <- replace_na(df2$annual_miles, median(df2$annual_miles, na.rm = TRUE))
df2$credit_score <- replace_na(df2$credit_score, median(df2$credit_score, na.rm = TRUE))
df2$horsepower <- replace_na(df2$horsepower, median(df2$horsepower, na.rm = TRUE))

summary(df2)

############################################################
# Train/Test Split
############################################################
# Remove columns
df3 <- select(df2, -policy_id, -model_year, -collision_total_loss)

install.packages("caret")
library(caret)

set.seed(123)
trainIndex <- createDataPartition(df3$collision_loss_cost,p=.7,list=FALSE)

train <- df3 %>% slice(trainIndex)
test  <- df3 %>% slice(-trainIndex)


############################################################
# GLM
############################################################
# Severity
## Gamma
sev_model <- glm(
  severity ~ vehicle_age + annual_miles + territory_group,
  data = filter(train, severity > 0),
  family = Gamma(link = "log")
)

summary(sev_gamma)

## Residual Plots
plot(residuals(sev_model, type = "deviance"))
termplot(sev_model, partial.resid = TRUE, se = TRUE)

## Variable Transformations - Train
### Territory
train$territory_group <- ifelse(train$territory %in% c("Suburban","Rural"),"Suburban/Rural", "Urban") %>% as.factor()

## Variable Transformations - Test
### Territory Group
test$territory_group <- ifelse(test$territory %in% c("Suburban","Rural"),"Suburban/Rural", "Urban") %>% as.factor()

## Predictions
sev_pred <- predict(sev_model, test, type = "response")

# Frequency
## Poisson
freq_model <- glm(
  collision_claim_count ~ credit_score + at_fault_accidents + territory_group + underwriting_group,
  data = train,
  family = poisson(link = "log"),
  offset = log(exposure)
)

summary(freq_model)

## Residual Plots
plot(residuals(freq_model, type = "deviance"))
termplot(freq_model, partial.resid = TRUE, se = TRUE)

## Variable Transformations - Train
### UW Group
train$underwriting_group <- ifelse(train$underwriting_company == "NonStandard", "NonStandard", "Standard/Preferred") %>% as.factor()
train$underwriting_group <- relevel(train$underwriting_group, ref = "Standard/Preferred")

### Vehicle Type
train$vehicle_group <- ifelse(train$vehicle_type == "Sports", "HighRisk", "Standard") %>% as.factor()
train$vehicle_group <- relevel(train$vehicle_group, ref = "Standard")



## Variable Transformations - Test
### UW Group
test$underwriting_group <- ifelse(test$underwriting_company == "NonStandard", "NonStandard", "Standard/Preferred") %>% as.factor()
test$underwriting_group <- relevel(test$underwriting_group, ref = "Standard/Preferred")

### Vehicle Group
test$vehicle_group <- ifelse(test$vehicle_type == "Sports", "HighRisk", "Standard") %>% as.factor()
test$vehicle_group <- relevel(test$vehicle_group, ref = "Standard")

freq_pred <- predict(freq_model, test, type = "response")

# Tweedie
install.packages("tweedie")
install.packages("statmod")

library(tweedie)
library(statmod)

## Power Parameter
tweedie_power <- tweedie_profile(
  collision_loss_cost ~ 1,
  data = train
)$xi.max

## Tweedie Model
twed_model <- glm(collision_loss_cost ~ vehicle_age + annual_miles + credit_score + at_fault_accidents + territory_group + underwriting_group,
               data=train,
               family=tweedie(var.power=tweedie_power, link.power = 0)
               )

summary(twed_model)

plot(residuals(twed_model, type = "deviance"))
termplot(twed_model, partial.resid = TRUE, se = TRUE)

twed_pred <- predict(twed_model, test, type = "response")

############################################################
# GBM
############################################################
install.packages("xgboost")
install.packages("Matrix")

library(xgboost)
library(Matrix)

X_train <- model.matrix(collision_loss_cost ~ . -1 -exposure -collision_claim_count -severity -territory_group -vehicle_group -underwriting_group, train)
y_train <- train$collision_loss_cost

X_test <- model.matrix(collision_loss_cost ~ . -1 -exposure -collision_claim_count -severity -territory_group -underwriting_group, test)

xgb_model <- xgboost(
  x = X_train,
  y = y_train,
  nrounds = 150,
  max_depth = 6,
  learning_rate = .1,
  objective = "reg:tweedie",
  verbosity = 0
)

xgb_pred <- predict(xgb_model,X_test, type = "response")

# Variable Importance
importance <- xgb.importance(model=xgb_model)
print(importance)
xgb.plot.importance(importance)

############################################################
# Model Comparison
############################################################
# RMSE
fs_pred <- sev_pred * freq_pred


rmse_fs <- sqrt(mean((test$collision_loss_cost - fs_pred)^2))
rmse_twed <- sqrt(mean((test$collision_loss_cost - twed_pred)^2))
rmse_xgb <- sqrt(mean((test$collision_loss_cost - xgb_pred)^2))

results <- data.frame(Model=c("F/S", "Tweedie","XGBoost"),
                      RMSE=c(rmse_fs,rmse_twed,rmse_xgb))

print(results)

# Double Lift
test$fs_pred <- fs_pred
test$twed_pred <- twed_pred

overall_mean <- mean(test$collision_loss_cost)
fs_mean <- mean(test$fs_pred)
twed_mean <- mean(test$twed_pred)

df_lift <- test %>% 
  mutate(
  sort_ratio = fs_pred / twed_pred
  ) %>% 
  arrange(sort_ratio) %>% 
  mutate(decile = ntile(sort_ratio, 10)) %>% 
  group_by(decile) %>% 
  summarise(
    avg_actual = mean(collision_loss_cost) / overall_mean,
    avg_fs = mean(fs_pred) / fs_mean,
    avg_twed = mean(twed_pred) / twed_mean,
    count = n()
  ) %>% 
  ungroup()

plot_df <- df_lift %>%
  pivot_longer(
    cols = c(avg_actual, avg_fs, avg_twed),
    names_to = "series",
    values_to = "lift"
  ) %>%
  mutate(
    series = recode(
      series,
      avg_actual = "Actual",
      avg_fs = "Frequency-Severity",
      avg_twed = "Tweedie"
    )
  )

ggplot(plot_df, aes(x = decile, y = lift, color = series, group = series)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:10,
    labels = paste("Decile", 1:10)
  ) +
  scale_color_manual(
    values = c(
      "Actual" = "#7A1F1F",
      "Frequency-Severity" = "#1F4E79",
      "Tweedie" = "#2F6B3D"
    )
  ) +
  labs(
    title = "Double Lift Chart",
    x = "Sort Ratio Decile",
    y = "Relative Loss Cost",
    color = "Series"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )