# Load necessary libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(forecast)
library(reshape2)

# loading the data set
filePath <- "C:/Users/mgujj/OneDrive/Desktop/luf/empdata1.csv"
data <- read.csv(filePath)

# looking up into the data
str(data)
head(data)

# Checking any occurrence of missing values
cat("Number of missing values: ", sum(is.na(data)), "\n")

# deleting rows if there is no value for the employment
data <- data %>% drop_na(EmploymentRate)

#converting categorical variables
data <- data %>%
  mutate(
    EducationLevel = as.factor(EducationLevel),
    Age = as.factor(Age),
    Gender = as.factor(Gender)
  )

# Creating dummy variables
processedData <- model.matrix(EmploymentRate ~ Year + EducationLevel + Age + Gender - 1, data = data) %>%
  as.data.frame()

# adding back EmploymentRate variable 
processedData$EmploymentRate <- data$EmploymentRate


# assigning the predictor variables (X) and target variable (y)
X <- processedData %>% select(-EmploymentRate)
y <- processedData$EmploymentRate

# Splitting the dataset into training and testing with 80-20 rule.
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# storing the train data 
trainData <- as.data.frame(cbind(X_train, y_train))

# Training linear regression model
model <- lm(y_train ~ ., data = trainData)

summary(model)
par(mfrow = c(2, 2))
plot(model)

# cross validation code
trainControl <- trainControl(method = "cv", number = 10)
cvModel <- train(y_train ~ ., data = trainData, method = "lm", trControl = trainControl)
print(cvModel)

# combining data for Both sexes employment rates by year
timeSeriesData <- data %>%
  filter(Gender == "Both sexes") %>%
  group_by(Year) %>%
  summarise(EmploymentRate = mean(EmploymentRate, na.rm = TRUE)) %>%
  arrange(Year)

# Converting data into time series
tsData <- ts(timeSeriesData$EmploymentRate, start = min(timeSeriesData$Year), frequency = 1)

# Forecasting with the Holt-Winters model
holtModel <- HoltWinters(tsData, beta = FALSE, gamma = FALSE)
holtModelForecast <- forecast(holtModel, h = 2)

# Forecasting with the help of ARIMA
arimaModel <- auto.arima(tsData)
arimaForecast <- forecast(arimaModel, h = 2)

# going with Ensemble Forecasting 
ensembleForecast <- (holtModelForecast$mean + arimaForecast$mean) / 2

# graph 
forecastYears <- c((end(tsData)[1] + 1):(end(tsData)[1] + 2))
comparisonDf <- data.frame(
  Year = c(time(tsData), forecastYears),
  Historical = c(as.numeric(tsData), rep(NA, length(forecastYears))),
  HoltWinters = c(rep(NA, length(tsData)), as.numeric(holtModelForecast$mean)),
  ARIMA = c(rep(NA, length(tsData)), as.numeric(arimaForecast$mean)),
  Ensemble = c(rep(NA, length(tsData)), as.numeric(ensembleForecast))
)

cm <- melt(comparisonDf, id.vars = "Year")

ggplot(cm, aes(x = Year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Holt-Winters vs ARIMA vs Ensemble",
    x = "Year",
    y = "Employment Rate (%)"
  ) +
  scale_color_manual(
    values = c("blue", "orange", "green", "red"),
    labels = c("Historical", "Holt-Winters", "ARIMA", "Ensemble")
  ) +
  theme_minimal()
cat("Forecast Results for 2024 and 2025:\n")
forecastResults <- data.frame(
  Year = forecastYears,
  HoltWinters = as.numeric(holtModelForecast$mean),
  ARIMA = as.numeric(arimaForecast$mean),
  Ensemble = as.numeric(ensembleForecast)
)
print(forecastResults)

anovaModel <- aov(EmploymentRate ~ EducationLevel, data = data)
summary(anovaModel)

# Tukey's Post-Hoc Test
tukeyResults <- TukeyHSD(anovaModel)
print(tukeyResults)

library(dplyr)
# code for combining Similar Education Levels because of same results
data$CombinedEducationLevel <- as.factor(
  ifelse(data$EducationLevel %in% c("Bachelor's level", "Tertiary education"), "Higher Education",
         ifelse(data$EducationLevel %in% c("Upper secondary", "Upper secondary or above"), "Upper Secondary Plus",
                as.character(data$EducationLevel)))
)


anovaModelCombined <- aov(EmploymentRate ~ CombinedEducationLevel, data = data)
summary(anovaModelCombined)

tukeyResultsCombined <- TukeyHSD(anovaModelCombined)
print(tukeyResultsCombined)


# Plot Tukey's Post-hoc results
plot(tukeyResultsCombined, las = 1, col = "blue")

# Residual analysis: Check assumptions of normality
residuals <- residuals(anovaModelCombined)

# normality test 
shapiro.test(residuals)

# residuals Q-Q plot 
qqnorm(residuals)
qqline(residuals, col = "red")

