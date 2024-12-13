# Data-Mining-Group-Project
# Data Mining Group Project for Geely Auto’s U.S. Market Positioning: A Supply-Demand Price Analysis Using Car Price and Sales Data 

# Part I Model Code
data <- read.csv("C:/Users/qi wang/Desktop/wa/my_course/data_mining_group_project/Car_price.csv/data.csv")
View(data)
# drop missing values
data <- na.omit(data)

# check the types of variables
str(data)

# check overall situation of the data
library(skimr)
skim(data)

#pivot table of character variables
table(data$Make)
table(data$Model)
table(data$Engine.Fuel.Type)
table(data$Transmission.Type)
table(data$Driven_Wheels)
table(data$Market.Category)
table(data$Vehicle.Size)
table(data$Vehicle.Style)
# regroup the Make and Vehicle.Style variables due to the large number of categories
# Create a new variable 'Make1' based on the market positioning categories
library(dplyr)

data <- data %>%
  mutate(Make1 = case_when(
    Make %in% c("Bentley", "Rolls-Royce", "Aston Martin", "Ferrari", "Bugatti", 
                "Maybach", "Maserati", "Lamborghini") ~ "Luxury Brand",
    Make %in% c("Ferrari", "Porsche", "Lamborghini", "McLaren") ~ "Sports Brand",
    Make %in% c("Toyota", "Honda", "Ford", "Chevrolet", "Volkswagen", "Nissan", 
                "Kia", "Mazda", "Subaru", "Buick", "Cadillac", 
                "Chrysler", "Dodge", "GMC", "Jeep", "Lincoln") ~ "Mainstream Brand",
    Make %in% c("Kia", "Hyundai", "FIAT", "Suzuki", "Scion") ~ "Economy Brand",
    TRUE ~ "Other"
  ))

# Create a new variable 'Vehicle_Style1' based on broader category classifications
Vehicle_Style<-data$Vehicle.Style
data <- data %>%
  mutate(Vehicle_Style1 = case_when(
    Vehicle_Style %in% c("2dr SUV", "4dr SUV", "Convertible SUV") ~ "SUV",
    Vehicle_Style %in% c("2dr Hatchback", "4dr Hatchback") ~ "Hatchback",
    Vehicle_Style %in% c("Sedan") ~ "Sedan",
    Vehicle_Style %in% c("Cargo Van", "Passenger Van", "Passenger Minivan", "Cargo Minivan") ~ "Passenger",
    Vehicle_Style %in% c("Regular Cab Pickup", "Crew Cab Pickup", "Extended Cab Pickup") ~ "Pickup",
    Vehicle_Style %in% c("Wagon") ~ "Wagon",
    Vehicle_Style %in% c("Convertible") ~ "Hardtop",
    Vehicle_Style %in% c("Coupe") ~ "Coupe",
    TRUE ~ "Other"
  ))


#change character variables to factors
data$Make <- as.factor(data$Make)
data$Engine.Fuel.Type <- as.factor(data$Engine.Fuel.Type)
data$Transmission.Type <- as.factor(data$Transmission.Type)
data$Driven_Wheels <- as.factor(data$Driven_Wheels)
data$Vehicle.Size <- as.factor(data$Vehicle.Size)
data$Vehicle.Style <- as.factor(data$Vehicle.Style)

# normalize the numeric variables
data$Year_Norm <- scale(data$Year)
data$Engine.HP_Norm <- scale(data$Engine.HP)
data$Engine.Cylinders_Norm <- scale(data$Engine.Cylinders)
data$city.mpg_Norm <- scale(data$city.mpg)
data$Popularity_Norm <- scale(data$Popularity)
data$MSRP_Norm <- scale(data$MSRP)

#partition the data into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(data), 0.6*nrow(data))
traindata <- data[trainIndex,]
testdata <- data[-trainIndex,]

#build a linear regression model, the dependent variable is MSRP, exclude Make, Model, and Market.Category
model1 <- lm(MSRP ~ Year + Engine.Fuel.Type + Engine.HP + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors + Vehicle.Size + Vehicle.Style + highway.MPG + city.mpg + Popularity, data = traindata)
summary(model1)

# use the test data to predict the MSRP
predict_MSRP <- predict(model1, newdata = testdata)

# Load necessary library for evaluation metrics
install.packages("Metrics")
library(Metrics)

# Calculate Mean Squared Error (MSE)
mse_value <- mse(testdata$MSRP, predict_MSRP)

# Calculate Root Mean Squared Error (RMSE)
rmse_value <- rmse(testdata$MSRP, predict_MSRP)

# Calculate Mean Absolute Error (MAE)
mae_value <- mae(testdata$MSRP, predict_MSRP)

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Error (MAE):", mae_value, "\n")

# calculate the VIF
install.packages("car")
library(car)
vif(model1)


# drop insignificant variables

# build a linear regression model, the dependent variable is MSRP, include Make1, Vehicle_Style1
model2 <- lm(MSRP ~ Year + Engine.HP + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors + Vehicle.Size + Vehicle_Style1 + Make1 +city.mpg + Popularity, data = traindata)
summary(model2)

# use the test data to predict the MSRP
predict_MSRP <- predict(model2, newdata = testdata)

# Load necessary library for evaluation metrics
library(Metrics)

# Calculate Mean Squared Error (MSE)
mse_value <- mse(testdata$MSRP, predict_MSRP)

# Calculate Root Mean Squared Error (RMSE)
rmse_value <- rmse(testdata$MSRP, predict_MSRP)

# Calculate Mean Absolute Error (MAE)
mae_value <- mae(testdata$MSRP, predict_MSRP)

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Error (MAE):", mae_value, "\n")

# calculate the VIF
library(car)
vif(model2)

# build a linear regression model with normalized dependent variable
model3 <- lm(MSRP_Norm ~ Year_Norm + Engine.HP_Norm + Engine.Cylinders_Norm + Transmission.Type + Driven_Wheels + Vehicle.Size + Vehicle_Style1 + Make1 + Popularity_Norm, data = traindata)
summary(model3)
vif(model3)

# predict the MSRP
predict_MSRP_Norm <- predict(model3, newdata = testdata)


# Load necessary library for evaluation metrics
library(Metrics)

# Calculate Mean Squared Error (MSE)
mse_value <- mse(testdata$MSRP_Norm, predict_MSRP_Norm)

# Calculate Root Mean Squared Error (RMSE)
rmse_value <- rmse(testdata$MSRP_Norm, predict_MSRP_Norm)

# Calculate Mean Absolute Error (MAE)
mae_value <- mae(testdata$MSRP_Norm, predict_MSRP_Norm)

# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Error (MAE):", mae_value, "\n")

# calculate the VIF
library(car)
vif(model3)

# Load necessary libraries
library(dplyr)       # For data manipulation
library(caret)       # For train-test split and accuracy metrics
library(car)         # For VIF to check multicollinearity

#------------------------------------------------------------------------------------------------
# Load necessary libraries for building and evaluating XGBoost models
install.packages("xgboost")
install.packages("Matrix")
install.packages("DiagrammeR")
install.packages("Metrics")  # For RMSE and MAE
install.packages("caret")    # For R-squared and train/test splitting

library(xgboost)
library(Matrix)
library(DiagrammeR)
library(Metrics)
library(caret)

# Convert the data to a sparse matrix
data_matrix <- sparse.model.matrix(MSRP_Norm ~ Year_Norm + Engine.HP_Norm + Engine.Cylinders_Norm + Transmission.Type + Driven_Wheels + Vehicle.Size + Vehicle_Style1 + Make1 + Popularity_Norm, data = data)

# Split data into training (60%) and testing (40%) sets for reproducibility
set.seed(123)
trainIndex <- sample(1:nrow(data), 0.6 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Convert training and testing data to sparse matrices
train_matrix <- sparse.model.matrix(MSRP_Norm ~ Year_Norm + Engine.HP_Norm + Engine.Cylinders_Norm + Transmission.Type + Driven_Wheels + Vehicle.Size + Vehicle_Style1 + Make1 + Popularity_Norm, data = trainData)
test_matrix <- sparse.model.matrix(MSRP_Norm ~ Year_Norm + Engine.HP_Norm + Engine.Cylinders_Norm + Transmission.Type + Driven_Wheels + Vehicle.Size + Vehicle_Style1 + Make1 + Popularity_Norm, data = testData)

# List to store models and evaluation metrics
models <- list()
results <- data.frame(Model = character(), RMSE = numeric(), MAE = numeric(), R2 = numeric(), Adj_R2 = numeric())

# Define hyperparameters for tuning
nrounds_options <- c(50, 100, 150)    # Number of boosting rounds
max_depth_options <- c(3, 6, 9)       # Depth of trees
eta_options <- c(0.01, 0.1, 0.3)      # Learning rate

# Loop over different parameter settings to train multiple models
for (nrounds in nrounds_options) {
  for (max_depth in max_depth_options) {
    for (eta in eta_options) {
      
      # Train XGBoost model with current parameters
      xgb_model <- xgboost(data = train_matrix, label = trainData$MSRP_Norm, nrounds = nrounds,
                           max_depth = max_depth, eta = eta, objective = "reg:squarederror",
                           print_every_n = 10, verbose = 0)
      
      # Store the model
      models[[paste("nrounds", nrounds, "max_depth", max_depth, "eta", eta, sep = "_")]] <- xgb_model
      
      # Predict on test data
      test_predictions <- predict(xgb_model, test_matrix)
      
      # Calculate evaluation metrics
      RMSE <- rmse(testData$MSRP_Norm, test_predictions)
      MAE <- mae(testData$MSRP_Norm, test_predictions)
      R2 <- R2(test_predictions, testData$MSRP_Norm)
      n <- nrow(testData)
      p <- ncol(test_matrix)
      Adj_R2 <- 1 - ((1 - R2) * (n - 1) / (n - p - 1))
      
      # Store evaluation metrics in the results data frame
      results <- rbind(results, data.frame(Model = paste("nrounds", nrounds, "max_depth", max_depth, "eta", eta, sep = "_"),
                                           RMSE = RMSE, MAE = MAE, R2 = R2, Adj_R2 = Adj_R2))
    }
  }
}

# Print the evaluation metrics for all models
print(results)

# Find the model with the lowest RMSE
best_model_name <- results[which.min(results$RMSE), "Model"]
best_model <- models[[best_model_name]]
cat("Best Model:", best_model_name, "\n")

# Plot the first tree of the best model
xgb.plot.tree(model = best_model, trees = 0)
#SIMPLIFY----------------------------------------------------------------------------------------------------------------
# Simplified model parameters
simplified_model <- xgboost(data = train_matrix, label = trainData$MSRP_Norm,
                            nrounds = 30,        # Reduced number of rounds for simplicity
                            max_depth = 5,       # Reduced max depth to simplify trees
                            min_child_weight = 3, # Minimum sum of weights for child nodes
                            eta = 0.1,           # Same learning rate as best model
                            objective = "reg:squarederror",
                            print_every_n = 10,
                            verbose = 0)

# Evaluate the simplified model on the test set
simplified_predictions <- predict(simplified_model, test_matrix)

# Calculate evaluation metrics for the simplified model
simplified_RMSE <- rmse(testData$MSRP_Norm, simplified_predictions)
simplified_MAE <- mae(testData$MSRP_Norm, simplified_predictions)
simplified_R2 <- R2(simplified_predictions, testData$MSRP_Norm)
n <- nrow(testData)
p <- ncol(test_matrix)
simplified_Adj_R2 <- 1 - ((1 - simplified_R2) * (n - 1) / (n - p - 1))

cat("Simplified Model Evaluation:\n")
cat("RMSE:", simplified_RMSE, "\n")
cat("MAE:", simplified_MAE, "\n")
cat("R2:", simplified_R2, "\n")
cat("Adjusted R2:", simplified_Adj_R2, "\n")

# Plot feature importance
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = simplified_model)
xgb.plot.importance(importance_matrix[1:10, ])  # Show top 10 features only
# plot the first tree of the simplified model
xgb.plot.tree(model = simplified_model, trees = 0)

# Part II Model Code
# Load required packages
library(caret)
library(xgboost)
library(Metrics)
library(DiagrammeR)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(car)

car_data <- read.csv("C:/Users/69389/Desktop/Car Sales.csv")

# drop missing values
car_data <- na.omit(car_data)

# check the types of variables
str(car_data)

# Step 1: Brand Classification (Make1)
car_data <- car_data %>%
  mutate(Make1 = case_when(
  Company %in% c("Acura", "Audi", "BMW", "Jaguar", "Lexus", "Mercedes-B") ~ "Luxury Brand",
    Company %in% c("Porsche") ~ "Sports Brand",
    Company %in% c("Buick", "Cadillac", "Chevrolet", "Chrysler", "Dodge", "Ford", 
                   "Honda", "Hyundai", "Infiniti", "Jeep", "Lincoln", "Nissan", 
                   "Subaru", "Toyota", "Volkswagen", "Volvo") ~ "Mainstream Brand",
    Company %in% c("Mercury", "Mitsubishi", "Oldsmobile", "Plymouth", "Pontiac", 
                   "Saab", "Saturn") ~ "Economy Brand",
    TRUE ~ "Other"
  ))



# Step 2: Select Relevant Variables
selected_vars <- c("Make1", "Engine", "Color", "Body.Style", "Annual.Income", "Price....","Gender")
car_data <- car_data %>%
  select(all_of(selected_vars))

# Step 3: Convert Relevant Variables to Factor (except numeric variables)
factor_vars <- c("Make1", "Engine", "Color", "Body.Style" ,"Gender")
car_data[factor_vars] <- lapply(car_data[factor_vars], as.factor)

# Step 4: Train/Test Split
set.seed(123)
train_index <- createDataPartition(car_data$Price...., p = 0.6, list = FALSE)
train_data <- car_data[train_index, ]
test_data <- car_data[-train_index, ]

# Step 5: Data Standardization for Annual.Income and Price....
preProcess_params <- preProcess(train_data[, c("Annual.Income", "Price....")], method = c("center", "scale"))
train_data[, c("Annual.Income", "Price....")] <- predict(preProcess_params, train_data[, c("Annual.Income", "Price....")])
test_data[, c("Annual.Income", "Price....")] <- predict(preProcess_params, test_data[, c("Annual.Income", "Price....")])

# Step 6: Linear Regression Model
linear_model <- lm(Price.... ~ ., data = train_data)
summary(linear_model)
# Linear Regression Predictions and Evaluation
linear_predictions <- predict(linear_model, newdata = test_data)

linear_r2 <- R2(linear_predictions, test_data$Price....)
linear_mae <- mae(test_data$Price...., linear_predictions)
linear_rmse <- rmse(test_data$Price...., linear_predictions)

cat("Linear Regression Model Results:\n")
cat("R^2:", linear_r2, "\n")
cat("MAE:", linear_mae, "\n")
cat("RMSE:", linear_rmse, "\n")

#Calculate VIF
vif_values <- vif(linear_model)
print(vif_values)

# Step 7: XGBoost Preparation (Dummy Encoding)
dummy_vars <- dummyVars("~ .", data = car_data)
car_data_transformed <- data.frame(predict(dummy_vars, newdata = car_data))

train_matrix <- as.matrix(car_data_transformed[train_index, -which(names(car_data_transformed) == "Price....")])
train_label <- train_data$Price....
test_matrix <- as.matrix(car_data_transformed[-train_index, -which(names(car_data_transformed) == "Price....")])
test_label <- test_data$Price....

# Step 8: XGBoost Model Training

xgb_model <- xgboost(
  data = train_matrix,
  label = train_label,
  max_depth = 5,
  eta = 0.1,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

# XGBoost Predictions and Evaluation
xgb_predictions <- predict(xgb_model, test_matrix)

xgb_r2 <- R2(xgb_predictions, test_label)
xgb_mae <- mae(test_label, xgb_predictions)
xgb_rmse <- rmse(test_label, xgb_predictions)

cat("XGBoost Model Results:\n")
cat("R^2:", xgb_r2, "\n")
cat("MAE:", xgb_mae, "\n")
cat("RMSE:", xgb_rmse, "\n")

xgb.plot.tree(model = xgb_model, trees = 0)  
# Step 9: Feature Importance Analysis
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix[1:10, ])

xgb.plot.tree(model = xgb_model, trees = 0)

