install.packages("dplyr")
install.packages("xlsx")
install.packages("nnet")
library(nnet)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(xlsx)

####multiple regression model of all covariates - total visits ####
data <- read.csv("master_working_data_boxes.csv")

total_visits <- data %>%
  group_by(design, diameter, height, material) %>%  # Adjust based on your dataset
  summarise(total_visits = n())  # Count the number of rows (visits)

#### creating dummy variables for design and material #####
# Convert design to a factor if not already
data$design <- as.factor(data$design)

# Create dummy variables for design
design_dummies <- model.matrix(~ design - 1, data = data)

# View current column names
colnames(design_dummies)

# Remove spaces or special characters in column names
colnames(design_dummies) <- make.names(colnames(design_dummies), unique = TRUE)



# Convert design to a factor if not already
data$material <- as.factor(data$material)

# Create dummy variables for material
material_dummies <- model.matrix(~ material - 1, data = data)

# View current column names
colnames(material_dummies)

# Remove spaces or special characters in column names
colnames(material_dummies) <- make.names(colnames(material_dummies), unique = TRUE)



# Combine dummy variables with the original dataset
data_with_dummies <- cbind(data, design_dummies, material_dummies)

# Fit the model using the dummy variables for design
model_with_dummies <- lm(total_visits$total_visits ~ diameter + height + materialPolypropylene.plastic.and.timber + materialRecycled.HDPE.and.wood, data = data_with_dummies)

# Summarize the model
summary(model_with_dummies)


str(total_visits)
##### models minus dummies ####
data_with_visits <- left_join(data, total_visits, by = c("design", "diameter", "height", "material"))

model <- lm(total_visits ~ diameter + height + material, data = total_visits)
summary(model)
fitted(model)

model_height <- lm(total_visits ~ height, data = total_visits)
summary(model_height)

# Add fitted values and residuals to the data
total_visits$fitted_values <- fitted(model)

# Plot actual vs. fitted values (all covariates)
ggplot(total_visits, aes(x = fitted_values, y = total_visits)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +  # Add regression line
  labs(title = "Actual vs. Fitted Values", x = "Fitted Values", y = "Total Visits") +
  theme_minimal()

# exporting to excel
total_visits$residuals <- residuals(model)

export_data <- data.frame(
  height = total_visits$height,
  total_visits = total_visits$total_visits,
  fitted_values = total_visits$fitted,
  residuals = total_visits$residuals
)

write.csv(export_data, "linear_regression_output.csv", row.names = FALSE)

library(broom)

#### some other models #####
# Tidy the model and plot coefficients
model_coefficients <- tidy(model)
ggplot(model_coefficients, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Regression Coefficients", x = "Term", y = "Coefficient Estimate") +
  theme_minimal()

# Generate basic diagnostic plots for the model
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(model)


####investigating height further####
model_height <- lm(total_visits ~ height, data = total_visits)
summary(model_height)

ggplot(total_visits, aes(x = height, y = total_visits)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Total Visits vs. Height", x = "Height", y = "Total Visits")+
  theme_hc()

# Extract fitted values (predicted total visits) and residuals
total_visits$fitted <- fitted(model_height)
total_visits$residuals <- residuals(model_height)

export_data <- data.frame(
  height = total_visits$height,
  total_visits = total_visits$total_visits,
  fitted_values = total_visits$fitted,
  residuals = total_visits$residuals
)

write.csv(export_data, "linear_regression_output.csv", row.names = FALSE)

#### multiple regression model of all covariates - nature of visit ####
data <- read.csv("master_working_data_boxes.csv")

data$aspect <- as.factor(data$aspect)

total_visits <- data %>%
  group_by(design, diameter, height, material, aspect) %>%  # Adjust based on your dataset
  summarise(total_visits = n())  # Count the number of rows (visits)

data_with_visits <- left_join(data, total_visits, by = c("design", "diameter", "height", "material"))

model <- lm(total_visits ~ diameter + aspect + height + material, data = total_visits)
summary(model)

data$nature_of_visit <- as.factor(data$nature_of_visit)
# Fit the multinomial logistic regression model
model <- multinom(nature_of_visit ~ height, data = data)
summary(model)

plot(model)

# Extract the model summary
summary_model <- summary(model)

# Calculate Z-scores and p-values
z_scores <- summary_model$coefficients / summary_model$standard.errors
p_values <- (1 - pnorm(abs(z_scores))) * 2  # Two-tailed test

# Display Z-scores and p-values
z_scores
p_values
