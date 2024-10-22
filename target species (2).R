# Load necessary libraries
library(tidyverse)
library(broom)

# Load your dataset
data <- read.csv("master_working_data_boxes_nonull.csv")

# Step 1: Data Preparation
# Ensure the 'Design' column is a factor and 'Target_Visited' is binary (0 or 1)
data$design <- as.factor(data$design)
data$material <- as.factor(data$material)
data$target_match <- as.numeric(data$target_match)

# Step 2: Logistic Regression Modeling
# Fit a logistic regression model
logit_model <- glm(target_match ~ design, data = data, family = binomial)

# Step 3: Summary and Significance Testing
summary(logit_model)

# Tidy the model output for easier interpretation
tidy(logit_model)

#### multinomial #####
multi_model <- glm(target_match ~ material, data = data, family = "poisson")
summary(multi_model)


##likelihood ratio test##
# Fit a null model (intercept-only model)
null_model <- glm(target_match ~ 1, data = data, family = binomial)

# Perform a Likelihood Ratio Test to compare the full model with the null model
anova_model <- anova(null_model, logit_model, test = "Chisq")
tidy(anova_model)

#visualising maximum likelihood
plot(logit_model)
return(logit_model)

AIC(logit_model)

deviance(logit_model) # Model deviance

# Fit a null model (intercept only)
null_model <- glm(target_match ~ 1, data = data, family = binomial)

# Compare the two models
anova(null_model, logit_model, test = "LRT")



# Calculate the predicted probabilities and add them to the data frame
data <- data %>%
  mutate(Predicted_Visit_Prob = predict(logit_model, type = "response"))

####Table#####
# Group by design and calculate the mean predicted probability for each design
design_probs <- data %>%
  group_by(design) %>%
  summarise(Mean_Predicted_Prob = mean(Predicted_Visit_Prob))

probs_percent <- data %>%
  group_by(design) %>%
  summarise(Mean_Predicted_Prob = mean(Predicted_Visit_Prob)*100)

# Print the mean predicted probabilities for each design, decimal and percentage
print(design_probs)
print(probs_percent)

view(data)

