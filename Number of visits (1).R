#check the wd
getwd()

install.packages("Redmonder")

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(broom)      # For tidy model output
library(ggplot2)
library(ggthemes)
library(paletteer)
library(Redmonder)


# Load your dataset
# Assuming your dataset is named `data` and it includes a column for nest box design ('Design') 
# and a binary column for visits ('Visited') indicating if a visit occurred (1) or not (0)
data <- read.csv("binary_count.csv")

# Step 1: Data Preparation
# Ensure the 'Design' column is treated as a factor (categorical variable)
data$Design <- as.factor(data$Design)

# Step 2: Exploratory Data Analysis (EDA)
# Summarize visit counts by nest box design
visit_summary <- data %>%
  group_by(Design) %>%
  summarize(Visit_Count = sum(Visited),
            Total_Observations = n(),
            Visit_Rate = mean(Visited))

print(visit_summary)

# Visualize the visit rates by nest box design
plot_liz <- ggplot(visit_summary, aes(x = Design, y = Visit_Count)) +
  geom_bar(stat = "identity", fill = paletteer_d("ggsci::blue_grey_material")) +
  labs(title = "Total number of visits by nest box design (1)", x = NULL, y = "Total number of visits") +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_x_discrete("Hollow Bearing Tree/Nest Box", 
                   labels = c("HBT2", "HBT3", "A", "B", "C", "D", "G", "H", "I", "J")) + 
  theme_hc()+
  theme(
    text = element_text(family = "serif"))
  

plot_liz


view(data)

# Step 3: Statistical Testing
# Use a chi-squared test to assess the association between nest box design and visits
chisq_test <- chisq.test(table(visit_summary$Design, visit_summary$Visit_Count))
print(chisq_test)




dataset <- read.csv("master_working_data.csv")
datasetBEE <- read.csv("master_working_data_nonull_WITHBEE.csv")

library(dplyr)

# Create a list of all nest boxes (designs)
all_designs <- dataset %>% distinct(design)

# Filter out rows where 'nature_of_visit' is blank before counting visits
visit_counts <- dataset %>%
  filter(!is.na(nature_of_visit) & nature_of_visit != "") %>%
  group_by(design, aspect) %>%
  summarise(total_visits = n())

# Merge with the full list of nest boxes, ensuring missing designs have a count of 0
complete_visit_counts <- all_designs %>%
  left_join(visit_counts, by = "design") %>%
  mutate(total_visits = ifelse(is.na(total_visits), 0, total_visits)) %>%
  mutate(aspect = ifelse(is.na(aspect), 0, aspect)) 
  
      

# View the result
print(complete_visit_counts)

abc <- glm(total_visits ~ aspect, data = complete_visit_counts, family = "poisson")
summary(abc)

lets_see <- chisq.test(table(complete_visit_counts$total_visits, complete_visit_counts$height))
lets_see

ggplot(complete_visit_counts, aes(x = aspect, y = total_visits)) +
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


anova_model <- aov(total_visits ~ design, data = complete_visit_counts)
summary(anova_model)
tidy(anova_model)
anova(anova_model)

chisq_test <- chisq.test(complete_visit_counts$design, complete_visit_counts$total_visits)
print(chisq_test)



