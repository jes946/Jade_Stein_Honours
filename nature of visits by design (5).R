#install other libs
install.packages("RColorBrewer")
install.packages("writexl")

# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(writexl)
library(nnet)


# Load your dataset
data <- read.csv("master_working_data_nonull.csv")

# Step 1: Summarize visit counts by nest box design
visit_summary <- data %>%
  group_by(design) %>%
  summarize(Visit_Count = n())

print(visit_summary)

# Analyze how the nature of visits varies by nest box design
nature_summary <- data %>%
  group_by(design, nature_of_visit) %>%
  summarize(Count = n())

print(nature_summary)

head(nature_summary)

nature_summary <- nature_summary %>%
  mutate(nature_of_visit = factor(nature_of_visit, levels = c("pass", "inspection", "use")))

# Visualize the nature of visits by nest box design
og_plot <- ggplot(nature_summary, aes(x = design, y = Count, fill = nature_of_visit)) +
  geom_bar(stat = "identity", position = "fill", colour = "white") +
  labs(title = "Nature of Visits by Nest Box Design (5)", x = "Hollow Bearing Tree/Nest Box", 
       y = "Likelihood of Occurrence", fill = "Nature of Visit") +
  scale_fill_manual(values = c("#B0BEC5FF", "#607D8BFF", "#263238FF"))+
  scale_x_discrete(limits = c("HBT 2", "HBT 3", "Nest Box A", "Nest Box B",
                              "Nest Box C", "Nest Box D", "Nest Box G", 
                              "Nest Box H", "Nest Box I", "Nest Box J"), 
                   labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G",
                            "H", "I", "J"))+
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme_hc() +
  theme(
    legend.position = "right",
    text = element_text(family = "serif")
  )

og_plot



# Write the summarized data to Excel
write_xlsx(list(Visit_Summary = visit_summary, Nature_Summary = nature_summary), "summary_data.xlsx")


####chi sq test####
nature_summary_ALL <- data %>%
  group_by(design, material, diameter, height, nature_of_visit, aspect) %>%
  summarize(Count = n())

print(nature_summary_ALL)

nature_summary_ALL <- nature_summary_ALL %>%
  mutate(nature_of_visit = factor(nature_of_visit, levels = c("pass", "inspection", "use")))


# Swap out the variables accordingly
contingency_table <- table(nature_summary_ALL$height, nature_summary_ALL$nature_of_visit)
contingency_table

chi_square_result <- chisq.test(contingency_table)

chi_square_result

#### multinomial?? ####
multinomial_model <- multinom(nature_of_visit ~ diameter + height + material, data = data)
summary(multinomial_model)
tidy(multinomial_model)


linear_model <- lm(nature_of_visit ~ diameter + height + material, data = data)
summary(linear_model)
tidy(linear_model)


library(tidyverse)

# Assuming your dataset is stored in the variable 'data'
# Step 1: Group by 'design' and 'nature_of_visit'
nature_percentage <- data %>%
  group_by(design, nature_of_visit) %>%
  
  # Step 2: Count the number of visits for each combination
  summarize(Count = n(), .groups = 'drop') %>%
  
  # Step 3: Calculate percentage for each 'nature_of_visit' in each 'design'
  group_by(design) %>%
  mutate(Total = sum(Count), 
         Percentage = (Count / Total) * 100) %>%
  
  # Select relevant columns for display
  select(design, nature_of_visit, Count, Percentage)

# Print the result
print(nature_percentage)

library(tidyverse)

# Step 1: Group by 'nature_of_visit'
overall_percentage <- data %>%
  group_by(nature_of_visit) %>%
  
  # Step 2: Count the number of visits for each 'nature_of_visit'
  summarize(Count = n(), .groups = 'drop') %>%
  
  # Step 3: Calculate percentage of each 'nature_of_visit' over the entire dataset
  mutate(Total = sum(Count), 
         Percentage = (Count / Total) * 100) %>%
  
  # Select relevant columns for display
  select(nature_of_visit, Count, Percentage)

# Print the result
print(overall_percentage)

