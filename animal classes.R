# Load necessary libraries
library(dplyr)
library(ggplot2)

nature_summary <- read.csv("master_working_data.csv")

# Assuming your data is stored in a dataframe called nature_summary
# Here's an example of summarizing the classes by nest box design
summary_table <- nature_summary %>%
  group_by(design, class) %>%
  summarise(Count = n()) %>%
  ungroup()

# Filter out rows with blank Class values
filtered_summary <- summary_table %>%
  filter(class != "")

# Create a factor for "Design" to ensure all levels are included on the x-axis
filtered_summary$design <- factor(filtered_summary$design, levels = unique(nature_summary$design))

view(filtered_summary)

# Create a stacked bar plot with the filtered data
option_1 <- ggplot(filtered_summary, aes(x = design, y = Count, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(drop = FALSE, labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G",
                                            "H", "I", "J")) +  # Ensure all designs are on the x-axis
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Classes of Animals Preferring Different Nest Box Designs",
       x = "HollowBearing Tree/Nest Box",
       y = "Count",
       fill = "Class") +
  scale_fill_paletteer_d("LaCroixColoR::Coconut")+
  theme_hc()+
  theme(
    legend.position = "right",
    text = element_text(family = "serif")
  )
option_4 <- option_1 + scale_fill_paletteer_d("tvthemes::Jasper")

option_4

# Create a contingency table
contingency_table <- xtabs(Count ~ design + class, data = filtered_summary)
# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)

print(contingency_table)

# Example of combining less common categories
filtered_summary$combined_class <- ifelse(filtered_summary$class %in% c("Class1", "Class2"), "Combined_Class", filtered_summary$class)

# Create a new contingency table
contingency_table_simplified <- xtabs(Count ~ design + combined_class, data = filtered_summary)

view(contingency_table_simplified)

# Perform Fisher's Exact Test
fisher_test <- fisher.test(contingency_table_simplified)

# View the test result
print(fisher_test)


# Perform Chi-squared test with continuity correction
chi_squared_test_corrected <- chisq.test(contingency_table_simplified, correct = TRUE)
print(chi_squared_test_corrected)
