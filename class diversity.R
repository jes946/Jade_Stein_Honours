
####ATTEMPT 2####

# Load necessary libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(dplyr)

# Load your dataset
data <- read.csv("master_working_data.csv")

# Step 1: Data Preparation
# Create a table of species counts by design
classes_counts <- data %>%
  group_by(design, class) %>%
  summarise(Count = n()) %>%
  spread(key = class, value = Count, fill = 0)

# Calculate Shannon Diversity Index for each design
classes_counts$Diversity <- diversity(classes_counts[-1], index = "shannon")

view(classes_counts)

classes_counts <- classes_counts %>%
  mutate(vjust_adjust = ifelse(design == "I", -0.5, -0.2))

head(species_counts)

write_xlsx(classes_counts, "classes_diversity.xlsx")

# Step 2: Visualize the Diversity by Nest Box Design
ggplot(classes_counts, aes(x = design, y = Diversity)) +
  geom_bar(stat = "identity", position = "dodge", fill = paletteer_d("ggsci::blue_grey_material")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Shannon Diversity Index (6)",
       x = "Hollow Bearing Tree/Nest Box", y = "Shannon Index (Classes)") +
  scale_x_discrete(labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G", "H", "I", "J")) +
  theme_hc() +
  theme(
    text = element_text(family = "serif"))

ggsave("diversity.jpeg", dpi = 1000)


# Step 3: Statistical Testing
div_krusk <- kruskal.test(Diversity ~ design, data = species_counts)
div_krusk

summary(species_counts$Diversity)

div_chisq <- chisq.test(table(species_counts$design, species_counts$Diversity))
print(div_chisq)

# Check normality of the diversity indices
shapiro_test <- shapiro.test(species_counts$Diversity)

# Handle p-value calculation inside a robust structure
p_value <- NA

if (shapiro_test$p.value > 0.05) {
  # If data is normally distributed, use ANOVA
  anova_result <- tryCatch({
    aov(Diversity ~ design, data = species_counts)
  }, error = function(e) NULL)
  
  if (!is.null(anova_result)) {
    summary_anova <- summary(anova_result)
    
    # Extract p-value and check for NA
    p_value <- summary_anova[[1]]$`Pr(>F)`[1]
    
    if (!is.na(p_value) && p_value < 0.05) {
      cat("The result is significant, meaning there is a significant difference in species diversity between nest box designs (ANOVA).\n")
    } else if (!is.na(p_value)) {
      cat("The result is not significant, meaning there is no significant difference in species diversity between nest box designs (ANOVA).\n")
    } else {
      cat("ANOVA was performed, but the p-value could not be computed (result is NA).\n")
    }
  } else {
    cat("ANOVA could not be performed due to insufficient or inappropriate data.\n")
  }
  
} else {
  # If not normally distributed, use the Kruskal-Wallis test
  kruskal_result <- kruskal.test(Diversity ~ design, data = species_counts)
  
  if (kruskal_result$p.value < 0.05) {
    cat("The result is significant, meaning there is a significant difference in species diversity between nest box designs (Kruskal-Wallis).\n")
  } else {
    cat("The result is not significant, meaning there is no significant difference in species diversity between nest box designs (Kruskal-Wallis).\n")
  }
}

#
#ATTEMPT 3
#

# Increase the workspace size to handle larger tables
fisher_result <- fisher.test(contingency_table, workspace = 2e8)  # 200 million double precision words

# Print the result
print(fisher_result)

chisq.test(contingency_table, correct = TRUE)

