
####ATTEMPT 2####

# Load necessary libraries
library(tidyverse)
library(tidyr)
library(vegan)
library(ggplot2)
library(dplyr)
library(paletteer)
library(ggthemes)
library(generics)
library(broom)

# Load your dataset
data <- read.csv("master_working_data.csv")

# Step 1: Data Preparation
# Create a table of species counts by design
species_counts <- data %>%
  group_by(design, common_name) %>%
  summarise(Count = n()) %>%
  spread(key = common_name, value = Count, fill = 0)

#species richness
# CARFEUL species_counts = subset(species_counts, select = -c(15))
# view(species_counts)

richness <- apply(species_counts[,-1]>0,1,sum)

print(richness)

species_richness <- cbind(species_counts$design, richness)

print(species_richness)

write_xlsx(df_species_richness, "richness_r.xlsx")

model_richness <- multinom(df_species_richness ~ diameter + height + material, data = df_species_richness)
summary(model_richness)

#species richness by nest box design
ggplot(species_counts, aes(x = design, y = richness)) +
  geom_bar(stat = "identity", position = "dodge", fill = paletteer_d("ggsci::blue_grey_material")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Species Richness",
       x = "Hollow Bearing Tree/Nest Box", y = "Number of species") +
  scale_x_discrete(labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G", "H", "I", "J")) +
  theme_hc() +
  theme(
    text = element_text(family = "serif"))





# Step 1: Check normality of species richness
shapiro_test <- shapiro.test(richness)
print(shapiro_test)

# Step 3: Perform one-way ANOVA
data_2 <- read.csv("master_working_data.csv")
# Step 1: Data Preparation
# Create a table of species counts by design
species_counts2 <- data_2 %>%
  group_by(design, material, diameter, height, common_name) %>%
  summarise(Count = n()) %>%
  spread(key = common_name, value = Count, fill = 0)



species_counts2 <- data_2 %>%
  filter(!is.na(common_name) & common_name != "") %>%
  group_by(design, common_name) %>%
  summarise(Count = n()) %>%
  spread(key = common_name, value = Count, fill = 0)

complete_species_counts <- all_designs %>%
  left_join(visit_counts, by = "design") %>%
  mutate(species_counts2 = ifelse(is.na(species_counts2), 0, species_counts2))

all_designs <- data_2 %>% distinct(design)
# Merge with the full list of nest boxes, ensuring missing designs have a count of 0
complete_species_counts <- all_designs %>%
  left_join(species_counts2, by = "design") %>%
  mutate(total_species_visits = ifelse(is.na(total_species_visits), 0, total_species_visits))

#species richness
richness2 <- apply(species_counts2[,-1]>0,1,sum)

species_richness2 <- cbind(complete_species_counts, richness2)

print(species_richness2)

df_species_richness2 <- as.data.frame(species_richness2)
colnames(df_species_richness2)[1] <- "design"
colnames(df_species_richness2)[2] <- "diameter"
colnames(df_species_richness2)[3] <- "material"
colnames(df_species_richness2)[4] <- "height"

model_richness2 <- multinom(richness2 ~ diameter + height + material, data = df_species_richness2)
tidy(model_richness2)

anova_model2 <- lm(richness2 ~ design + diameter + material, data = df_species_richness2)
tidy(anova_model2, n=40)

kruskal.test(richness ~ design, data = df_species_richness)

# Step 4: If ANOVA is significant, perform post-hoc Tukey test
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

head(species_counts)

# more stats attempts
# Convert from wide to long format
long_data <- species_counts %>%
  pivot_longer(cols = -design, names_to = "species", values_to = "count")

head(long_data)

# Run ANOVA for each species
results <- long_data %>%
  group_by(species) %>%
  do(tidy(aov(count ~ design, data = .)))

print(results)








# Step 2: Visualize the Diversity by Nest Box Design
ggplot(species_counts, aes(x = design, y = Diversity)) +
  geom_bar(stat = "identity", position = "dodge", fill = paletteer_d("ggsci::blue_grey_material")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Shannon Diversity Index (6)",
       x = "Hollow Bearing Tree/Nest Box", y = "Shannon Index") +
  scale_x_discrete(labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G", "H", "I", "J")) +
  theme_hc() +
  theme(
    text = element_text(family = "serif",
                        size = 8))

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

