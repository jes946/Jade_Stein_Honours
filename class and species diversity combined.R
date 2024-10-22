# Example of summarizing class and species diversity
diversity_summary <- nature_summary %>%
  filter(!is.na(common_name) & common_name != "") %>%
  group_by(design) %>%
  summarise(
    Species_Diversity = n_distinct(common_name)
  )



library(tidyr)

# Reshape data to long format
diversity_long <- diversity_summary %>%
  pivot_longer(cols = c(Species_Diversity),
               names_to = "Diversity_Type",
               values_to = "Count")

print(diversity_long)
library(ggplot2)

ggplot(diversity_long, aes(x = design, y = Count, fill = Diversity_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, colour = "black") +
  scale_fill_manual(values = c("grey25"), labels = c("Species diversity")) +
  scale_x_discrete(labels = c("HBT 2", "HBT 3", "A", "B", "C", "D", "G", "H", "I", "J")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Class and Species Diversity for Each Nest Box",
       x = "Nest Box Design",
       y = "Diversity Count") +
  theme_hc() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    text = element_text(family = "serif")
  )
