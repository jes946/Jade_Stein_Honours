library(tidyverse)
library(tidyr)
library(vegan)
library(ggplot2)
library(dplyr)
library(paletteer)
library(ggthemes)
library(generics)
library(broom)

data <- read.csv("simplest_data.csv")


head(data)

data$aspect <- as.factor(data$aspect)

# diameter
anova_richness <- aov(sp_richness ~ diameter, data = data)
anova(anova_richness)
      
linear_richness <- glm(sp_richness ~ diameter, data = data, family = "poisson")
tidy(linear_richness)

#height
anova_richness <- aov(richness ~ height, data = data)
anova(anova_richness)

linear_richness <- glm(sp_richness ~ height, data = data,family = "poisson")
tidy(linear_richness)
summary(linear_richness)

ggplot(data, aes(x = height, y = sp_richness)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Richness vs. Height", x = "Height", y = "Total Visits")+
  theme_hc()

# Extract fitted values (predicted total visits) and residuals
data$fitted <- fitted(linear_richness)
data$residuals <- residuals(linear_richness)

export_data <- data.frame(
  height = data$height,
  richness = data$sp_richness,
  fitted_values = data$fitted,
  residuals = data$residuals
)

write.csv(export_data, "richness_regression_height_poisson.csv", row.names = FALSE)


#design
krusk_richness <- kruskal.test(richness ~ design, data = data)
tidy(krusk_richness)

#material
krusk_richness <- kruskal.test(richness ~ material, data = data)
tidy(krusk_richness)

#aspect
krusk_richness <- kruskal.test(richness ~ aspect, data = data)
tidy(krusk_richness)


