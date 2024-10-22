#check the wd
getwd()

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(broom)      # For tidy model output
library(ggplot2)
library(ggthemes)
library(gridExtra)

grid.arrange(plot_liz, plot_no_liz, nrow=1)
