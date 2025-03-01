# Load Reticulate and other packages
library(reticulate)
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(broom)

# Open up CMD and run the following command to install the Kaggle API
# python -m pip install kaggle

# Install Kaggle API if not installed
reticulate::py_require("kaggle")

# Set environment variable for API key (ensure correct path format)
Sys.setenv(KAGGLE_CONFIG_DIR = "C:/Users/berm0006/Downloads/")

# Download the dataset from Kaggle
reticulate::py_run_string("
import kaggle
kaggle.api.dataset_download_files('deadier/play-games-and-success-in-students', path='C:/Users/berm0006/Downloads/', unzip=True)
")

# Read the correct dataset file (update filename if needed)
game <- read.csv("C:/Users/berm0006/Downloads/gameandgrade.csv")

# Display first few rows
head(game)

# Clean Data - Remove rows with missing values
game <- na.omit(game)

# Get rid of Playing.Games with a 2 instead of a 0 or 1
game <- game[game$Playing.Games %in% c(0, 1), ]

# Turn Grade into numeric
str(game$Grade)

game$Grade <- as.numeric(game$Grade)

# Summary statistics
summary(game)

# Data Visualization
# Boxplot of Grade Distribution by Game Playing
ggplot(game, aes(x = as.factor(Playing.Games), y = Grade)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Grade Distribution by Game Playing",
       x = "Playing Games (0 = No, 1 = Yes)",
       y = "Student Grade") +
  theme_minimal()

## Interpretation: Students who do not play games tend to have slightly higher grades compared to those who do play games.

# Scatterplot with Regression Line
ggplot(game, aes(x = Playing.Hours, y = log(Grade))) +
  geom_jitter(color = "blue", width = 0.3, height = 0.2, alpha = 0.8) +
  geom_smooth(method = "lm", color = "red", se = TRUE, lwd = 1.2) + 
  scale_x_continuous(breaks = seq(0, max(game$Playing.Hours), by = 1)) +
  scale_y_continuous(breaks = seq(40, 100, by = 5)) + 
  labs(title = "Relationship Between Hours of Game Playing and Student Grades",
       x = "Hours of Game Playing",
       y = "Student Grade") +
  theme_minimal()

## Interpretation: There is a negative relationship between the number of hours spent playing games and student grades.

# Density Plot of Grade Distribution
game$GamingCategory <- ifelse(game$Playing.Hours > 0, "Gamer", "Non-Gamer")

ggplot(game, aes(x = Grade, fill = GamingCategory)) +
  geom_density(alpha = 0.5) +
  labs(title = "Grade Distribution: Gamers vs Non-Gamers",
       x = "Student Grade",
       y = "Density") +
  theme_minimal()

## Interpretation: Non-gamers tend to have higher grades distribution compared to gamers.