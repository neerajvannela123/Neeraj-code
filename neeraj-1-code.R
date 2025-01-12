rm(list = ls())

getwd()


install.packages("tzdb")

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")


install.packages("corrplot")     # For correlation plot
install.packages("ggplot2")      # For data visualization


library(corrplot)
library(ggplot2)

data <- read.csv("est16us.csv", header = TRUE)


# Drop the first row if it contains unnamed or unwanted data
data <- data[-1, ]

head(data)

# Rename the columns as specified
colnames(data) <- c(
  "State FIPS Code",
  "Postal Code",
  "Name",
  "Poverty Estimate (All Ages)",
  "90% CI Lower Bound (All Ages)",
  "90% CI Upper Bound (All Ages)",
  "Poverty Percent (All Ages)",
  "90% CI Lower Bound (All Ages)",
  "90% CI Upper Bound (All Ages)",
  "Poverty Estimate (Age 0-17)",
  "90% CI Lower Bound (Age 0-17)",
  "90% CI Upper Bound (Age 0-17)",
  "Poverty Percent (Age 0-17)",
  "90% CI Lower Bound (Age 0-17)",
  "90% CI Upper Bound (Age 0-17)",
  "Poverty Estimate (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Poverty Percent (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Median Household Income (Age 5-17 in Families)",
  "90% CI Lower Bound (Age 5-17 in Families)",
  "90% CI Upper Bound (Age 5-17 in Families)",
  "Poverty Estimate (Age 0-4)",
  "90% CI Lower Bound (Age 0-4)",
  "90% CI Upper Bound (Age 0-4)",
  "Poverty Percent (Age 0-4)",
  "90% CI Lower Bound (Age 0-4)",
  "90% CI Upper Bound (Age 0-4)"
)

# View the modified data
head(data)

str(data)


library(dplyr)

# Ensure column names are unique
colnames(data) <- make.unique(colnames(data))

# Convert appropriate columns to numeric while preserving column names
data <- data %>%
  mutate(across(
    .cols = -c(`State FIPS Code`, `Postal Code`, Name), # Exclude non-numeric columns
    .fns = ~ as.numeric(.),                            # Convert to numeric
    .names = "{.col}"                                  # Keep original column names
  ))


# Verify the structure of the updated data
str(data)

# Check for any NA values in numeric columns after conversion
summary(data)

# Check for missing values
colSums(is.na(data))

# Drop rows with missing values (if necessary)
data <- na.omit(data)


# Summary statistics of the dataset
summary(data)

head(data)

str(data)

#Question-1(A)

# Scatterplot with Linear Trendline
scatterplot <- ggplot(data, aes(x = `Median Household Income (Age 5-17 in Families)`, y = `Poverty Percent (All Ages)`)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Scatterplot of Poverty Percent vs Median Household Income",
    x = "Median Household Income (Age 5-17 in Families)",
    y = "Poverty Percent (All Ages)"
  ) +
  theme_minimal()

# Print the scatterplot
print(scatterplot)


#Question-1(B)

##Histogram

# Histogram with Normal Curve Overlay
# Calculate mean and standard deviation for the normal curve
mean_poverty <- mean(data$`Poverty Percent (All Ages)`, na.rm = TRUE)
sd_poverty <- sd(data$`Poverty Percent (All Ages)`, na.rm = TRUE)

histogram <- ggplot(data, aes(x = `Poverty Percent (All Ages)`)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "skyblue", alpha = 0.7) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean_poverty, sd = sd_poverty),
    color = "red",
    size = 1
  ) +
  labs(
    title = "Histogram of Poverty Percent (All Ages) with Normal Curve",
    x = "Poverty Percent (All Ages)",
    y = "Density"
  ) +
  theme_minimal()

# Print the histogram
print(histogram)


