# DA 2007 Final Exam R Code Note
# Covers: Introduction to R, Data Management, Descriptive Analysis, Writing Functions,
# Distributions, and Basic Statistical Inference
# Excludes: Matrices, duplicates, missing value handling, ggplot(), measures of shape,
# Pearson correlation
# Uses base R for plots
# Date: June 15, 2025

# -----------------------------------
# 1. Introduction to R
# -----------------------------------
# Install R and RStudio (refer to "Installation and Setup for R.pdf")
# R is a free, open-source language for statistical analysis and visualization
# RStudio is an IDE for writing and executing R code

# Basic R operations
# Arithmetic
5 + 3          # Addition, output: 8
10 - 4         # Subtraction, output: 6
6 * 2          # Multiplication, output: 12
8 / 2          # Division, output: 4
2^3            # Exponentiation, output: 8

# Vectors
x <- c(1, 2, 3, 4, 5)  # Create a numeric vector
y <- c("a", "b", "c")  # Create a character vector
length(x)              # Length of vector, output: 5
sum(x)                 # Sum of elements, output: 15
mean(x)                # Mean of elements, output: 3

# Data frames (basic structure)
data(iris)             # Load built-in iris dataset
head(iris)             # View first 6 rows
dim(iris)              # Dimensions: 150 rows, 5 columns
colnames(iris)         # Column names

# -----------------------------------
# 2. Data Management in R
# -----------------------------------
# Excludes: Identifying/handling duplicates, missing value identification/handling
# Focus: Importing data, data exploration, handling categorical variables,
# data selection/subsetting, creating new variables, exporting data, user input

# Importing Data
# Read CSV file (correct path with double backslashes or forward slashes)
iris_data <- read.csv("D:\\Downloads\\IRIS.csv", header = TRUE)  # Example path
# Alternative: forward slash
iris_data <- read.csv("D:/Downloads/IRIS.csv", header = TRUE)
head(iris_data)  # View first few rows

# Read Excel files (requires readxl package)
# install.packages("readxl")
library(readxl)
# Example: excel_data <- read_xlsx("data.xlsx")

# Read tabular data
sample_data <- read.table("D:/Downloads/sample_data.txt", header = TRUE, sep = ",")
head(sample_data)

# Data Exploration
dim(sample_data)        # Number of rows and columns
colnames(sample_data)   # Column names
head(sample_data)       # First 6 rows
tail(sample_data)       # Last 6 rows
summary(sample_data)    # Summary statistics

# Handling Categorical Variables
# Nominal: No order (e.g., Gender)
gender <- c("Male", "Female", "Male", "Female")
gender_factor <- factor(gender)  # Convert to factor
levels(gender_factor)           # Levels: Female, Male

# Ordinal: Ordered categories (e.g., Education)
education <- c("Bachelor", "Masters", "High School", "PhD")
education_ordered <- factor(education, 
                            levels = c("High School", "Bachelor", "Masters", "PhD"), 
                            ordered = TRUE)
levels(education_ordered)

# Data Selection and Subsetting
# Using subset()
# Filter rows where sepal length > 7.0
subset(iris, Sepal.Length > 7.0)

# Filter Iris-setosa species
subset(iris, Species == "setosa")

# Filter Iris-setosa with sepal width > 4.0
subset(iris, Species == "setosa" & Sepal.Width > 4.0)

# Select specific columns
subset(iris, Sepal.Length > 4.0, select = c(Sepal.Length, Sepal.Width))

# Using indexing
iris[1:2, ]                     # First 2 rows
iris[, c("Petal.Width", "Sepal.Width")]  # Select specific columns

# Creating New Variables (using base R, avoiding dplyr::mutate)
# Example: Create new variable for sepal area
iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width

# Modify sepal length by multiplying by 10
iris$Sepal.Length <- iris$Sepal.Length * 10

# Conditional column: Classify sepal width
iris$Sepal.Width.Class <- ifelse(iris$Sepal.Width > 3.5, "Large", "Small")

# Exporting Data
# Export to CSV
write.csv(iris, "output_iris.csv", row.names = FALSE)

# Export to text file
write.table(iris, "output_iris.txt", sep = "\t", row.names = FALSE)

# Export to Excel (requires writexl package)
# install.packages("writexl")
library(writexl)
# write_xlsx(iris, "output_iris.xlsx")

# Getting User Input
# readline() for single-line input
name <- readline(prompt = "Enter your name: ")
print(name)

# scan() for multiple values
numbers <- scan(what = double(), nmax = 5)  # Enter 5 numbers
print(numbers)

# -----------------------------------
# 3. Descriptive Analysis in R
# -----------------------------------
# Excludes: ggplot(), measures of shape (skewness, kurtosis), Pearson correlation
# Focus: Measures of central tendency, measures of spread, base R plots

# Load heart disease dataset for demonstration
df <- read.csv("D:\\Demonstrators\\heart-diseases.csv", header = TRUE)
head(df)

# Measures of Central Tendency
# Mean
mean_age <- mean(df$Age)  # Example output: 54.37
mean_bp <- mean(df$Resting.bp)

# Median
median_age <- median(df$Age)  # Example output: 55
median_bp <- median(df$Resting.bp)

# Mode (custom function)
mode_func <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}
mode_age <- mode_func(df$Age)
mode_chest_pain <- mode_func(df$Chest.pain.type)

# Measures of Spread/Dispersion
# Range
range_age <- range(df$Age)
diff_age <- diff(range_age)  # Max - Min
cat("Range of Age:", diff_age, "\n")

# Variance
var_age <- var(df$Age)
var_bp <- var(df$Resting.bp)

# Standard Deviation
sd_age <- sd(df$Age)
sd_bp <- sd(df$Resting.bp)

# Interquartile Range (IQR)
iqr_age <- IQR(df$Age)
iqr_bp <- IQR(df$Resting.bp)

# Data Visualization (Base R Plots)
# Convert categorical variables to factors
df$Chest.pain.type <- as.factor(df$Chest.pain.type)
df$Gender <- as.factor(df$Gender)

# Bar Plot (Vertical)
barplot(table(df$Chest.pain.type),
        col = "red",
        xlab = "Chest Pain Type",
        ylab = "Frequency",
        main = "Bar Chart for Chest Pain Type")

# Bar Plot (Horizontal)
barplot(table(df$Chest.pain.type),
        col = "red",
        xlab = "Frequency",
        ylab = "Chest Pain Type",
        main = "Horizontal Bar Chart for Chest Pain Type",
        horiz = TRUE)

# Stacked Bar Chart
barplot(table(df$Chest.pain.type, df$Gender),
        col = c("red", "orange", "yellow", "white"),
        xlab = "Gender",
        ylab = "Frequency",
        main = "Stacked Bar Chart for Gender by Chest Pain Type")
legend("topright", legend = levels(df$Chest.pain.type),
       fill = c("red", "orange", "yellow", "white"), title = "Chest Pain Type")

# Pie Chart
freq_table <- table(df$Gender)
barplot(freq_table,
        col = c("lightblue", "pink"),
        main = "Pie Chart for Gender")
legend("topright", legend = names(freq_table), fill = c("lightblue", "pink"), title = "Gender")

# Histogram
hist(df$Age,
     col = "skyblue",
     breaks = 20,
     xlab = "Age",
     ylab = "No. of Patients",
     main = "Histogram of Age")

# Box Plot (Single Variable)
boxplot(df$Age,
        col = "skyblue",
        xlab = "Age",
        ylab = "No. of Patients",
        main = "Boxplot of Age")

# Box Plot (Compare Across Categories)
boxplot(Age ~ Chest.pain.type, data = df,
        col = "skyblue",
        xlab = "Chest Pain Type",
        ylab = "Age",
        main = "Boxplot of Age by Chest Pain Type")

# Scatter Plot
plot(df$Age, df$Cholesterol,
     col = "red",
     xlab = "Age",
     ylab = "Cholesterol Levels",
     main = "Scatter Plot of Age vs Cholesterol")

# Line Chart
d <- rnorm(100, 0, 2.2)
plot(d, type = "l", main = "Line Chart of Random Data", xlab = "Index", ylab = "Value")

# -----------------------------------
# 4. Writing Functions in R
# -----------------------------------
# Focus: All examples and tutorial questions

# Basic Function: Difference
difference <- function(x, y) {
  return(x - y)
}
difference(9, 5)  # Output: 4

# Function: Division
division <- function(v, z) {
  k <- v / z
  return(k)
}
division(8, 2)  # Output: 4

# Function: Maximum Value in Vector
max_value <- function(d) {
  max_val <- d[1]  # Initialize first element
  for (x in d) {
    if (x > max_val) {
      max_val <- x  # Update if current element is larger
    }
  }
  return(max_val)
}
max_value(c(25, 56, 12, 98, 15, 14))  # Output: 98

# Nested Functions: Circle Calculations
radius_from_diameter <- function(d) {
  d / 2
}
circumference <- function(r) {
  2 * pi * r
}
circumference(radius_from_diameter(4))  # Output: 12.56637

# Nested Function: Sum of Circle Areas
sum_circle_areas <- function(r1, r2, r3) {
  circle_area <- function(r) {
    pi * r^2
  }
  circle_area(r1) + circle_area(r2) + circle_area(r3)
}
sum_circle_areas(1, 2, 3)  # Output: 53.40708

# Exercise Functions
# 1. Check Even or Odd
check_even_odd <- function(n) {
  if (n %% 2 == 0) {
    return("Even")
  } else {
    return("Odd")
  }
}
check_even_odd(4)  # Output: "Even"

# 2. Sum of Vector (without sum())
sum_vector <- function(vec) {
  total <- 0
  for (x in vec) {
    total <- total + x
  }
  return(total)
}
sum_vector(c(1, 2, 3, 4))  # Output: 10

# 3. Celsius to Fahrenheit
celsius_to_fahrenheit <- function(c) {
  f <- (9/5) * c + 32
  return(f)
}
celsius_to_fahrenheit(25)  # Output: 77

# 4a. Function f(x) = x^2 - 3x + 5
f <- function(x) {
  return(x^2 - 3*x + 5)
}
f(2)  # Output: 3

# 4b. Function g(x) = e^x - 4x
g <- function(x) {
  return(exp(x) - 4*x)
}
g(1)  # Output: -1.281718

# 5. Variance
my_variance <- function(x) {
  n <- length(x)
  x_bar <- sum(x) / n
  sum_squares <- sum((x - x_bar)^2)
  var <- sum_squares / (n - 1)
  return(var)
}
my_variance(c(1, 2, 3, 4, 5))  # Output: 2.5

# 6. Stairs
stairs <- function(n) {
  for (i in 1:n) {
    cat(rep("#", i), "\n")
  }
}
stairs(3)  # Output:
# #
# ##
# ###

# -----------------------------------
# 5. Distributions in R
# -----------------------------------
# Focus: All discrete and continuous distributions, R functions (d, p, q, r)

# Discrete Distributions
# Binomial: Number of successes in n trials
set.seed(100)  # For reproducibility
binomial_data <- rbinom(1000, size = 10, prob = 0.5)  # Simulate 1000 values
barplot(table(binomial_data), col = "blue", main = "Binomial Distribution",
        xlab = "Number of Successes", ylab = "Frequency")
mean(binomial_data)  # Mean
var(binomial_data)   # Variance
dbinom(3, size = 10, prob = 0.5)  # P(X = 3)
pbinom(3, size = 10, prob = 0.5)  # P(X <= 3)
qbinom(0.25, size = 10, prob = 0.5)  # 25th percentile

# Poisson: Number of events in fixed interval
poisson_data <- rpois(1000, lambda = 5)
barplot(table(poisson_data), col = "blue", main = "Poisson Distribution",
        xlab = "Number of Events", ylab = "Frequency")

# Geometric: Trials until first success
geometric_data <- rgeom(1000, prob = 0.3)
barplot(table(geometric_data), col = "blue", main = "Geometric Distribution",
        xlab = "Trials Until First Success", ylab = "Frequency")

# Negative Binomial: Failures until r successes
negbinom_data <- rnbinom(1000, size = 3, prob = 0.5)
barplot(table(negbinom_data), col = "blue", main = "Negative Binomial Distribution",
        xlab = "Number of Failures", ylab = "Frequency")

# Continuous Distributions
# Normal
normal_data <- rnorm(1000, mean = 0, sd = 1)
hist(normal_data, col = "red", breaks = 30, main = "Normal Distribution",
     xlab = "Values", ylab = "Frequency")
dnorm(0, mean = 0, sd = 1)  # Density at x = 0
pnorm(1, mean = 0, sd = 1)  # P(X <= 1)
qnorm(0.95, mean = 0, sd = 1)  # 95th percentile

# Uniform
uniform_data <- runif(1000, min = 0, max = 1)
hist(uniform_data, col = "red", breaks = 30, main = "Uniform Distribution",
     xlab = "Values", ylab = "Frequency")

# Exponential
exponential_data <- rexp(1000, rate = 1)
hist(exponential_data, col = "red", breaks = 30, main = "Exponential Distribution",
     xlab = "Values", ylab = "Frequency")

# Exercise Example: Binomial Distribution
# Bank credit card payments, Bin(25, 0.75)
set.seed(123)
credit_data <- rbinom(100, size = 25, prob = 0.75)  # i. 100 observations
barplot(table(credit_data), col = "blue", main = "Credit Card Payments",
        xlab = "Number of On-Time Payments", ylab = "Frequency")  # ii. Bar chart
dbinom(20, size = 25, prob = 0.75)  # iii. P(X = 20)
pbinom(18, size = 25, prob = 0.75)  # iv. P(X <= 18)
pbinom(20, size = 25, prob = 0.75) - pbinom(14, size = 25, prob = 0.75)  # v. P(15 <= X <= 20)
qbinom(0.90, size = 25, prob = 0.75)  # vi. Top 10% threshold

# -----------------------------------
# 6. Basic Statistical Inference in R
# -----------------------------------
# Focus: All tests (normality, homogeneity, means, variances, proportions)

# Normality Tests
library(nortest)  # For Anderson-Darling test
# Example: Blood glucose data
blood_glucose <- c(112, 135, 144, 158, 160, 166, 170, 174, 178, 185, 190, 200)
ad.test(blood_glucose)  # p-value = 0.8155, do not reject normality

# Shapiro-Wilk Test
shapiro.test(blood_glucose)

# Q-Q Plot
qqnorm(blood_glucose, main = "Q-Q Plot for Blood Glucose")
qqline(blood_glucose, col = "red")

# Homogeneity of Variances
library(car)
# Example: Blood glucose for males and females
blood_glucose_males <- c(112, 135, 144, 158, 160, 166, 170, 174, 178, 185, 190, 200)
blood_glucose_females <- c(100, 132, 154, 164, 112, 156, 201, 195, 144, 154, 185, 110)
group <- rep(c(1, 2), each = 12)
blood_glucose <- c(blood_glucose_males, blood_glucose_females)
leveneTest(blood_glucose ~ factor(group), data = data.frame(blood_glucose, group))

# F-Test for two variances
var.test(blood_glucose_males, blood_glucose_females)

# One-Sample T-Test
# H0: Mean blood glucose = 150
t.test(blood_glucose, mu = 150, conf.level = 0.95)  # p-value = 0.07018, do not reject H0

# Two-Sample T-Test (Independent)
t.test(blood_glucose_females, blood_glucose_males, var.equal = TRUE)  # p-value = 0.2611

# Two-Sample T-Test (Binary Grouping)
heights <- c(155, 123, 140, 115, 133, 127, 157, 164, 144, 125, 174, 145, 119, 156, 155, 135, 149, 167, 176, 140)
gender <- c(2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2)
t.test(heights ~ gender, var.equal = TRUE)  # p-value = 0.5559

# Paired T-Test
marks_1 <- c(40, 52, 70, 54, 69, 70, 70, 58, 44, 51)
marks_2 <- c(65, 68, 90, 64, 79, 69, 85, 80, 60, 55)
t.test(marks_2, marks_1, paired = TRUE, alternative = "greater")  # p-value = 0.0002256

# One-Sample Variance Test
library(EnvStats)
lifespan <- c(980, 1020, 970, 1010, 990, 995, 1005, 985, 1008, 992)
varTest(lifespan, sigma.squared = 400, alternative = "two.sided")  # p-value = 0.3762416

# F-Test for Two Variances
brand_A <- c(36, 40, 34, 37, 39, 38, 35, 33, 36, 32, 41, 30, 29, 34, 38, 40, 42, 39, 37, 35)
brand_B <- c(28, 27, 30, 26, 29, 32, 25, 28, 27, 26, 30, 31, 33, 29, 30, 28, 27, 31, 30, 26, 25, 28, 29, 32, 30)
var.test(brand_A, brand_B)  # p-value = 0.03275

# One-Sample Proportion Test
prop.test(90, 200, p = 0.4, alternative = "greater")  # p-value = 0.07446

# Two-Sample Proportion Test
successes1 <- c(200, 250)
samples1 <- c(500, 600)
prop.test(successes1, samples1, alternative = "two.sided")  # p-value = 0.6183

# End of Code Note
