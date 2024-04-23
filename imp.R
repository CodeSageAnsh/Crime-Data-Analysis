# Load necessary libraries
library(readr)
library(dplyr, warn.conflicts = FALSE)  # Suppress warning about conflicts

# Read the CSV file
crime_data <- read_csv("a.csv")
# Filter out numeric columns
numeric_cols <- sapply(crime_data, is.numeric)

# Iterate over each numeric column and create a histogram
for (i in which(numeric_cols)) {
  hist(crime_data[[i]], main = colnames(crime_data)[i])
}

# Display the structure of the dataset
str(crime_data)

# Check for missing values
missing_values <- colSums(is.na(crime_data))
missing_values <- missing_values[missing_values > 0]
missing_values

# Load necessary library
library(ggplot2)

# Create a histogram for the "MURDER" variable using base R graphics
hist(crime_data$MURDER, 
     col = "skyblue", 
     main = "Distribution of Murder Cases",
     xlab = "Number of Murder Cases",
     ylab = "Frequency")

# Create a bar plot for the "MURDER" variable by district using base R graphics
barplot(crime_data$MURDER, 
        names.arg = crime_data$DISTRICT,
        col = "skyblue",
        main = "Murder Cases by District",
        xlab = "District",
        ylab = "Number of Murder Cases",
        las = 2)  # Rotate x-axis labels for better readability

# Calculate the total number of cases for each crime type
total_cases <- colSums(crime_data[, -c(1:2, 3)])  # Exclude STATE/UT and DISTRICT columns, and YEAR column

# Create a data frame with crime types and their corresponding total cases
crime_intensity <- data.frame(Crime_Type = names(total_cases), Total_Cases = total_cases)

# Order the data frame by the total number of cases in descending order
crime_intensity <- crime_intensity[order(crime_intensity$Total_Cases, decreasing = TRUE), ]

# Calculate the proportion of each crime type relative to the total number of cases
crime_intensity$Proportion <- crime_intensity$Total_Cases / sum(crime_intensity$Total_Cases)

# Plot a pie chart
pie(crime_intensity$Proportion, labels = crime_intensity$Crime_Type, main = "Crime Distribution", col = rainbow(length(crime_intensity$Crime_Type)))


# Group similar crime types into broader categories
crime_intensity$Category <- ifelse(grepl("MURDER|RAPE|KIDNAPPING", crime_intensity$Crime_Type), "Violent Crimes",
                                   ifelse(grepl("BURGLARY|THEFT|ROBBERY", crime_intensity$Crime_Type), "Property Crimes",
                                          "Other Crimes"))

# Aggregate the total number of cases for each category
total_cases_category <- aggregate(Total_Cases ~ Category, data = crime_intensity, sum)

# Calculate the proportion of each category relative to the total number of cases
total_cases_category$Proportion <- total_cases_category$Total_Cases / sum(total_cases_category$Total_Cases)

# Plot a pie chart for the broader crime categories
pie(total_cases_category$Proportion, labels = total_cases_category$Category, main = "Crime Distribution by Category", col = rainbow(length(total_cases_category$Category)))

# Summary statistics for numerical variables related to crime
crime_summary <- summary(crime_data[, -c(1:2)])

# Print the summary statistics
print(crime_summary)


# Define numeric_cols as the names of numeric columns
numeric_cols <- names(Filter(is.numeric, crime_data))

# Check if numeric_cols is not empty
if (length(numeric_cols) > 0) {
  # Iterate over each numeric column and try to create a histogram
  for (col in numeric_cols) {
    hist(crime_data[[col]], main = col)
  }
} else {
  print("No numeric columns found in the data.")
}





















# Histograms for numerical variables related to crime
par(mfrow = c(4, 4))  # Set up a 4x4 grid for multiple plots

for (i in 4:ncol(crime_data)) {  # Starting from the 4th column which corresponds to numerical variables
  hist(crime_data[, i], main = colnames(crime_data)[i], xlab = "", col = "skyblue", border = "black")
}

# Check for missing values
any_missing <- any(is.na(crime_data))
if (any_missing) {
  print("There are missing values in the dataset.")
} else {
  print("No missing values found in the dataset.")
}

# Check for non-numeric entries in numeric columns
numeric_cols <- sapply(crime_data, is.numeric)
non_numeric_entries <- sapply(crime_data[, numeric_cols], function(x) any(!is.na(as.numeric(x))))
if (any(non_numeric_entries)) {
  print("There are non-numeric entries in numeric columns.")
} else {
  print("No non-numeric entries found in numeric columns.")
}

# Check for zero variance columns
zero_variance_cols <- sapply(crime_data[, numeric_cols], function(x) length(unique(x)) == 1)
if (any(zero_variance_cols)) {
  print("There are zero variance columns.")
} else {
  print("No zero variance columns found.")
}
sapply(crime_data, class)
# Get the names of all numeric columns
numeric_cols <- names(Filter(is.numeric, crime_data))

# Iterate over each numeric column and try to create a histogram
for (col in numeric_cols) {
  hist(crime_data[[col]], main = col)
}
