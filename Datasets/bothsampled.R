# Load required library
library(ROSE)

# Load the initial dataset
data <- read.csv("Dataset.csv")

# Check the structure of the initial dataset
str(data)

# Check the proportion of attrition cases
prop.table(table(data$Attrition))

# Perform oversampling using ROSE package
oversampled_data <- ovun.sample(Attrition ~ ., data = data, method = "both", p = 0.5, seed = 222, N = 1000)$data

# Check the balance after oversampling
table(oversampled_data$Attrition)

# Save the oversampled dataset as "bothsampled.csv"
write.csv(oversampled_data, file = "bothsampled.csv", row.names = FALSE)

# Confirm that the dataset has been saved
if (file.exists("bothsampled.csv")) {
  print("Oversampled dataset saved successfully as 'bothsampled.csv'.")
} else {
  print("Error: Oversampled dataset could not be saved.")
}
