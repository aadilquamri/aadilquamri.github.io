library(dplyr)
library(tidyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate artificial customer data
customer_data <- data.frame(
  CustomerID = 1:100,
  PurchaseFrequency = sample(c("Weekly", "Monthly", "Rarely"), 100, replace = TRUE),
  AverageSpend = round(runif(100, 5, 50), 2),  # Random spend between $5 and $50
  PreferredProduct = sample(c("Coffee", "Snacks", "Desserts"), 100, replace = TRUE)
)

head(customer_data)

# Convert categorical data to numeric
customer_data$PurchaseFrequencyNum <- as.numeric(factor(customer_data$PurchaseFrequency))

# Normalize AverageSpend
customer_data$AverageSpendNorm <- scale(customer_data$AverageSpend)

# Prepare data for clustering (using two variables: PurchaseFrequencyNum and AverageSpendNorm)
cluster_data <- customer_data %>%
  select(PurchaseFrequencyNum, AverageSpendNorm)

# Apply k-means clustering with 3 clusters
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3)

# Add the cluster assignments to the original data
customer_data$Cluster <- as.factor(kmeans_result$cluster)

# View the clustered data
head(customer_data)

# Visualize the clusters
ggplot(customer_data, aes(x = PurchaseFrequencyNum, y = AverageSpendNorm, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation", x = "Purchase Frequency (Numeric)", y = "Normalized Average Spend") +
  theme_minimal()
