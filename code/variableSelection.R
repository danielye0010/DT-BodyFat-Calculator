library(ggplot2)
library(reshape2)
library(car)

data <- read.csv("BodyFat.csv")

# Remove rows with 0 or NA values
data <- data[rowSums(data == 0) == 0, ]
data <- na.omit(data)
data <- data[-which.max(data$BODYFAT), ]

# Calculate the correlation matrix
correlation_matrix <- cor(data)

# Melting the correlation matrix for ggplot usability
melted_correlation_matrix <- melt(correlation_matrix)

# Creating the heatmap
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = c(1.4, 0), # Adjust this coordinate to move the legend
        legend.direction = "horizontal") +
  coord_fixed() +
  labs(x = "", y = "", title = "Correlation Matrix Heatmap",
       subtitle = "Color intensity and hue represent correlation strength")


# Extract correlations with 'BODYFAT'
bodyfat_correlations <- correlation_matrix['BODYFAT', ]

# Removing the 'BODYFAT' self-correlation
bodyfat_correlations <- bodyfat_correlations[bodyfat_correlations != 1.0]

# Sorting the correlations
sorted_correlations <- sort(bodyfat_correlations, decreasing = TRUE)

# Printing the sorted correlations
print(sorted_correlations)

# Fit a linear model with the independent variables
lm_model <- lm(BODYFAT ~ ABDOMEN+   ADIPOSITY    +   CHEST, data = data)

# Calculate VIF
vif_results <- vif(lm_model)
print(vif_results)





