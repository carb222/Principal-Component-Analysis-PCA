# This R script provides a simple explanation and examples of PCA in R.

# PCA is an unsupervised technique for continuous data that creates new variables called principal components.

# PCA golden rule
# Variation = Information
# Our goal is to reduce the dimensionality of the data while still retaining most of the variability from de original data.
# We will try to keep q < p dimensions, where q are the principal components and p are the original variables.

# Some important points:
# - PCA is useful to get rid of multicollinearity.
# - It can be used with a correlation or a covariance matrix.
# - The number of PC to retain can be found with these methods:
#     - Proportion of variation.
#     - Cattell's method.
#     - Kaiser's method.
# - No model or distribution is assumed.
# - Outliers highly affect PCA.

# R code example

# We will take a look at a dataset on wine. This data set gives the results of a chemical analysis of wines grown in the same region in Italy but derived from three different cultivars.
# The analysis determined the quantities of 13 variables found in each of the three types of wines. The data were sourced from the UCI Machine Learning Repository and you can find more information about them here: https://archive.ics.uci.edu/dataset/109/wine.

# First we will read the data into R and review the different variables we will be working with:

wine <- read.csv("wine.data.csv")
str(wine, vec.len = 2)

# We can also make a pair plot to review patterns, outliers, etc.
pairs(wine[, c(1:2, 5:8, 14)], pch = 20, lower.panel = NULL)

# From this we can see that the variable "Class" is not continuous. Also, we can see some correlation between "Flavanoids" — "Total.phenols" and "Alcohol" — "Proline". There a couple outliers so we need to take care of them.

# Here we plot flavanoids vs proline to identify the outlier:
plot(wine$Flavanoids, wine$Proline, xlab = "Flanavoids", ylab = "Proline")

# We identify the outlier by plotting the max value in flavanoids and then removing it. We also will delete the first column since it is not continuous.
print(max(wine$Flavanoids)) # It prints 5.08
wine.new <- wine[wine$Flavanoids < 5.08, -1] # delete outlier and 1st column

# Load the corrplot library for visualizing correlation matrices
library(corrplot)

# Compute the correlation matrix for the wine.new data frame
M <- cor(wine.new)
# Plot
corrplot(M, method = "number", type = "upper", tl.cex = 0.7, tl.col = "black", tl.srt = 45, addrect = 2)

# You can see that some of the variables have high correlation, which means that some dimension reduction might be possible.

# We need to review the variances to decide if we are using the correlation or covariance matrix in our PCA.
print(round(diag(var(wine.new)), 2))

# As you can see, there are huge differences in the variances, so we proceed using the correlation matrix.
# Perform principal component analysis (PCA) on the wine.new data frame
# Set cor = TRUE to perform PCA using correlation matrix instead of covariance matrix
wine.pca <- princomp(wine.new, cor = TRUE)

# Output the results of PCA
print(wine.pca)

# It's anticipated that we obtain 13 components, given the initial 13 variables and 177 observations. Although the first component exhibits a notably larger standard deviation compared to the others, determining the number of components to retain isn't straightforward from this observation alone. To make an informed decision, we need to choose from one of three methods to determine the retained components. In this example, we'll explore all three methods, but it's important to note that, in practice, only one method should be selected.

# Proportion of Variance: A standard number is to keep 80% of the variability. We need to see the cumulative proportions of variance for all the components which we get using the summary command.
summary(wine.pca)

# So if we want to retain 80% of the variability we would keep 5 principal components.

# Cattell’s method: To use this method, we need to produce a scree plot of the proportion of variance versus component number. We expect this to drop off (since the eigenvalues are in decreasing order) but what we are looking for in the plot is a bend, where the rate of decrease suddenly reduces.
plot(wine.pca)

# There are a couple of different place that we could say represent a change in rate of decrease: between components 3 and 4, or 5 and 6 or 7 and 8. If we wanted to really reduce the dimensionality we could argue the first one and say we are retaining 3 components.

# Kaiser’s method: Here we need to look at finding the average eigenvalue to discover which set of components have variation above it that we will retain. Because we used the correlation matrix, we know the average should be 1 but let’s check.
# Extract the component standard deviations
sd.pca <- wine.pca$sdev 
# Find the average variance, take the mean after squaring them
ave.var <- mean((sd.pca ** 2)) 
ave.var

# Find which components have higher than average variance (TRUE) 
sd.pca ** 2 > ave.var

# So based on this, we would retain the first 3 PCs.

# We will now examine if there is any meaningful interpretation that can be derived from the loadings of the PCA for our wine dataset. To accomplish this, we extract the loadings (or rotation) matrix from the fitted object. Interpreting these loadings can be somewhat subjective. We examine each column in turn, looking for variables with relatively large positive values and grouping them together. Similarly, we identify variables with relatively large negative values and group them together as well. The principal component (PC) is interpreted as the contrast between these groups of variables. If all variables with significant loadings have the same sign, then that PC is interpreted as the (weighted) average or overall score of these variables.

wine.pca$loadings[, 1:7]

# For the first principal component, we observe two sets of variables with significant weightings: one with positive values and another with negative values. Therefore, we interpret this component as the disparity between the average values of alcohol, magnesium, total phenols, flavanoids, proanthocyanins, hue, OD280/OD315 of diluted wines, and proline, and the average values of malic acid, alcalinity of ash, nonflavanoid phenols, and color intensity. This same approach can be applied to all other components that we have chosen to retain.

# We are typically interested in visualizing the data in the new reduced space. The scores for the principal components on the original data are automatically generated by the princomp function. Therefore, we can create a pairs plot of the three principal components we have chosen to retain.

# Extract the scores for the first three principal components
scores.wine <- wine.pca$scores[, 1:3]

# Create a pairs plot of the scores
# Use pch=20 to set the point shape to filled circles
# Set lower.panel=NULL to remove lower panels (we only want scatterplots in the upper triangle)
pairs(scores.wine, pch = 20, lower.panel = NULL)

