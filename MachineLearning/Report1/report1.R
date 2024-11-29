# Created on the 30-08 2023
# Author: Jeppe Aarup Andersen

rm(list=ls())
# setwd("C:/Faculdade/Intercâmbio/Machine Learning/Report1")

library(dplyr)
library(ggplot2)
library(GGally)
library(rgl)
library(reshape2)
library(data.table)
library(gridExtra)
library(plotly)
library(nnet)
library(glmnet)
library(caret)
library(rpart)
library(class)

# Data format ##################################################################
data <- read.csv(here::here("Glass/glass.data"))


#Add class names
data$typeName[data$Type == "1"] <- "Building, float"
data$typeName[data$Type == "2"] <- "Building, non-float"
data$typeName[data$Type == "3"] <- "Vehcle, float"
data$typeName[data$Type == "4"] <- "Vehicle, non-float" #None in this data set
data$typeName[data$Type == "5"] <- "Container"
data$typeName[data$Type == "6"] <- "Tableware"
data$typeName[data$Type == "7"] <- "Headlamp"

#Remove numeric class attribute as there is one missing type.
data <- dplyr::select(data, -c("Type"))

# Extract attributes.
X <- data[, 2:10] #RI and ions M%
classLabels <- data[, 11] 

# Extract class labels of observations.
attributeNames <- colnames(data[2:10])

# Check and record dimensions
(N <- dim(X)[1])
(M <- dim(X)[2])

# Extract numeric class assignments.
y <- as.numeric(as.factor(classLabels)) - 1

# Extract the class names present in data.
classNames <- unique(classLabels)

# Extract number of classes.
C = length(classNames)

# Count number of data points per class
table(classLabels)

###############################################################################
# Variance explained ###########################################################
# Calculate variance of each column of X
for (i in 1:length(X)) {
    print(var(X[,i]))
}

# We have different scales and variance so we will be using Zero mean and unit variance

# Subtract the column means form columns of X
Y <- t(apply(X, 1, "-", colMeans(X)))

# Devide by the standard deviation in Y2
stds <- apply(X, 2, sd)
Y <- t(apply(Y, 1, "*", 1 / stds)) 


# Computing PCA:
s <- svd(Y)
diagS <- s$d
rho <- diagS^2 / sum(diagS^2)

# Confirm calculations with R base function
rho_base <- summary(prcomp(X, scale. = T, center = TRUE))$importance[2,]


xlimits <- c(1, M)
plot(rho,
     type = "o",
     xlab = "Principal components",
     ylab = "Variance explained",
     xlim = xlimits,
     ylim = c(0, 1),
     col = "blue"
)
lines(cumsum(rho), type = "o", col = "orange")
lines(xlimits, c(0.9, 0.9), lty = "dashed")

legend("right", # Define position
       legend = c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col = c("blue", "orange", "black"), lty = c(1, 1, 2), # Match appereance of lines
       cex = 1, bg = "lightgrey"
) # Setup how the box looks (cex controls size)

################################################################################
# Some plots for PCA ########################################################
# manual
U <- s$u
S <- diag(s$d)
V <- s$v
Z <- U %*% S

# confirm calculations with R base function
pca = prcomp(X, scale. = T, center = TRUE)

V_base <- pca$rotation
Z_base = pca$x

# Correlation between components and attributes 
# Bar plot
pcs <- 1:5
test <- as.data.frame(melt(data.table(V_base[, pcs])))
ggplot(test, aes(x = rep(1:9, length(pcs)), y = value, fill=variable)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(fill="PCA", x = "Attributes", y = "Component coefficients") +
  scale_x_continuous(labels = rownames(V_base), breaks = seq(1,9, by = 1))

# Radius plot
i = 1
j = 2
par(mfcol = c(1, 1), pty = "s")
plot(c(-1, 1), c(-1, 1),
     xlab = paste("PC", toString(i)), ylab = paste("PC", toString(j)),
     type = "n",
     main = paste('Zero-mean and unit variance', "\nAttribute coefficients")
)
arrows(integer(M), integer(M),
       V[, i], V[, j],
       length = .1,
       col = "blue"
)
text(V[, i] * 1.1, V[, j] * 1.1, attributeNames, cex = 1)
# Add a unit circle
th <- seq(0, 2.1 * pi, 0.1)
lines(cos(th), sin(th))

# Data projection
# Iterate over values of i and j from 1 to 5 to see all the pairs, here we have the ones used in the report
i=1
j=2
ggplot() +
  geom_point(aes(x = Z_base[, i], y = Z_base[, j], color = factor(classLabels)), size = 4, alpha = 0.5) +
  theme(legend.position = c(0.1, 0.15), legend.title = element_blank(), legend.background = element_rect('#EBEBEB')) +
  labs(x = colnames(Z_base)[i], y = colnames(Z_base)[j])
i=1
j=4
ggplot() +
  geom_point(aes(x = Z_base[, i], y = Z_base[, j], color = factor(classLabels)), size = 4, alpha = 0.5) +
  theme(legend.position = c(0.1, 0.15), legend.title = element_blank(), legend.background = element_rect('#EBEBEB')) +
  labs(x = colnames(Z_base)[i], y = colnames(Z_base)[j])

# Iterate over values of i, j and hfrom 1 to 5 to see all the combinations of 3, here we have the one used in the report
i=1
j=2
h=4
Z_base <- as.data.frame(Z_base)
fig <- plot_ly(data = Z_base, x = ~Z_base[, i], y = ~Z_base[, j], z = ~Z_base[, h], showlegend = T,
               type = "scatter3d",
               mode = "markers",
               color = factor(classLabels))
fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(Z_base)[i]),
                                   yaxis = list(title = colnames(Z_base)[j]),
                                   zaxis = list(title = colnames(Z_base)[h])))




