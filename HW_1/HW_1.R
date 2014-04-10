# Problem 1
setwd("/Volumes/有能出没/Stat 154/Stat 154")
meta <- read.csv("stock.csv")
# To calculate daily returns, I only need price, so I extract the data with price,
# date and company name.
stock <- data.frame("date" = meta$date, "COMP" = meta$COMNAM, "PRC" = meta$PRC)

# Just remove the raw data since it takes too much memory.
rm(meta); gc()
compname <- levels(factor(stock$COMP))

# f is a function that extract the price of the same company with 1342 price records.
f <- function(x){
  if(length(stock$PRC[stock$COMP == compname[x]]) == 1342){
    return(stock$PRC[stock$COMP == compname[x]])
  }
}
stock_l <- sapply(c(1:length(compname)), f)

# g is a function that extract the name of the company with 1342 price records.
g <- function(x){
  if(length(stock$PRC[stock$COMP == compname[x]]) == 1342){
    return(compname[x])
  }
}
names <- sapply(c(1:length(compname)), g)

# Now I construct the price_matrix with price record in different date as row and 
# with different company as column.
price_ma <- matrix(unlist(stock_l), nrow = 1342)
colnames(price_ma) <- unlist(names)

# I calculated the daily return matrix based on price_ma, and I transformed daily 
# return matrix into percentage representation so that I can avoid numerical issue.
daily_return <- (price_ma[-1,]-price_ma[1:1341,])/price_ma[1:1341,] * 100

# The I do PCA to the daily return matrix with scaled value.
prin_stock <- prcomp(daily_return, rtex = TRUE, scale = TRUE)
screeplot(prin_stock)
explain_var_stock <- sapply(1:144, function(i) sum(prin_stock$sdev[1:i]^2)/sum(prin_stock$sdev^2))
sum(explain_var_stock < 0.5)

# Now I plotted the 

plot(prin_stock$x[,1], prin_stock$x[,2], xlab = "First Principal Component", ylab = "Second Principal Component")

###############################################
prin_stock <- prcomp(t(daily_return), rtex = TRUE, scale = TRUE)
stock_proj <- t(prin_stock$rotation[,1:2])%*%scale(daily_return)
plot(t(stock_proj))
################################################



dist_stock <- dist(t(daily_return))
hc_stock <- hclust(dist_stock, method = "complete")
plclust(hc_stock, labels = FALSE)

# I chose the subset of data

dist_stock_sub <- dist(t(daily_return[,1:25]))
hc_stock_sub <- hclust(dist_stock_sub, method = "complete")
plclust(hc_stock_sub, labels = c(1:25))


# Problem Two
# 1). PCA and Kernel PCA to the data
cancerdata <- read.table("Cancer.txt", header = FALSE)
names(cancerdata) <- NULL
cancerdata <- as.matrix(cancerdata)
prin_cancer <- prcomp(t(cancerdata), rtex = TRUE)
prin_cancer$sdev
plot(prin_cancer$x[,1], prin_cancer$x[,2])

# Based on the screeplot, I found that the first several principal components contain a large portion
# of information, which can be used to compress and cluster the data
screeplot(prin_cancer)

# To further illustrate, I can explain most of the variance by first 10 principal components.
explain_var_cancer <- sapply(1:144, function(i) sum(prin_cancer$sdev[1:i]^2)/sum(prin_cancer$sdev^2))
sum(explain_var_cancer < 0.9) 

library(kernlab)
rbf <- rbfdot(sigma = 0.0001)
kern_cancer <- kernelMatrix(rbf, scale(t(cancerdata)))
kp_cancer <- kpca(kern_cancer)
plot(eig(kp_cancer))
cancer_proj <- t(pcv(kp_cancer)[,1:1])%*%scale(t(cancerdata))
hist(cancer_proj)
dim(pcv(kp_cancer))


# 2).
# If I don't use PCA, the kmeans cluster looks very bad
kmean <- kmeans(t(cancerdata), 14, nstart = 1)
plot(t(cancerdata), col = kmean$cluster)
kmean$cluster

# If I use the data projected in the first principal component, if we want to keep 80% information
project <- function(n){
  projection <- prin_cancer$rotation[,1:n]%*%(t(prin_cancer$rotation[,1:n])%*%scale(cancerdata))
  return(projection)
}
project_cancer <- project(28)
project_coord <- t(prin_cancer$rotation[,1:2])%*%scale(cancerdata)

kmeans_1st <- kmeans(t(project_cancer), 14, nstart = 1)
plot(cancerdata, col = kmeans_1st$cluster)
kmeans_1st$cluster
kmeans_1st$size

# k-medoids
library(cluster)
diss <- dist(t(cancerdata))
kmedoids <- pam(diss, 14, do.swap = FALSE)
plot(cancerdata, col = kmedoids$cluster)
kmedoids$clustering

diss_proj <- dist(t(project_cancer))
kmedoids_proj <- pam(diss_proj, 14, do.swap = FALSE)
plot(cancerdata, col = kmedoids_proj$cluster)
kmedoids_proj$clustering
kmedoids_proj$silinfo

# Comment: compared with k-means, k-medoids have a more spread out clustering in that
# all of the clusters have more than 15 points while the result of k-means contain a 
# cluster with only 6 points

# Problem 3
set.seed(100)
rgene <- function(r){
  theta <- runif(150, min = -pi, max = pi)
  x <- r*cos(theta) + rnorm(150, mean = 0, sd = 0.25)
  y <- r*sin(theta) + rnorm(150, mean = 0, sd = 0.25)
  return(cbind("x" = x, "y" = y))
}
X <- rgene(5); Y <- rgene(2.8); Z <- rgene(1);
# Draw the plot of graph
plot(X);points(Y, col="red");points(Z, col = "blue")
# Spectral Clustering
s_data <- rbind(X,Y,Z)
spec_cl <- specc(s_data, centers = 3)
# It is a really accurate clustering.
centers(spec_cl)

# Kernel PCA
s_kpc <- kpca(t(s_data))
eig(s_kpc)

# PCA
s_pc <- prcomp(s_data, rtex = TRUE, scale = TRUE)

# Problem 4
library(MASS)
set.seed(20)
X <- data.frame(rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100)) 
prin_X <- prcomp(X)
screeplot(prin_X)
## Comment: This is a bad screeplot since all of the principla components have similar variance,
########## thus, we can not use some of the principal compents to compress the data through projection.
########## We need to use all of the principal components since each of them contain similar amount of
########## information.

Y <- data.frame(rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100), rnorm(100, sd = 10))
prin_Y <- prcomp(Y)
screeplot(prin_Y)
## Comment: This is a good screeplot since the first principal component containa a large amount of
##########  information. We can use it to compress data and project point based on first principal 
##########  component.


# Problem 5
set.seed(250)
simulation <- cbind(matrix(rnorm(1000),nrow = 10), matrix(rnorm(30, sd = 10), nrow = 10))
screeplot(prcomp(simulation), main = "dimension 100 * ")

simulation_1 <- cbind(matrix(rnorm(1000),nrow = 20), matrix(rnorm(60, sd = 10), nrow = 20))
screeplot(prcomp(simulation_1), main = "dimension 20 * ")

simulation_2 <- cbind(matrix(rnorm(1000),nrow = 40), matrix(rnorm(120, sd = 10), nrow = 40))
screeplot(prcomp(simulation_1), main = "dimension 40 * 35")

simulation_3 <- cbind(matrix(rnorm(1000),nrow = 50), matrix(rnorm(150, sd = 10), nrow = 50))
screeplot(prcomp(simulation_3), main = "dimension 50 * 30")

simulation_4 <- cbind(matrix(rnorm(1000),nrow = 100), matrix(rnorm(300, sd= 10), nrow = 100))
screeplot(prcomp(simulation_4), main = "dimension 100 * 110")

simulation_5 <- cbind(matrix(rnorm(10000),nrow = 200), matrix(rnorm(600, sd= 10), nrow = 200))
screeplot(prcomp(simulation_5), main = "dimension 200 * 60")




# Problem 6
# I did this problem by constructing a matrix with fixed eigen value, and trace back the
# "original" matrix by multiplying orthogonal matrix with it. It looks like doing SVD 
# decomposition backwards.
set.seed(0)
A <- matrix(0, nrow = 8, ncol = 8)
eigenvalue <- c(2,2,2,2,1.5,1.25,1,0.75)
diag(A) <- eigenvalue
# Now we need to construct an orthogonal matrix, I did this by doing svd decomposition
# to a 8x8 random matrix a extract the U matrix.
U <- svd(matrix(rnorm(64),8))$u
# TheN I can construct a Z matrix
Z <- U%*%A%*%t(U)
prin_Z <- prcomp(Z)
screeplot(prin_Z)
explain_var_Z <- sapply(1:7, function(i) sum(prin_Z$sdev[1:i]^2)/sum(prin_Z$sdev^2))
explain_var_Z

# Problem 7
# For N(0,1) distribution
iqr <- qnorm(0.75) - qnorm(0.25); iqr
# For N(miu, sd) distribution, IQR = sd*(qnorm(0.75) - qnorm(0.25))
# Verification 
miu <- 10; sigma <- 5
iqr_new <- qnorm(0.75, mean = miu, sd = sigma) - qnorm(0.25, mean = 10, sd = sigma); iqr_miu
all.equal(iqr_new, sigma*iqr)
# For N(0,1)
fraction_outlier <- 1 - pnorm(qnorm(0.75) + 1.5*iqr) + pnorm(qnorm(0.25) - 1.5*iqr); fraction_outlier

