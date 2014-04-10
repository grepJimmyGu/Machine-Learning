
# General Data Analysis
data <- read.csv("general_data.csv", header=TRUE, sep = ";")
# Profit Per Click
data["PRP"] <- (data["Revenue"]-data["Spend"])/data["Clicks"]
# Revenue per order
data["RPO"] <- data["Revenue"]/data["Orders"]
# Correlation 
library(corrplot)
part <- cbind(data[,2],data[,3],data[,6],data[,9])
colnames(part) <- c("Imp","Clicks","Orders","Revenue")
corrplot(cor(part), method = "ellipse")
# Performance 
plot(cbind(c(1:12),data["ROAS"]), type = "l")
# Other plots
plot(cbind(c(1:12),scale(data["PRP"])), ylab = "", type = "l", main = "Comparison", xlab = "Every Month")
lines(cbind(c(1:12), scale(data["RPO"])), lty = 2)
legend("topleft", c("Profit Per Click", "Revenue Per Order"), lty = c(1,2), cex=0.75)

# Key Words Analysis
keywords <- read.csv("Keywords.csv", header = TRUE, sep = ";")
keywords <- as.data.frame(keywords)
keywords <- keywords[-9,]
keywords <- keywords[-10,]
row.names(keywords) <- keywords[,1]
# Profit Per Click
keywords["PRP"] <- (keywords["Revenue"] - keywords["Spend"])/keywords["Clicks"]
plot(cbind(keywords["Clicks"],keywords["PRP"]))
abline(h = 0)
identify(cbind(keywords["Clicks"],keywords["PRP"]))
legend("topleft", c("2:Acme Tennis", "3:Acme Tennis Balls", "4:Tennis Balls", "6:Buy Tennis Balls"), cex = 0.75)
name <- c("Acme","Acme Tennis","Acme Tennis Balls","Tennis Balls","Bouncy Tennis Balls","Buy Tennis Balls",
          "Free Balls","Tennis Raquets","Sports Equipment","Andre Agassi")
rownames(key) <- name
colnames(key) <- c("Imp", "Clicks", "Orders")
key_pr <- prcomp(key, rtex = TRUE)

# Ridge Regression
library(glmnet)
X <- cbind(as.matrix(data["Imp"]),as.matrix(data["Clicks"]),as.matrix(data["Orders"]))
fit <- glmnet(X, as.matrix(data["Revenue"]), family = "gaussian", alpha = 0)
cv.fit <- cv.glmnet(X, as.matrix(data["Revenue"]), type.measure = "mse", nfolds = 12, grouped=FALSE)
coef(fit, s = cv.fit$lambda.min)
ad_1 <- c(1160023, 5796, 1129)
ad_2 <- c(1070790, 4554, 1273)
predict(fit, newx = rbind(ad_1,ad_2), s = cv.fit$lambda.min)
cbind(predict(fit, newx = X, s = cv.fit$lambda.min), as.matrix(data["Revenue"]))
