#-------------------------------------------------------------------------------------------------------------------
# C744 10/12/2020
# James Shea
#-------------------------------------------------------------------------------------------------------------------
# Clear Memory
#-------------------------------------------------------------------------------------------------------------------
rm(list=ls())
sinkoutput <- FALSE
closeAllConnections()
R.version.string
#-------------------------------------------------------------------------------------------------------------------
# Library Loads
#-------------------------------------------------------------------------------------------------------------------
library(plyr)
library(FactoMineR)
library(caTools)
library(MASS)
library(car)
library(MLmetrics)
library(ROCR)
library(pROC)
#install.packages("ggplot2")
library(ggplot2)
library(factoextra)
#install.packages("dplyr")
#install.packages("factoextra")
#install.packages("broom")
#install.packages("rstatix")
#install.packages("ggpubr")
#install.packages("Rcpp")
#install.packages('e1071', dependencies=TRUE)
#install.packages('caret', dependencies=TRUE)

#-------------------------------------------------------------------------------------------------------------------
# Set Working Directory (Which already contains data file)
#-------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/brnfr/OneDrive/WGU/C744")
#-------------------------------------------------------------------------------------------------------------------
# Read Data File
#-------------------------------------------------------------------------------------------------------------------
raw <- read.csv("raw_data.csv")
#-------------------------------------------------------------------------------------------------------------------
# Get summary of data
#-------------------------------------------------------------------------------------------------------------------
summary(raw)
#-------------------------------------------------------------------------------------------------------------------
# Clean Data PART 2
#-------------------------------------------------------------------------------------------------------------------
# Remove Uneeded Columns
reduced <- subset(raw, select = -c(customerID, TotalCharges)) # No need for PrimaryKey. Totalcharges depends on others
# Recode Categorical Variables
reduced$SeniorCitizen <- ifelse(reduced$SeniorCitizen == 0, "False", "True")
reduced$Partner <- ifelse(reduced$Partner == "No", "False", "True")
reduced$Dependents <- ifelse(reduced$Dependents == "No", "False", "True")
reduced$PhoneService <- ifelse(reduced$PhoneService == "No", "False", "True")
reduced$MultipleLines <- ifelse(reduced$MultipleLines == "Yes", "True", "False")
reduced$InternetService <- ifelse(reduced$InternetService == "No", "False", "True")
reduced$OnlineSecurity <- ifelse(reduced$OnlineSecurity == "Yes", "True", "False")
reduced$OnlineBackup <- ifelse(reduced$OnlineBackup == "Yes", "True", "False")
reduced$DeviceProtection <- ifelse(reduced$DeviceProtection == "Yes", "True", "False")
reduced$TechSupport <- ifelse(reduced$TechSupport == "Yes", "True", "False")
reduced$StreamingMovies <- ifelse(reduced$StreamingMovies == "Yes", "True", "False")
reduced$StreamingTV <- ifelse(reduced$StreamingTV == "Yes", "True", "False")
reduced$Contract <- ifelse(reduced$Contract == "Month-to-month", "False", "True")
reduced$PaperlessBilling <- ifelse(reduced$PaperlessBilling == "Yes", "True", "False")
reduced$Churn <- ifelse(reduced$Churn == "Yes", "True", "False")
reduced$PaymentMethod <- ifelse(reduced$PaymentMethod == "Electronic check", "Manual", 
                                ifelse(reduced$PaymentMethod == "Mailed check", "Manual","Automatic"))
# Recode Numeric Variables - Tenure
summary(reduced$tenure) # Look at tenure
sd(reduced$tenure) # Summary and SD suggest bimodal distribution
hist(reduced$tenure, breaks=40, main="Tenure Frequency Chart", xlab="Tenure", ylab="Frequency") # Frequency Chart
reduced$tenure <- ifelse(reduced$tenure < 30, "Short", "Long") # Split at median

# Recode Numeric Variables - MonthlyCharges
summary(reduced$MonthlyCharges) # Look at MonthlyCharges
sd(reduced$MonthlyCharges) #
hist(reduced$MonthlyCharges, breaks=20, main="MonthlyCharges Frequency Chart", 
     xlab="MonthlyCharges", ylab="Frequency") # Frequency Chart
reduced$MonthlyCharges <- ifelse(reduced$MonthlyCharges < 71, "Low", "High") # Split at median

# Change Variables into Factors
summary(reduced)
reduced$SeniorCitizen <- as.factor(reduced$SeniorCitizen)
reduced$Partner <- as.factor(reduced$Partner)
reduced$Dependents <- as.factor(reduced$Dependents)
reduced$PhoneService <- as.factor(reduced$PhoneService)
reduced$MultipleLines <- as.factor(reduced$MultipleLines)
reduced$InternetService <- as.factor(reduced$InternetService)
reduced$OnlineSecurity <- as.factor(reduced$OnlineSecurity)
reduced$OnlineBackup <- as.factor(reduced$OnlineBackup)
reduced$DeviceProtection <- as.factor(reduced$DeviceProtection)
reduced$TechSupport <- as.factor(reduced$TechSupport)
reduced$StreamingMovies <- as.factor(reduced$StreamingMovies)
reduced$StreamingTV <- as.factor(reduced$StreamingTV)
reduced$Contract <- as.factor(reduced$Contract)
reduced$PaperlessBilling <- as.factor(reduced$PaperlessBilling)
reduced$Churn <- as.factor(reduced$Churn)
reduced$PaymentMethod <- as.factor(reduced$PaymentMethod)
reduced$tenure <- as.factor(reduced$tenure)
reduced$MonthlyCharges <- as.factor(reduced$MonthlyCharges)
summary(reduced)

# Rename Columns
reduced <- rename(reduced, c("tenure"="Tenure", "gender"="Gender"))

# Save Cleaned Data
clean <- reduced
#-------------------------------------------------------------------------------------------------------------------
# Univariate Statistics SECTION I
#-------------------------------------------------------------------------------------------------------------------
for (i in 1:19) {
  plot(clean[i], 
       main=names(clean)[i], 
       ylim = c(0, 6000),
       col = 2:3)
} 

#-------------------------------------------------------------------------------------------------------------------
# Bivariate Statistics SECTION J
#-------------------------------------------------------------------------------------------------------------------
counts <- table(clean$Churn, clean$Gender)
barplot(counts, main="Churn by Gender",
        xlab="Gender", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$SeniorCitizen)
barplot(counts, main="Churn by SeniorCitizen",
        xlab="SeniorCitizen", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$Partner)
barplot(counts, main="Churn by Partner",
        xlab="Partner", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$Dependents)
barplot(counts, main="Churn by Dependents",
        xlab="Dependents", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$Tenure)
barplot(counts, main="Churn by Tenure",
        xlab="Tenure", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$PhoneService)
barplot(counts, main="Churn by PhoneService",
        xlab="PhoneService", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$MultipleLines)
barplot(counts, main="Churn by MultipleLines",
        xlab="MultipleLines", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$InternetService)
barplot(counts, main="Churn by InternetService",
        xlab="InternetService", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$OnlineSecurity)
barplot(counts, main="Churn by OnlineSecurity",
        xlab="OnlineSecurity", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$OnlineBackup)
barplot(counts, main="Churn by OnlineBackup",
        xlab="OnlineBackup", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$DeviceProtection)
barplot(counts, main="Churn by DeviceProtection",
        xlab="DeviceProtection", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$TechSupport)
barplot(counts, main="Churn by TechSupport",
        xlab="TechSupport", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$StreamingTV)
barplot(counts, main="Churn by StreamingTV",
        xlab="StreamingTV", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$StreamingMovies)
barplot(counts, main="Churn by StreamingMovies",
        xlab="StreamingMovies", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$Contract)
barplot(counts, main="Churn by Contract",
        xlab="Contract", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$PaperlessBilling)
barplot(counts, main="Churn by PaperlessBilling",
        xlab="PaperlessBilling", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$PaymentMethod)
barplot(counts, main="Churn by PaymentMethod",
        xlab="PaymentMethod", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

counts <- table(clean$Churn, clean$MonthlyCharges)
barplot(counts, main="Churn by MonthlyCharges",
        xlab="MonthlyCharges", col=c("darkgreen","darkblue"),
        legend = rownames(counts), beside=TRUE)

#-------------------------------------------------------------------------------------------------------------------
# Create Training and Testing Samples
#-------------------------------------------------------------------------------------------------------------------
set.seed(420) 
ransample = sample.split(clean, SplitRatio = .75)
training_data = subset(clean, ransample == TRUE)
testing_data  = subset(clean, ransample == FALSE)
#-------------------------------------------------------------------------------------------------------------------
# Analytic Method SECTION K
#-------------------------------------------------------------------------------------------------------------------
churn_mca = MCA(training_data, quali.sup = 19)
mca_eig <- churn_mca$eig
barplot(mca_eig[, 2], 
        names.arg = 1:nrow(mca_eig), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        ylim = c(0, 25),
        col ="steelblue")
lines(x = 1:nrow(mca_eig), mca_eig[, 2], 
      type = "b", pch = 19, col = "red")
text(x = 1:nrow(mca_eig), y = mca_eig[, 2], label = sprintf("%.2f", mca_eig[,2]), pos = 3, cex = 0.8, col = "red")

plot(churn_mca, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")

res = dimdesc(churn_mca, axes=1:2, proba=0.05)
res

fviz_contrib(churn_mca,choice="var",axes=1:3)
#-------------------------------------------------------------------------------------------------------------------
# Evaluative Method SECTION K (Part 2)
#-------------------------------------------------------------------------------------------------------------------
# Create Full Model
full_model = glm(Churn~., family=binomial, data=training_data)
summary(full_model)
confint(full_model)
exp(coef(full_model))
vif(full_model)

# Create Null Model
null_model = glm(Churn~1, family=binomial, data=training_data)
summary(null_model)

# Create stepwise model using training data
stepwise_model <- stepAIC(null_model,scope =
                        list(lower=null_model,upper=full_model),direction="forward")
# Summarize Stepwise Model
summary(stepwise_model)
# ANOVA Stepwise Model
anova(stepwise_model)

# ROC Curve
prediction <- predict(stepwise_model, newdata=testing_data, type = 'response')
roc <-plot.roc(testing_data$Churn, prediction,
               identity.col = "deeppink",
               print.auc=TRUE,auc.polygon=TRUE,auc.polygon.col="goldenrod2",
               main = "Receiver Operating Characteristic (ROC) Curve for Logistic Model")

# Accuracy on Test Data
testing_data$Churn <- ifelse(testing_data$Churn == "True", 1, 0) # Convert to binary for test
str(testing_data) #Check conversion
prediction <- ifelse(prediction > 0.5,1,0)
mis <- mean(prediction != testing_data$Churn)
print(paste('Accurate Prediction Rate', 1-mis))
#-------------------------------------------------------------------------------------------------------------------
