# ClassifyStudents.R
# Copyright ? 2017 by Ernst Henle

rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

source("CollegeStudentsDataset_template.R")

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
StudentsSplit <- PartitionExact(Students, fractionOfTest=0.4) # ********** Change here
TestStudents <- StudentsSplit$testingData
TrainStudents <-StudentsSplit$trainingData

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
glmModel <- glm(formula = formula, data = TrainStudents, family = "binomial")
# Predict the outcomes for the test data. (predict type="response")
predictedProbabilities.GLM <- predict(glmModel, newdata = TestStudents, type = "response")
###################################################

# Naive Bayes
# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Get the algorithm
reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

# Create Naive Bayes model
nbModel <- naiveBayes(formula = formula, data = TrainStudents)
# Predict the outcomes for the test data. (predict type="raw")
predictedOutcomes.NB <- predict(nbModel, newdata = TestStudents, type = "raw")
###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.7

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
predictedOutcomes <- ifelse(predictedProbabilities.GLM > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
print(table(predictedOutcomes, actual, dnn = c("Predicted", "Outcome")))

#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
NBpredictedOutcomes <- ifelse(predictedOutcomes.NB[,2] > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
print(table(NBpredictedOutcomes, actual, dnn = c("PredictedNB", "Outcome")))
###################################################

# Bad Partition; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       325        84
# NotAttend   1368      1103
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (325 + 1103)/(325 + 1368 + 84 + 1103) = 50%

# Fill in the rest:

# Exact Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      682    212   add results here
# NotAttend   280    1706   add results here 
# Accuracy defined as fraction of predictions that are correct
# Accuracy:    (682+1706)/(682+212+280+1706) = 0.8292  add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      543    206   add results here
# NotAttend   419    1712   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (543+1712)/(543+206+419+1712) = 0.7830   add results here

# Fast Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      657    243   add results here
# NotAttend   258    1697   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (657 + 1697)/(657 + 243 + 258 + 1697) = 0.8245  add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      550    221   add results here
# NotAttend   365    1719   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (550+1719)/(550+221+365+1719) = 0.7948   add results here

# Exact Partition;  threshold = 0.7
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      464    75   add results here
# NotAttend   514    1827   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (464+1827)/(464+75+514+1827) = 0.7955   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      401    88   add results here
# NotAttend   577    1814   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (401+1814)/(401+88+577+1814) = 0.7691   add results here

###################################################
