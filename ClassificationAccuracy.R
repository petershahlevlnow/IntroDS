# ClassificationAccuracy.R
# Copyright 2016 by Ernst Henle

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# A Classification is tested on 1000 cases.  In the 
# approximate middle of its ROC chart there is a point
# where the false positive rate is 0.4, the true
# positive rate is 0.8, and the accuracy is  0.7. 
# What does the confusion matrix look like? 

# Problem statement
# I   A Classification is tested on 1000 cases.
# II  The false positive rate is 0.4
# III The true positive rate is 0.8.
# IV  The accuracy is 0.7.

# Problem statement expressed using TP, FP, FN, TN
# I   N = TP + FP + FN + TN   = 1000
# II  FPR = FP/(FP + TN) = 0.4
# III TPR = TP/(TP + FN) = 0.8
# IV  (TP + TN)/(TP + FP + FN + TN) = 0.7

# Problem statement expressed as linear equations
# I    1*TP + 1*FP + 1*FN + 1*TN  = 1000
# II   0*TP + 3*FP + 0*FN - 2*TN  = 0
# III  1*TP + 0*Fp - 4*FN + 0*TN  = 0
# IV  -3*TP + 7*FP + 7*FN - 3*TN  = 0

# Problem statement expressed in terms of linear algebra:
# We want to solve the linear equation:  Ax = b
# Where:
#     A is the matrix
#     x is a vector of TP, FP, FN, TN
#     b is the right-hand side of the linear equation
# --------------    --------
#   matrix A        vector b
# --------------    --------
# TP  FP  FN  TN  | b
# --------------    --------
#  1   1   1   1  | 1000
#  0   3   0  -2  | 0
#  1   0  -4   0  | 0
# -3   7   7  -3  | 0
# --------------    --------

# Some matrix algebra in R:
?solve
# x <- solve(A, b)
# x <- solve(A) %*% b # solve(A) is used for inverse(A)
# b <- A %*% x; 

# Construct the matrix
#       TP  FP  FN  TN
r1 <- c( 1,  1,  1,  1) # I
r2 <- c( 0,  3,  0, -2) # II
r3 <- c( 1,  0, -4,  0) # III
r4 <- c(-3,  7,  7, -3) # IV
A = matrix(c(r1,r2,r3,r4), ncol=4, byrow=TRUE)

# The right-hand side of the linear system:
b <- c(1000, 0, 0, 0)

# Solve the equation:
x <- solve(A, b)
names(x) <- c('TP', 'FP', 'FN', 'TN')
print(x)
