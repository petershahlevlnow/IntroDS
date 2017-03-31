#Assignment 1

# 3
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv" 
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)

# 4
# 1. Age	Age of the patient 
# 2. Gender	Gender of the patient 
# 3. TB	Total Bilirubin 
# 4. DB	Direct Bilirubin 
# 5. Alkphos Alkaline Phosphotase 
# 6. Sgpt Alamine Aminotransferase 
# 7. Sgot Aspartate Aminotransferase 
# 8. TP	Total Protiens 
# 9. ALB	Albumin 
# 10. A/G Ratio	Albumin and Globulin Ratio 
# 11. Selector field used to split the data into two sets (labeled by the experts) 
a.headers <- c("Age", "Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "ALB", "AG", "Sel")
names(ILPD) <- a.headers

# 5
head(ILPD)

# 6
sapply(ILPD[, -2], mean, na.rm = T)
sapply(ILPD[, -2], median, na.rm = T)
sapply(ILPD[, -2], sd, na.rm = T)

#7
sapply(names(ILPD), function(s){
                        if (is.numeric(ILPD[,s])){ 
                          hist(ILPD[, s], main=paste("Histogram of", s), xlab = s, col = rgb(0,1,0,0.5))
                        }})
                                   
#8
ILPD$Gender[ILPD$Gender == "Female"] <- "1"
ILPD$Gender[ILPD$Gender == "Male"] <- "0"
ILPD$Gender <- as.numeric(ILPD$Gender)

plot(ILPD)

# 9 
# You can tell a vector is binary data by looking at the plot - all datapoints will lie at 1 or 0 on the axis
# TB and DB are the most strongly correlated
# Age and TP have little correlation. Sgot and TP also have little correlation.

# 10
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
goodFlag <- (x < highLimit) & (x > lowLimit)
x <- x[goodFlag]
x


# 11 

x <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'MS', 'BS', 'MS')
x[x == 'Bachelors'] <- "BS"
x[x == 'Masters'] <- "MS"
x[x == 'High School'] <- "HS"
x

# 12
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- min(x)
b <- max(x) - min(x)
x_norm <- (x - a) / b
x_norm

# 13
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- mean(x)
b <- sd(x)
x_norm <- (x - a) / b
x_norm

#14
colors <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Red', 'Blue', 'Green', 'Blue')
colors.binary <- model.matrix(~colors - 1)
colors.binary

# 15
x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized[bin1Min < x & x <= bin1Max] <- "Low"
xDiscretized[bin1Max < x & x <= bin2Max] <- "Medium"
xDiscretized[bin2Max < x & x <= bin3Max] <- "High"
xDiscretized

# 16

x <- sort(x)
x
# x = [3  3  4  4  5  5  5  5  5|  5  5  6  6  6  6  6  7  7 | 7  7  8  8  9 12 24 24 25 81]
a <- floor(length(x)/3)
x1 <- x[1:a]
x1
x2 <- x[(a+1):(2*a)]
x2
x3 <- x[(2*a+1):length(x)]
x3
