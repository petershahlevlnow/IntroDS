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
