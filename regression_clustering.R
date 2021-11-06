#install.packages("ggplot2")
#install.packages('corrplot')
#install.packages("olsrr")
#install.packages('tsoutliers')
#install.packages("NbClust")
install.packages("factoextra")
data <- read.csv("Docs/Новая папка/dataKidneyReg.csv", header = TRUE, sep = ';')
str(data)#dataset`s characteristics
summary(data) 
library(ggplot2)
sum(data,na.rm=TRUE) # check na

#analysis of variables
plot(data$Diabetes, data$Glucose, main = 'Diabetes VS Glucose', col = "red", lwd = 2)
plot(data$Hemoglobin, data$Glucose,  main = 'Hemoglobin VS Glucose')
hist(data$Diabetes, main = 'Diabetes', col = 'blue')
boxplot(data$Glucose[data$Age], xlab = 'Age', medcol = 'red')
boxplot(data$Glucose[data$Sugar], xlab = 'Sugar',  main = 'Glucose of individuals', medcol = 'red')
boxplot(data$Glucose[data$Blood_Pressure], xlab = 'Blood_Pressure',   medcol = 'red')
boxplot(data$Glucose[data$Diabetes], xlab = 'Diabetes',  main = 'Glucose of individuals', medcol = 'red')
plot(data$Sugar, data$Glucose,  main = 'Sugar VS Glucose', col = "blue")
library(corrplot)
# correaltion analysis
corrplot(cor(data[,]), 'number', tl.cex=0.8)
cor(data[,])
cor_matrix <- cor(data)  # correlation matrix 
cor_matrix_rm <- cor_matrix# to modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm
data_new <- data[ , !apply(cor_matrix_rm,    # To remove variables with correlation higher than 0.8
                           2,
                           function(x) any(x > 0.8))]
#characteristic of new data set
head(data_new) 
cor(data) 
str(data_new)
#first linear regression model 
linregmodel = lm(Glucose~Anemia + Pedal_Edema + Diabetes +
                  Red_BloodCount+ White_BloodCount+Cell_Volume + 
                   Potassium + Sodium + Bacteria+Red_Blood + Sugar + Gravity + Blood_Pressure + Age, data = data_new)

# summary of first linear regression model
summary(linregmodel)
# second linear regression model without non-significant variables
linregmodel2 = lm(Glucose~Pedal_Edema + Diabetes + Potassium  + Red_Blood + Sugar + Gravity, data = data_new)
summary(linregmodel2)
# checking properties for linear regression
library('olsrr')

ols_plot_resid_qq(linregmodel2) #checking that error terms are normally distributed
mean(residuals(linregmodel2))#mean of residuals
ols_test_normality(linregmodel2) #normality tests
library('tsoutliers')
JarqueBera.test(residuals(linregmodel2))  #normality tests
ols_test_correlation(linregmodel2) #Correlation between observed residuals and expected residuals under normality.
ols_plot_resid_fit(linregmodel2) #residuals vs fitted values plot
library(car)
library(lmtest)

#Breusch-Pagan Test
bptest(linregmodel2)
#Durbin-Watson test
durbinWatsonTest(linregmodel2)
resid = residuals(linregmodel2)
#cor(data_new, resid)

#clustering


data <- read.csv("Docs/Новая папка/dataShopping.csv", header = TRUE, sep = ';')
str(data) #structure of dataset
summary(data) 

sum(data,na.rm=TRUE) #check na
# analysis of variables

plot(data$ProductRelated_Duration,main = 'Product Related Duration', col = "red", lwd = 2)
hist(data$NewVisitor,main = 'New Visitor', col = "blue", lwd = 2)
hist(data$SpecialDay,main = 'Special Day', col = "gray", lwd = 2)
hist(data$BounceRates,main = 'Bounce Rates', col = "red", lwd = 2)
hist(data$Informational_Duration,main = 'Informational Duration ', col = "black", lwd = 2)
hist(data$Administrative_Duration,main = 'Administrative Duration ', col = "pink", lwd = 2)
corrplot(cor(data[,]), 'number', tl.cex=0.8)
#percentage of revenue == 1
nrow(data[data$Revenue == 1, ])/nrow(data)
#percentage of new visitor == 1
nrow(data[data$NewVisitor == 1, ])/nrow(data)
#percentage of special day == 0
nrow(data[data$SpecialDay == 0, ])/nrow(data)
# function to normalize data using min max method
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#new normalized variables
data$Administrative_Duration_norm<-normalize(data$Administrative_Duration)
data$Informational_Duration_norm<-normalize(data$Informational_Duration)
data$ProductRelated_Duration_norm<-normalize(data$ProductRelated_Duration)
data$BounceRates_norm<-normalize(data$BounceRates)
data$SpecialDay_norm<-normalize(data$SpecialDay)
data$NewVisitor_norm<-normalize(data$NewVisitor)
data$PageValues_norm<-normalize(data$PageValues)
data$Revenue_norm<-normalize(data$Revenue)

#selecting only six variables

Data_removed <- data[,(9:14),drop=FALSE]
str(Data_removed) #structure of new data set
summary(Data_removed)
library(factoextra)
#kmeans algorithm 
#kmeansm1 <- kmeans(Data_removed, centers = 5, nstart = 20)

#plot(Data_removed, col = kmeansm1$cluster, pch = 16)


library(purrr)
# function ro calculate TWSS
tot_within_ss = map_dbl(1:7, function(k){
  model = kmeans(Data_removed, centers = k)
  model$tot.withinss
}  )
# Elbow plot
plot(1:7,tot_within_ss, type = 'o', col = 'blue', xlab = 'number of clusters', ylab = 'Total WSS', main = 'Elbow plot')

library(NbClust)
library(cluster) 
library(factoextra)
#trying out different indexes for cluster selection
SilClust = NbClust(Data_removed, distance = 'euclidean', 
                   min.nc = 2, max.nc = 8, 
                   method = 'kmeans', index="silhouette")

GapClust = NbClust(Data_removed, distance = 'euclidean', 
                   min.nc = 2, max.nc = 8, 
                   method = 'kmeans', index="gap")


HCClust = NbClust(Data_removed, distance = 'euclidean', 
                   min.nc = 2, max.nc = 8, 
                   method = 'kmeans', index="ch")

#plotting
par(mfrow=c(1,3))
plot(2:8, SilClust$All.index, type = 'o', xlab='Number of clusters', 
     ylab='Silhouette Values')
plot(2:8, GapClust$All.index, type = 'o', xlab='Number of clusters', 
     ylab='Gap values')
plot(2:8, HCClust$All.index, type = 'o', xlab='Number of clusters', 
     ylab='Calinski Harabasz Index')


#running final model

kmeansmodel = kmeans(Data_removed, centers = 6, nstart = 25)
kmeansmodel$centers #Display cluster centers
table(kmeansmodel$cluster)#a way to assess number of elements in clusters

