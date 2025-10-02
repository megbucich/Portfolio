#Homework 3 

#3.1

N = 5
K = 2
data = matrix(c(1,1,2,2,2,3,3,4,4,4), nrow = N, byrow = TRUE)
clusters = kmeans(data,K)
C = clusters$centers
print(C)
#      [,1] [,2]
#1 3.500000    4
#2 1.666667    2
color = c(2,4)
plot(0,xlim = c(0, 5), ylim = c(0, 5), xlab = "X", ylab = "Y",
     main = "K-means Clustering for 5 Points and 
     2 Clusters")
axis(2, at = 0:5, tck = 1, lty = 3, col = "grey",labels = NA)
axis(1, at = 0:5, tck = 1, lty = 3, col = "grey",labels = NA)
for (i in 1:N) {
  points(data[i, 1],data[i, 2], col = color[clusters$cluster[i]], pch = 16)
  text(data[i,1], data [i,2]+.3, paste('P',i), col = color[clusters$cluster[i]])
}
for (i in 1:K) {
  cluster_color = color[i]
  points(C[i,1], C[i,2], col = cluster_color, pch = 4, cex = 2)
  text(C[i,1], C[i,2] + .3, paste('C',i), col = cluster_color, 
       font = 2, cex = 1.2)
}


dev.off()

final_tWCSS = clusters$tot.withinss
final_tWCSS
#3.166667

#-----------------------------------------------------------------
dev.off()
#3.3 
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
miami = read.csv("MiamiIntlAirport2001_2020.csv", header=TRUE)
tmin = miami[,'TMIN']
wdf2 = miami[,'WDF2']
setEPS() #Plot the data of 150 observations
postscript("fig0902a.eps",  width=5, height=5)
par(mar=c(4.5, 4.5, 2, 4.5))
K = 2 
data = cbind(tmin, wdf2)
clusters = kmeans(data, K) 
color = c(2,4)
plot(tmin[5114:5478], wdf2[5114:5478], 
     pch =16, cex = 0.5,
     xlab = 'Tmin [deg C]',
     ylab = 'Wind Direction [deg]', grid(),
     col =color[clusters$cluster[5114:5478]] )
title('K-means for 2015 Daily Miami Tmin vs WDF2 with 2 Clusters', 
      cex.main = 0.9, line = 1)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)
C = clusters$centers
for (i in 1:K) {
  points(C[i,1], C[i,2], col = color[i], pch = 18, cex = 2, font = 2)
  text(C[i,1], C[i,2] + 25, paste('C',i), col = color[i], font = 2)
}


dev.off()

#--------------------------------------------------------------------------

#3.8 SMV for 5 points
x = matrix(c(1,1,2,2,2,3,3,4,4,4), ncol = 2, byrow = TRUE)
y = c(1,1,1,2,2)
library(e1071)
data = data.frame(x,y=as.factor(y))
svm5 = svm(y ~., data=data, kernel = "linear",
           cost = 10, scale = "FALSE",
           type = "C-classification")
SV = svm5$SV
w = t(svm5$coefs) %*% SV
b = svm5$rho
DM = 2/norm(w, type = '2')

setEPS()
postscript("fig0907.eps", height = 7, width = 7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
x1 = seq(0,5,length.out = 100)
x2 = (b - w[1] * x1 ) / w[2]
m1 = (1 + b - w[1]*x1) / w[2]
m2 = (-1 + b - w[1]*x1 ) / w[2]
theta = atan(-w[1]/w[2])* 180 / pi
theta
colors = c(2,4)
dx = 1.4
dy = dx * (-w[1]/w[2])
plot(x[,1],x[,2], col = colors[as.numeric(y)],
     pch = 16, xlab = "X", ylab = "Y",
     main = "SVM Analysis for 5 Points with Support Vectors",
     xlim = c(0,5), ylim = c(0,5))
lines(x1,x2,lwd=1, col = "violet")
lines(x1,m1,lty = 2, col = 2)
lines(x1,m2,lty = 2, col = 4)
axis(2, at = -15:15, tck = 1, lty = 3, col = "grey",labels = NA)
axis(1, at = -15:15, tck = 1, lty = 3, col = "grey",labels = NA)
points(SV[,1], SV[,2], col = "black",pch = 12, cex = 1.5)
text(SV[,1], SV[,2]+ .3, col = "black", paste('SV',1:2))



#-----------------------------------------------------------------
#3.9

new = matrix(c(1.5,1,3,3), ncol = 2, byrow= TRUE)
points(new, pch = 17, cex =2)
predict(svm5,new)
for (i in 1:2){
  text(new[i,1],new[i,2]-.4, paste('Q',i), cex = 1.5)
}
dev.off()

#-----------------------------------------------------------------------
#3.13
library(randomForest)
data(iris)
train1 = iris[1:40,]
train2 = iris[51:90,]
train3 = iris[101:140,]
train = rbind(train1,train2,train3)
new1 = iris[41:50,]
new2 = iris[91:100,]
new3 = iris[141:150,]
new = rbind(new1,new2,new3)
classify = randomForest(x = train[,1:4],
                          y = train[, 5], ntree = 800)
classify
#Confusion matrix:
#             setosa versicolor virginica class.error
#setosa         40          0         0       0.000
#versicolor      0         37         3       0.075
#virginica       0          4        36       0.100
predict(classify, newdata = new[,1:4])
#41         42         43         44         45         46 
#setosa     setosa     setosa     setosa     setosa     setosa 
#47         48         49         50         91         92 
#setosa     setosa     setosa     setosa versicolor versicolor 
#93         94         95         96         97         98 
#versicolor versicolor versicolor versicolor versicolor versicolor 
#99        100        141        142        143        144 
#versicolor versicolor  virginica  virginica  virginica  virginica 
#145        146        147        148        149        150 
#virginica  virginica  virginica  virginica  virginica  virginica

#---------------------------------------------------------------------------
#3.14
train_data = sort(c(
  sample(1:50, 10, replace = FALSE),
  sample(51:100,10,replace=FALSE),
  sample(101:150,10,replace=FALSE)))
train = iris[train_data, ]
dim(train)
#[1] 30  5
remaining = iris[-train_data,]
set = remaining[remaining$Species == "setosa", ]
vers = remaining[remaining$Species == "versicolor", ]
virg = remaining[remaining$Species == "virginica", ]
new1 = set[sample(1:nrow(set),4),]
new2 = vers[sample(1:nrow(vers),4),]
new3 = virg[sample(1:nrow(virg),4),]
new = rbind(new1,new2,new3)
classify = randomForest(x = train[,1:4],
                        y = train[, 5], ntree = 800)
classify
predict(classify, newdata = new[,1:4])

#----------------------------------------------------------------------------
#3.19
require(neuralnet)
data(iris)
train1 = iris[1:40,]
train2 = iris[51:90,]
train3 = iris[101:140,]
train = rbind(train1,train2,train3)
new1 = iris[41:50,]
new2 = iris[91:100,]
new3 = iris[141:150,]
new = rbind(new1,new2,new3)
set.seed(123)
nn = neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data = train, hidden = 5, act.fct = "logistic", linear.output = FALSE)
SL = new$Sepal.Length
SW = new$Sepal.Width
PL = new$Petal.Length
PW = new$Petal.Width
new_data = data.frame(SL,SW,PL,PW)
predictions = neuralnet::compute(nn,new_data)
predictions$net.result
#not sure what the results mean
#-----------------------------------------------------------------
#3.20
require(neuralnet)
train_data = sort(c(
  sample(1:50, 10, replace = FALSE),
  sample(51:100,10,replace=FALSE),
  sample(101:150,10,replace=FALSE)))
train = iris[train_data, ]
dim(train)
#[1] 30  5
remaining = iris[-train_data,]
set = remaining[remaining$Species == "setosa", ]
vers = remaining[remaining$Species == "versicolor", ]
virg = remaining[remaining$Species == "virginica", ]
new1 = set[sample(1:nrow(set),4),]
new2 = vers[sample(1:nrow(vers),4),]
new3 = virg[sample(1:nrow(virg),4),]
new = rbind(new1,new2,new3)
set.seed(123)
nn = neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data = train, hidden = 5, act.fct = "logistic", linear.output = FALSE)
SL = new$Sepal.Length
SW = new$Sepal.Width
PL = new$Petal.Length
PW = new$Petal.Width
new_data = data.frame(SL,SW,PL,PW)
predictions = neuralnet::compute(nn,new_data)
predictions$net.result
#---------------------------------------------------------------
#4.11 Linear Multiple regression
#A) compute 12th order polynomail using annual (1880-2018) 
#B) plot the data and fitted polynomial on the same figure
#C) produce a scattor plot of the residuals of the fitting against time in a separate plot
getwd()
annual = read.table('NOAAGlobalTempAnn2019.txt',header=FALSE)
dim(annual)
# 139x6 
x = annual[,1]
y = annual[,2]

polynomial = matrix(0,ncol = 12, nrow = length(x))
for (i in 1:12){
  polynomial[,i] = x^i
}
data = data.frame(polynomial,y)
reg = lm(y ~.,data = data)
plot(x,y,xlab = "Year",ylab = "Temperature Anomly",
     main = "Global Annual Temperature Anomalies with 12th
     Order Polynomial Fit", col = "blue")
lines(x,predict(reg), col = "red")
legend("topleft", legend = c("Data", "Regression"),
       col = c("blue", "red"), pch = c(16, NA), lwd = c(NA, 3), bty = 'n')
dev.off()
plot(x,reg$residuals, xlab = "Year",ylab = "Residuals",
     main = "Residuals of Global Annual Temperature Anomalies
     from 12th Order Polynomial Fit")
dev.off()
#--------------------------------------------------------------------------------------------
#4.13 
#A) use monthly temperature from 1880-2018 to 3rd order orthogonal polynomial
monthly = read.table("NOAAGlobalTmonthly.txt",header=FALSE)
year = monthly[,1]
month = monthly[,2]
temperature = monthly[,3]
time = (year - min(year))*12 + month
#convert the time into days to make plotting easier
x1 = time
x2 = x1^2
x3 = x1^3
data = data.frame(x1,x2,x3, temperature)
reg = lm(temperature ~ x1 + x2 + x3, data=data)
#B) plot the data and fitted polynomial on the same figure
par(mar=c(5,4.5,2.5,.7))
plot(time,temperature,xlab="Time (months) ",ylab = "Temperature Anomaly",
     main = "Global Monthly Temperature Anomalies", col = "blue",xaxt = "n")
year_lab =seq(min(year),max(year),by = 8)
x_ticks = (year_lab - min(year))* 12 + 1
axis(1,at = x_ticks,labels = year_lab)
lines(time, predict(reg), col = "red",lwd = 3)
legend("topleft", legend = c("Data", "Regression"),
       col = c("blue", "red"), pch = c(16,NA),lwd=c(NA,3),bty = 'n')
dev.off()
#C) produce a scattor plot of the residuals of the fitting against time in a separate plot

par(mar = c(4.5, 4.5, 2.5, 0.7))
plot(time,reg$residuals,xlab="Time",ylab = "Residuals",
     main = "Residuals of Global Monthly Temperature Anomalies from 3rd Order Orthogonal Polynomial",
     xaxt = 'n')
year_lab = seq(min(year),max(year),by = 8)
x_ticks = (year_lab - min(year))* 12 + 1
axis(1,at = x_ticks,labels = year_lab)

