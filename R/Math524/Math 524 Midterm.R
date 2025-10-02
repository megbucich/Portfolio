#1 
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
data = read.csv("NOAAGlobalT.csv", header = TRUE, sep = ",")
dim_data = dim(data)
# 2592x1648
#gives data from Jan 1880 to Jan 2017, 137 years
#to find year 1991, we can do 1991-1880 = 111
#then 111x12 = 1332
#so 1336 is 1991 jan, we want june which is 1341

#san francisco has lat/lon = 37.5, 122.5 
#chicago has lat/lon = 42.5,87.5
#london has lat/lon = 52.5, 2.5
#toyko has lat/lon = 37.5,137.5
SF = data[1825,seq(1341,1412, by = 12)]
Chicago = data[1890, seq(1341,1412, by = 12)]
London = data[2017, seq(1341,1412, by = 12)]
Toyko = data[1828, seq(1341,1412, by = 12)]
#create matrix by using rbind and name the rows and columns acccordingly
matrix = rbind(SF, Chicago, London, Toyko)
colnames(matrix) = c("1991", "1992", "1993", "1994", "1995", "1996")
rownames(matrix) = c("San Francisco","Chicago", "London", "Toyko")
print(matrix)
#                 1991    1992    1993    1994    1995    1996
#San Francisco  0.3252 -0.4216 -1.4006  1.2336  0.3994  0.9921
#Chicago        0.3062 -0.3859 -0.9173  0.4357  0.4861  0.1971
#London        -1.0923  2.1091  0.5678 -0.1498  0.0424 -0.5692
#Toyko          1.0840 -0.1687 -0.4509  0.2126 -0.4990 -0.1933


#2A Create a visual of your matrix
Matrix = as.matrix(matrix)
image(t(Matrix), col = topo.colors(100), 
      main = "Visualization of June Temperature Data Matrix by Meg",
      xlab = "Year", ylab = "Location", axes = FALSE,)
axis(1, at = seq(0, 1, length.out = ncol(matrix)),
     labels = colnames(matrix), cex.axis = 1.2)
axis(2, at = seq(0, 1, length.out = nrow(matrix)),
     labels = rownames(matrix), cex.axis = .8)
box()
grid(nx = 6, ny = 4, col = "black",lty = 1)
dev.off()

#2B Create SVD of matrix
svd(Matrix)
#$d
#[1] 3.1730495 1.7536347 1.0735298 0.4629459

#$u
#           [,1]       [,2]         [,3]        [,4]
#[1,]  0.5813071  0.6911406  0.002966861  0.42941585
#[2,]  0.3365920  0.2762298 -0.009535021 -0.90017331
#[3,] -0.7020823  0.6323260 -0.320948949 -0.06508494
#[4,]  0.2363648 -0.2149084 -0.947043850  0.03246537

#$v
#[,1]       [,2]       [,3]        [,4]
#[1,]  0.41449385 -0.3503067 -0.6315407 -0.06415932
#[2,] -0.59740842  0.5542271 -0.4794635  0.05095112
#[3,] -0.51311957 -0.4364992  0.2322976  0.37303553
#[4,]  0.32119785  0.4747467 -0.1432264  0.33302755
#[5,]  0.07818255  0.3104220  0.4243167 -0.61567749
#[6,]  0.31420657  0.2404989  0.3416876  0.60346163

#2C plot EOF1, the first column of the U matrix
#use red dots for positive
#use blue dots for negative
S_dot = svd(Matrix)$u[1,1]
#[1] 0.5813071, red
C_dot = svd(Matrix)$u[2,1]
#[1] 0.336592, red
L_dot = svd(Matrix)$u[3,1]
#[1] -0.7020823, blue
T_dot = svd(Matrix)$u[4,1]
#[1] 0.2363648, red

library(maps)
install.packages("mapdata")
library(mapdata)
par(mar = c(0,0,0,0))
map(database = "world", ylim = c(-90,90),
    xlim = c(-180,180))
grid(nx = 12, ny = 6, col = "black", lty = 1)
points(-122.5,37.5, col = "red", pch = 16, cex = 2)
text(-122.5, 27.5, "San Francisco: 0.58", col ="red")
points(-87.5, 42.5, col = "red", pch = 16, cex = 1.8)
text(-87.5, 52.5, "Chicago: .34", col ="red")
points(2.5, 52.5, col = "blue", pch = 16, cex = 2.5)
text(2.5, 42.5, "London: -.70", col = "blue")
points(137.5, 37.5, col = "red", pch = 16, cex = 1.5)
text(137.5, 27.5,"Toyko: .24", col = "red" )
title(main = "EOF1 from SVD")
dev.off()

#3A Extract the data and convert it into a vector
#data is from 1850 to 2015
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
data = read.csv("EarthTemperatureData.csv", header = TRUE, sep = ",")
monthly = data[,-1]
monthly_v = c(t(monthly))
print(monthly_v)

#3B plot the sequence from Jan 1901 to Dec 2000
time = seq(1901, 2000, len = length(monthly_v))
plot(time, monthly_v, xlab = "Time", ylab = "Temperature Anomalies in Celcius",
     main = "Monthly Global Temperature Anomaly Data", type = 'l', col = "purple")

#5A
A = matrix(c(1,2,3,4,
             2,4,6,0,
             0,0,0,1), nrow = 3, byrow = TRUE)
qr(A)$rank
#the rank(A) is 2
#this means A has 2 linearly independent rows and the other is a linear combination.
#lets find the linear combination by hand: 

#(insert handwritten work here)

#5B
A = matrix(c(2,3,4,
             4,6,0,
             0,0,1), nrow = 3, byrow = TRUE)
det(A)
#[1] 0
#Because the det(A) = 0, this matrix does NOT have an inverse

#5C
B = matrix(c(1,2,3,4,
             4,5,6,0,
             7,1,9,0,
             0,1,0,8), nrow = 4, byrow = TRUE)
solve(B)
#            [,1]        [,2]        [,3]        [,4]
#[1,] -0.86666667  0.23333333  0.13333333  0.43333333
#[2,] -0.13333333  0.26666667 -0.13333333  0.06666667
#[3,]  0.68888889 -0.21111111  0.02222222 -0.34444444
#[4,]  0.01666667 -0.03333333  0.01666667  0.11666667

#5D
det(B)
#[1] -360

#5E
eigen(B)
values = eigen(B)$values
#[1] 13.200401  7.269414  3.578543 -1.048358
vectors = eigen(B)$vectors
#          [,1]       [,2]        [,3]         [,4]
#[1,] -0.3147967  0.1563868 -0.07853434  0.818962684
#[2,] -0.6507493 -0.5525792 -0.93494531  0.027010169
#[3,] -0.6795365 -0.3132630  0.27385363 -0.573202985
#[4,] -0.1251344  0.7563503  0.21145639 -0.002985091

#5F
v1 = vectors[,1]
v2 = vectors[,2]
dot((v1),(v2))
#[1] 0.4285886
#v1 and v2 are NOT orthogonal because their dot product is not zero
#this means the angle between v1 and v2 is not 90