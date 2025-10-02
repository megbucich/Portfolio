
#1.1 
A1 = matrix(c(1,1,1,-1), nrow = 2, byrow = TRUE)
svdA = svd(A1)
U = svd(A1)$u
D = svd(A1)$d
V = svd(A1)$v
D%*%t(V) 
#[,1]      [,2]
#[1,] -1.414214 -1.414214
U%*%t(D%*%t(V))
#    [,1]
#[1,]    2
#[2,]    0
#therefore the first column of A is 
#     [,1]
#[1,]    2
#[2,]    0


#1.10 Balance Chemical Equation
w = 1
z = 3*w
y = 4*w
x = (2*z + y)/2
solution = c(w,x,y,z)
print(solution)
#[1] 1 5 4 3

#2.2 write a code to find the inverse of A

#start by defining matrix A and use det(A) function to make sure the inverse exists first
A = matrix(c(1.7, -0.7, 1.3,-1.6, -1.4, 0.4,-1.5, -0.3, 0.6), nrow = 3, byrow = TRUE)
#     [,1] [,2] [,3]
#[1,]  1.7 -0.7  1.3
#[2,] -1.6 -1.4  0.4
#[3,] -1.5 -0.3  0.6
det_A = det(A)
det_A # = -3.582, means A inverse
inverse_A = solve(A)
print(inverse_A)
#           [,1]         [,2]       [,3]
#[1,]  0.2010050 -0.008375209 -0.4299274
#[2,] -0.1005025 -0.829145729  0.7705193
#[3,]  0.4522613 -0.435510888  0.9771078


#2.3 write a code to solve Ax = b
A = matrix(c(1,2,3,4,5,6,7,8,0), nrow = 3, byrow = TRUE)
b = matrix(c(1,-1,0), nrow = 3, byrow = TRUE)
x = solve(A,b)
print(x)
x =   #     [,1]
#[1,] -2.6666667
#[2,]  2.3333333
#[3,] -0.3333333
  
  
#2.4
#A
A = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = TRUE)
b = matrix(0, nrow = 3, ncol =1)
det(A) # = ~0 (6.661338e-16)
qr(A)$rank # = 2
#rows are linearly dependent in a square matrix when the det = 0
#the rank of A is = 2, which means A has two linearly independent rows
# since the rank is less than the dimension of A, the rows of A are linearly dependent 

#2.5 Balance chemical equation
#my way
w = 2
y = 2*w
z = 3*w
x = (2*y + z)/2
coef = c(w,x,y,z)
print(coef)

#[1] 2 7 4 6
#Sam's way 
A = matrix(c(0,1,0,0,0,1,2,-2,-1), nrow = 3, byrow = TRUE)
B = c(4,6,0)
solve(A,B)
# [1] 7 4 6 (same answer)

#2.7 
library(geometry)
#show that v is not an eigen vector of A
A = matrix(c(0,4,-2,-7), nrow = 2, byrow = TRUE)
v = matrix(c(1,1))
#A%*%v =
#     [,1]
#[1,]    4
#[2,]   -9
# V is not an eigen vector of A because its not a scalar of A. Eigen vectors of A are scalars of A.
#A changed its direction and scaled it, therefor it is not its eigen vector

#to find the eigen vectors of A: 
eigen(A)
eigenV1 = eigen(A)$vectors[,1]
v1 = matrix(eigenV1, nrow =2)
#           [,1]
#[1,] -0.5838895
#[2,]  0.8118331
eigenV2 = eigen(A)$vectors[,2]
v2 = matrix(eigenV2, nrow =2)
#          [,1]
#[1,]  0.9410038
#[2,] -0.3383961
#show that Av = ev
ev1 = eigen(A)$values[1]
ev2 = eigen(A)$values[2]
A%*%v1 
#         [,1]
#[1,]  3.247332
#[2,] -4.515053
ev1*v1
#          [,1]
#[1,]  3.247332
#[2,] -4.515053

#proves that v1 is an eigen vector of A
A%*%v2
#          [,1]
#[1,] -1.3535842
#[2,]  0.4867649
ev2*v2
#           [,1]
#[1,] -1.3535842
#[2,]  0.4867649

#proves that v2 is an eigen vector of A

#unit vectors have magnitude 1, so divide v1 and v2 by its length
mag_v1 = sqrt(sum(v1^2)) # = 1
mag_v2 = sqrt(sum(v2^2)) # = 1
#v1 and v2 are already unit vectors because 
#they have magnitude 1



#1.2
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
pta = read.table("PSTANDtahiti.txt", header = F)
pda = read.table("PSTANDdarwin.txt", header = F)
pdaDec = pda[,13]
ptaDec = pta[,13]
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix
ptada1
ptada = t(ptada1[1:65,]) #2009-2015 data
ptada
colnames(ptada) = 1951:2015
rownames(ptada) = c("Darwin", "Tahiti")
ptada
svdptd = svd(ptada)
U = round(svdptd$u, digits = 2)
D = round(diag(svdptd$d), digits = 2)
V = round((svdptd$v), digits = 2)
EOF = U #empirical orthogonal functions, space
print(EOF)
#     [,1] [,2]
#[1,] -0.75 0.66
#[2,]  0.66 0.75
PC = V #principal components, time
print(PC)
#       [,1]  [,2]
#[1,] -0.10  0.14
#[2,] -0.14 -0.03
#[3,] -0.05  0.05
#[4,]  0.16  0.08
#[5,]  0.10  0.21
#[6,]  0.13 -0.14
#[7,] -0.04  0.01
#[8,] -0.07  0.05
#[9,]  0.11 -0.08
#[10,]  0.09  0.10
#[11,]  0.16  0.13
#[12,]  0.02  0.01
#[13,] -0.15  0.08 
#[14,] -0.03 -0.01
#[15,]  0.03 -0.11
#[16,] -0.04 -0.02
#[17,] -0.07 -0.03
#[18,]  0.02  0.01
#[19,]  0.04 -0.02
#[20,]  0.23 -0.06
#[22,] -0.16  0.18
#[23,]  0.20  0.18
#[24,]  0.04 -0.20
#[25,]  0.24  0.02
#[26,] -0.03 -0.22
#[28,] -0.01 -0.01
#[29,] -0.09  0.07
#[30,] -0.01 -0.03
#[31,]  0.05  0.10
#[32,] -0.26 -0.05
#[33,] -0.01  0.20
#[34,]  0.01 -0.43
#[35,]  0.02  0.08
#[36,] -0.18  0.17
#[37,] -0.06  0.00
#[38,]  0.14  0.05
#[39,] -0.07  0.18
#[40,] -0.03  0.02
#[41,] -0.20 -0.11
#[42,] -0.06 -0.11
#[43,]  0.03 -0.04
#[44,] -0.15  0.13
#[45,] -0.05 -0.06
#[46,]  0.11 -0.16
#[47,] -0.12  0.05
#[48,]  0.16 -0.04
#[49,]  0.16  0.10
#[50,]  0.11 -0.31
#[51,] -0.10 -0.09
#[52,] -0.13  0.11
#[53,]  0.14 -0.02
#[54,] -0.09 -0.16
#[55,] -0.01  0.05
#[56,] -0.04  0.19
#[57,]  0.20  0.04
#[58,]  0.17  0.09
#[59,] -0.08 -0.03
#[60,]  0.34  0.05
#[61,]  0.29 -0.01
#[62,] -0.07 -0.05
#[63,]  0.00  0.07
#[64,] -0.06 -0.08
#[65,] -0.06 -0.17

#PC time series plot
plot(1951:2015, -V[,1], type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'blue', ylim = c(-0.75, 0.45))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'red')
legend(1951, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'red'),
       lty = c(1,2), bty="n")
dev.off()


#EOF plots
EOF_Darwin = matrix(EOF[, 1], nrow = 1)
EOF_Tahiti = matrix(EOF[, 2], nrow = 1)
plot(1:2, EOF[1, ], type = 'o', 
     ylab = "EOF", xlab = 'Year',
     main = "EOFs for Darwin and Tahiti",
     col = 'blue', ylim = range(EOF))
lines(1:2, EOF[2,], type = 'o', 
      col = 'purple', lty = 2)
legend("bottomright", legend = c("Darwin EOF", "Tahiti EOF"), 
       col = c("blue", "purple"), lty = c(1, 2), bty = "n")
dev.off()



#1.8
#A
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
data = read.csv("CAprcpV2.csv")
data
sf = data[11:15, 4]
sf
msf = mean(sf)
sdsf = sd(sf)
anomly_sf = (sf - msf)/sdsf
sb = data[6:10, 4]
msb = mean(sb)
sdsb = sd(sb)
anomly_sb = (sb - msf)/sdsb
sd = data[1:5, 4]
msd = mean(sd)
sdsd = sd(sd)
anomly_sd = (sd - msd)/(sdsd)
years = c("2001", "2002", "2003", "2004", "2005")
stations = c("San Francisco", "Santa Barbara", "San Diego")
A = rbind(anomly_sf, anomly_sb, anomly_sd)
rownames(A)= stations
colnames(A) = years
A
#                    2001       2002       2003       2004     2005
#San Francisco  0.9853590 -0.6404543 -0.8446488 -0.6936804 1.193424
#Santa Barbara  0.7411194 -0.9584819 -0.9501560 -0.6415412 1.142985
#San Diego     -0.3504873 -1.4052947 -0.1684543  0.8558503 1.068386

Y = 5
C = (A%*%t(A))/Y
print(C)
#              San Diego   Santa Barbara   San Francisco
#San Diego     0.8000000     0.7911542     0.2756615
#Santa Barbara 0.7911542     0.8177462     0.3838680
#San Francisco 0.2756615     0.3838680     0.8000000

#B
invC = solve(C)
print(invC)
#               San Diego   Santa Barbara   San Francisco
#San Diego      50.532324    -52.552593      7.804306
#Santa Barbara -52.552593     56.232032     -8.873692
#San Francisco   7.804306     -8.873692      2.818725

#C
eigen(C)
#eigen decomposition
#$values
#[1] 1.815317196 0.593113386 0.009315654

#$vectors
#[,1]       [,2]       [,3]
#[1,] -0.6261837 -0.3760122  0.6830145
#[2,] -0.6577237 -0.2156663 -0.7217255
#[3,] -0.4186808  0.9011675  0.1122654

#D
svd(A)
#$d
#[1] 3.012737 1.722082 0.215820
#
#$u
#           [,1]       [,2]       [,3]
#[1,] -0.6261837  0.3760122  0.6830145
#[2,] -0.6577237  0.2156663 -0.7217255
#[3,] -0.4186808 -0.9011675  0.1122654

#$v
#           [,1]       [,2]      [,3]
#[1,] -0.3178920  0.4913757 0.4577061
#[2,]  0.5376600  0.4755145 0.4473875
#[3,]  0.4063992 -0.2152681 0.4167029
#[4,]  0.1652982 -0.6796746 0.3952613
#[5,] -0.6460516 -0.1553639 0.5103693


#E
EV = (eigen(C)$values)
MD = (svd(A)$d)
EV
MD
#EV = 1.815317196 0.593113386 0.009315654
#MD = 3.012737 1.722082 0.215820
#how do these relate?
plot(EV, type = 'o', main = "EV vs MD",
     col = 'black', ylim = range(c(EV,MD)))
lines(MD, type = 'o', 
      col = 'purple', lty = 2)
legend("topright", legend = c("EV", "MD"), 
       col = c("black", "purple"), lty = c(1, 2), bty = "n")
#F
EV = (eigen(C)$vectors)
MU = (svd(A)$u)
#EV
#          [,1]       [,2]       [,3]
#[1,] -0.6261837 -0.3760122  0.6830145
#[2,] -0.6577237 -0.2156663 -0.7217255
#[3,] -0.4186808  0.9011675  0.1122654
#MU
#          [,1]       [,2]       [,3]
#[1,] -0.6261837 -0.3760122  0.6830145
#[2,] -0.6577237 -0.2156663 -0.7217255
#[3,] -0.4186808  0.9011675  0.1122654
#how do these relate? They are the same

#G
#plot the PC time series and describe their behavior (Ev and MU)
pc1_MU = svd(A)$v[,1]
pc1_MU
pc2_MU = svd(A)$v[,2]
length(pc1_MU)
plot(2001:2005, pc1_MU, type = 'o', 
     ylab = "PC", xlab = "Year", 
     main = "Principal Components Time Series",
     col = 'black', ylim = range(c(pc1_EV, pc1_MU)))
lines(2001:2005, pc2_MU, type = 'o', 
      col = 'purple', lty = 2)
legend("topright", legend = c("PC1", "PC1"), 
       col = c("black", "purple"), lty = c(1, 2), bty = "n")
dev.off()


#2.1 
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
data = read.csv("NOAAGlobalT.csv", header = TRUE, sep = ",")
dim_data = dim(data) # 2592x1648 
#want years 2000-2008
#rows indicate which grid boxes
#columns indicates years 2000-2008 and to go by 12 so we only get December data
december = data[394:397, seq(1455,1550, by = 12)]
colnames(december) = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007")
rownames(december) = c("Lat: -62.5, Lon: 167.5", "Lat: -62.5, Lon: 172.5", "Lat: -62.5, Lon: 177.5", "Lat: -62.5, Lon: 182.5")
print(december)
#                            2000    2001    2002    2003    2004    2005    2006    2007
#Lat: -62.5, Lon: 167.5   -0.0066 -0.0668 -0.1621 -0.6411 -0.3250  0.1705 -0.4347 -0.3852
#Lat: -62.5, Lon: 172.5 -999.9000 -0.0688 -0.1389 -0.3719 -0.2842  0.1068 -0.4102 -0.3804
#Lat: -62.5, Lon: 177.5   -0.0744 -0.0540 -0.0978 -0.2105 -0.2511  0.0210 -0.3623 -0.3390
#Lat: -62.5, Lon: 182.5 -999.9000 -0.0764 -0.1034 -0.2055 -0.2830 -0.1162 -0.3682 -0.3273