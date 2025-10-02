#1 
#read the data files from directory
getwd()
darwin_data = read.table("PSTANDdarwin.txt")
tahiti_data = read.table("PSTANDtahiti.txt")
darwin = darwin_data[11:50, 2]
tahiti = tahiti_data[11:50, 2]
#-----------------------------------------------------------
#A create space time matrix
A = rbind(darwin,tahiti)
dim(A)
# 2 40
print(A[,1:6])
#       [,1] [,2] [,3] [,4] [,5] [,6]
#darwin  0.5 -1.4 -0.7  0.6 -0.3  0.7
#tahiti  0.1  1.8  1.0  0.0 -1.0 -1.5
#-----------------------------------------------------------
#B SVD of space time matrix
svdA = svd(A)
print(svdA)
U = svdA$u
D = svdA$d
V = svdA$v
#$d
#[1] 9.231838 6.429866

#$u
#[,1]      [,2]
#[1,] -0.6063979 0.7951614
#[2,]  0.7951614 0.6063979

#$v
#[,1]          [,2]
#[1,] -0.024229499  0.0712643902
#[2,]  0.246998228 -0.0033763944
#[3,]  0.132112366  0.0077427595
#[4,] -0.039411301  0.0742001203
#[5,] -0.066426865 -0.1314096266
#[6,] -0.175178623 -0.0548975427
#[7,]  0.210500521 -0.0628978643
#[8,]  0.021877880 -0.4258981442
#[9,] -0.223075647 -0.4007241065
#[10,] -0.135025959  0.1259788308
#[11,]  0.035321898 -0.1177954070
#[12,]  0.052548401 -0.0989334937
#[13,] -0.030798049  0.0836310770
#[14,]  0.295457226 -0.1459051749
#[15,] -0.075040116 -0.1408405833
#[16,]  0.160865713 -0.0976859609
#[17,] -0.074605670 -0.2186002001
#[18,] -0.023053690  0.2485812672
#[19,] -0.072560969 -0.1968025567
#[20,]  0.045979851 -0.0865668070
#[21,]  0.023053690 -0.2485812672
#[22,]  0.157517674  0.1137952462
#[23,] -0.442316947  0.0633385287
#[24,]  0.034018560  0.1154834434
#[25,] -0.028753348  0.1054287203
#[26,]  0.101007399 -0.2414622742
#[27,] -0.086566961 -0.0165499497
#[28,]  0.003220511  0.1991145204
#[29,]  0.199408122 -0.0163668475
#[30,] -0.004523849  0.0341643301
#[31,]  0.079998411  0.0289166364
#[32,] -0.341437077  0.2324719819
#[33,] -0.127154071 -0.1196667064
#[34,] -0.017226503 -0.0188619133
#[35,] -0.054158656 -0.0006237664
#[36,]  0.128022963 -0.0358525273
#[37,]  0.081174220  0.2062335134
#[38,] -0.358536051 -0.1969856589
#[39,]  0.225682322 -0.0658335944
#[40,]  0.094745766  0.1037405231
#--------------------------------------------------------------------
#C plot PC1
dev.off()
PC1 = V[,1]
time = 1961:2000
plot(time,PC1, xlab = 'year', ylab = paste("PC1"),
     main = "First Principle Component for Darwin and Tahiti SLP", 
     type = 'l',col = "blue")
#------------------------------------------------------------------------------
#3 Linear Equations
A = matrix(c(1,2,3,4,
             4,5,6,0,
             7,8,0,9,
             3,1,2,9), nrow = 4,byrow=TRUE)
b = matrix(c(2,5,0,1), nrow =4)
solve(A,b)

#             [,1]
#[1,]  0.116402116
#[2,]  0.005291005
#[3,]  0.751322751
#[4,] -0.095238095
#---------------------------------------------------------------------
#4
#B show by hand too
a = c(2,1)
b = c(1,2)
mag_a = sqrt(a[1]^2 + a[2]^2)
mag_b = sqrt(b[1]^2 + b[2]^2)
dem = mag_a * mag_b
num = dot(a,b)
num
theta = (acos(num/dem))*(180/pi)
theta
#[1] 36.8699
#-----------------------------------------------------------
#C recreate the plot
plot(0:3,0:3,main = 'Two 2D vectors and the angle between them',
     cex.axis = 1.4, cex.lab = 1.4, xlab = '', ylab = '',type = 'n')
arrows(0,0,a[1],a[2], col = 'black', code = 2,lwd = 1,pch = 16)
arrows(0,0,b[1],b[2], col = 'blue', code = 2,lwd = 1, pch =16)
text(2.05,1, "a", col = "black",pch = 16)
text(1.05,2, "b", col = "blue",pch = 16)
text(.6,.5, "θ = 36.87 ")
grid(col = "gray", lty = "dotted")
dev.off()
#----------------------------------------------------------------
#D
library(pracma)
u = c(2,1,0)
v = c(1,2,0)
cross(u,v)
#[1] 0 0 3
#---------------------------------------------------------------
#5
A = matrix(c(5,1,0,3), nrow = 2, byrow = TRUE)
eigen = eigen(t(A)%*%A)
values = eigen$values
vectors = eigen$vectors
#$values
#[1] 26.513878  8.486122

#$vectors
#           [,1]       [,2]
#[1,] -0.9570920  0.2897841
#[2,] -0.2897841 -0.9570920
#------------------------------------------------------------------
X = vectors[,1]
P = t(X)%*%t(A)%*%A%*%X
#26.51388
#------------------------------------------------------------------------
#6 
library(imager)
sam = load.image('SamPhoto.png')
#A
graydat = grayscale(sam)
graydat[1:4,1:3,1,1]
#          [,1]      [,2]      [,3]
#[1,] 0.6075294 0.6126667 0.6215686
#[2,] 0.6072549 0.6112549 0.6186667
#[3,] 0.6053725 0.6098431 0.6107843
#[4,] 0.6158431 0.6192941 0.6158431
#-------------------------------------------------------------------------
#B
min = min(graydat)
#[1] 0.1607843
max = max(graydat)
#[1] 1
#-------------------------------------------------------------------------
#C
dev.off()
plot(graydat)
x = 200
y = 100
points(x,y,col = 'blue',pch = 16)

#--------------------------------------------------------------------------
#7
#A
P1 = c(1,1)
P2 = c(1,0)
P3 = c(3,4)

#Case C1 = (P1, P2)
c1 = (P1 + P2)/2
c2 = P3
tWCSS = norm(P1 - c1, type = '2')^2 + 
  norm(P2- c1, type = '2')^2 + 
  norm(P3- c2, type = '2')^2
tWCSS
#.5

#Case C2 = (P1, P3)
c1 = (P1 + P3)/2
c2 = P2
tWCSS = norm(P1 - c1, type = '2')^2 + 
  norm(P2- c2, type = '2')^2 + 
  norm(P3- c1, type = '2')^2
tWCSS
#6.5

#Case C3 = (P2, P3)
c1 = (P2 + P3)/2
c2 = P1
tWCSS = norm(P1 - c2, type = '2')^2 + 
  norm(P2- c1, type = '2')^2 + 
  norm(P3- c1, type = '2')^2
tWCSS
#10
#----------------------------------------------------------------
#B
dev.off()
plot(0, xlim= c(0,4), ylim = c(0,5),type = 'n', xlab = "X", ylab = "Y",
     main = "K-means Clustering for 3 points and 2 Clusters")
points(P1[1], P1[2], col = 'red', pch = 16)
points(P2[1], P2[2], col = 'red', pch = 16)
points(P3[1], P3[2], col = 'blue', pch = 16)
points(c1[1],c1[2], col = "red", pch = 4,cex = 2)
points(P3[1], P3[2], col = 'blue', pch = 4, cex = 2)
grid(col = 'gray', lty ="dotted")
#---------------------------------------------------------------------------
#C SVM
x = matrix(c(1,1,1,0,3,4), ncol = 2, byrow = TRUE)
y = c(1,1,2)
library(e1071)
data = data.frame(x,y=as.factor(y))
svm3 = svm(y ~., data=data, kernel = "linear",
           cost = 10, scale = "FALSE",
           type = "C-classification")
SV = svm3$SV
w = t(svm3$coefs) %*% SV
b = svm3$rho
DM = 2/norm(w, type = '2')
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
     main = "SVM Analysis for 3 Points with Support Vectors",
     xlim = c(0,5), ylim = c(0,5))
lines(x1,x2,lwd=1, col = "violet")
lines(x1,m1,lty = 2, col = 2)
lines(x1,m2,lty = 2, col = 4)
axis(2, at = -15:15, tck = 1, lty = 3, col = "grey",labels = NA)
axis(1, at = -15:15, tck = 1, lty = 3, col = "grey",labels = NA)
points(SV[,1], SV[,2], col = "black",pch = 12, cex = 1.5)
text(SV[,1], SV[,2]+ .3, col = "black", paste('SV',1:2))
points(c1[1],c1[2], col = "red", pch = 4,cex = 2)
points(P3[1], P3[2], col = 'blue', pch = 4, cex = 2)
#----------------------------------------------------------------------------
#D
SV = svm3$SV
#  X1 X2
#1  1  1
#3  3  4
DM = 2/norm(w, type = '2')

