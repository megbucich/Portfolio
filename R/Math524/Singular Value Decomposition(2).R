#2.9 
A = matrix(c(1,2,3,4,5,
             6,7,8,9,10,
             11,12,13,14,15), nrow = 3, byrow = TRUE)
svdA = svd(A)
U = svdA$u
print(U)
#           [,1]       [,2]       [,3]
#[1,] -0.2016649  0.8903171  0.4082483
#[2,] -0.5168305  0.2573316 -0.8164966
#[3,] -0.8319961 -0.3756539  0.4082483
D = diag(svdA$d)
print(D)
#         [,1]     [,2]         [,3]
#[1,] 35.12722 0.000000 0.000000e+00
#[2,]  0.00000 2.465397 0.000000e+00
#[3,]  0.00000 0.000000 1.583122e-15
V = svdA$v
print(V)
#           [,1]        [,2]        [,3]
#[1,] -0.3545571 -0.68868664  0.56979546
#[2,] -0.3986964 -0.37555453 -0.33522256
#[3,] -0.4428357 -0.06242242 -0.62629151
#[4,] -0.4869750  0.25070970 -0.02093113
#[5,] -0.5311143  0.56384181  0.41264974

#2.10 reconstruct A using its SVD
A1 = D[1,1]*U[,1]%*%t(V[,1])#mode 1
#          [,1]      [,2]      [,3]      [,4]      [,5]
#[1,]  2.511657  2.824337  3.137016  3.449696  3.762376
#[2,]  6.436920  7.238261  8.039602  8.840944  9.642285
#[3,] 10.362183 11.652185 12.942188 14.232191 15.522194
A2 = D[2,2]*U[,2]%*%t(V[,2]) #mode 2
#           [,1]       [,2]        [,3]       [,4]       [,5]
#[1,] -1.5116568 -0.8243365 -0.13701626  0.5503040  1.2376243
#[2,] -0.4369197 -0.2382610 -0.03960231  0.1590564  0.3577151
#[3,]  0.6378174  0.3478145  0.05781164 -0.2321912 -0.5221941
A3 = D[3,3]*U[,3]%*%t(V[,3]) #mode 3
#             [,1]          [,2]          [,3]          [,4]          [,5]
#[1,]  3.682628e-16 -2.166567e-16 -4.047766e-16 -1.352794e-17  2.666984e-16
#[2,] -7.365256e-16  4.333134e-16  8.095532e-16  2.705588e-17 -5.333969e-16
#[3,]  3.682628e-16 -2.166567e-16 -4.047766e-16 -1.352794e-17  2.666984e-16
A = A1 + A2 + A3 #A is the sum of all 3 modes
#      [,1] [,2] [,3] [,4] [,5]
#[1,]    1    2    3    4    5
#[2,]    6    7    8    9   10
#[3,]   11   12   13   14   15


#2.11
A = matrix(c(1.2,-.5,.9,-.6,
             1,-.7,-.4,.9,
             -.2,1.1,1.6,-.4), nrow = 3, byrow = TRUE)
#A
E1 = eigen(A%*%t(A))
#$values
#[1] 5.3834039 3.2895832 0.6170129

#$vectors
#[,1]      [,2]       [,3]
#[1,]  0.1586046 0.8912905  0.4247892
#[2,] -0.5272625 0.4402097 -0.7267803
#[3,]  0.8347687 0.1087047 -0.5397634
#B
E2 = eigen(t(A)%*%A)
#$values
#[1] 5.383404e+00 3.289583e+00 6.170129e-01 5.890591e-16

#$vectors
#           [,1]       [,2]      [,3]       [,4]
#[1,]  0.2171740  0.8204225 0.1388675 -0.5103511
#[2,] -0.5206528 -0.3496772 0.3785963 -0.6806698
#[3,] -0.7280696  0.4410850 0.2426462  0.4652756
#[4,]  0.3894493 -0.1003834 0.8823284  0.2444361

#C
svd(A)
#$d
#[1] 2.3202163 1.8137208 0.7855017
#
#$u
#           [,1]      [,2]       [,3]
#[1,]  0.1586046 0.8912905  0.4247892
#[2,] -0.5272625 0.4402097 -0.7267803
#[3,]  0.8347687 0.1087047 -0.5397634
#$v
#           [,1]       [,2]       [,3]
#[1,] -0.2171740  0.8204225 -0.1388675
#[2,]  0.5206528 -0.3496772 -0.3785963
#[3,]  0.7280696  0.4410850 -0.2426462
#[4,] -0.3894493 -0.1003834 -0.8823284

#D The eigen vectors of A%*%t(A) are equal to svd(A)$U
#and the first 3 columns of eigen vectors of t(A)%*%(A) are equal to -svd(A)$v
eigen(A%*%t(A))$vectors
#           [,1]      [,2]       [,3]
#[1,]  0.1586046 0.8912905  0.4247892
#[2,] -0.5272625 0.4402097 -0.7267803
#[3,]  0.8347687 0.1087047 -0.5397634
svd(A)$u
#           [,1]      [,2]       [,3]
#[1,]  0.1586046 0.8912905  0.4247892
#[2,] -0.5272625 0.4402097 -0.7267803
#[3,]  0.8347687 0.1087047 -0.5397634
eigen(t(A)%*%(A))$vectors[,1:3]
#           [,1]       [,2]      [,3]
#[1,]  0.2171740  0.8204225 0.1388675
#[2,] -0.5206528 -0.3496772 0.3785963
#[3,] -0.7280696  0.4410850 0.2426462
#[4,]  0.3894493 -0.1003834 0.8823284
-svd(A)$v
#          [,1]       [,2]      [,3]
#[1,]  0.2171740 -0.8204225 0.1388675
#[2,] -0.5206528  0.3496772 0.3785963
#[3,] -0.7280696 -0.4410850 0.2426462
#[4,]  0.3894493  0.1003834 0.8823284


#E - verify that the columns vectors of U and V in the SVD
#are orthonormal to each other
U = svd(A)$u
V = svd(A)$v
#notice that when vectors are orthonormal, U%*%t(U) = I
#this is because when a matrix is orthonormal, t(U) = U inverse
#I will show both ways here
solve(U)
#         [,1]       [,2]       [,3]
#[1,] 0.1586046 -0.5272625  0.8347687
#[2,] 0.8912905  0.4402097  0.1087047
#[3,] 0.4247892 -0.7267803 -0.5397634
t(U)
#          [,1]       [,2]       [,3]
#[1,] 0.1586046 -0.5272625  0.8347687
#[2,] 0.8912905  0.4402097  0.1087047
#[3,] 0.4247892 -0.7267803 -0.5397634
t(U)%*%U
#              [,1]          [,2]          [,3]
#[1,]  1.000000e+00  1.110223e-16 -5.551115e-17
#[2,]  1.110223e-16  1.000000e+00 -1.734723e-16
#[3,] -5.551115e-17 -1.734723e-16  1.000000e+00
t(V)%*%V
#              [,1]          [,2]         [,3]
#[1,]  1.000000e+00 -1.665335e-16 2.775558e-16
#[2,] -1.665335e-16  1.000000e+00 1.387779e-17
#[3,]  2.775558e-16  1.387779e-17 1.000000e+00


#2.20
A = matrix(c(2,-1,-1,3), nrow =2, byrow = TRUE)
#     [,1] [,2]
#[1,]    2   -1
#[2,]   -1    3
t(A)
#     [,1] [,2]
#[1,]    2   -1
#[2,]   -1    3
A%*%t(A)
#     [,1] [,2]
#[1,]    5   -5
#[2,]   -5   10
eigen(A%*%t(A))
#eigen() decomposition
#$values
#[1] 13.09017  1.90983

#$vectors
#[,1]       [,2]
#[1,] -0.5257311 -0.8506508
#[2,]  0.8506508 -0.5257311

#8 
setwd("C:/Users/megan/OneDrive/Pictures/Documents/LinAlg")
getwd()
library(imager)
picture = load.image("Myphoto.jpg")
dim(picture)
graypicture = grayscale(picture)
dim(graypicture)
#[1] 1536 2048    1    1
graypicture[1:3,1:5,1,1] #first 3 rows and first 5 columns
#          [,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 0.4013333 0.4052549 0.3817255 0.3660392 0.3738824
#[2,] 0.3817255 0.3895686 0.3738824 0.3660392 0.3738824
#[3,] 0.3738824 0.3817255 0.3856471 0.3856471 0.3934902
plot(graypicture)

#9 
#A: make SVD analysis
svdP = svd(graypicture)
U = svdP$u
D = diag(svdP$d)
V = svdP$v

#B: plot the screen plot for the variances with the first 30 modes
k = 30
lamda = (svdP$d)^2
lamdak=lam[1:k]
par(mar=c(4,4,2,4))
plot(1:K, 100*lamdak/sum(lamda), ylim=c(0,100), type="o", 
     ylab="Percentage of Variance [%]",
     xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.1, lwd=2, col = "red",
     main="Scree Plot of the First 30 Eigenvalues")
legend(3,30, col=c("red"),lty=1, lwd=2.0,
       legend=c("Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="red")
par(new=TRUE)
plot(1:K,cumsum(100*lamdak/sum(lamda)),
     ylim = c(90,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")
legend(3,94.5, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4, col="black", col.axis="black", mgp=c(3,0.7,0))
mtext("Cumulative Variance Percentage (%)",col="black", 
      cex=1.2, side=4,line=2)
dev.off()
m= 1500
k = 1536 #we want only first 30 modes
m30 = as.cimg(U[,m:k]%*%D[m:k, m:k]%*%t(V[,m:k]))
plot(m30, main = "Grayscale first 30 modes")
dev.off()

#C: reconstruct the gray scale data matrix from the first 30 modes
recon = U%*%D%*%t(V)
dim(recon)
#[1] 1536 1536
image(recon)
dev.off()

#D: plot the reconstructed gray scale data matrix next to the 30 modes
kR = 1536
R1536 = as.cimg(U[,1:kR]%*%D[1:kR, 1:kR]%*%t(V[,1:kR]))
par(mfrow = c(1,2))
plot(R1536, main = "All 1536 Modes")
plot(m30, main = "The first 30 Modes")
dev.off()
