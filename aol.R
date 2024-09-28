library(readxl)
aol <- read_excel("aol.xlsx", sheet = "Sheet1")

library(expm)

#Uji Asumsi

#Distirbusi normal multivariate
library(MVN)
result <- mvn(data = aol[2:7], mvnTest = "mardia")
print(result) #Tidak distribusi normal multivariate

qqPlot <- mvn(data=aol[2:7], multivariatePlot = "qq")

#Log transformasi
aol_new = log(aol[2:7] + 1)
result_new <- mvn(data = aol_new, mvnTest = "mardia")
print(result_new)

qqPlot <- mvn(data=aol_new, multivariatePlot = "qq")


#Multikolinearitas
model = lm(Provinsi ~., data = aol_new)
summary(model)

library(car)

vif(model) #Tidak terjadi multikolinearitas karena VIF nya tidak ada yang lebih dari 10

#Linearitas
library(lmtest)
reset_result <- resettest(model, power = 2:3, type = "fitted")
print(reset_result)

#Matrix korelasi
matrix_corr <- cor(aol[, 2:7], method = c("pearson"))
matrix_corr

rho11 <- matrix_corr[1:3, 1:3]
rho11

rho12 <- matrix_corr[1:3, 4:6]
rho12

rho21 <- matrix_corr[4:6, 1:3]
rho21

rho22 <- matrix_corr[4:6, 4:6]
rho22

#1. hitung rho11 ^ -1/2
rho11_sqrtm <- solve(sqrtm(rho11))
rho11_sqrtm

#2. hitung rho22 ^ -1
rho22_inv <- solve(rho22)
rho22_inv

#3. hitung rho22 ^ -1/2
rho22_sqrtm <- solve(sqrtm(rho22))
rho22_sqrtm

#4. hitung 11 ^ -1
rho11_inv <- solve(rho11)
rho11_inv

matA <- rho11_sqrtm %*% rho12 %*% rho22_inv %*% rho21 %*% rho11_sqrtm
matB <- rho22_sqrtm %*% rho21 %*% rho11_inv %*% rho12 %*% rho22_sqrtm

matA
matB

eigenA <- eigen(matA)
eigenA

eigenB <- eigen(matB)
eigenB

#Koefisien Korelasi Kanonik
sqrt_kanonik <- sqrt(eigenA$values)
sqrt_kanonik

#Fungsi Kanonik
U1_1 <- eigenA$vectors[1:3, 1] %*% rho11_sqrtm
U1_1

U1_2 <- eigenA$vectors[1:3, 2] %*% rho11_sqrtm
U1_2

U1_3 <- eigenA$vectors[1:3, 3] %*% rho11_sqrtm
U1_3

V1_1 <- eigenB$vectors[1:3, 1] %*% rho22_sqrtm
V1_1

V1_2 <- eigenB$vectors[1:3, 2] %*% rho22_sqrtm
V1_2

V1_3 <- eigenB$vectors[1:3, 3] %*% rho22_sqrtm
V1_3

cor(as.vector(U1_1), as.vector(V1_1))
cor(as.vector(U1_2), as.vector(V1_2))
cor(as.vector(U1_3), as.vector(V1_3))

#Bobot Kanonik
cancor_result = cancor(aol[, 2:4], aol[, 5:7])
cancor_result

weight_x = cancor_result$xcoef
weight_y = cancor_result$ycoef

weight_x
weight_y

#Muatan Kanonik
X <- aol[, 2:4]
Y <- aol[, 5:7]

cancor_result <- cancor(X, Y)
cancor_result

xcoef <- cancor_result$xcoef
ycoef <- cancor_result$ycoef

U <- as.matrix(X) %*% xcoef
V <- as.matrix(Y) %*% ycoef

canonical_loadings_X <- cor(X, U)
canonical_loadings_X
canonical_loadings_Y <- cor(Y, V)
canonical_loadings_Y

#Uji Independence Kanonik
lambda.test <- det(matrix_corr) / (det(rho11) * det(rho22))
lambda.test

#Uji Parsial
lambda_1 <- (1-(sqrt_kanonik[1]^2))*(1-(sqrt_kanonik[2]^2))*(1-(sqrt_kanonik[3]^2))
lambda_2 <- (1-(sqrt_kanonik[2]^2))*(1-(sqrt_kanonik[3]^2))
lambda_3 <- (1-(sqrt_kanonik[3]^2))

lambda_1
lambda_2
lambda_3

p <- 3
q <- 3
n <- nrow(aol)
n

k = 1
df1 = (p-k+1)*(q-k+1)
w = n-0.5*(p+q+3)
t = sqrt(((p-k+1)^2*(q-k+1)^2-4)/((p-k+1)^2+(q-k+1)^2-5))
df2=round((w*t)-(0.5*(df1))+1)
k
df1
df2

F_1=(1-lambda_1^(1/t))/lambda_1^(1/t)*df2/df1
F_1
Ftabel_1 = qf(p=0.05,df1,df2, lower.tail = FALSE)
Ftabel_1
F_1>=Ftabel_1

k = 2
df1 = (p-k+1)*(q-k+1)
w = n-0.5*(p+q+3)
t = sqrt(((p-k+1)^2*(q-k+1)^2-4)/((p-k+1)^2+(q-k+1)^2-5))
df2=round((w*t)-(0.5*(df1))+1)
k
df1
df2

F_2=(1-lambda_2^(1/t))/lambda_2^(1/t)*df2/df1
F_2
Ftabel_2 = qf(p=0.05,df1,df2, lower.tail = FALSE)
Ftabel_2
F_2>=Ftabel_2

k = 3
df1 = (p-k+1)*(q-k+1)
w = n-0.5*(p+q+3)
t = sqrt(((p-k+1)^2*(q-k+1)^2-4)/((p-k+1)^2+(q-k+1)^2-5))
df2=round((w*t)-(0.5*(df1))+1)
k
df1
df2

F_3=(1-lambda_3^(1/t))/lambda_3^(1/t)*df2/df1
F_3
Ftabel_3 = qf(p=0.05,df1,df2, lower.tail = FALSE)
Ftabel_3
F_3>=Ftabel_3

