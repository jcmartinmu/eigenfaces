# Set up working directory and load libraries ####

rm(list=ls())
setwd("~/R/eigenfaces")

if(!(require(dplyr))){install.packages('dplyr')}
library(dplyr)

# Load data ####
dat <- "olivetti_X.csv" %>% # csv file contains data of face images taken between April 1992 and April 1994 at AT&T Laboratories Cambridge 
                               # Each row contains data of one image quantized to 256 grey levels between 0 and 1
  read.csv(header=FALSE) %>% # Load csv file with data
  data.matrix # Convert data into matrix

# Add labels
faceLab <- rep(1:40, each=10) # Images for 40 persons; 10 images for each person
rownames(dat) <- faceLab
  
# Select faces for trining and for testing
testFaces <- seq(from = 10, to = 400, by=10) # Variable selecting one face image of each person (one face image out of every ten face images) 
datMat <- dat [-testFaces, ] # Data matrix for training excluding face images for testing
datMatTest <- dat[testFaces, ] # Data matrix with face images for testing
rm(dat) # Remove variable dat from working environemnt

# Define function to show face image ####
showFace <- function(x){
  x %>%
  matrix(nrow = 64, byrow = TRUE) %>% # Create matrix 64 by 64
  apply(2, rev) %>% #Rotate matrix by 90 degrees, step one : reverse columns 
  t %>% # Rotate matrix by 90 degrees, step two: transpose matrix
  image(col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n") # 256 diffrent intensities between 0 and 1 defined
  }

# Display selected faces from dataset ####
dev.off()
par(mfrow=c(4, 10))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:40) {
  showFace(datMat[i, ])
  }

# Compute and display average face (mean by each column) #### 
avFace <- colMeans(datMat)
dev.off()
showFace(avFace)

# Center data, calculate covariance matrix and its eigenvectors and eigenvalues #### 
datMatCen <- scale(datMat, center = TRUE, scale = FALSE)
covMat <- t(datMatCen) %*% datMatCen / nrow(datMat-1)
eig <- eigen(covMat)
eigVec <- eig$vectors # Eigenvectors as unit vectors define axes of the preincipal components
eigVal <- eig$values # Corresponding eigenvalues define variances along the axes of the principal components 

# Compute and display proportions of variance explained by the principal components
varProp <- eigVal/sum(eigVal) # Proportion of variance in the total variance
varCumProp <- cumsum(eigVal)/sum(eigVal) # Proportion of cumulative varinace in the total variance

dev.off()
par(mfrow=c(1, 2))
plot(varProp*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion in the total variance")
plot(varCumProp*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion of the cumulative variance in the total variance")


#Select eigenvectors 
thresNum <- min(which(varCumProp > 0.95)) # Princiapal components explains at least 95% of the total variance
eigVecSel <-  eigVec[, 1:thresNum]

# Display first 16 eigenvectors also called eigenfaces ####
dev.off()
par(mfrow=c(4, 4))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:16) {
  showFace(eigVecSel[, i])
}

# Project the data matrix onto the space spanned by the selected eigenvectors
dev.off()
coef <- datMatCen %*% eigVecSel
barplot(coef[1, ], main = "Coefficients of the projection onto eigenvectors for the first image", ylim = c(-8, 4)) #

# Reconstruct first image using coefficients and eigenvectors (eigenfaces)
dev.off()
par(mfrow=c(1, 2))
par(mar=c(0.05, 0.05, 0.05, 0.05))
showFace((datMat[1, ]))
(coef[1, ] %*% t(eigVecSel) + avFace) %>%
  showFace()

# New image under test  ####
coef142 <- (datMat[142, ] - avFace) %*% eigVecSel #Computing coefficients for image 142 by projecting it into eigenvector space
calDiff <- function(x){
  ((x-coef142) %*% t(x-coef142)) %>%
    sqrt
}
diff <- apply(coef, 1, calDiff)
NumFace <- which(min(diff)==diff)

# Display similarity plot
dev.off()
barplot(diff,main = "Similarity Plot ") # Warning: extend horizontaly plot to see better results
      

#####################################################################
# Calculate covariance matrix for centered data ####
# Covariance matrix could be calculated as follows:
# covMatrix <- t(dataMatrixCent) %*% dataMatrixCent
# This would however give a large matrix 4096x4096
# Instead we calculated the follwoing matrix 400x400:
# CovMat_ = dataMatrixCent %*% t(dataMatrixCent)
# Instead of A'*A, calculate A*A'
covMatrix_ <- dataMatrixCent %*% t(dataMatrixCent)

#Eginvalues and eigenvectors for CovMatrix_ (with underscore)
# Function eigen() gives by default eigenvectors as unit vectors 
eigen <- eigen(covMatrix_)
eigenvectors_ <- eigen$vectors
eigenvalues_ <- eigen$values

# Postive eigenvalues for CovMatrix are the same as egigenvalues for CovMatrix_
eigenvalues <- eigenvalues_

# Calculate eginevectors for CovMatrix using the following identity:
# A*A'*X(i) = Lambda(i)*X(i)
# A'*A*A'*X(i) = A'*Lambda(i)*X(i)
# A'*A*[A'*X(i)] = Lambda(i)*[A'*X(i)]
# CovMatrix has 400 eigenvectors linked to positive eigenvalues 
# Other eigenvectors are linked with eigenvalues having value of zero
eigenvectors <- t(dataMatrixCent) %*% eigenvectors_

# Display first 16 eigenfaces (eigenvectors) ####
dev.off()
par(mfrow=c(4, 4))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:16) {
  showFace(eigenvectors[, i])
}

# Calculate and display the proportion of variance explained by each component
# and cumulative variance explained by the components
varProportion <- eigenvalues/sum(eigenvalues)
varCumulative <- cumsum(eigenvalues)/sum(eigenvalues)
dev.off()
par(mfrow=c(1, 2))
plot(varProportion*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion in the total variance")
plot(varCumulative*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion of the cumulative variance in the total variance")


#Choose the number of principal components
threshold <- min(which(varCumulative > 0.95))
#threshold <- 400

# Projection of 1st image  onto eigenvectors
# coefs = A * E
dev.off()
coefsFace1 <- dataMatrixCent[1, ] %*% eigenvectors 
barplot(CoefsFace1, main = "Coefficients of the projection onto eigenvectors", ylim = c(-600, 100))
eigenvectSelec <- eigenvectors[, 1:threshold]
coefsFace1Selec <- coefsFace1[, 1:threshold, drop = FALSE]
dim(eigenvectSelec)
dim(coefsFace1Selec)
# Reconstruct first image using coefficients and eigenfaces
dev.off()
par(mfrow=c(1, 2))
par(mar=c(0.05, 0.05, 0.05, 0.05))
showFace((dataMatrix[1, ]))
(coefsFace1Selec %*% t(eigenvectSelec) + averageFace) %>%
  showFace()





dev.off()
par(mfrow=c(4, 4))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:16) {
  showFace(eigenvectors[, i])
}

dev.off()
par(mfrow=c(1, 2))
par(mar=c(0.05, 0.05, 0.05, 0.05))
plotImage(image1)
plotImage(image1Reconst)


# Rotate first image 90 degrees for display
#image2 <- t(apply(matrix(as.numeric(faceData[1, ]), nrow = 64, byrow = T), 2, rev))
#plotImage(image2)
image2 <- t(matrix(as.numeric(faceData[1, ncol(faceData):1]), nrow = 64, byrow = T), 2, rev)
plotImage(image2)






# Rotate every image and save to a new file for easy display in R
dataTemp <- NULL
for (i in 1:nrow(faceData)) {
  #imageRotated <- as.numeric(t(apply(matrix(as.numeric(faceData[1, ]), nrow = 64, byrow = T), 2, rev)))
  #dataTemp <- rbind(dataTemp, imageRotated)
  image <- faceData[i, ] %>% 
           as.numeric %>% 
           matrix(nrow = 64, byrow = TRUE) %>%
           apply(2, rev) %>% 
           t %>%
           as.numeric
    #t %>%
           
  dataTemp <- rbind(dataTemp, image)
} 

#dataTemp2 <- as.data.frame(dataTemp)
write.csv(dataTemp, "train_faces.csv")

faceData2 <- read.csv("train_faces.csv", header = FALSE)

dev.off()
image1 <- matrix(as.numeric(faceData2[1, ]), nrow = 64, byrow = T)
plotImage(image1)




img1 <- as.numeric(faceData[1, ])
# Image upside down
img2 <- matrix(img1, nrow=64, byrow=T)
# Image corrected
img3 <- t(img2)[, nrow(img2):1]
# Vector containing the image
img4 <- as.numeric(t(img3))
plotImage(img2)
plotImage(img3)
plotImage(img4)

imageT <- faceData[1, ] %>% 
       as.numeric %>% 
       matrix(nrow = 64, byrow = TRUE) %>%
       apply(2, rev) %>% 
       t #%>%
       t #%>%
       as.numeric

       image <- faceData[1, ] %>% 
         as.numeric %>% 
         matrix(nrow = 64, byrow = TRUE) %>%
         apply(2, rev) %>% 
         t %>%
         t %>%
         as.numeric      
       
       plotImage(image)


faceData2 <- read.csv("train_faces.csv", header = FALSE)

dev.off()
image1 <- matrix(as.numeric(faceData2[1, ]), nrow = 64, byrow = T)
plotImage(image1)


p
#clear parameters to visualise images
dev.off()
#Vectorize image

#Compute the mean vector: average faces

#Principal component analysis (PCA)

#Plot of eigenfaces (eigenvectors)

#Projection of the photo from the eigenvector space

#Reconstruction of the photo from the eigenvector space


#Converting data from data frame format into matrix 
#and rescaling data from 0 to 1 instead of 0 to 255 required by function image
X <- t(as.matrix(faces)/255)

dim(X)
#Looking at the structure of matrix
str(X)

#Defining the function to plot image
#grey scale intensity is required to be from 0 to 1
#256 diffrent intenities are defined
plt.img <- function(x){image(x, col=grey(seq(0, 1, length=256)))}

#Creating matrix for first image  64x64 
#from the first row of X with length of 4096
img1 <- apply(matrix(X[4, 4096:1], nrow=64, byrow=T), 1, rev)

#Plotting image1
plt.img(img1)

#####
par(mfrow=c(1, 2))
img1 <- apply(matrix(X[1, 4096:1], nrow=64, byrow=T), 1, rev)

#Plotting image1
plt.img(img1)

img2 <- apply(matrix(X[1, 1:4096], nrow=64, byrow=T), 1, rev)

#Plotting image2
plt.img(img2)

t <- t(faces)

df <- as.data.frame(t(faces))

str(df)
im <- t(matrix(as.numeric(df[2, 4096:1]), ncol = 64, nrow = 64))
#im <- t(matrix(as.numeric(df[2, 1:4096]), ncol = 64, nrow = 64))

plt.img <- function(x){image(x, col=grey(seq(0, 1, length=256)))}
plt.img(im)



temp <- as.numeric(df[2, 4096:1])

temp2 <- df[2, 4096:1]
#Rotating first image 90 degree for display
#by firstly reversing the order of columns and secondly transposing
img1.r <- t(apply(matrix(X[1, ], nrow=64, byrow=T), 2, rev))
plt.img(img1.r)

img1.r <- apply(matrix(X[1, ], nrow=64, byrow=T), 1, rev)
plt.img(img1.r)
dev.off()
  

head(faces)
str(faces)
faces$V1
X <- faces
X <- (data(faces)) 
X <- data.matrix(data("faces")) # %>% data.matrix()


X2 <- read.csv("olivetti_X.csv", header = F) %>% as.matrix()
str(X)
b <- matrix(as.numeric(X2[1, ]), nrow=64, byrow=T)
plotImage <- function(x){image(x, col=grey(seq(0, 1, length=256)))}

b <- matrix(as.numeric(X2[1, ]), nrow=64, byrow=T)
plotImage(b)

c <- t(apply(matrix(as.numeric(X[1, ]), nrow=64, byrow=T), 2, rev))
plotImg(c)

c <- t(matrix(as.numeric (X[1, ]), nrow=64, byrow=T))
plotImg(c)

#plotImage <- function(x){image(x, col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")}
#rotateImage <- function(x){x <- t(apply(x, 2, rev))}