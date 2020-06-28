# Clear workspace ####
rm(list=ls())

# Set working directory to source file location ####
#setwd("~/R/eigenfaces")

# Install and load libraries ####

if(!(require(dplyr))){install.packages('dplyr')}
library(dplyr)

# Define a function to show face images ####
#  The purpose of this function is to convert a vector with the image into a matrix 
# and fix the issue concerning R function `image` which creates a grid with 90 degree counter-clockwise rotation of the conventional printed layout of a matrix.
showFace <- function(x){
  x %>%
  as.numeric() %>%
  matrix(nrow = 64, byrow = TRUE) %>% # Create matrix 64 by 64
  apply(2, rev) %>% #Rotate matrix by 90 degrees, step one : reverse columns 
  t %>% # Rotate matrix by 90 degrees, step two: transpose matrix
  image(col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n") # 256 diffrent intensities between 0 and 1 defined
}

# Load data with face images####
dataX <- "olivetti_X.csv" %>% # The loaded csv file contains data of face images taken between April 1992 and April 1994 at AT&T Laboratories Cambridge 
                               # Each row contains data of one image quantized to 256 grey levels between 0 and 1
  read.csv(header=FALSE) %>% # Load csv file with data
  data.frame() # Convert data into data frame
str(dataX)

# Display faces selected from dataset  ####
par(mfrow=c(4, 10))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:40) {
  showFace(dataX[i, ])
  }

# Create labels####
dataY<-seq(1:40) %>% # Create a sequence of label numbers from 1 to 40 corresponding to 40 persons
  rep(each=10) %>% # Replicate 10 times each label number as we have 10 face images in sequence for each person
  data.frame() %>% # Convert to a data frame
  mutate(index = row_number()) %>% #Add a column with indices 
  select(2, label = 1) # Move the index column to the front and give a name to the column with labels

# Split data with image faces into training data and test data 
set.seed(1234)
trainSampInd <- dataY %>% # Use data with indices and lables
  group_by(label) %>% # Group data by label
  sample_n(8) %>% # Sample 8 indices of face images from each group and set them in one data frame
  arrange(index) # Sort out the results by index
   
testSampInd <-  setdiff(dataY, trainSampInd) # Determine indices of the data to be included in the set of test data

dataMat <- dataX %>%
  filter(row_number() %in% trainSampInd[, "index", drop=TRUE]) %>%
  data.matrix() %>%
  `rownames<-`(trainSampInd[, "label", drop=TRUE])

testDataMat <- dataX %>%
  filter(row_number() %in% testSampInd[, "index", drop=TRUE]) %>%
  data.matrix() %>%
  `rownames<-`(testSampInd[, "label", drop=TRUE])

# Compute and display the average face (mean by each column) #### 
avFace <- colMeans(dataMat)
dev.off()
showFace(avFace)

#Center data
dataMatCen <- scale(dataMat, center = TRUE, scale = FALSE)

# A) Calculate covariance matrix and its eigenvectors and eigenvalues #### 
covMat <- t(dataMatCen) %*% dataMatCen / nrow(dataMat-1) # Calculate covariance matrix
eig <- eigen(covMat)
eigVec <- eig$vectors # Eigenvectors as unit vectors define axes of the preincipal components
eigVal <- eig$values # Corresponding eigenvalues define variances along the axes of the principal components 

# B) Conduct svd (more numerically stable )####
#svd <- svd(dataMatCen) # Conduct singular value decomposition
#eigVec <- svd$v # Eigenvectors of covariance matrix are equal to right singular vectors of svd
                # Eigenvectors as unit vectors define axes of the preincipal components
#eigVal <- svd$d^2/(ncol(dataMatCen)-1) # Eigenvalues of covariance matrix are equal to squared singular values devided by n-1, where n is the number of columns in the data matrix 
                                       # Eigenvalues (corresponding to eigenvectors) define variances along the axes of the principal components 

# Compute and display the proportions of variance explained by the principal components ####
varProp <- eigVal/sum(eigVal) # Proportion of variance in the total variance
varCumProp <- cumsum(eigVal)/sum(eigVal) # Proportion of cumulative varinace in the total variance

dev.off()
par(mfrow=c(1, 2))
plot(varProp*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion in the total variance")
plot(varCumProp*100, xlab = "Eigenvalues", ylab = "Percentage" , main = "Proportion of the cumulative variance in the total variance")


#Select eigenvectors ####
thresNum <- min(which(varCumProp > 0.95)) # Princiapal components explains at least 95% of the total variance
eigVecSel <-  eigVec[, 1:thresNum]

# Display first 16 eigenvectors also called eigenfaces ####
dev.off()
par(mfrow=c(4, 4))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:16) {
  showFace(eigVecSel[, i])
}

# Project the data matrix onto the space spanned by the selected eigenvectors ####
dev.off()
coefTrainFaces <- dataMatCen %*% eigVecSel %>% # Calculate coefficients (weights) for each training face
  `rownames<-`(rownames(dataMat)) # Make each rowname with the coefficients the label of the corresponding face image.

 
barplot(coefTrainFaces[1, ], main = "Coefficients of the projection onto eigenvectors for the first image", ylim = c(-8, 4)) #

# Reconstruct first image using coefficients and eigenvectors (eigenfaces) ####
dev.off()
par(mfrow=c(1, 2))
par(mar=c(0.05, 0.05, 0.05, 0.05))
showFace((dataMat[1, ]))
(coefTrainFaces[1, ] %*% t(eigVecSel) + avFace) %>%
  showFace()

# Project the matrix with test data onto the space spanned by the eigenvectors determined by training data  ####
coefTestFaces<- testDataMat %>% # Use test data
  apply(1, function(x) x-avFace) %>% # Deduct the vector of the average face from each row vector of the test data matrix
  t %*% # Transpose the results from columns into rows
  eigVecSel %>%# Calculate coefficients (weights) for each test face by projecting the row vectors with test images onto the space spanned by the eigenvector
  `rownames<-`(rownames(testDataMat)) # Make each rowname with the coefficients the label of the corresponding face image.

# Define a function to calculate the Euclidean distance between two vectors with weights
calDif <- function(x){
  ((x-coefTestSel) %*% t(x-coefTestSel)) %>%
    sqrt
} # This function will be used in a test exercise

# Create an empty matrix to store test results ####
testRes <- matrix(NA, nrow = 80, ncol = 3) %>%
  data.frame %>%
  `colnames<-`(c("Face.label", "Identified.label", "Correct.1/Wrong.0"))

# Conduct test exercise for all test faces ####
for (i in 1:nrow(coefTestFaces)) { # Start loop for row vectors with test faces
  coefTestSel <- coefTestFaces[i, , drop=FALSE] # Extract coefficients of i test face
  difCoef <- apply(coefTrainFaces, 1, calDif) # Calculating the diffrence between the coefficients of i test face and each traing face
  testRes[i, 1]  <- rownames(coefTestFaces)[i] # Provide the label of i test face
  testRes[i, 2] <- rownames(coefTrainFaces)[which(min(difCoef)==difCoef)] # Provide the label of training face recording the minimum diffrence with i test face
  
}

testRes[, 3] <- ifelse(testRes[, 2] == testRes[, 1], 1, 0) # Provide 1 for correct and 0 for wrong results
testRes[1:10, ] # Display the results for the first 10 test faces
(shareCor <- sum(testRes[, 3])/nrow(testRes)) # Calclulate share of correct results

