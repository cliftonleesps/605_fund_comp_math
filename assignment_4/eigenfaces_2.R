
# With the attached data file, build and visualize  eigenimagery that accounts 
# for 80% of the variability.  Provide full R code and discussion.

# References
#  * https://rpubs.com/dherrero12/543854
#  * https://rpubs.com/R-Minator/eigenshoes

library(rstudioapi)
library(jpeg)
library(OpenImageR)
library(xROI)


# Dimensions of the image files
height=1200 
width=2500

# How much we want to reduce the resolution of the original file
scale=20


# Set the working directory to the source file location
setwd(dirname(getActiveDocumentContext()$path))

# get the image filenames
shoes_files <- list.files(path="jpg", pattern="*")


# initialize array with all zeroes, and 
# size and resolution we want
im=array(
  rep(0,length(shoes_files)*height/scale*width/scale*3), 
  dim=c(length(shoes_files), 
        height/scale, 
        width/scale,
        3)
)


for (i in 1:length(shoes_files)){
  
  # Read and resize the image
  temp = resizeImage(
    readJPEG(paste0("jpg/", shoes_files[i])),
    height/scale, 
    width/scale
  )
  
  # add the image to an image list
  im[i,,,] = array(
    temp,
    dim=c(1, height/scale, width/scale,3)
  )
}


# show a random image and how it's scaled down
imageShow(im[3,,,])


# Create a matrix of all images of the training set are stored 
# in a single matrix data, where each row of the matrix is an image.
data=matrix(0, length(shoes_files), prod(dim(im))) 
for (i in 1:length(shoes_files)) {
  r=as.vector(im[i,,,1]); 
  g=as.vector(im[i,,,2]);
  b=as.vector(im[i,,,3])
  data[i,] <- t(c(r, g, b))
}


# now transpose the data frame so each column is an image
shoes <- data.frame(t(data))


par(mfrow=c(4,5))
par(mai=c(.1,.1,.1,.1))
for (i in 1:length(shoes_files)){  #plot the first images only
  plotJPEG(writeJPEG(im[i,,,]))
}

# Scale and center the shoes data frame.
# This step is a substitute for subtracting the average image from each image
scaled <- scale(shoes, center = TRUE, scale = TRUE)

# Calculate the correlation coefficient of 'scaled'
Sigma_=cor(scaled)

# Get the Eigenvalues and Eigenvectors
eig          <- eigen(Sigma_)
eigenvalues  <- eig$values
eigenvectors <- eig$vectors

# Choose the principal components
cum.var  <- cumsum(eigenvalues) / sum(eigenvalues)
thres    <- min(which(cum.var > .80))

# Multiply the scaled data frame by the Eigenvectors
scaling    <- diag(eigenvalues[1:thres]^(-1/2)) / (sqrt(nrow(scaled)-1))
eigenshoes <- scaled%*%eigenvectors[,1:thres]%*%scaling


par(mfrow=c(4,5))
par(mai=c(.1,.1,.1,.1))
imageShow(array(eigenshoes[,1], c(60,125,3)))


# Display the Eigen-shoe
eigenshoe <- array(eigenshoes[,2], dim(temp))
imageShow(eigenshoe)
