# Data 605 - Assignment 4

library(jpeg)
library(OpenImageR)
library(xROI)


# With the attached data file, build and visualize  eigenimagery that accounts 
# for 80% of the variability.  Provide full R code and discussion.

# Many… You are simply going to create the principal components from the images 
# by getting the Eigenvectors and multiplying by the original matrix.   

height=1200; 
width=2500;
scale=20

image <- readJPEG("jpg/RC_2500x1200_2014_us_53446.jpg")
imageShow(image)


shoes_files <- list.files(path="jpg", pattern="*")
shoes_files

# initialize array with all zeroes of the 
# size and resolution we want
im=array(
        rep(0,length(shoes_files)*height/scale*width/scale*3), 
        dim=c(length(shoes_files), 
              height/scale, 
              width/scale,
              3)
        )



for (i in 1:length(shoes_files)){
  temp = resizeImage(
                readJPEG(paste0("jpg/", shoes_files[i])),
                height/scale, 
                width/scale
                )
  im[i,,,] = array(
                  temp,
                  dim=c(1, height/scale, width/scale,3)
                  )
  }

# show a random image and how it's scaled down
imageShow(im[3,,,])


# Create a matrix of all images of the training set are stored 
# in a single matrix T, where each column of the matrix is an image.
flat=matrix(0, length(shoes_files), prod(dim(im))) 
for (i in 1:length(shoes_files)) {
  #newim <- readJPEG(paste0("jpg/", shoes_files[i]))
  r=as.vector(im[i,,,1]); 
  g=as.vector(im[i,,,2]);
  b=as.vector(im[i,,,3])
  flat[i,] <- t(c(r, g, b))
}



# transpose the flat matrix so every column is an image
# instead of a row
shoes=as.data.frame(t(flat))


par(mfrow=c(4,5))
par(mai=c(.1,.1,.1,.1))
for (i in 1:length(shoes_files)){  #plot the first images only
  plotJPEG(writeJPEG(im[i,,,]))
}


# Subtract the mean. The average image a has to be calculated 
# and then subtracted from each original image in T.
colnames(shoes)
typeof(shoes$V1)
length(unlist(shoes['V1']))

# Calculate the average shoe image
shoe_columns <- colnames(shoes)
average_shoe <- 0
for (c in shoe_columns) {
  average_shoe <- average_shoe + mean(unlist(shoes[c]))
}
average_shoe <- average_shoe / length(shoes_files)



# subtract the average show image from each original shoe image
for (i in 1:length(shoes_files)){  #plot the first images only
  print(im[i,,,])
}




# Eigencomponents from Correlation Structure
scaled=scale(shoes, center = TRUE, scale = TRUE)
mean.shoe=attr(scaled, "scaled:center") #saving for classification
std.shoe=attr(scaled, "scaled:scale")  #saving for classification...later

Sigma_=cor(scaled)

myeigen=eigen(Sigma_)
cumsum(myeigen$values) / sum(myeigen$values)



scaling=diag(myeigen$values[1:5]^(-1/2)) / (sqrt(nrow(scaled)-1))
eigenshoes=scaled%*%myeigen$vectors[,1:5]%*%scaling
par(mfrow=c(2,3))
imageShow(array(eigenshoes[,1], c(60,125,3)))



# Generate Principal Components
height=1200
width=2500
scale=20
newdata=im
dim(newdata)=c(length(shoes_files),height*width*3/scale^2)
mypca=princomp(t(as.matrix(newdata)), scores=TRUE, cor=TRUE)

mypca2=t(mypca$scores)
dim(mypca2)=c(length(shoes_files),height/scale,width/scale,3)
par(mfrow=c(5,5))
par(mai=c(.001,.001,.001,.001))
for (i in 1:length(shoes_files)){#plot the first 25 Eigenshoes only
  plotJPEG(writeJPEG(mypca2[i,,,], bg="white"))  #complete without reduction
}



a=round(mypca$sdev[1:length(shoes_files)]^2/ sum(mypca$sdev^2),3)
cumsum(a)



x = t(t(eigenshoes)%*%scaled)
x
