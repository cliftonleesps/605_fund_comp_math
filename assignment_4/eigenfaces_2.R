# https://rpubs.com/dherrero12/543854


library(jpeg)
library(OpenImageR)

height=1200; 
width=2500;
scale=20


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
data=matrix(0, length(shoes_files), prod(dim(im))) 
for (i in 1:length(shoes_files)) {
  r=as.vector(im[i,,,1]); 
  g=as.vector(im[i,,,2]);
  b=as.vector(im[i,,,3])
  data[i,] <- t(c(r, g, b))
}


# faces <- data.frame(labels = factor(rep(1:80, each = 6)),
#                     x = data)
faces <- data.frame(t(data))

scaled <- scale(faces, center = TRUE, scale = TRUE)
mean.face <- attr(scaled, "scaled:center")
std.face  <- attr(scaled, "scaled:scale")

#Sigma_ <- scaled%*%t(scaled) / (nrow(scaled)-1)
Sigma_=cor(scaled)


eig          <- eigen(Sigma_)
eigenvalues  <- eig$values
eigenvectors <- eig$vectors


prop.var <- eigenvalues / sum(eigenvalues)
cum.var  <- cumsum(eigenvalues) / sum(eigenvalues)
thres    <- min(which(cum.var > .95))


scaling    <- diag(eigenvalues[1:thres]^(-1/2)) / (sqrt(nrow(scaled)-1))
eigenfaces <- scaled%*%eigenvectors[,1:thres]%*%scaling


eigenface <- array(eigenfaces[,2], dim(temp))
imageShow(eigenface)
