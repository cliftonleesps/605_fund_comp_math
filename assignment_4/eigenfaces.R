# Data 605 - Assignment 4

library(jpeg)
library(OpenImageR)


image <- readJPEG("jpg/RC_2500x1200_2014_us_53446.jpg")
imageShow(image)


shoes <- list.files(path="jpg", pattern="*")

