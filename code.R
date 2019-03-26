library(keras)
library(EBImage)
library(reticulate)

#Read Image Sushrut

pic1 <- c('n02111277_13.jpeg','n02111277_19.jpeg','n02111277_25.jpeg','n02111277_32.jpeg'
          ,'n02111277_36.jpeg','n02111277_39.jpeg','n02111277_53.jpeg','n02111277_56.jpeg'
          ,'n02111277_77.jpeg','n02111277_80.jpeg','n02111277_94.jpeg','n02111277_101.jpeg')

train <- list()
for (i in 1:12){train[[i]] <- readImage(pic1[i])}

pic2 <- c('n02111277_109.jpeg','n02111277_110.jpeg','n02111277_115.jpeg','n02111277_116.jpeg'
          ,'n02111277_120.jpeg','n02111277_121.jpeg','n02111277_129.jpeg','n02111277_131.jpeg'
          ,'n02111277_137.jpeg','n02111277_138.jpeg','n02111277_142.jpeg','n02111277_143.jpeg')

test <- list()
for (i in 1:12){test[[i]] <- readImage(pic2[i])}

#display(train[[1]])
summary(train[[1]])
str(train) #we can see every images are of different sizes
#Let's resize all the images into same size

#To print all the images together

par(mfrow =c(3,5))
for(i in 1:12) plot(train[[i]])
par(mfrow=c(1,1))

#Resize all image to 100*100

for(i in 1:12) {train[[i]] <- resize(train[[i]],100,100)}
for(i in 1:12) {test[[i]] <- resize(test[[i]],100,100)}
str(train)

for(i in 1:12) {train[[i]] <- array_reshape(train[[i]],c(100,100,3))}
