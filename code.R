library(reticulate)
library(stringr)
library(pbapply)
library(EBImage)
library(XML)

posurl<-"http://utdallas.edu/~sxp175331/dataset_assignment3/newfoundland_image/"
posdoc<-htmlParse(posurl)
poslinks <- xpathSApply(posdoc, "//a/@href")
free(posdoc)
posData<-poslinks[grepl("*.JPEG", poslinks)]
pos<- paste(posurl, posData, sep = "")

pos_image<-list()
original_pos_image<-list()
for(i in 1:200)
{
  original_pos_image[[i]]<-readImage(pos[i])
  pos_image[[i]]<-readImage(pos[i])
  pos_image[[i]]<-channel(pos_image[[i]],"gray")
}
negurl<-"http://utdallas.edu/~sxp175331/dataset_assignment3/dalmatian_image/"
negdoc<-htmlParse(negurl)
neglinks <- xpathSApply(negdoc, "//a/@href")
free(negdoc)
negData<-neglinks[grepl("*.JPEG", neglinks)]
neg<- paste(negurl, negData, sep = "")

neg_image<-list()
original_neg_image<-list()
for(i in 1:200)
{
  original_neg_image[[i]]<-readImage(neg[i])
  neg_image[[i]]<-readImage(neg[i])
  neg_image[[i]]<-channel(neg_image[[i]],"gray")
}

for(i in 1:200) {pos_image[[i]] <- resize(pos_image[[i]],28,28)}
for(i in 1:200) {neg_image[[i]] <- resize(neg_image[[i]],28,28)}


pos_label=list(rep(1, 200))
neg_label=list(rep(0, 200))

all_original_imagelist<-append(original_pos_image,original_neg_image)
all_imagelist<-append(pos_image,neg_image)
all_labellist<-append(pos_label,neg_label)
all_labellist<-unlist(all_labellist)

require(caTools)
set.seed(25) 
sample = sample.split(all_imagelist, SplitRatio = 0.80)

train_data = subset(all_imagelist, sample == TRUE)
test_data  = subset(all_imagelist, sample == FALSE)
train_labels = subset(all_labellist, sample == TRUE)
test_labels  = subset(all_labellist, sample == FALSE)
test_original_images  = subset(all_original_imagelist, sample == FALSE)


reshaped_trainImage <- array_reshape(train_data, c(320, 28, 28, 1))
reshaped_testImage <- array_reshape(test_data, c(80, 28, 28, 1))

#display(all_original_imagelist[[1]])


y_train <- to_categorical(train_labels)
y_test <- to_categorical(test_labels)

library(keras)

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(4,4 ), activation = "tanh",
                data_format="channels_last", input_shape = c(28, 28, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(4, 4), activation = "tanh") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(4, 4), activation = "tanh")

summary(model)
## ------------------------------------------------------------------------
model <- model %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 2, activation = "softmax")

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(
  reshaped_trainImage, y_train, 
  epochs = 10, batch_size=64
)

results <- model %>% evaluate(reshaped_testImage, y_test)
results


predicted_labels<-model %>% predict_classes(reshaped_testImage)

predicted_labels
test_labels



par(mfrow=c(5,5))
for(i in 1:25)
{
  plot(test_original_images[[i]])
  
  
}


