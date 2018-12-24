Music Genre Wise Classification.

Given the increasing content of music in the web by artists and also on social platforms by enthusiasts. There is a requirement to tag the songs appropriately for customisation by reccomendation engines. So the following is an attempt to classify music genre wise. Data has been from Free Music Archive. The features have been extracted for each song using librosa package. 

The features correspond to Pitch Class intensities (Chromagram), Centroid frequency    
This is an attempt to classify Music based on genre
The Code is to classify Music Songs Genre Wise

The features used have been extracted from

This following code is written in R language in RStudio

The 




##############################

rm(list=ls(all=T))
require(DMwR)
require(caret)
#require(corrplot)
setwd("C:\\Users\\Lokesh Bharat\\Desktop\\lab resources\\internship")
music<-read.csv("musicfeatures.csv", skip = 4, stringsAsFactors = F)

############################################################################
#Changing COlumn Names
hl=readLines("musicfeatures.csv", 4)

hl=strsplit(hl,',')

colnames(music)=sub('_$','',paste(hl[[1]],hl[[2]],hl[[3]],hl[[4]],sep="_"))

colnames(music)
#Changing colnames for tracks.csv
track_label<-read.csv("tracks.csv",skip = 3, stringsAsFactors = F)

h2<-readLines("tracks.csv", 3)

h2=strsplit(h2,',')

colnames(track_label)=sub('_$','',paste(h2[[2]],h2[[3]],sep=""))
colnames(track_label)
#########################################################################
#renaming some colnames for convenience sake
colnames(music)[which(names(music) == "feature_statistics_number_track_id")] <- "tid"
colnames(music)[which(names(music) == "zcr_std_1_track_id")] <- "zcr_std_1"
colnames(track_label)[which(names(track_label) == "track_id")] <- "tid"

#music$tid<-as.character(music$tid)

#track_label$tid<-as.character(track_label$tid)

names(music)

names(track_label)

#track_label$subset<- as.ordered(track_label$subset)
dim(track_label)

###################################################
#track_label<-lapply(track_label, function(x) {x[x == ""] <- NA})
#sum(is.na(track_label$genre_top))
#summary(track_label$genre_top)
#View(track_label)
#################################################

genre<-as.data.frame(track_label[track_label$tid %in% music$tid, c("tid","genre_top")])
head(genre)

#filter(track_label, track_label$tid %in% music$tid))
head(music[,1])

music<-cbind(music,genre$genre_top)
# View(head(music[,c(1,520)]))
# View(tail(music[,c(1,520)]))
names(music)
colnames(music)[which(names(music) == "genre$genre_top")] <- "genre"
levels(music$genre)

str(music)

summary(music$genre)

dim(music)
View(head(music))
######################################
#music = lapply(music, function(x) {x[x == ""] <- NA})
sum(is.na(music))
# 7770
names(tail(music))
d_x<-music[,-c(1,520)]


###########################################
#KNN Imputation
data_imputed<-knnImputation(d_x,k=10, meth = 'weighAvg')
sum(is.na(data_imputed))

data<-cbind.data.frame(data_imputed,music[,c(1,520)])
sum(is.na.data.frame(data))
names(data)
data_no_id<-data[,-519] # removing id
names(data_no_id)
str(data_no_id$genre)
#########################################################################
set.seed(1234)
Train = createDataPartition(data_no_id$genre, p = 0.8, list = FALSE)
data_Train=data_no_id[Train,]
data_Test=data_no_id[-Train,]

train = createDataPartition(data_Train$genre, p = 7/8, list = FALSE)
data_train=data_Train[train,]
data_val=data_Train[-train,]


names(data_train)
#######################################################################
x_train<-data_train[,-519]
y_train<-data_train[,519]

x_val<-data_val[,-519]
y_val<-data_val[,519]

x_test<-data_Test[,-519]
y_test<-data_Test[,519]

########################################################################
train_mfcc<-data_train[,grep("^mfcc", colnames(data_train))]
dim(train_mfcc)

train_power<-data_train[,grep("^spectral", colnames(data_train))]
dim(train_power)

train_chrm_cens<-data_train[,grep("^chroma_cens", colnames(data_train))]
dim(train_chrm_cens)

train_chrm_cqt<-data_train[,grep("^chroma_cqt", colnames(data_train))]
dim(train_chrm_cqt)

train_chrm_stft<-data_train[,grep("^chroma_stft", colnames(data_train))]
dim(train_chrm_stft)

train_chrm<-cbind.data.frame(train_chrm_stft,train_chrm_cqt,train_chrm_cens)

train_zcr<-data_train[,grep("^zcr", colnames(data_train))]
dim(train_zcr)

train_rmse<-data_train[,grep("^rmse", colnames(data_train))]
dim(train_rmse)

train_tones<-data_train[,grep("^tonne", colnames(data_train))]
dim(train_tones)

train_signal<-cbind.data.frame(train_rmse,train_zcr, train_power)
#######################################################################
val_mfcc<-data_val[,grep("^mfcc", colnames(data_val))]
dim(val_mfcc)

val_power<-data_val[,grep("^spectral", colnames(data_val))]
dim(val_power)

val_chrm_cens<-data_val[,grep("^chroma_cens", colnames(data_val))]
dim(val_chrm_cens)

val_chrm_cqt<-data_val[,grep("^chroma_cqt", colnames(data_val))]
dim(val_chrm_cqt)

val_chrm_stft<-data_val[,grep("^chroma_stft", colnames(data_val))]
dim(val_chrm_stft)

val_chrm<-cbind.data.frame(val_chrm_stft,val_chrm_cqt,val_chrm_cens)


val_zcr<-data_val[,grep("^zcr", colnames(data_val))]
dim(val_zcr)

val_rmse<-data_val[,grep("^rmse", colnames(data_val))]
dim(val_rmse)

val_tones<-data_val[,grep("^tonne", colnames(data_val))]
dim(val_tones)

val_signal<-cbind.data.frame(val_rmse,val_power,val_zcr)

##############################################################
test_mfcc<-data_Test[,grep("^mfcc", colnames(data_Test))]
dim(test_mfcc)

test_power<-data_Test[,grep("^spectral", colnames(data_Test))]
dim(test_power)

test_chrm_cens<-data_Test[,grep("^chroma_cens", colnames(data_Test))]
dim(test_chrm_cens)

test_chrm_cqt<-data_Test[,grep("^chroma_cqt", colnames(data_Test))]
dim(test_chrm_cqt)

test_chrm_stft<-data_Test[,grep("^chroma_stft", colnames(data_Test))]
dim(test_chrm_stft)

test_chrm<-cbind.data.frame(test_chrm_stft,test_chrm_cqt,test_chrm_cens)
dim(test_chrm)

test_zcr<-data_Test[,grep("^zcr", colnames(data_Test))]
dim(test_zcr)

test_rmse<-data_Test[,grep("^rmse", colnames(data_Test))]
dim(test_rmse)

test_signal<-cbind.data.frame(test_rmse,test_power,test_zcr)

test_tones<-data_Test[,grep("^tonne", colnames(data_Test))]
dim(test_tones)

#######################################################################
######################################################################

# Models Using Different Data Sets
#####################################################################

library(e1071)

nb<-naiveBayes(x_train,y_train)
nb_train<-predict(nb,x_train)

conf_mat<-table(y_train,nb_train)
accuracy_train<-sum(diag(conf_mat))/sum(conf_mat)
print(accuracy_train)
# 24.15 %


####################################################################
# Using Voacl Part
nb_model <-naiveBayes(train_mfcc, y_train)
summary(nb_model)

predictnb_train= predict(nb_model,train_mfcc)

conf_nb_train <- table(y_train, predictnb_train)
print(conf_nb_train)

accuracy_nb_train <- sum(diag(conf_nb_train))/sum(conf_nb_train)
print(accuracy_nb_train)
# 41.03%

predictnb_val<-predict(nb_model,val_mfcc)

conf_nb_val <- table(y_val, predictnb_val)
print(conf_nb_val)
accuracy_nb_val <- sum(diag(conf_nb_val))/sum(conf_nb_val)
print(accuracy_nb_val)
# 42.25 %

predictnb_test<-predict(nb_model,test_mfcc)

conf_nb_test <- table(y_test, predictnb_test)
print(conf_nb_test)
accuracy_nb_test <- sum(diag(conf_nb_test))/sum(conf_nb_test)
print(accuracy_nb_test)
# 41.55%


#############################################################
# Using Signal Proccessing
nb_model <-naiveBayes(train_signal, y_train)
summary(nb_model)

predictnb_train= predict(nb_model,train_signal)

conf_nb_train <- table(y_train, predictnb_train)
print(conf_nb_train)

accuracy_nb_train <- sum(diag(conf_nb_train))/sum(conf_nb_train)
print(accuracy_nb_train)
# 38.32 %

predictnb_val<-predict(nb_model,val_signal)

conf_nb_val <- table(y_val, predictnb_val)
print(conf_nb_val)
accuracy_nb_val <- sum(diag(conf_nb_val))/sum(conf_nb_val)
print(accuracy_nb_val)
# 39.16 %

predictnb_test<-predict(nb_model,test_signal)

conf_nb_test <- table(y_test, predictnb_test)
print(conf_nb_test)
accuracy_nb_test <- sum(diag(conf_nb_test))/sum(conf_nb_test)
print(accuracy_nb_test)
#38.78 %
#############################################
# NB using CHROMAGRAM
nb_model <-naiveBayes(train_chrm, y_train)
summary(nb_model)

predictnb_train= predict(nb_model,train_chrm)

conf_nb_train <- table(y_train, predictnb_train)
print(conf_nb_train)

accuracy_nb_train <- sum(diag(conf_nb_train))/sum(conf_nb_train)
print(accuracy_nb_train)
# 11.87 %

predictnb_val<-predict(nb_model,val_chrm)

conf_nb_val <- table(y_val, predictnb_val)
print(conf_nb_val)
accuracy_nb_val <- sum(diag(conf_nb_val))/sum(conf_nb_val)
print(accuracy_nb_val)
# 12.78 %

predictnb_test<-predict(nb_model,test_chrm)

conf_nb_test <- table(y_test, predictnb_test)
print(conf_nb_test)
accuracy_nb_test <- sum(diag(conf_nb_test))/sum(conf_nb_test)
print(accuracy_nb_test)
#11.97 %
#####################################################################
# tones
nb_model <-naiveBayes(train_tones, y_train)
summary(nb_model)

predictnb_train= predict(nb_model,train_tones)

conf_nb_train <- table(y_train, predictnb_train)
print(conf_nb_train)

accuracy_nb_train <- sum(diag(conf_nb_train))/sum(conf_nb_train)
print(accuracy_nb_train)
# 30 %

predictnb_val<-predict(nb_model,val_tones)

conf_nb_val <- table(y_val, predictnb_val)
print(conf_nb_val)
accuracy_nb_val <- sum(diag(conf_nb_val))/sum(conf_nb_val)
print(accuracy_nb_val)
# 29.77 %

predictnb_test<-predict(nb_model,test_tones)

conf_nb_test <- table(y_test, predictnb_test)
print(conf_nb_test)
accuracy_nb_test <- sum(diag(conf_nb_test))/sum(conf_nb_test)
print(accuracy_nb_test)
# 30.03 %

###############################################################
###############################################################
x_train<-cbind.data.frame(train_mfcc,train_signal)
x_val<-cbind.data.frame(val_mfcc,val_signal)
x_test<-cbind.data.frame(test_mfcc,test_signal)


pre = preProcess(x = x_train, method = c("center","scale"))
train_std = predict(pre, x_train)
val_std<-predict(pre, x_val)
test_std = predict(pre, x_test)

train_ready<-as.data.frame(cbind(train_std,y_train))
val_ready<- cbind(val_std, y_val)
test_ready<-cbind(test_std,y_test)
names(train_ready)

colnames(train_ready)[225]<-"genre"
colnames(val_ready)[225]<-"genre"
colnames(test_ready)[225]<-"genre"

table(train_ready$genre)

#########################################################
# SVM 
library(e1071)

obj <- tune(svm, genre~., data = train_ready, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix")
)

summary(obj)
obj$best.performance
#71.8% acccuracy

obj$best.model
svm_model<-obj$best.model
predicted= predict(svm_model,train_std)

conf_matrix_train <- table(y_train, predicted)
print(conf_matrix_train)
accuracy <- sum(diag(conf_matrix_train))/sum(conf_matrix_train)
print(accuracy)
# Accuracy = 99.93%


predict_val= predict(svm_model,val_std)

conf_matrix_val <- table(y_val, predict_val)
print(conf_matrix_val)
accuracy_val <- sum(diag(conf_matrix_val))/sum(conf_matrix_val)
print(accuracy_val)
#accuracy=28.81%%

predict_test= predict(svm_model,test_std)

conf_matrix_test <- table(y_test, predict_test)
print(conf_matrix_test)
accuracy <- sum(diag(conf_matrix_test))/sum(conf_matrix_test)
print(accuracy)
#28.72%

##############################################################

library(randomForest)

rf_fit <- randomForest(genre ~ ., train_ready,ntree=100)

summary(rf_fit)
varImp(fit)
varImpPlot(rf_fit,type=2)
predict_rf_train<-predict(rf_fit,train_std)

conf_matrix_rf_train <- table(y_train, predict_rf_train)
print(conf_matrix_rf_train)
accuracy <- sum(diag(conf_matrix_rf_train))/sum(conf_matrix_rf_train)
print(accuracy)

predict_rf_val= predict(rf_fit,val_std)

conf_matrix_rf_val <- table(y_val, predict_rf_val)
print(conf_matrix_rf_val)
accuracy_val <- sum(diag(conf_matrix_rf_val))/sum(conf_matrix_rf_val)
print(accuracy_val)
# accuRACY=61.88%

predict_rf_test= predict(rf_fit,test_std)

conf_matrix_rf_test <- table(y_test, predict_rf_test)
print(conf_matrix_rf_test)
accuracy <- sum(diag(conf_matrix_rf_test))/sum(conf_matrix_rf_test)
print(accuracy)
#accuracy = 61.39%


#######################################################################


