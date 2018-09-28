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

#########################################################
#########################################################
# SVM 
library(e1071)
x_train<-cbind.data.frame(train_mfcc,train_signal)
svm_model  =  svm(x = x_train, y = y_train, type = "C-classification", kernel = "linear", cost = 10)

predicted= predict(svm_model,xd_train)

conf_matrix_train <- table(y_train, predicted)
print(conf_matrix_train)
accuracy <- sum(diag(conf_matrix_train))/sum(conf_matrix_train)
print(accuracy)












#######################################################################
#######################################################################



















































































































##########################################################################
pre = preProcess(x = data_train[,-519], method = c("center","scale"))
train_std = predict(pre, data_train[,-519])
val_std<-predict(pre, data_val[,-519])
test_std = predict(pre, data_Test[,-519])

train_ready<-as.data.frame(cbind(train_std,data_train$genre))
val_ready<- cbind(val_std, data_val$genre)
test_ready<-cbind(test_std,data_Test$genre)

colnames(train_ready)[519]<-"genre"
colnames(val_ready)[519]<-"genre"
colnames(test_ready)[519]<-"genre"

summary(train_ready$chroma_cens_kurtosis_1)
########################################################################
data_mfcc<-data_no_id[,grep("^mfcc", colnames(data_no_id))]
dim(data_mfcc)

data_power<-data_no_id[,grep("^spectral", colnames(data_no_id))]
dim(data_power)

data_chrm_cens<-data_no_id[,grep("^chroma_cens", colnames(data_no_id))]
dim(data_chrm_cens)

data_chrm_cqt<-data_no_id[,grep("^chroma_cqt", colnames(data_no_id))]
dim(data_chrm_cqt)

data_chrm_stft<-data_no_id[,grep("^chroma_stft", colnames(data_no_id))]
dim(data_chrm_stft)

data_zcr<-data_no_id[,grep("^zcr", colnames(data_no_id))]
dim(data_zcr)

data_rmse<-data_no_id[,grep("^rmse", colnames(data_no_id))]
dim(data_rmse)
names(data_no_id)

data_tones<-data_no_id[,grep("^tonne", colnames(data_no_id))]
dim(data_tones)


#######################################################################
#######################################################################



#######################################################################
#######################################################################
names(data_chrm_cens)
ggplot(data = data_chrm_cens, aes(x = displ, y = hwy))+geom_smooth()












#######################################################################
#######################################################################

##############################################################################
# Checking for the presence of levels in all the sets all have 16 levels
levels(data_train$genre)
levels(data_val$genre)
levels(data_Test$genre)


##############################################################################
library(cluster)

model1 <- kmeans(data_train[,-519], 16)

data_with_clusters<-cbind(data_train,model1$cluster)
colnames(data_for_model)
colnames(data_for_model)[520] <- "cluster"
data<- data_for_model
data$cluster<-as.factor(data$cluster)
str(data$cluster)
dev.off()
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(datafor_clust,col=model1$cluster)


#################################################################################
x_train<-data_train[,-519]
y_train<-data_train[,519]

x_val<-data_val[,-519]
y_val<-data_val[,519]

x_test<-data_Test[,-519]
y_test<-data_Test[,519]
#################################################################################
# RANDOM Forest

library(randomForest)

fit <- randomForest(genre ~ ., data_train,ntree=500)

summary(fit)
varImp(fit)
varImpPlot(fit,type=2)
###############################################################################
install.packages(xgboost)
library(xgboost)
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
model<- train(y_train ~ ., data = x_train, method = "xgbTree", trControl = TrainControl,verbose = FALSE)

predicted <- predict(model, x_val)

################################################################################
library(e1071)

# Fitting model
fit <-naiveBayes(y_train ~ ., data = data_train)
summary(fit)
pred_val = predict(fit, x_val, type="response")

View(prop.table(table(y_val, pred_val),1))

#Predict Output 
predicted= predict(fit,x_val)



################################################################################
# splitting data based on types of features











#############################################################
# trial in changing data

train_mfcc<-data[,grep("^mfcc", colnames(data_train))]
dim(dat_mfcc)

dat_power<-data[,grep("^spectral", colnames(data))]
dim(dat_power)

dat_chrm_cens<-data[,grep("^chroma_cens", colnames(data))]
dim(dat_chrm_cens)

dat_chrm_cqt<-data[,grep("^chroma_cqt", colnames(data))]
dim(dat_chrm_cqt)

dat_chrm_stft<-data[,grep("^chroma_stft", colnames(data))]
dim(dat_chrm_stft)


##############################################################################
#View(labels)

#x_name<- "tid"

#y_name<- "genre"

labels<-as.data.frame(cbind(tid,genre))


View(head(labels))

dim(labels)

names(data)

subset(labels, labels$tid== music$tid ) 
data<- as.data.frame(merge(labels, music, by="tid"))

dim(data)
#17000 520

str(data$genre)

sum(is.na(data$genre))

m<-cor(data[,-c(1,2)])
corrplot(m, method = "number")

################################################
#data split

set.seed(3456)

train_rows<-createDataPartition(data$genre, times = 1, p = 0.8, list = F)

train_data<-data[train_rows,]

valid_data<-data[-train_rows,]

x.train=train_data[,-c(1,2)]

y.train=train_data$genre

x.test = valid_data[,-c(1,2)]

y.test = valid_data$genre


library(e1071)


# Building the model on train data
model  =  svm(x = x.train, y = y.train, type = "C-classification", kernel = "linear", cost = 10)
summary(model)

