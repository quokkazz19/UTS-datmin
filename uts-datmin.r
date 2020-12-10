library(caret)
library(neuralnet)

#Read Data
data <- read.csv("../dataset/tae.csv", sep = ";", header=T)
head(data)

#nmelihat struktur variabel
str(data)

#Ubah tipe variabel menjadi tipe faktor
data$native <- as.factor(data$native)
data$native <- as.numeric(data$native)

data$instructor <- as.factor(data$instructor)
data$instructor<- as.numeric(data$instructor)

data$course <- as.factor(data$course)
data$course <- as.numeric(data$course)

data$semester <- as.factor(data$semester)
data$semester <- as.numeric(data$semester)

data$skor <- as.factor(data$skor)
data$skor <- as.numeric(data$skor)

data$size <- as.numeric(data$size)


#Split Data training & tes
set.seed(123)
sampel <- sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
train <- data[sampel==1, ]
testdat <- data[sampel==2, ]

#membuat model
model<-neuralnet(skor~., train, hidden=5, linear.output= T)
plot(model)

#prediksi dengan data tes
prediksi <- compute(model, testdat[1:5])
str(prediksi)
View(prediksi$net.result)
pred <- ifelse(prediksi$net.result>0.5, 1, 0)
head(pred)

#confusion matrix untuk evaluasi model
confusionMatrix(table(pred, testdat$skor))



