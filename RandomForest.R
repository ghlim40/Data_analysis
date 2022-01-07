#install.packages("randomForest")
#install.packages('ggplot2')
#install.packages('Epi')
#install.packages('gbm')
#아래 라이브러리가 존자 안할경우 모두 인스톨 해야함
library(Epi)
library("car")
library(readxl)
library(ROCR)
library(MASS)
library(randomForest)
library('ggplot2')


getwd()
setwd('C:/Users/USER/Desktop/공공빅데이터/공공와이파이 분석/분석 리소스/격자/Test')
df<-read_excel('도로존재격자_버퍼없음.xlsx')
df$wifi[df$wifi==0]='0'
df$wifi[df$wifi!=0]='1'
df$wifi<-as.integer(df$wifi)
df



#데이터 프레임 분리
train_idx <- sample(1:nrow(df), size=0.6*nrow(df), replace=F)
test_idx <- (-train_idx)
train_ind <- sample(seq_len(nrow(df)), size = floor(0.6*nrow(df)))
train <- df[train_ind, ]
test <- df[-train_ind, ]
attach(train)


#랜덤 포레스트 구동
train$wifi<-as.factor(train$wifi)
rf_m <- randomForest(train$wifi~TM_MAX+Bus+Metro+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX
                     , data = train)
print(rf_m)

rf_info <- randomForest(train$wifi~TM_MAX+Bus+Metro+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX,
data = train,importance = TRUE)

importance(rf_info)
varImpPlot(rf_info)
pred.rf<-predict(rf_m,test[1:10])

r<-ROC(test=pred.rf, stat=test$wifi, plot="ROC", AUC=T, main="randomforest")
r$AUC
