#install.packages('gbm')
#아래 라이브러리가 존자 안할경우 모두 인스톨 해야함
library(Epi)
library("car")
library(readxl)
library(ROCR)
library(MASS)
library(randomForest)
library('ggplot2')
library(gbm)
library(caret)

getwd()
#데이터셋을 가져올 경로설정, 인코딩문제로 글자 깨짐 각자 맞게 수정할것
setwd('C:/Users/USER/Desktop/����������/������������ �м�/�м� ���ҽ�/����')
df<-read_excel('���������ͼ�.xlsx')
df$wifi[df$wifi==0]='0'
df$wifi[df$wifi!=0]='1'
df$wifi<-as.integer(df$wifi)
df


train_idx <- sample(1:nrow(df), size=0.7*nrow(df), replace=F)
test_idx <- (-train_idx)
train_ind <- sample(seq_len(nrow(df)), size = floor(0.7*nrow(df)))
train_result <- df[train_ind, ]
test_result <- df[-train_ind, ]
attach(train)

train$wifi<-as.factor(train$wifi)
test$wifi<-as.factor(test$wifi)

gbm.model <- gbm(wifi~TM_MAX+Bus+Metro+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX,
                data = train, distribution = "laplace", interaction.depth = 4,n.trees = 100,shrinkage =0.01)
tr_result<-predict(gbm.model,train)
table(tr_result,train$wifi)
te_result<-predict(gbm.model,test)
table(te_result,test$wifi)

r<-ROC(test=te_result, stat=test$wifi, plot="ROC", AUC=T, main="GBM")
r$AUC
r

