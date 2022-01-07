#install.packages('gbm')
#아래 라이브러리가 존자 안할경우 모두 인스톨 해야함
library(Epi)
library(readxl)
library(e1071)
library(caret)
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
train <- df[train_ind, ]
test <- df[-train_ind, ]
attach(train)

train$wifi<-as.factor(train$wifi)
test$wifi<-as.factor(test$wifi)

df

svm.model<-svm(wifi~TM_MAX+Bus+Metro+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX
               ,data=train)


tr_result<-predict(svm.model,train)
table(tr_result,train$wifi)
te_result<-predict(svm.model,test)
table(te_result,test$wifi)


confusionMatrix(as.factor(te_result),as.factor(test$wifi))

r<-ROC(test=te_result, stat=test$wifi, plot="ROC", AUC=T, main="SVM")
r$AUC
r


