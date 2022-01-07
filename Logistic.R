#install.packages('gbm')
#아래 라이브러리가 존자 안할경우 모두 인스톨 해야함
library(Epi)
library("car")
library(readxl)
library(ROCR)
library(MASS)

getwd()
#데이터셋을 가져올 경로설정, 인코딩문제로 글자 깨짐 각자 맞게 수정할것
setwd('C:/Users/USER/Desktop/����������/������������ �м�/�м� ���ҽ�/����/Test')
df_buffer_off<-read_excel('���������ͼ�.xlsx')



#로지스틱 회귀분석 실시
glm_buffer_off <- glm(wifi~TM_MAX+Bus+Metro+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX,data=df_buffer_off,family = binomial)
summary(glm_buffer_off)
coef(glm_buffer_off)

#유효한 항목들만 다시 사용하여 2차 로지스틱 회귀분석 실시
glm_buffer_off <- glm(wifi~TM_MAX+Bus+Metro+HOS_MAX+ADM_MAX,data=df_buffer_off,family = binomial)
summary(glm_buffer_off)
off=predict(glm_buffer_off, newdata=df_buffer_off, type="response")
pr_off <- prediction(off, df_buffer_off$wifi)
prf_off <- performance(pr_off, measure = "tpr", x.measure = "fpr")
plot(prf_off, main='ROC of Test Data')
vif(glm_buffer_off)
  neural_ROC <- ROC(form=wifi~Bus+Metro+TM_MAX+Bank_MAX+HOS_MAX+SC_MAX+PK_MAX+ADM_MAX,data=df_buffer_off, plot="ROC",main="Logistic")

