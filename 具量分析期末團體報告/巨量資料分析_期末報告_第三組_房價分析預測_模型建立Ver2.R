#####Wayne Xiao 2017/06/18  Report Of BigData       Ver3
library(car)
library(boot)
library(glmnet)
library(leaps)
library(bestglm)
require(boot)
require(car)
require(glmnet)



#特徵資料01  是將特徵資料把時間拿掉
data_ver1<- read.csv("C:/Users/user/Desktop/特徵資料01.csv", header = TRUE)
#敘述統計
mean(data_ver1$每平方公尺價格)   #48873.51
var(data_ver1$每平方公尺價格)    #638045303
sd(data_ver1$每平方公尺價格)   #25259.56
mean(data_ver1$屋齡)
summary(data_ver1)

#建立敘述統計表格(作圖)
colnames(data_ver1)[2:37]

data_age_mean<-NULL
data_sum_11<-NULL
data_sum_totain<-NULL
data_HP<-NULL
data_sum<-NULL
data_mean<-NULL
data_sd<-NULL
for (i in 2:37) {
  data_sum[i-1]<-sum(data_ver1[,i])
  data_mean[i-1]<-mean(data_ver1$每平方公尺價格[ which(data_ver1$住=='1') & (data_ver1[,i]=='1') ])
  data_sd[i-1]<-sd(data_ver1$每平方公尺價格[ which(data_ver1$住=='1') & (data_ver1[,i]=='1') ])
  data_HP[i-1]<-sum(data_ver1[,i][which(data_ver1$每平方公尺價格>=48873+25259)])
  data_sum_totain[i-1]<-sum(data_ver1[,i][data_ver1$透天厝=='1'])
  data_sum_11[i-1]<-sum(data_ver1[,i][data_ver1$住宅大樓.11層含以上有電梯.=='1'])
  data_age_mean[i-1]<-mean(data_ver1$屋齡[which(data_ver1[,i]=='1')])
}


THE_TABLE<-rbind(data_sum,data_mean,data_sd,data_HP,data_sum_totain,data_sum_11,data_age_mean)
colnames(THE_TABLE)<-colnames(data_ver1)[2:37]



#############################################################


#將資料刪除只有一筆的欄位
data_ver2<- read.csv("C:/Users/user/Desktop/特徵資料02.csv", header = TRUE)
data_ver3<-data_ver2[which(data_ver2$住=='1'),]
data_ver4<-data_ver3[,-c(29:38)]


mean(data_ver4$每平方公尺價格)   #48873.51
var(data_ver4$每平方公尺價格)  #638045303
sd(data_ver4$每平方公尺價格) 

#找出問題值移除
which(data_ver4$每平方公尺價==0)
data_ver4<-data_ver4[-which(data_ver4$每平方公尺價==0),]


#資料分割
set.seed(999)
test.num <- sample(dim(data_ver4)[1], round(0.2*(dim(data_ver4)[1])))
train<- data_ver4[-test.num, ]
test<- data_ver4[ test.num, ]

# original full model #
fit <- glm(log(train$每平方公尺價格)~.  , data = train, family = "gaussian")
summary(fit)
#MSE
sqrt(mean((exp(predict(fit, newdata = test, type = "response")) - test$每平方公尺價格)^2))  #15567.61
AIC(fit)   #22643.07

#殘差分析
par(mfrow=c(2,2))
plot(fit)


#best subset#
nullModel<-lm(log(train$每平方公尺價格)~1,data= train)
fullModel<-lm(log(train$每平方公尺價格)~.,data= train)
houseStep<-step(nullModel,scope = list(lower =nullModel ,upper = fullModel) , direction = "both")
summary(houseStep)
sqrt(mean(exp(predict(houseStep, newdata = test, type = "response")) - test$每平方公尺價格)^2)  #1898.247
AIC(houseStep)   #22639.4


# Lasso #
xmat <- model.matrix(log(train$每平方公尺價格) ~., data=train)
fit.lasso <- cv.glmnet(xmat, log(train$每平方公尺價格) , alpha=1, family="gaussian")
sqrt(mean(exp(predict(fit.lasso,newx=xmat,s=best.lambda,type="response")) - test$每平方公尺價格)^2)   #2025.738


#找出最小懲罰項之LAMBDA
best.lambda <- fit.lasso$lambda.min
best.lambda
plot(fit.lasso)
coef(fit.lasso,s="lambda.1se")

#不同LAMDBA下表現#
plot(fit.lasso$glmnet.fit, xvar = "lambda")
abline(v= log(c(fit.lasso$lambda.min,fit.lasso$lambda.1se)), lty = 2)

#Lasso  係數圖 #
theCoef<- as.matrix(coef(fit.lasso, s="lambda.1se"))
coefDF<-data.frame(Value=theCoef, Coefficient =rownames(theCoef))
coefDF <- coefDF[nonzeroCoef(coef(fit.lasso,s="lambda.1se")),]
require(ggplot2)
ggplot(coefDF , aes(x= X1 ,y= reorder(Coefficient , X1))) +
  geom_vline(xintercept = 0 ,color="grey" , linetype = 2)+
  geom_point(color = "blue") + labs(x="Value" , y = "Coefficient" , title = "Coefficient Point")


#模型參數#
lasso.coef=predict(fit.lasso,type="coefficients",s=best.lambda)
lasso.coef
#找出LASSO中顯著參數#
NAME=rownames(lasso.coef)[which(lasso.coef!=0)]


#Distion Tree#

require(rpart)
require(rpart.plot)
DT<-rpart(log(train$每平方公尺價格) ~ . ,data= train)
rpart.plot(DT)

#Distion Tree MSE #
sqrt(mean(exp((predict(DT, newdata = test, type = "vector"))) - test$每平方公尺價格)^2)  #2550.851




# 隨機森林 #
require( useful)
require(randomForest)
fit.rf <- randomForest(log(train$每平方公尺價格)~.,
                       data = train, mtry = 4,
                       importance = TRUE, ntree = 500)
summary(fit.rf)
#建立重要指標圖
varImpPlot(fit.rf,type = 2, col = "blue", pch = 16)
prd <- predict(fit.rf, newdata = test)
#隨機森林MSE#
sqrt(mean((exp(prd) - test$每平方公尺價格)^2))  #13672.74







############################以下複雜#################################































require(boot)


####以下為cross validation

#FULL MD
housePriceMD1<- lm(log(data_ver4$每平方公尺價格)~  data_ver4$三民區+ data_ver4$大社區 + data_ver4$大寮區+
                     data_ver4$大樹區+ data_ver4$小港區+data_ver4$仁武區+data_ver4$內門區+
                     data_ver4$六龜區+data_ver4$左營區+data_ver4$永安區+
                     data_ver4$甲仙區+data_ver4$杉林區+data_ver4$岡山區+
                     data_ver4$林園區+data_ver4$阿蓮區+data_ver4$前金區+data_ver4$前鎮區+
                     data_ver4$美濃區+data_ver4$苓雅區+data_ver4$茄萣區+data_ver4$桃源區+
                     data_ver4$梓官區+data_ver4$鳥松區+data_ver4$湖內區+data_ver4$新興區+
                     data_ver4$楠梓區+data_ver4$路竹區+data_ver4$鼓山區+data_ver4$旗山區+
                     data_ver4$旗津區+data_ver4$鳳山區+data_ver4$橋頭區+data_ver4$燕巢區+
                     data_ver4$彌陀區+
                     data_ver4$建物現況格局.房+data_ver4$建物現況格局.廳+data_ver4$建物現況格局.衛+data_ver4$有無管理組織+
                     data_ver4$公寓.5樓含以下無電梯.+ data_ver4$住宅大樓.11層含以上有電梯.+
                     data_ver4$套房.1房1廳1衛.+ data_ver4$透天厝+ data_ver4$屋齡 ,data= data_ver4)



#LASSO MD
housePriceMD2<-glm(data_ver1$每平方公尺價格~ data_ver1$鼓山區 + data_ver1$透天厝 +data_ver1$苓雅區
                   +data_ver1$左營區 + data_ver1$前鎮區 +data_ver1$前金區 +data_ver1$新興區 +data_ver1$三民區
                   +data_ver1$商 +data_ver1$屋齡 +data_ver1$小港區 +data_ver1$大樹區  +data_ver1$梓官區 
                   +data_ver1$岡山區 +data_ver1$美濃區 +data_ver1$特定農業區 +data_ver1$套房.1房1廳1衛. 
                   +data_ver1$一般農業區 +data_ver1$公寓.5樓含以下無電梯. +data_ver1$茄萣區 +data_ver1$大寮區
                   +data_ver1$旗山區 +data_ver1$鄉村區 +data_ver1$湖內區 +data_ver1$林園區
                   ,data= data_ver1,family = gaussian)
#DT MD
housePriceMD3<-glm(data_ver1$每平方公尺價格~ data_ver1$屋齡 +data_ver1$透天厝 +data_ver1$建物現況格局.衛
                   +data_ver1$鼓山區
                   ,data= data_ver1,family = gaussian)

#RF MD
housePriceMD4<-glm(data_ver1$每平方公尺價格~ data_ver1$屋齡 +data_ver1$透天厝 +data_ver1$建物現況格局.衛
                   +data_ver1$鼓山區 +data_ver1$公寓.5樓含以下無電梯. +data_ver1$建物現況格局.房
                   +data_ver1$商 +data_ver1$住宅大樓.11層含以上有電梯. +data_ver1$左營區 +data_ver1$苓雅區 
                   +data_ver1$三民區 +data_ver1$高樓層區間 + data_ver1$林園區 +data_ver1$大寮區 +data_ver1$有無管理組織
                   +data_ver1$新興區 +data_ver1$建物現況格局.廳 +data_ver1$住 +data_ver1$鳳山區 +data_ver1$鄉村區 +data_ver1$湖內區
                   +data_ver1$楠梓區 +data_ver1$前金區 +data_ver1$中樓層區間 +data_ver1$岡山區 +data_ver1$旗山區 +data_ver1$小港區
                   +data_ver1$套房.1房1廳1衛. +data_ver1$低樓層區間 +data_ver1$茄萣區
                   ,data= data_ver1,family = gaussian)
summary(houseCV1)

houseCV1<-cv.glm(data_ver4 ,housePriceMD1 ,K= 5 )
houseCV2<-cv.glm(data_ver1 ,housePriceMD2 ,K= 5 )
houseCV3<-cv.glm(data_ver1 ,housePriceMD3 ,K= 5 )
houseCV4<-cv.glm(data_ver1 ,housePriceMD4 ,K= 5 )


CV.Result<- as.data.frame(rbind(houseCV1$delta , houseCV2$delta ,houseCV3$delta ,houseCV4$delta))
names(CV.Result)<- c("Error" ,"Adj.Error")
CV.Result$Model <- sprintf("housePriceMD%s",1:4)
CV.Result


#CV.ANOVA <-anova(housePriceMD1 , housePriceMD2 ,housePriceMD3 ,housePriceMD4)
#CV.Result$ANOVA <-CV.ANOVA$`Resid. Dev`


CV.Result$BIC <-BIC(housePriceMD1 , housePriceMD2 ,housePriceMD3 ,housePriceMD4)$BIC
CV.Result$AIC <-AIC(housePriceMD1 , housePriceMD2 ,housePriceMD3 ,housePriceMD4)$AIC


require(reshape2)
CV_Melt<- melt(CV.Result , id.vars = "Model" ,variable.name ="Measure" , value.name ="Value")
CV_Melt

ggplot(CV_Melt , aes(x=Model , y= Value)) +
  geom_line(aes(group = Measure , color = Measure)) +
  facet_wrap(~Measure , scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90 ,vjust = .5)) +
  guides(color = FALSE)




####敘述統計
mean(data_ver1$每平方公尺價格)
summary(data_ver1)
DATA001<-data_ver1[which(data_ver1$每平方公尺價格>mean(data_ver1$每平方公尺價格)),]
DATA002<-data_ver1[order(data_ver1$每平方公尺價格 , decreasing = TRUE),]
DATA003<- DATA002[which(DATA002$住==1),]
summary(DATA003)
nrow(DATA003)
#住宅區建模
#FULL MD
#FULL MD LIVE
log(10)
housePriceMD_LIVE_FULL<- glm(DATA003$每平方公尺價格~
                              DATA003$三民區+DATA003$大社區+DATA003$大寮區+DATA003$大樹區
                             +DATA003$小港區+DATA003$仁武區+DATA003$左營區+DATA003$甲仙區
                             +DATA003$岡山區+DATA003$林園區+DATA003$阿蓮區+DATA003$前金區
                             +DATA003$前鎮區+DATA003$美濃區+DATA003$苓雅區+DATA003$茄萣區
                             +DATA003$梓官區+DATA003$鳥松區+DATA003$湖內區+DATA003$新興區
                             +DATA003$楠梓區+DATA003$路竹區+DATA003$鼓山區+DATA003$旗山區
                             +DATA003$旗津區+DATA003$橋頭區+DATA003$燕巢區+DATA003$彌陀區
                             +DATA003$鳳山區+DATA003$鳥松區+DATA003$高樓層區間
                             +DATA003$內門區+DATA003$六龜區+DATA003$永安區+DATA003$田寮區
                             +DATA003$杉林區+DATA003$那瑪夏區+DATA003$桃源區
                             +DATA003$建物現況格局.房+DATA003$建物現況格局.廳+DATA003$建物現況格局.衛
                             +DATA003$有無管理組織+DATA003$公寓.5樓含以下無電梯.+DATA003$住宅大樓.11層含以上有電梯.
                             +DATA003$套房.1房1廳1衛.+DATA003$透天厝+DATA003$屋齡+DATA003$中樓層區間
                             +DATA003$低樓層區間  ,data= DATA003,family = gaussian)
summary(housePriceMD_LIVE_FULL)
#FULL MD LIVE 顯著變數
DATA003_sign_FULL<-DATA003[,c(1,2,3,4,5,6,7,10,13,16,17,18,19,20,21,22,23,25,26,27,28,29,30,31,32,33,35,36,37,48,49,50,51,52,53,54,55,56,57,58)]
housePriceMD_LIVE_sign_FULL<- glm(DATA003_sign_FULL$每平方公尺價格~
                                    DATA003_sign_FULL$三民區+DATA003_sign_FULL$大社區+DATA003_sign_FULL$大寮區+DATA003_sign_FULL$大樹區
                                  +DATA003_sign_FULL$小港區+DATA003_sign_FULL$仁武區+DATA003_sign_FULL$左營區+DATA003_sign_FULL$甲仙區
                                  +DATA003_sign_FULL$岡山區+DATA003_sign_FULL$林園區+DATA003_sign_FULL$阿蓮區+DATA003_sign_FULL$前金區
                                  +DATA003_sign_FULL$前鎮區+DATA003_sign_FULL$美濃區+DATA003_sign_FULL$苓雅區+DATA003_sign_FULL$茄萣區
                                  +DATA003_sign_FULL$梓官區+DATA003_sign_FULL$鳥松區+DATA003_sign_FULL$湖內區+DATA003_sign_FULL$新興區
                                  +DATA003_sign_FULL$楠梓區+DATA003_sign_FULL$路竹區+DATA003_sign_FULL$鼓山區+DATA003_sign_FULL$旗山區
                                  +DATA003_sign_FULL$旗津區+DATA003_sign_FULL$橋頭區+DATA003_sign_FULL$燕巢區+DATA003_sign_FULL$彌陀區
                                  +DATA003_sign_FULL$建物現況格局.房+DATA003_sign_FULL$建物現況格局.廳+DATA003_sign_FULL$建物現況格局.衛
                                  +DATA003_sign_FULL$有無管理組織+DATA003_sign_FULL$公寓.5樓含以下無電梯.+DATA003_sign_FULL$住宅大樓.11層含以上有電梯.
                                  +DATA003_sign_FULL$套房.1房1廳1衛.+DATA003_sign_FULL$透天厝+DATA003_sign_FULL$屋齡+DATA003_sign_FULL$中樓層區間
                                  +DATA003_sign_FULL$低樓層區間
                                    ,data= DATA003_sign_FULL,family = gaussian)
summary(housePriceMD_LIVE_sign_FULL)

#Lasso選模
## Lasso ##
require(useful)
xmat_LIVE <- build.x(DATA003$每平方公尺價格 ~ ., data=DATA003)
fit.lasso_LIVE <- cv.glmnet(xmat_LIVE, DATA003$每平方公尺價格 , family="gaussian" ,nfold =5 ,alpha=1)
best.lambda_LIVE <- fit.lasso_LIVE$lambda.min

best.lambda_LIVE
plot(fit.lasso_LIVE)
coef(fit.lasso_LIVE,s="lambda.1se")
plot(fit.lasso_LIVE$glmnet.fit, xvar = "lambda")
abline(v= log(c(fit.lasso_LIVE$lambda.min,fit.lasso_LIVE$lambda.1se)), lty = 2)

#######Lasso  係數圖  ######
theCoef_LIVE<- as.matrix(coef(fit.lasso_LIVE, s="lambda.1se"))
coefDF_LIVE<-data.frame(Value=theCoef_LIVE, Coefficient =rownames(theCoef_LIVE))

coefDF_LIVE <- coefDF_LIVE[nonzeroCoef(coef(fit.lasso_LIVE,s="lambda.1se")),]

require(ggplot2)
ggplot(coefDF_LIVE , aes(x= X1 ,y= reorder(Coefficient , X1))) +
  geom_vline(xintercept = 0 ,color="grey" , linetype = 2)+
  geom_point(color = "blue") + labs(x="Value" , y = "Coefficient" , title = "Coefficient Point")
################LASSO MD####
DATA003_LASSO<-DATA003[,c(1,2,3,4,5,10,13,16,17,18,19,20,21,22,23,25,26,27,30,31,32,36,37,50,52,53,54,55,56,59)]
housePriceMD_LIVE_LESSO<- glm(DATA003_LASSO$每平方公尺價格~DATA003_LASSO$三民區+DATA003_LASSO$大社區
                              +DATA003_LASSO$大寮區+DATA003_LASSO$大樹區+DATA003_LASSO$左營區+DATA003_LASSO$甲仙區
                              +DATA003_LASSO$岡山區+DATA003_LASSO$林園區+DATA003_LASSO$阿蓮區+DATA003_LASSO$前金區
                              +DATA003_LASSO$前鎮區+DATA003_LASSO$美濃區+DATA003_LASSO$苓雅區+DATA003_LASSO$茄萣區
                              +DATA003_LASSO$梓官區+DATA003_LASSO$鳥松區+DATA003_LASSO$湖內區+DATA003_LASSO$路竹區
                              +DATA003_LASSO$鼓山區+DATA003_LASSO$旗山區+DATA003_LASSO$燕巢區+DATA003_LASSO$彌陀區
                              +DATA003_LASSO$建物現況格局.衛+DATA003_LASSO$公寓.5樓含以下無電梯.+DATA003_LASSO$住宅大樓.11層含以上有電梯.
                              +DATA003_LASSO$套房.1房1廳1衛.+DATA003_LASSO$透天厝+DATA003_LASSO$屋齡+DATA003_LASSO$高樓層區間
                              ,data= DATA003_LASSO,family = gaussian)
summary(housePriceMD_LIVE_LESSO)


#######LIVE RF
#### 隨機森林
require( useful)
require(randomForest)
fit.rf_LIVE <- randomForest(DATA003$每平方公尺價格~.,
                       data = DATA003, mtry = 4,
                       importance = TRUE, ntree = 500)
summary(fit.rf_LIVE)

#
varImpPlot(fit.rf_LIVE,type = 2, col = "blue", pch = 16)
###RF選出變數建立模型
housePriceMD_LIVE_RF<- glm(DATA003$每平方公尺價格~DATA003$屋齡+DATA003$鼓山區+DATA003$建物現況格局.衛 
                           +DATA003$透天厝+DATA003$公寓.5樓含以下無電梯.+DATA003$建物現況格局.房
                           +DATA003$高樓層區間+DATA003$左營區+DATA003$大寮區+DATA003$住宅大樓.11層含以上有電梯.
                           +DATA003$林園區+DATA003$苓雅區+DATA003$三民區+DATA003$有無管理組織
                           +DATA003$鳳山區+DATA003$湖內區+DATA003$建物現況格局.廳+DATA003$中樓層區間
                           +DATA003$楠梓區+DATA003$茄萣區+DATA003$岡山區+DATA003$小港區+DATA003$低樓層區間
                           +DATA003$套房.1房1廳1衛.+DATA003$前鎮區+DATA003$大樹區+DATA003$鳥松區
                           +DATA003$旗山區+DATA003$仁武區+DATA003$前金區
                           ,data= DATA003,family = gaussian)
summary(housePriceMD_LIVE_RF)

###比較圖

houseCV_LIVE1<-cv.glm(DATA003 , housePriceMD_LIVE_FULL ,K= 5 )
houseCV_LIVE2<-cv.glm(DATA003_sign_FULL ,housePriceMD_LIVE_sign_FULL ,K= 5 )
houseCV_LIVE3<-cv.glm(DATA003_LASSO ,housePriceMD_LIVE_LESSO ,K= 5 )
houseCV_LIVE4<-cv.glm(DATA003 ,housePriceMD_LIVE_RF ,K= 5 )


CV.Result_LIVE<- as.data.frame(rbind(houseCV_LIVE1$delta , houseCV_LIVE2$delta ,houseCV_LIVE3$delta ,houseCV_LIVE4$delta))
names(CV.Result_LIVE)<- c("Error" ,"Adj.Error")
CV.Result_LIVE$Model <- sprintf("housePriceMD_LIVE%s",1:4)
CV.Result_LIVE


#CV.ANOVA <-anova(housePriceMD1 , housePriceMD2 ,housePriceMD3 ,housePriceMD4)
#CV.Result$ANOVA <-CV.ANOVA$`Resid. Dev`


CV.Result_LIVE$BIC <-BIC(housePriceMD_LIVE_FULL , housePriceMD_LIVE_sign_FULL ,housePriceMD_LIVE_LESSO ,housePriceMD_LIVE_RF)$BIC
CV.Result_LIVE$AIC <-AIC(housePriceMD_LIVE_FULL , housePriceMD_LIVE_sign_FULL ,housePriceMD_LIVE_LESSO ,housePriceMD_LIVE_RF)$AIC


require(reshape2)
CV_Melt_LIVE<- melt(CV.Result_LIVE , id.vars = "Model" ,variable.name ="Measure" , value.name ="Value")
CV_Melt_LIVE

ggplot(CV_Melt_LIVE , aes(x=Model , y= Value)) +
  geom_line(aes(group = Measure , color = Measure)) +
  facet_wrap(~Measure , scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90 ,vjust = .5)) +
  guides(color = FALSE)






