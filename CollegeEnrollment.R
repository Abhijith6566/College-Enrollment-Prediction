library(dplyr)
library(tidyverse)
library(modeest)
library(caret)
library(car)
library(e1071)
library(pROC)
AC_df<- read.csv("C:/Users/asibh/OneDrive/Desktop/Fall 2021/ADBA/inq2019.csv")
summary(AC_df)
length(AC_df$Enroll)
cor(AC_df$Enroll,AC_df$satscore,use="complete.obs")

str(AC_df)
AC_df1= subset(AC_df,select=-c(IRSCHOOL,ACADEMIC_INTEREST_1,ACADEMIC_INTEREST_2,CONTACT_CODE1,CONTACT_DATE))
summary(AC_df1)
sum(is.na(AC_df1))
apply(AC_df1,2,function(col)sum(is.na(col)/length(col)))
#Since telecq and satscore have more than 50 percent of missing values in them we will remove those variables as well.
AC_df2=subset(AC_df1,select=-c(telecq,satscore))
apply(AC_df2,2,function(col)sum(is.na(col)/length(col)))
str(AC_df2)
AC_df2$ETHNICITY <- as.factor(AC_df2$ETHNICITY)
summary(AC_df2$ETHNICITY)
print(AC_df2$ETHNICITY)
AC_df2$ETHNICITY[AC_df2$ETHNICITY==""]<-NA
#Adding factor level W and replacing NA with W
levels <- levels(AC_df2$ETHNICITY)
levels[length(levels) + 1] <- "W"
AC_df2$ETHNICITY <- factor(AC_df2$ETHNICITY, levels = levels)
AC_df2$ETHNICITY[is.na(AC_df2$ETHNICITY)] <- "W"

table(AC_df2$ETHNICITY)

AC_df2$ETHNICITY<-droplevels(AC_df2$ETHNICITY)

#Dummy coding

levels(AC_df2$ETHNICITY)
AC_df2$E_A<-ifelse(AC_df2$ETHNICITY=="A",1,0)
AC_df2$E_B<-ifelse(AC_df2$ETHNICITY=="B",1,0)
AC_df2$E_C<-ifelse(AC_df2$ETHNICITY=="C",1,0)
AC_df2$E_H<-ifelse(AC_df2$ETHNICITY=="H",1,0)
AC_df2$E_I<-ifelse(AC_df2$ETHNICITY=="I",1,0)
AC_df2$E_N<-ifelse(AC_df2$ETHNICITY=="N",1,0)
AC_df2$E_O<-ifelse(AC_df2$ETHNICITY=="O",1,0)

#Imputing with mean for avg_income and dis, mode for sex (using mfv(mostfrequent value))

AC_df2$avg_income[is.na(AC_df2$avg_income)]<-round(mean(AC_df2$avg_income,na.rm=TRUE))
AC_df2$distance[is.na(AC_df2$distance)]<-round(mean(AC_df2$distance,na.rm=TRUE))
mfv(AC_df2$sex)
AC_df2$sex[is.na(AC_df2$sex)]<- 1
table(AC_df2$sex)
#Changing Instate values too. Converting YES to 1 and No to 0

AC_df2$Instate<-ifelse(AC_df2$Instate=="Y",1,0)
AC_df2$Instate<- as.integer(AC_df2$Instate)
table(AC_df2$Instate)
table(AC_df2$TERRITORY)
AC_df2$Enroll<- as.factor(AC_df2$Enroll)
AC_df2<- subset(AC_df2,select=-c(LEVEL_YEAR))
# Removing outliers for init_span and log transformation of distance
plot(AC_df2$init_span)
AC_df2<-AC_df2[!(AC_df2$init_span==-216 | AC_df2$init_span==228),]
plot(AC_df2$init_span)

ggplot(AC_df2,aes(x=distance))+ geom_boxplot()
AC_df2$distance<- log10(AC_df2$distance+1)
AC_df2$avg_income <- log10(AC_df2$avg_income+1)


#Regression Model
#I removed main variable Ethnicity as I have already used dummy coding for that
AC_df2<- subset(AC_df2,select= -c(ETHNICITY))

vif(glm(formula=Enroll~.,family=binomial(link="logit"),data=AC_df2))

set.seed(101)
trainIndex<- createDataPartition(AC_df2$Enroll,p=0.7,list=FALSE,times=1)

AC_trainR<-AC_df2[trainIndex,]

AC_validR<-AC_df2[-trainIndex,]

AC_Reg_mod<- train(Enroll~.,data=AC_trainR,method='glm',family='binomial',na.action=na.pass)
summary(AC_Reg_mod)

#Evaluation

prediction <- predict(AC_Reg_mod,newdata=AC_validR)
confusionMatrix(prediction,AC_validR$Enroll)


#ROC

pred.probabilities <- predict(AC_Reg_mod,newdata=AC_validR,type='prob')

regression.ROC <- roc(predictor=pred.probabilities$`1`,response=AC_validR$Enroll,levels=levels(AC_validR$Enroll))
plot(regression.ROC)
regression.ROC$auc 

#Lift
lift <-function(depvar, predcol, groups=10)
  {if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
   helper <- data.frame(cbind(depvar, predcol)) 
   helper <- helper[order(-helper$predcol),]
   helper[,"bucket"]<- ntile(-helper[, "predcol"], groups)
   gaintable <- helper %>% group_by(bucket)  %>% summarise_at(vars(depvar), funs(total = n(),totalresp=sum(., na.rm =TRUE))) %>% mutate(Cumresp = cumsum(totalresp),Gain=Cumresp/sum(totalresp)*100,Cumlift=Gain/(bucket*(100/groups)))
   return(gaintable)}


dt=lift(AC_validR$Enroll,pred.probabilities$`1`,groups=10)
print(dt)


plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")



#Decision_tree model


library(rpart.plot)
library(rpart)


set.seed(101)
trainIndex<- createDataPartition(AC_df3$Enroll,p=0.7,list=FALSE,times=1)

AC_trainD<-AC_df3[trainIndex,]

AC_validD<-AC_df3[-trainIndex,]

str(AC_trainD)
AC_tree.model <- train(Enroll~.,data=AC_trainD,method="rpart",na.action=na.pass)

AC_tree.model


prp(AC_tree.model$finalModel,type=2,extra=106)



prediction <- predict(AC_tree.model,newdata=AC_validD,na.action = na.pass)

confusionMatrix(prediction,AC_validD$Enroll)

tree.probabilities <- predict(AC_tree.model,newdata=AC_validD,type='prob',na.action=na.pass)

AC_tree.ROC <- roc(predictor=tree.probabilities$`1`,response=AC_validD$Enroll,levels=levels(AC_validD$Enroll))

plot(AC_tree.ROC)

AC_tree.ROC$auc
