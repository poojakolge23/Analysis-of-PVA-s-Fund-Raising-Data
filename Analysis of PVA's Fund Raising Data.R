library("tidyverse")
library("dplyr")
library("lubridate")
library("zoo")
library("car")
library("mice")
library("randomForest")
library(rpart)
library(rpart.plot)
library(purrr)
library(ggplot2)
library(ggthemes)


#---- Import the data set.-----------------

pva_full <- read_csv("pvaDataForModeling_Spr2019.csv")
  
head(pva_full$ADATE_2,100)
lubridate::y

summary(pva_full$MAILCODE)

#---- Subset the data to keep only significant vars.------------------

pva <- pva_full %>% select(ADATE_2,
                    RFA_2,
                    RFA_3,
                    CARDPROM,
                    MAXADATE,
                    NUMPROM,
                    CARDPM12,
                    
                    RAMNTALL,
                    NGIFTALL,
                    CARDGIFT,
                    MINRAMNT,
                    MINRDATE,
                    MAXRAMNT,
                    MAXRDATE,
                    LASTGIFT,
                    LASTDATE,
                    FISTDATE,
                    NEXTDATE,
                    TIMELAG,
                    AVGGIFT,
                    TARGET_D,
                    TARGET_B,
                    MAILCODE,
                    DOB,
                    MDMAUD,
                    DOMAIN,
                    HOMEOWNR,
                    CHILD03,
                    CHILD07,
                    CHILD12,
                    CHILD18,
                    NUMCHLD,
                    INCOME,
                    GENDER,
                    HIT,
                    MBCRAFT,
                    MBGARDEN,
                    MBBOOKS,
                    MBCOLECT,
                    MAGFAML,
                    MAGFEM,
                    MAGMALE,
                    PUBGARDN,
                    PUBCULIN,
                    PUBHLTH,
                    PUBDOITY,
                    PUBNEWFN,
                    PUBPHOTO,
                    PUBOPP,
                    DATASRCE,
                    MALEMILI,
                    MALEVET,
                    LOCALGOV,
                    FEDGOV,
                    STATEGOV
                    )

#----- Analyse the data and change to correct data type. ------------ 

#Creating vectors for each data type with column names.
cols_num <- c("CARDPROM",
              "CARDPM12",
              "NUMPROM",
              "RAMNTALL",
              "NGIFTALL",
              "CARDGIFT",
              "MINRAMNT",
              "MAXRAMNT",
              "LASTGIFT",
              "TIMELAG",
              "AVGGIFT",
              "TARGET_D",
              "HIT",
              "MALEMILI",
              "MALEVET",
              "LOCALGOV",
              "FEDGOV",
              "STATEGOV")

cols_fac <- c("RFA_2",
              "RFA_3",
              "CARDPM12",
              "TARGET_B",
              "MAILCODE",
              "MDMAUD",
              "DOMAIN",
              "HOMEOWNR",
              "CHILD03",
              "CHILD07",
              "CHILD12",
              "CHILD18",
              "NUMCHLD",
              "GENDER",
              "MBCRAFT",
              "MBGARDEN",
              "MBBOOKS",
              "MBCOLECT",
              "MAGFAML",
              "MAGFEM",
              "MAGMALE",
              "PUBGARDN",
              "PUBCULIN",
              "PUBHLTH",
              "PUBDOITY",
              "PUBNEWFN",
              "PUBPHOTO",
              "PUBOPP",
              "DATASRCE",
              "INCOME")

cols_date <- c("ADATE_2",
              "MAXADATE",
              "MINRDATE",
              "MAXRDATE",
              "LASTDATE",
              "FISTDATE",
              "NEXTDATE",
              "DOB")

#Changing the data types.

#Changing the numeric variables. 
glimpse(pva[,cols_num])
pva[,cols_num] = data.frame(sapply(pva[,cols_num], as.numeric))

#Changing categorical variables
glimpse(pva[,cols_fac])
pva[,cols_fac] = data.frame(sapply(pva[,cols_fac], as.factor))

#Changing date variables
glimpse(pva[,cols_date])
testdate <- pva$ADATE_2
ym(testdate)

glimpse(pva)

save.image("PVA_OVER.Rdata")
write.csv(pva, file="PVA.csv")

#Changing dates.
rm(pva_num)
testdate <- pva$ADATE_2
glimpse(testdate)
testdate <- paste("19",testdate,sep="")
testdate <- as.Date(as.yearmon(testdate, "%Y%m"), frac = 1)
glimpse(pva$MAXRDATE)
head(pva$MAXRDATE)
pva$ADATE_2 <- paste("19",pva$ADATE_2,sep="")
pva$ADATE_2 <- as.Date(as.yearmon(pva$ADATE_2, "%Y%m"), frac = 1)

pva$MAXADATE <- paste("19",pva$MAXADATE,sep="")
pva$MAXADATE <- as.Date(as.yearmon(pva$MAXADATE, "%Y%m"), frac = 1)

pva$MINRDATE <- paste("19",pva$MINRDATE,sep="")
pva$MINRDATE <- as.Date(as.yearmon(pva$MINRDATE, "%Y%m"), frac = 1)

pva$MAXRDATE <- paste("19",pva$MAXRDATE,sep="")
pva$MAXRDATE <- as.Date(as.yearmon(pva$MAXRDATE, "%Y%m"), frac = 1)

pva$LASTDATE <- paste("19",pva$LASTDATE,sep="")
pva$LASTDATE <- as.Date(as.yearmon(pva$LASTDATE, "%Y%m"), frac = 1)

pva$FISTDATE <- paste("19",pva$FISTDATE,sep="")
pva$FISTDATE <- as.Date(as.yearmon(pva$FISTDATE, "%Y%m"), frac = 1)

pva$NEXTDATE <- paste("19",pva$NEXTDATE,sep="")
pva$NEXTDATE <- as.Date(as.yearmon(pva$NEXTDATE, "%Y%m"), frac = 1)

pva$DOB <- paste("19",pva$DOB,sep="")
pva$DOB <- as.Date(as.yearmon(pva$DOB, "%Y%m"), frac = 1)

glimpse(pva[,cols_date])


#Handling the RFA Column

pva <- pva[,1:55]
pva <- cbind(pva,RFA_2R=pva_full$RFA_2R)
pva <- cbind(pva,RFA_2F=pva_full$RFA_2F)
pva <- cbind(pva,RFA_2A=pva_full$RFA_2A)

head(pva$RFA_3)
pva$RFA_3R <- substr(pva$RFA_3,1,1)
pva$RFA_3F <- substr(pva$RFA_3,2,2)
pva$RFA_3A <- substr(pva$RFA_3,3,3)

#Removing original RFA column

pva$RFA_3 <- NULL


#------ Checking for missing values.------------


#Checking for missing values
sapply(pva,function(x) sum(is.null(x))) #No NUll Values.

#Let's check for NA now
sapply(pvaTrn,function(x) sum(is.na(x)))

#Many NA values. Let's handle the numeric first.

#df$value[is.na(df$value)] <- median(df$value, na.rm=TRUE)
pva$TIMELAG[is.na(pva$TIMELAG)] <- median(pva$TIMELAG, na.rm = TRUE)
pva$AVGGIFT[is.na(pva$AVGGIFT)] <- median(pva$AVGGIFT, na.rm = TRUE)

#Let's check for DATE variables.
#testdate <- pva$FISTDATE
#testdate[is.na(testdate)] <- median(testdate, na.rm = TRUE)
pva$FISTDATE[is.na(pva$FISTDATE)] <- median(pva$FISTDATE, na.rm = TRUE)

pva$NEXTDATE[is.na(pva$NEXTDATE)] <- median(pva$NEXTDATE, na.rm = TRUE)
pva$DOB[is.na(pva$DOB)] <- median(pva$DOB, na.rm = TRUE)

#Let's check for Factor variables.
glimpse(pva$HOMEOWNR)
summary(pva$HOMEOWNR)

pva$MAILCODE = factor(pva$MAILCODE, levels=c(levels(pva$MAILCODE), "G"))
pva$MAILCODE[is.na(pva$MAILCODE)] = "G"

pva$DOMAIN <- substr(pva$DOMAIN,1,1)
pva$DOMAIN <- as.factor(pva$DOMAIN)
pva$DOMAIN[is.na(pva$DOMAIN)] <- "S"

pva$HOMEOWNR[is.na(pva$HOMEOWNR)] <- "U"

m <- pva[,c("CHILD03","CHILD07","CHILD12","CHILD18")]
summary(m)

m$CHILD03 <- car::recode(m$CHILD03,"c('B','F','M')=1;else =0")
m$CHILD07 <- car::recode(m$CHILD07,"c('B','F','M')=1;else =0")
m$CHILD12 <- car::recode(m$CHILD12,"c('B','F','M')=1;else =0")
m$CHILD18 <- car::recode(m$CHILD18,"c('B','F','M')=1;else =0")
m$NUMCHILD <- as.numeric(as.character(m$CHILD03)) + as.numeric(as.character(m$CHILD07)) + as.numeric(as.character(m$CHILD12)) + as.numeric(as.character(m$CHILD18))
m$NUMCHILD <- as.factor(m$NUMCHILD)
#pva <- cbind(pva,RFA_2R=pva_full$RFA_2R)
pva <- cbind(pva, NUMCHILD = m$NUMCHILD)

pva$CHILD03 <- NULL
pva$CHILD07 <- NULL
pva$CHILD12 <- NULL
pva$CHILD18 <- NULL
pva$NUMCHILD <- NULL

pva$NUMCHLD[is.na(pva$NUMCHLD)] <- 0
pva$NUMCHLD = factor(pva$NUMCHLD, levels=c(levels(pva$NUMCHLD), '0'))
pva$NUMCHLD[is.na(pva$NUMCHLD)] = '0'

summary(pva$INCOME)

head(pva$GENDER)
pva$GENDER <- car::recode(pva$GENDER,"c('A','C','J')='U'")
pva$GENDER[is.na(pva$GENDER)] <- 'U'

summary(pva$MBCOLECT)
#m <- pva$DATASRCE
#mice(data = as.data.frame(m), m = 5, method = "pmm", maxit = 50, seed = 500)

summary(pva$PUBOPP)
pva$MBCOLECT[is.na(pva$MBCOLECT)] <- 0
pva$MBCRAFT[is.na(pva$MBCRAFT)] <- 0
pva$MBGARDEN[is.na(pva$MBGARDEN)] <- 0
pva$MBBOOKS[is.na(pva$MBBOOKS)] <- 0
pva$MBCOLECT[is.na(pva$MBCOLECT)] <- 0
pva$MAGFAML[is.na(pva$MAGFAML)] <- 0
pva$MAGFEM[is.na(pva$MAGFEM)] <- 0
pva$MAGMALE[is.na(pva$MAGMALE)] <- 0

pva$PUBGARDN[is.na(pva$PUBGARDN)] <- 0
pva$PUBCULIN[is.na(pva$PUBCULIN)] <- 0
pva$PUBHLTH[is.na(pva$PUBHLTH)] <- 0
pva$PUBDOITY[is.na(pva$PUBDOITY)] <- 0
pva$PUBNEWFN[is.na(pva$PUBNEWFN)] <- 0
pva$PUBPHOTO[is.na(pva$PUBPHOTO)] <- 0
pva$PUBOPP[is.na(pva$PUBOPP)] <- 0


pva$RFA_3A <- as.factor(pva$RFA_3A)
pva$RFA_3F <- as.factor(pva$RFA_3F)
pva$RFA_3R <- as.factor(pva$RFA_3R)
summary(pva$RFA_3R)
summary(pva$RFA_3F)
summary(pva$RFA_3A)


pva$RFA_3R[is.na(pva$RFA_3R)]
pva$RFA_3F[is.na(pva$RFA_3F)]
pva$RFA_3A[is.na(pva$RFA_3A)]

pva$RFA_3R[is.na(pva$RFA_3R)] <- 'A'
pva$RFA_3F[is.na(pva$RFA_3F)] <- 1

pva$RFA_3A = factor(pva$RFA_3A, levels=c(levels(pva$RFA_3A), 'X'))
pva$RFA_3A <- car::recode(pva$RFA_3A,"c('B','C')='X'")
pva$RFA_3A[is.na(pva$RFA_3A)] <- 'X'


pva <- cbind(pva,MDMAUD_R=pva_full$MDMAUD_R,MDMAUD_F=pva_full$MDMAUD_F,MDMAUD_A=pva_full$MDMAUD_A)
pva$MDMAUD <- NULL
pva$TARGET_D <- NULL

glimpse(pva$MDMAUD_F)

pva$DATASRCE = factor(pva$DATASRCE, levels=c(levels(pva$DATASRCE), "9"))
pva$DATASRCE[is.na(pva$DATASRCE)] = "9"

#------Using rfimpute to use find out missing values in INCOME and DATASRCE column ------
rm(subs_inc)
subset <- cbind(pva,Census)

subs_inc <- pva[,c("TARGET_B","INCOME","IC2","IC6","AVGGIFT","HU1","HU2","RAMNTALL","CARDPROM","NUMPROM")]
subs_inc <- cbind(subs_inc,pva_full[,c("IC1","IC3","IC4","IC5")])

sapply(subs_inc,function(x) sum(is.na(x)))


income.imputed <- rfImpute(TARGET_B ~ .,data=subs_inc, iter=5, ntree=300)



#------ Combining with Census data -------------

m <- pva
pva <- cbind(pva,Census)
View(Census)



#------ Univariate Analysis --------------------

pva %>%  keep(is.factor) %>%  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

pva %>%  keep(is.factor) %>% gather() %>% 
  ggplot(aes(value)) +
  geom_bar(position="fill")+
  facet_wrap(~ key, scales = "free") 

pva %>%  keep(is.numeric) %>%  gather() %>% 
  ggplot(aes(x=pva$TARGET_B,y=value)) + theme_excel_new()+
  facet_wrap(~ key, scales = "free") +
  geom_density()

ggplot(data=pva, aes(x=TARGET_B,y=CARDPROM))+theme_excel_new()+
  geom_boxplot()


ggplot(data=pva, aes(x=CARDPROM))+theme_excel_new()+
  geom_density(aes(group=TARGET_B, color=TARGET_B, fill=TARGET_B), alpha=0.2)

pva %>%  keep(is.numeric) %>%  gather() %>% 
  ggplot(aes(x=pva$TARGET_B,y=value)) + theme_excel_new()+
  facet_wrap(~ key, scales = "free") +
  geom_density()

pva %>%  keep(is.factor) %>%  gather() %>% 
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") +
  geom_bar(position="fill")

library(tidyverse)
rm(pva_num)
pva_num$`pva$TARGET_B` <- NULL
pva_num <- pva %>%  keep(is.numeric)
pva_num <- cbind(pva_num,TARGET_B=pva$TARGET_B)

pva_num %>% 
  ggplot(aes(x=TARGET_B,y=value, colour=variable, group=variable))+theme_excel_new()+
  geom_density(aes(group=TARGET_B, color=TARGET_B, fill=TARGET_B), alpha=0.2)+
  facet_grid(.~TARGET_B, scales = "free") 


#------- Bivariate Analysis -------------

install.packages("reshape2")
library("reshape2")
pva_date <- pva %>%  keep(is.Date)
pva_num <- pva %>%  keep(is.numeric)
pva_num <- cbind(pva_num,TARGET_B=pva$TARGET_B)
pva_fac <- pva %>%  keep(is.factor)

pva1 <- melt(pva_fac, id.vars = 'TARGET_B')
pva2 <- melt(pva_num, id.vars = 'TARGET_B')

ggplot(data=pva_fac) + aes(x=TARGET_B, fill=MDMAUD_A) + 
  geom_bar(position="fill")+
  scale_fill_brewer(palette="Pastel2") + 
  labs(title='Relationship between TARGET_B and MDMAUD_A') 

ggplot(pva_num, aes(x=TARGET_B, y=IC2, color=TARGET_B)) +
  geom_boxplot()+ 
  labs(title='Relationship between TARGET_B and IC2') 

cols_num <- colnames(pva_num)
rm(pva_num)

save.image("PVA_clean_bak.Rdata")  

#------------------- Breaking the data into Test/Train --------------------

set.seed(123)

nr=nrow(pva)

trnIndex = sample(1:nr, size = round(0.6*nr), replace=FALSE)  #get a random 60% sample of row-indices

pvaTrn=pva[trnIndex,]                                       #training data with the randomly selected row-indices

pvaTst = pva[-trnIndex,]                                    #test data with the other row-indicesdim(creditTrn)

dim(pvaTrn)
dim(pvaTst)

save.image("PVA_Model.Rdata")


##----------- Over sampling --------------------------
library("ROSE")
#data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
?ovun.sample
PVA_TRAIN_OVER <- ovun.sample(TARGET_B ~ ., data = pvaTrn, method = "over",seed=1,p=0.5)$data
table(PVA_TRAIN_OVER$TARGET_B)

#------------------------ Decision Tree  ----------------------------
glimpse(pva)
rmodel_1 <- rpart(TARGET_B ~ . , data = PVA_TRAIN_OVER , method = "class", 
                  parms = list(split = 'information')
                  ,control= rpart.control(minsplit=20,minbucket = round(20/3),maxdepth=10,cp=0.0001))

rmodel_1$variable.importance
summary(rmodel_1)
#Plotting the model
plot(rmodel_1,  uniform=TRUE,  main="Decision Tree for pvaTrn Target")
text(rmodel_1, use.n=TRUE, all=TRUE, cex=.7)

######--------------------Predicting the probabilities using the refined model ######--------------------

pred1 <- predict(rmodel_1, pvaTst, type='prob') #Doing the same for test 
predTst=predict(rmodel_1, pvaTst, type='class') #Predicting value for just '1' response variable  in test data

#######--------------------Creating the Confusion Matrix to find out the accuracy ######--------------------

cmTst <- table(true=pvaTst$TARGET_B,pred = predTst) #Confusion matrix for Test data
cmTst

#Accuracy

tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst);accTst
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- (2*precTst*recTst)/(precTst+recTst);f_Tst

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, pvaTst$TARGET_B)
#Finding the AUC Value
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values



#------------------------ Random Forest ----------------------------
rfModel<-randomForest(TARGET_B~., data=PVA_TRAIN_OVER, ntree=500, importance=TRUE)
rfModel #Check OOB error rate

#Variable Importance
importance(rfModel)
varImpPlot(rfModel)

#Predict on Test Data
rfPredict<-predict(rfModel, pvaTst, type="prob")

#######--------------------Creating the Confusion Matrix to find out the accuracy ######--------------------

cmTst1 <- table(true=pvaTst$TARGET_B,pred = rfPredict) #Confusion matrix for Test data
cmTst1

#Accuracy

tpTst1 <- cmTst1[2,2]
tnTst1 <- cmTst1[1,1]
fpTst1 <- cmTst1[1,2]
fnTst1 <- cmTst1[2,1]

accTst1 <- (tpTst1+tnTst1)/(tpTst1+tnTst1+fpTst1+fnTst1);accTst1
precTst1 <- tpTst1 / (tpTst1+fpTst1);precTst1
recTst1 <- tpTst1 / (tpTst1+fnTst1);recTst1
f_Tst1 <- (2*precTst1*recTst1)/(precTst1+recTst1);f_Tst1

scoreTst = rfPredict[,1]
rocPredTst = prediction(scoreTst, pvaTst$TARGET_B)  
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

#------------------------ Boosted Trees ----------------------------
install.packages("caret")
library(gbm);library(caret);library(Ecdat)

PVA_TRAIN_OVER1 <- subset(PVA_TRAIN_OVER, select=-c(RFA_2R,ADATE_2,MAXADATE,MINRDATE,MAXRDATE,LASTDATE,FISTDATE,NEXTDATE,DOB))

grid<-expand.grid(.n.trees=400,.interaction.depth=1,.shrinkage=.01,
                  .n.minobsinnode=3) #grid features
control<-trainControl(method="CV",number = 10) #control

set.seed(654481566)
gbm.lfp.train<-train(TARGET_B~.,data=PVA_TRAIN_OVER1,method='gbm',trControl=control,tuneGrid=grid)

summary(gbm.lfp.train,plotit = TRUE)

gbm.lfp.test<-predict(gbm.lfp.train,newdata = pvaTst,type = 'raw', n.trees = 400)
gbm.lfp.test<-predict(gbm.lfp.train,newdata = pvaTst,type = 'prob', n.trees = 400)
gbm.class<-ifelse(gbm.lfp.test<0.5,0,1)



#######--------------------Creating the Confusion Matrix to find out the accuracy ######--------------------

cmTst2 <- table(true=pvaTst$TARGET_B,pred = gbm.lfp.test) #Confusion matrix for Test data
cmTst2

#Accuracy

tpTst2 <- cmTst2[2,2]
tnTst2 <- cmTst2[1,1]
fpTst2 <- cmTst2[1,2]
fnTst2 <- cmTst2[2,1]

accTst2 <- (tpTst2+tnTst2)/(tpTst2+tnTst2+fpTst2+fnTst2);accTst2
precTst2 <- tpTst2 / (tpTst2+fpTst2);precTst2
recTst2 <- tpTst2 / (tpTst2+fnTst2);recTst2
f_Tst2 <- (2*precTst2*recTst2)/(precTst2+recTst2);f_Tst2


scoreTst = gbm.lfp.test[,1]
rocPredTst = prediction(scoreTst, pvaTst$TARGET_B)  
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

