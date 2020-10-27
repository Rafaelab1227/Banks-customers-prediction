setwd("C:/Users/Rafaela Becerra/Desktop/UC3M/Statistical Learning/Assignment 1")
set.seed(100)
library(readr)
dataset=read_delim("bank-full.csv",";", escape_double = FALSE, trim_ws = TRUE)
##################################### PREPROCESSING #####################################
library(dplyr)
library(caret)
##### 1. Treatment of the dataset
#Summary of the variables
summary(dataset)

#Transform of unknown categories as NA
dataset[dataset=="unknown"]=NA

#Transformation of the dataset varibles which have to be trated as categorical
names=c("job","marital","education","default", "housing","loan","contact","month","poutcome","y")
dataset[,names] = lapply(dataset[,names] , factor)
summary(dataset)
glimpse(dataset)

# Divide training and testing sets
div = createDataPartition(dataset$y, p = 0.8, list = FALSE)  # 80% for training
data = dataset[div,]
datatest = dataset[-div,]

####################### MISSING VALUES
#Identify missing values
library(skimr)
skim_to_wide(data)
library(VIM)
aggr(data, numbers = TRUE, sortVars = TRUE, labels = names(data),
     cex.axis = .5, gap = 1, ylab= c('Missing data','Pattern'))

#contact
prop.table(table(data$contact))
ggplot(data, aes(x = y, y = data$contact)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
  geom_jitter()

#poutcome
prop.table(table(data$poutcome))
prop.table(table(data$poutcome, data$y),2)

data=data[,-c(match("contact", names(data)),match("poutcome", names(data)))]

data=na.omit(data)
aggr(data, numbers = TRUE, sortVars = TRUE, labels = names(data),
     cex.axis = .5, gap = 1, ylab= c('Missing data','Pattern'))


#First correlation visualization
ggcorr(data, label = T)

################ TRANSFORMATIONS

#Age
prop.table(table(data$age))
sort(prop.table(table(data$age)))
#Create age group Generational Cohorts
data=mutate(data,age_group=2008-age)
data[data$age_group>=1996,match("age", names(data))]="Gen_z"
data[data$age_group>=1977&data$age_group<=1995,match("age", names(data))]="Gen_y"
data[data$age_group>=1965&data$age_group<=1976,match("age", names(data))]="Gen_x"
data[data$age_group>=1946&data$age_group<=1964,match("age", names(data))]="Gen_bb"
data[data$age_group<=1945,match("age", names(data))]="Gen_s"

data=data[,-match("age_group",names(data))]

prop.table(table(data$age))
sort(prop.table(table(data$age)))


#Job
prop.table(table(data$job))
(table(data$job))
#reducing job categories
table(data$job,data$y)
prop.table(table(data$job,data$y),1)
prop.table(table(data$job,data$y),2)
levels(data$job)[levels(data$job)=="admin."|levels(data$job)=="services"|levels(data$job)=="technician"|levels(data$job)=="management"]= "white_collar"
levels(data$job)[levels(data$job)=="self-employed"|levels(data$job)=="entrepreneur"]= "self_employed"
levels(data$job)[levels(data$job)=="blue-collar"|levels(data$job)=="housemaid"]= "blue_collar"

prop.table(table(data$job,data$y),2)

#Marital
prop.table(table(data$marital))

#Education
prop.table(table(data$education))

#default
prop.table(table(data$default))
#Elimination of the variable
data=data[,-match("default",names(data))]

#balance
prop.table(table(data$balance))
ggplot(data, aes(balance)) + geom_density()
ggplot(data, aes(log(balance^2))) + geom_density() #data lose when estandarize, no transformation

#data=mutate(data, balance=log(balance))

#housing
prop.table(table(data$housing))

#loan
prop.table(table(data$loan))

#day
#Categorizing days
prop.table(table(data$day))
table(data$y, data$day)
prop.table(table(data$y, data$day),1)

prop.table(table(data$y, data$day),2)

data$week=0
data[data$day>=1&data$day<=15,dim(data)[2]]="w1"
data[data$day>=16&data$day<=31,dim(data)[2]]="w2"
data=data[,-match("day",names(data))]
names(data)[dim(data)[2]]="day"
prop.table(table(data$day))
table(data$y, data$day)
prop.table(table(data$y, data$day),1)
prop.table(table(data$y, data$day),2)

#Month
#Quarter agroupation of campaign months
levels(data$month)[levels(data$month)=="jan"|levels(data$month)=="feb"|levels(data$month)=="mar"]= "q1"
levels(data$month)[levels(data$month)=="apr"|levels(data$month)=="may"|levels(data$month)=="jun"]= "q2"
levels(data$month)[levels(data$month)=="jul"|levels(data$month)=="aug"|levels(data$month)=="sep"]= "q3"
levels(data$month)[levels(data$month)=="oct"|levels(data$month)=="nov"|levels(data$month)=="dec"]= "q4"

#duration
ggplot(data, aes(x = y, y = data$duration)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
  labs(y="Duration (sec)")
ggplot(data, aes(duration)) + geom_density()
ggplot(data, aes(log(duration))) + geom_density()
data=data[data$duration!=0,]
data=mutate(data, duration=log(duration))

#campaign
ggplot(data, aes(x = y, y = data$campaign)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
  geom_jitter()
ggplot(data, aes(x=campaign,fill=y)) + geom_bar()


#pdays
ggplot(data, aes(x = y, y = data$pdays)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
  labs(y="Days between contacts") #Plot distortioned by all the clients that were contacted for the first time.

pdays_wna=data[data$pdays!=-1, match("pdays", names(data))]
quantile(pdays_wna$pdays, probs = seq(0, 1, 0.20), na.rm=TRUE,names = TRUE)

data$pdays_n="NC"
data[data$pdays<=110&data$pdays>=0,match("pdays_n", names(data))]="q1"
data[data$pdays>=111&data$pdays<=182,match("pdays_n", names(data))]="q2"
data[data$pdays>=183&data$pdays<=259,match("pdays_n", names(data))]="q3"
data[data$pdays>=260&data$pdays<=343,match("pdays_n", names(data))]="q4"
data[data$pdays>=344,match("pdays_n", names(data))]="q5"

data=data[,-match("pdays",names(data))]
names(data)[match("pdays_n",names(data))]="pdays"
prop.table(table(data$pdays))

#previous
prop.table(table(data$previous))
table(data$previous, data$y)
prop.table(table(data$previous, data$y),1)

ggplot(data, aes(previous)) + geom_density()
ggplot(data, aes(previous)) + geom_bar(aes(fill=y))
ggplot(data, aes(log(previous))) + geom_density()

ggplot(data, aes(y=previous)) + geom_boxplot(aes(fill=y))

#Create categories to treat previous variable
data$previous_n=0
data[data$previous<=5,match("previous_n", names(data))]="l_5"
data[data$previous>=6&data$previous<=10,match("previous_n", names(data))]="l_10"
data[data$previous>=11,match("previous_n", names(data))]="l_11"

data=data[,-match("previous",names(data))]
names(data)[match("previous_n",names(data))]="previous"
prop.table(table(data$previous))
prop.table(table(data$previous, data$y))

#Final category of variables
names=c("age","job","marital","education","housing","loan","month","day","pdays","previous","y")
data[,names] = lapply(data[,names] , factor)

#Division of the variable in categorical variables and quantitative
library(dplyr)
data_cat=select_(data, .dots=names)
names_num=c("balance","duration", "campaign","y")
data_num=select_(data,.dots=names_num)

################## CORRELATIONS 
#Correlations between numerical variables
ggcorr(data, label = T)
library(GGally)
ggpairs(data_num)

library(reshape2)
data_num1 = melt(data_num ,  id.vars = 'y', variable.name = 'variable')

#Almost normal
qplot(sample =balance, data= data_num, color=y)
qplot(sample =duration, data= data_num, color=y)
qplot(sample =campaign, data= data_num, color=y)

#Outlier treatment
#Boxplot
ggplot(data_num1, aes(x = y, y = value)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
 # geom_jitter()+
  xlab("Deposit")+
  facet_wrap(~variable, scales="free_y")

ggplot(data_num1,aes(x= y, y=value, fill=y)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2) +
  xlab("Deposit")+ 
  facet_wrap(~variable, scales="free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

outlier= function(dataframe, var) {
  variable = eval(substitute(var),eval(dataframe))
  tot = sum(!is.na(variable))
  na1 = sum(is.na(variable))
  m1 = mean(variable, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(variable, main="With outliers")
  hist(variable, main="With outliers", xlab=NA, ylab=NA)
  outlier = boxplot.stats(variable)$out
  mo = mean(outlier)
  variable = ifelse(variable %in% outlier, NA, variable)
  boxplot(variable, main="Without outliers")
  hist(variable, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 = sum(is.na(variable))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 = mean(variable, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
}

x11()
outlier(data_num, campaign)
outlier(data_num, duration)
outlier(data_num, balance)

#########################OUTLIERS MULTIVARIATE APROACH
#data_num_no=data_num[data_num$y=="no",]
#data_num_no=data_num[data_num$previous==0,]
data_num_no=as.matrix(data_num)
data_num_no=data_num_no[,-4]
data_num_no=apply(data_num_no,2,as.numeric)
rownames(data_num_no)=1:nrow(data_num_no)

#Review outliers
m.data_num_no = colMeans(data_num_no)
s.data_num_no = cov(data_num_no)
det(s.data_num_no)

mah.data_num_no = mahalanobis(data_num_no,m.data_num_no,s.data_num_no)

sort.mah.data_num_no= sort(mah.data_num_no,index.return=TRUE)$x
sort.mah.data_num_no

plot(sort.mah.data_num_no,pch=19,col="deepskyblue2",xlab="",ylab="",main="Mahalanobis distances")
e.nopoor = sort(mah.data_num_no,index.return=TRUE)$x

# FDA
n.data_num_no = nrow(data_num_no)
n.data_num_no
p.data_num_no = ncol(data_num_no)
p.data_num_no

p.values.data_num_no = 1 - pchisq(mah.data_num_no,p.data_num_no)
p.values.data_num_no

# Sort them in increasing order

sort.p.values.data_num_no = sort(p.values.data_num_no,index.return=TRUE)
sort.p.values.data_num_no$x

# Outliers
outliers=as.data.frame(which(sort.p.values.data_num_no$x < ((1:n.data_num_no)/n.data_num_no*0.01)))
index_out=as.numeric(rownames(outliers))

#Remove outliers
data_num = data_num[-index_out, ]
data_num1 = melt(data_num ,  id.vars = 'y', variable.name = 'variable')

data = data[-index_out, ]

ggplot(data_num1, aes(x = y, y = value)) + 
  geom_boxplot(aes(fill = y), position = position_dodge(0.9))+
  #geom_jitter()+
  xlab("Deposit")+
  facet_wrap(~variable, scales="free_y")

#Correlations between categorical variables
data_cat1 = melt(data_cat ,  id.vars = 'y', variable.name = 'variable')
data_cat_table=data_cat1%>%group_by(y, variable,value)%>%summarise(count=n())


#Categorical plots
ggList = lapply(split(data_cat1, data_cat1$variable), function(i) {
  ggplot(i, aes(x=y,fill =value)) + 
    geom_bar()})

# plot as grid
library(cowplot)
plot_grid(plotlist = ggList, ncol = 5,
          align = 'h', axis = "tb",label_size = 8,labels = levels(data_cat1$variable))

#plot interact in, different behaviour within groups
ggplot(data, aes(duration)) + geom_density(aes(group=previous, colour=previous, fill=previous), alpha=0.1) +xlab("duration")

#levels(data$y)=c(0,1)

##################### PREPROCESS TESTING SET
#Identify missing values
datatest=datatest[,-c(match("contact", names(datatest)),match("poutcome", names(datatest)))]
datatest=na.omit(datatest)
aggr(datatest, numbers = TRUE, sortVars = TRUE, labels = names(datatest),
     cex.axis = .5, gap = 1, ylab= c('Missing datatest','Pattern'))

#Changing in variables
#Age
datatest=mutate(datatest,age_group=2008-age)
datatest[datatest$age_group>=1996,match("age", names(datatest))]="Gen_z"
datatest[datatest$age_group>=1977&datatest$age_group<=1995,match("age", names(datatest))]="Gen_y"
datatest[datatest$age_group>=1965&datatest$age_group<=1976,match("age", names(datatest))]="Gen_x"
datatest[datatest$age_group>=1946&datatest$age_group<=1964,match("age", names(datatest))]="Gen_bb"
datatest[datatest$age_group<=1945,match("age", names(datatest))]="Gen_s"

datatest=datatest[,-match("age_group",names(datatest))]

#Job
levels(datatest$job)[levels(datatest$job)=="admin."|levels(datatest$job)=="services"|levels(datatest$job)=="technician"|levels(datatest$job)=="management"]= "white_collar"
levels(datatest$job)[levels(datatest$job)=="self-employed"|levels(datatest$job)=="entrepreneur"]= "self_employed"
levels(datatest$job)[levels(datatest$job)=="blue-collar"|levels(datatest$job)=="housemaid"]= "blue_collar"

#default
#Elimination of the variable
datatest=datatest[,-match("default",names(datatest))]

#day
#Categorizing days
datatest$week=0
datatest[datatest$day>=1&datatest$day<=15,dim(datatest)[2]]="w1"
datatest[datatest$day>=16&datatest$day<=31,dim(datatest)[2]]="w2"
datatest=datatest[,-match("day",names(datatest))]
names(datatest)[dim(datatest)[2]]="day"


#Month
#Quarter agroupation of campaign months
levels(datatest$month)[levels(datatest$month)=="jan"|levels(datatest$month)=="feb"|levels(datatest$month)=="mar"]= "q1"
levels(datatest$month)[levels(datatest$month)=="apr"|levels(datatest$month)=="may"|levels(datatest$month)=="jun"]= "q2"
levels(datatest$month)[levels(datatest$month)=="jul"|levels(datatest$month)=="aug"|levels(datatest$month)=="sep"]= "q3"
levels(datatest$month)[levels(datatest$month)=="oct"|levels(datatest$month)=="nov"|levels(datatest$month)=="dec"]= "q4"


#duration
datatest=datatest[datatest$duration!=0,]
datatest=mutate(datatest, duration=log(duration))

#pdays
datatest$pdays_n="NC"
datatest[datatest$pdays<=110&datatest$pdays>=0,match("pdays_n", names(datatest))]="q1"
datatest[datatest$pdays>=111&datatest$pdays<=182,match("pdays_n", names(datatest))]="q2"
datatest[datatest$pdays>=183&datatest$pdays<=259,match("pdays_n", names(datatest))]="q3"
datatest[datatest$pdays>=260&datatest$pdays<=343,match("pdays_n", names(datatest))]="q4"
datatest[datatest$pdays>=344,match("pdays_n", names(datatest))]="q5"

datatest=datatest[,-match("pdays",names(datatest))]
names(datatest)[match("pdays_n",names(datatest))]="pdays"

#previous
#Create categories to treat previous variable
datatest$previous_n=0
datatest[datatest$previous<=5,match("previous_n", names(datatest))]="l_5"
datatest[datatest$previous>=6&datatest$previous<=10,match("previous_n", names(datatest))]="l_10"
datatest[datatest$previous>=11,match("previous_n", names(datatest))]="l_11"

datatest=datatest[,-match("previous",names(datatest))]
names(datatest)[match("previous_n",names(datatest))]="previous"

#Final category of variables
names=c("age","job","marital","education","housing","loan","month","day","pdays","previous","y")
datatest[,names] = lapply(datatest[,names] , factor)


######################################## CLASSIFIERS ####################################
########################## LOGISTIC 
# Logistic regression
logit.model = glm(y ~ .+duration:previous,family=binomial(link='logit'), data=data)
summary(logit.model)

glm.probs =predict(logit.model ,type="response")

#inf=influence.measures(logit.model) #Outliers after the modelization
# make predictions (posterior probabilities)
probability = predict(logit.model,newdata=datatest, type='response')
head(probability)
prediction = as.factor(ifelse(probability > 0.5,"yes","no"))
head(prediction)

# Performance: confusion matrix
confusionMatrix(prediction, datatest$y)


# Penalized logistic regression
# 1 repeat of 5-fold cross validation
ctrl = trainControl(method = "cv", number = 5,
                     classProbs = TRUE, 
                     verboseIter=T)

#Regression 
lrFit = train(y ~ .+duration:previous, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
               metric = "Kappa",
               data = data,
               preProcess = c("center", "scale"),
               trControl = ctrl)

print(lrFit)
lrPred = predict(lrFit, datatest)
confusionMatrix(lrPred, datatest$y)

###################### BAYES CLASSIFIERS NON CONSERVATIVE
# LDA
library(MASS)
prop.table(table(data$y))
lda.model = lda(y ~ .+duration:previous, data=data, prior = c(0.88, 0.12))
probability = predict(lda.model, newdata=datatest)$posterior

prediction = rep("no", nrow(datatest))
prediction[which(probability[,2] > 0.5)] = "yes"

# Produce a confusion matrix
confusionMatrix(as.factor(prediction), datatest$y)

#QDA
qda.model = qda(y ~ .+duration:previous, data=data, prior = c(0.88, 0.12))
probability = predict(qda.model, newdata=datatest)$posterior

prediction = rep("no", nrow(datatest))
prediction[which(probability[,2] > 0.5)] = "yes"

# Produce a confusion matrix
confusionMatrix(as.factor(prediction), datatest$y)

#Naive
library(e1071)
naive.model = naiveBayes(y ~ ., data=data, prior = c(0.88, 0.12))
probability = predict(naive.model, newdata=datatest)

# Produce a confusion matrix
confusionMatrix(probability, datatest$y)

###################### BAYES CLASSIFIERS CONSERVATIVE
# LDA
library(MASS)
prop.table(table(data$y))
lda.model = lda(y ~ .+duration:previous, data=data, prior = c(.5, .5))
probability = predict(lda.model, newdata=datatest)$posterior

prediction = rep("no", nrow(datatest))
prediction[which(probability[,2] > 0.5)] = "yes"

# Produce a confusion matrix
confusionMatrix(as.factor(prediction), datatest$y)

#QDA
qda.model = qda(y ~ .+duration:previous, data=data, prior = c(.5, .5))
probability = predict(qda.model, newdata=datatest)$posterior

prediction = rep("no", nrow(datatest))
prediction[which(probability[,2] > 0.5)] = "yes"

# Produce a confusion matrix
confusionMatrix(as.factor(prediction), datatest$y)

#Naive
library(e1071)
naive.model = naiveBayes(y ~ ., data=data, prior = c(.5, .5))
probability = predict(naive.model, newdata=datatest)

# Produce a confusion matrix
confusionMatrix(probability, datatest$y)


################### HYPERPARAMETER DETERMINATION
# ROC curve
library(pROC)
lrProb = predict(lrFit, datatest, type="prob")

plot.roc(datatest$y, lrProb[,2],col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE)

plot.roc(datatest$y, lrProb[,1],col="darkblue", print.auc = TRUE,  auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", print.thres=TRUE)

# Optimization of the hyper-parameters of the model using the ROC
ctrl = trainControl(method = "cv", number = 5,
                     classProbs = TRUE, 
                     summaryFunction=twoClassSummary,
                     verboseIter=T)

lrFit = train(y ~ .+duration:previous, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, 0.1, 0.01)),
               metric = "ROC",
               data = data,
               preProcess = c("center", "scale"),
               trControl = ctrl)
print(lrFit)
lrPred = predict(lrFit, datatest)
confusionMatrix(lrPred, datatest$y)

threshold = 0.13
lrProb = predict(lrFit, datatest, type="prob")
lrPred = rep("no", nrow(datatest))
lrPred[which(lrProb[,2] > threshold)] = "yes"
confusionMatrix(factor(lrPred), datatest$y)

#################################### COST SENTTIVITY ANALYSYS
#Cost-sensitive classifier with PLR
library(glmnet)
cost.unit = c(0, 1, 2, 1)
cost.i = matrix(NA, nrow = 100, ncol = 10)
j = 0


for (threshold in seq(0.1,1,0.1)){
  
  j = j + 1
  cat(j)
  for(i in 1:100){
    
    d = createDataPartition(data$y, p = 0.8, list = FALSE)
    # select training sample
    
    train = data[d,]
    test  = data[-d,]  
    
    #Creating penalized linear regression
    f = as.formula(y ~ .+duration:previous)
    y = train$y
    x = model.matrix(f, train)[, -1]
    
    lrFit =glmnet(x, y, family=c("binomial"), alpha=0.4, lambda=0)
    
    #Fitting new data
    x = model.matrix(f, test)[, -1]
    
    #Predict
    lrProb = predict(lrFit, x, type="response")
    prediction = as.factor(ifelse(lrProb > threshold,"yes","no"))

    CM =confusionMatrix(prediction, test$y)$table
    cost = sum(as.vector(CM)*cost.unit)/sum(CM)
    cost
    
    cost.i[i,j] = cost
    
  }
}

# Threshold optimization:
boxplot(cost.i, main = "Threshold selection",
        ylab = "unit cost",
        xlab = "threshold value",
        names = seq(0.1,1,0.1),col="royalblue2",las=2)

median_cost=apply(cost.i, 2, median)
mean_cost=apply(cost.i, 2, mean)

# Final prediction
f = as.formula(y ~ .+duration:previous)
y = data$y
x = model.matrix(f, data)[, -1]
threshold = 0.45

lrFit =glmnet(x, y, family=c("binomial"), alpha=0.4, lambda=0)

x = model.matrix(f, datatest)[, -1]

lrProb = predict(lrFit, x, type="response")
prediction = as.factor(ifelse(lrProb > threshold,"yes","no"))
confusionMatrix(factor(prediction),datatest$y)
CM = confusionMatrix(factor(prediction),datatest$y)$table
cost = sum(as.vector(CM)*cost.unit)/sum(CM)
cost

################# MACHINE LEARNING TOOLS ##################
#Cost unit
cost.unit = c(0, -1, -2, -1)

#Economic profict function as a metric
EconomicCost <- function(datat, lev = NULL, model = NULL) 
{
  y.pred = datat$pred 
  y.true = datat$obs
  CM = confusionMatrix(y.pred, y.true)$table
  out = sum(cost.unit*CM)/sum(CM)
  names(out) <- c("EconomicCost")
  out
}

ctrl <- trainControl(method = "cv", number = 5,
                     classProbs = TRUE, 
                     summaryFunction = EconomicCost,
                     verboseIter=T)

knnFit <- train(y ~ ., 
                method = "knn", 
                data = data,
                preProcess = c("center", "scale"),
                tuneLength = 3,
                metric = "EconomicCost",
                trControl = ctrl)
print(knnFit)
knnPred = predict(knnFit, datatest)
confusionMatrix(knnPred,datatest$y)
EconomicCost(data = data.frame(pred  = knnPred, obs = datatest$y))

