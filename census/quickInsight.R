rm(list=ls())
getwd()
odf<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep=",", header=FALSE)
testdf <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", sep=",", header=FALSE)

library(pander)
library(ggplot2)

## Feature engineering01 - giving the correct names
fRawHeaderBackfill <- function(f){
  names(f)[names(f)=="V1"] <- 'age'
  names(f)[names(f)=="V2"] <- 'work_class'
  names(f)[names(f)=="V3"] <- 'fnlwgt'
  names(f)[names(f)=="V4"] <- 'education'
  names(f)[names(f)=="V5"] <- 'eduY'
  names(f)[names(f)=="V6"] <- 'mar_stat'
  names(f)[names(f)=="V7"] <- 'occupation'
  names(f)[names(f)=="V8"] <- 'relationship'
  names(f)[names(f)=="V9"] <- 'race'
  names(f)[names(f)=="V10"] <- 'sex'
  names(f)[names(f)=="V11"] <- 'cap_gain'
  names(f)[names(f)=="V12"] <- 'cap_loss'
  names(f)[names(f)=="V13"] <- 'hrpw'
  names(f)[names(f)=="V14"] <- 'nat_ctry'
  names(f)[names(f)=="V15"] <- 'mt50K'
  str(f)
  return(f)
}

testdf2 <- subset(testdf, testdf$V1!= "|1x3 Cross validator")
testdf2$V1 <- as.numeric(testdf2$V1)

df <- fRawHeaderBackfill(odf)
tdf <- fRawHeaderBackfill(testdf2)
testdf <- NULL
testdf2<- NULL
odf <- NULL

pander(summary(df))
pander(summary(tdf))


## Feature engineering02 - data cleaning
cdf <- subset(df, df$work_class!=" ?"&
                df$occupation!= " ?" &
                df$nat_ctry != " ?"
                )
ctdf <- subset(tdf, tdf$work_class!=" ?"&
                 tdf$occupation!= " ?" &
                 tdf$nat_ctry != " ?"
)

cdf$mt50K <- as.factor(ifelse(cdf$mt50K == " <=50K",0,1 ))
#ctdf$mt50K <- as.factor(ifelse(ctdf$mt50K == " <=50K",0,1 ))


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## continuous variables

h1 <- ggplot(cdf)+aes(x=age)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    title='Histogram of age distribution - 50 bins',
    x='Age groups',
    y='Number of citizens in a group'
  )+
  theme_bw()

h2 <- ggplot(cdf)+aes(x=eduY)+
  geom_histogram(bins=20, fill='dodgerblue3')+
  labs(
    title='Histogram of the years of education distribution - 20 bins',
    x='Years spent in education',
    y='Number of citizens in a group'
  )+
  theme_bw()

h3 <- ggplot(cdf)+aes(x=cap_gain)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    title='Histogram of distribution of capital gain - 50 bins',
    x='Capital gain groups',
    y='Number of citizens in a group'
  )+
  theme_bw()

h3B <- ggplot(cdf)+aes(x=cap_gain)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    title='Histogram of distribution of capital log gain - 50 bins',
    x='Capital gain groups of nonzero values - log scale ',
    y='Number of citizens in a group'
  )+
  scale_x_log10()+
  theme_bw()

h4 <- ggplot(cdf)+aes(x=cap_loss)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    title='Histogram of distribution of capital loss - 50 bins',
    x='Capital loss groups',
    y='Number of citizens in a group'
  )+
  theme_bw()

h4B <- ggplot(cdf)+aes(x=cap_loss)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    title='Histogram of distribution of capital log loss - 50 bins',
    x='Capital loss groups of nonzero values - log scale',
    y='Number of citizens in a group'
  )+
  scale_x_log10()+
  theme_bw()

multiplot(h1, h2, h3, h3B, h4, h4B, cols=3)

## factors


## work and education

fb1 <- ggplot(cdf)+aes(x=work_class,fill=work_class)+
  geom_bar() +
  labs(
    title='Distribution of different work classifications',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-60, hjust=0, size=8))


fb4 <- ggplot(cdf)+aes(x=occupation,fill=occupation)+
  geom_bar() +
  labs(
    title='Distribution of different occupations',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-60, hjust=0, size=8))

multiplot(fb1,  fb4, cols=2)

## personal data


fb2 <- ggplot(cdf)+aes(x=education,fill=education)+
  geom_bar() +
  labs(
    title='Distribution of different levels of education',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-60, hjust=0, size=8))

## 

fb3 <- ggplot(cdf)+aes(x=mar_stat,fill=mar_stat)+
  geom_bar() +
  labs(
    title='Distribution of different martial status',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-45, hjust=0, size=8))



fb5 <- ggplot(cdf)+aes(x=relationship,fill=relationship)+
  geom_bar() +
  labs(
    title='Distribution of different relationships',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-45, hjust=0, size=8))

fb6 <- ggplot(cdf)+aes(x=race,fill=race)+
  geom_bar() +
  labs(
    title='Distribution of different races',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-45, hjust=0, size=8))

fb7 <- ggplot(cdf)+aes(x=sex,fill=sex)+
  geom_bar() +
  labs(
    title='Distribution of different sexes',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  heme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-45, hjust=0, size=8))

multiplot(fb3,fb5, fb6, fb7, cols=2)


fb8 <- ggplot(cdf)+aes(x=nat_ctry,fill=nat_ctry)+
  geom_bar() +
  labs(
    title='Distribution of different native country',
    y='# of citizens',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-90, hjust=0, size=8))


fb8B <- ggplot(cdf)+aes(x=nat_ctry,fill=nat_ctry)+
  geom_bar() +
  scale_y_log10()+
  labs(
    title='Distribution of different native country',
    subtitle='Y axis on logarytmic scale',
    y='# of citizens\nlog',
    x=''
  )+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-90, hjust=0, size=8))

multiplot(fb8, fb8B, cols=2)


fb9 <- ggplot(cdf)+aes(x=mt50K,fill=mt50K)+
  geom_bar()+
  labs(
    title='Distribution of wage categories',
    subtitle='The output vairable of the modeling',
    y='# of citizens',
    x='Is the annual wage more than 50,000 USD?'
  )+
  theme_bw()+
  theme(legend.position="none")


## Joint distributions

### discrete variables

jd1<- ggplot(cdf)+aes(x=education, y=occupation)+
  geom_count(col='dodgerblue3')+
  labs(
    title='Joint distribution of levels of education\nand occupation',
    y='Occupation',
    x='level of education'
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, size=8))

jd2<- ggplot(cdf)+aes(x=relationship, y=occupation)+
  geom_count(col='dodgerblue3')+
  labs(
    title='Joint distribution of relationship status\nand occupation',
    y='Occupation',
    x='relationship status'
  )+
  theme_bw()
  
jd3 <- ggplot(cdf)+aes(x=education, y=work_class)+
  geom_count(col='dodgerblue3')+
  labs(
    title='Joint distribution of education\nand occupation',
    y='Work class',
    x='level of education'
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, size=8))

jd4 <- ggplot(cdf)+aes(x=mar_stat, y=relationship)+
  geom_count(col='dodgerblue3')+
  labs(
    title='Joint distribution of martial status\nand relationship',
    y='Relationship',
    x='martial status'
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, size=8))

multiplot(jd1, jd2, jd3, jd4, cols=2)


### continuous variables

jdc1 <- ggplot(cdf)+aes(x=age, y=log(cap_gain+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(1600,0),  colours=rainbow(4))+
  facet_wrap(~sex)+
  labs(
    title='Joint distribution of age and capital gain',
    subtitle='Faceted by sex',
    y='Capital gain\non log scale',
    x='Age'
  )+
  theme_bw()

jdc2 <- ggplot(cdf)+aes(x=age, y=log(cap_loss+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(1600,0),  colours=rainbow(4))+
  facet_wrap(~sex)+
  labs(
    title='Joint distribution of age and capital loss',
    subtitle='Faceted by sex',
    y='Capital loss\non log scale',
    x='Age'
  )+
  theme_bw()

multiplot(jdc1, jdc2, cols=1)

jdc3 <- ggplot(cdf)+aes(x=age, y=log(cap_gain+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(1600,0),  colours=rainbow(4))+
  facet_wrap(~work_class)+
  labs(
    title='Joint distribution of age and capital gain',
    subtitle='Faceted by work class',
    y='Capital gain\non log scale',
    x='Age'
  )+
  theme_bw()

jdc4 <- ggplot(cdf)+aes(x=age, y=log(cap_loss+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(1600,0),  colours=rainbow(4))+
  facet_wrap(~work_class)+
  labs(
    title='Joint distribution of age and capital loss',
    subtitle='Faceted by work class',
    y='Capital loss\non log scale',
    x='age'
  )+
  theme_bw()

ggplot(cdf)+aes(x=age, y=log(cap_gain+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(900,0),  colours=rainbow(4))+
  facet_wrap(~education)+
  labs(
    title='Joint distribution of age and capital gain',
    subtitle='Faceted by education',
    y='Capital gain\non log scale',
    x='Age'
  )+
  theme_bw()

ggplot(cdf)+aes(x=age, y=log(cap_loss+1))+
  geom_bin2d()+
  scale_fill_gradientn(limits=c(900,0),  colours=rainbow(4))+
  facet_wrap(~education)+
  labs(
    title='Joint distribution of age and capital gain',
    subtitle='Faceted by education',
    y='Capital loss\non log scale',
    x='Age'
  )+
  theme_bw()

cdf$fnlwgt <- NULL

#randomizing order of the rows
cdf$rnd <-runif(nrow(cdf))
cdf <- cdf[order(cdf$rnd),]
cdf$rnd <- NULL
cdfm <- cdf 
cdfm$lncg <- log(cdfm$cap_gain + 1)
cdfm$lncl <- log(cdfm$cap_loss + 1)
cdfm$cap_gain <- NULL
cdfm$cap_loss <- NULL

#70% training set 30% test set
train_nolog <- cdf[0:round( nrow(cdf) * 0.7 ),]
train_log <- cdfm[0:round( nrow(cdfm) * 0.7 ),]
test_nolog <- cdf[(round( nrow(cdf) * 0.7 )+1) : nrow(cdf),]
test_log <- cdfm[(round( nrow(cdfm) * 0.7 )+1) : nrow(cdfm),]


library(randomForest)
#install.packages('ROCR')
library(ROCR)

#Random forest on no log, 100 trees
rfmod_t100n <- randomForest(mt50K ~ .,data=train_nolog,ntree=100, importance=TRUE)
#Model details
pander(rfmod_t100n)



#Model validation 
phat <- predict(rfmod_t100n, test_nolog, type = "prob")[,"1"]
#Confusion matrix
pander(table(ifelse(phat>0.5,1,0),test_nolog$mt50K))
#Error rate
sum(ifelse(phat>0.5,1,0)!=test_nolog$mt50K)/nrow(test_nolog)
#ROC
rocr_obj <- prediction(phat, test_nolog$mt50K)
#cutoff vs error rate
proc <- performance(rocr_obj, "err")

#AUC
performance(rocr_obj, "auc")@y.values[[1]]    # AUC



layout(matrix(c(1,2,3,2), 2, 2 , byrow = TRUE),
       widths=c(1,2))
#Importance of variables
varImpPlot(rfmod_t100n, type=2)
#ROC curve
plot(performance(rocr_obj, "tpr", "fpr"), colorize=TRUE)
plot(proc)

d_phat <- data.frame(phat, mt50K = test_nolog$mt50K)
ggplot(d_phat) +
  geom_density( aes(x = phat, fill = mt50K, col=mt50K), alpha=0.4)+
  labs(
    title='Kernel density functions for the predicted values',
    x='Score',
    y='Density'
  )+
  theme_bw()



?randomForest

#Random forest on log, 100 trees
rfmod_t100l <- randomForest(mt50K ~ .,data=train_log,ntree=100, importance=TRUE)
#Model details
pander(rfmod_t100l)
#Importance of variables
iplot2 <- varImpPlot(rfmod_t100l, type=2)
phat2 <- predict(rfmod_t100l, test_log, type = "prob")[,"1"]
pander(table(ifelse(phat2>0.5,1,0),test_log$mt50K))
#Error rate
mod_rf_t100_log_errorrate <- sum(ifelse(phat2>0.5,1,0)!=test_log$mt50K)/nrow(test_log)
#ROC
rocr_obj2 <- prediction(phat2, test_log$mt50K)
auc2 <- performance(rocr_obj2, "auc")@y.values[[1]]   

#Random forest on level, 500 trees
rfmod_t500n <- randomForest(mt50K ~ .,data=train_nolog,ntree=500, importance=TRUE)
#Model details
pander(rfmod_t500n)
#Importance of variables
iplot3 <- varImpPlot(rfmod_t500n, type=2)
phat3 <- predict(rfmod_t500n, test_nolog, type = "prob")[,"1"]
pander(table(ifelse(phat3>0.5,1,0),test_nolog$mt50K))
#Error rate
mod_rf_t500_nolog_errorrate <- sum(ifelse(phat3>0.5,1,0)!=test_nolog$mt50K)/nrow(test_nolog)
#ROC
rocr_obj3 <- prediction(phat3, test_nolog$mt50K)
auc3 <- performance(rocr_obj3, "auc")@y.values[[1]] 

#Random forest on log, 500 trees
rfmod_t500l <- randomForest(mt50K ~ .,data=train_log,ntree=500, importance=TRUE)
#Model details
pander(rfmod_t500l)
#Importance of variables
iplot4 <- varImpPlot(rfmod_t500l, type=2)
phat4 <- predict(rfmod_t500l, test_log, type = "prob")[,"1"]
pander(table(ifelse(phat4>0.5,1,0),test_log$mt50K))
#Error rate
mod_rf_t500_log_errorrate <- sum(ifelse(phat4>0.5,1,0)!=test_log$mt50K)/nrow(test_log)
#ROC
rocr_obj4 <- prediction(phat4, test_log$mt50K)
auc4 <- performance(rocr_obj4, "auc")@y.values[[1]]

#Random forest on level, 500 trees, 5 variables
rfmod_t500n5 <- randomForest(mt50K ~ .,data=train_nolog,ntree=500, mtry=5, importance=TRUE)
#Model details
pander(rfmod_t500n5)
#Importance of variables
iplot5 <- varImpPlot(rfmod_t500n5, type=2)
phat5 <- predict(rfmod_t500n5, test_nolog, type = "prob")[,"1"]
pander(table(ifelse(phat5>0.5,1,0),test_nolog$mt50K))
#Error rate
mod_rf_t500_nolog_var5_errorrate <- sum(ifelse(phat5>0.5,1,0)!=test_nolog$mt50K)/nrow(test_nolog)
#ROC
rocr_obj5 <- prediction(phat5, test_nolog$mt50K)
auc5 <- performance(rocr_obj5, "auc")@y.values[[1]] 

str(phat5)

#Random forest on log, 500 trees, 5 variables
rfmod_t500l5 <- randomForest(mt50K ~ .,data=train_log,ntree=500, mtry=5, importance=TRUE)
#Model details
pander(rfmod_t500l5)
#Importance of variables
iplot6 <- varImpPlot(rfmod_t500l5, type=2)
phat6 <- predict(rfmod_t500l5, test_log, type = "prob")[,"1"]
pander(table(ifelse(phat5>0.5,1,0),test_log$mt50K))
#Error rate
mod_rf_t500_log_var5_errorrate <- sum(ifelse(phat6>0.5,1,0)!=test_log$mt50K)/nrow(test_log)
#ROC
rocr_obj6 <- prediction(phat6, test_log$mt50K)
auc6 <- performance(rocr_obj6, "auc")@y.values[[1]]

set.seed(123)


library(gbm)
train_nolog$mt50K <- ifelse(train_nolog$mt50K==1, 1, 0)
test_nolog$mt50K <- ifelse(test_nolog$mt50K==1, 1, 0) 
gbm100t <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01, cv.folds = 5)

yhat <- predict(gbm100t, test_nolog, n.trees = 100) 
summary(yhat)
t<- table(ifelse(yhat>0,1,0), test_nolog$mt50K)

gbm_perf_p1 <- gbm.perf(gbm100t, plot.it = TRUE)
rocr_obj_gbm1 <- prediction(yhat, test_nolog$mt50K)
auc_gbm1 <- performance(rocr_obj_gbm1, "auc")@y.values[[1]]

gbm_500_10_001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, interaction.depth = 10, shrinkage = 0.01, cv.folds = 5)
yhat2 <- predict(gbm_500_10_001_5, test_nolog, n.trees = 500) 
t2<- table(ifelse(yhat2>0,1,0), test_nolog$mt50K)

gbm_100_20_001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 100, interaction.depth = 20, shrinkage = 0.01, cv.folds = 5)
yhat3 <- predict(gbm_100_20_001_5, test_nolog, n.trees = 100) 
t3<- table(ifelse(yhat3>0,1,0), test_nolog$mt50K)

gbm_500_20_001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, interaction.depth = 20, shrinkage = 0.01, cv.folds = 5)
yhat4 <- predict(gbm_500_20_001_5, test_nolog, n.trees = 500) 
t4<- table(ifelse(yhat4>0,1,0), test_nolog$mt50K)

#gbm_100_10_0001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 100, interaction.depth = 10, shrinkage = 0.001, cv.folds = 5)
#yhat5 <- predict(gbm_100_10_0001_5, test_nolog, n.trees = 100) 
#t5<- table(ifelse(yhat5>0,1,0), test_nolog$mt50K)
#t5 #no positive findings

gbm_500_10_0001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, interaction.depth = 10, shrinkage = 0.001, cv.folds = 5)
yhat6 <- predict(gbm_500_10_0001_5, test_nolog, n.trees = 500) 
t6<- table(ifelse(yhat6>0,1,0), test_nolog$mt50K)

#gbm_100_20_0001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 100, interaction.depth = 20, shrinkage = 0.001, cv.folds = 5)
#yhat7 <- predict(gbm_100_20_0001_5, test_nolog, n.trees = 100) 
#t7<- table(ifelse(yhat7>0,1,0), test_nolog$mt50K)

gbm_500_20_0001_5 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, interaction.depth = 20, shrinkage = 0.001, cv.folds = 5)
yhat8 <- predict(gbm_500_20_0001_5, test_nolog, n.trees = 500) 
t8<- table(ifelse(yhat8>0,1,0), test_nolog$mt50K)

#throws error in mrarkdown, leaving out 
par(mfrow=c(2,3))
gbm.perf(gbm_p1, plot.it = TRUE)
gbm.perf(gbm_500_10_001_5, plot.it = TRUE)
gbm.perf(gbm_100_20_001_5, plot.it = TRUE)
gbm.perf(gbm_500_20_001_5, plot.it = TRUE)
gbm.perf(gbm_500_10_0001_5, plot.it = TRUE)
gbm.perf(gbm_500_20_0001_5, plot.it = TRUE)


## Checking some cross validation effects too
gbm_500_10_001_1 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, 
                        interaction.depth = 10, shrinkage = 0.01, cv.folds = 1)
yhat11 <- predict(gbm_500_10_001_1, test_nolog, n.trees = 500) 
t11<- table(ifelse(yhat11>0,1,0), test_nolog$mt50K)
rocr_obj11 <- prediction(yhat11, test_nolog$mt50K)
auc11 <- performance(rocr_obj11, "auc")@y.values[[1]]

gbm_500_10_001_10 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, 
                        interaction.depth = 10, shrinkage = 0.01, cv.folds = 10)
yhat12 <- predict(gbm_500_10_001_10, test_nolog, n.trees = 500) 
t12<- table(ifelse(yhat12>0,1,0), test_nolog$mt50K)

gbm_500_10_001_3 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, 
                         interaction.depth = 10, shrinkage = 0.01, cv.folds = 3)
yhat13 <- predict(gbm_500_10_001_3, test_nolog, n.trees = 500) 
t13<- table(ifelse(yhat12>0,1,0), test_nolog$mt50K)



gbm_500_20_001_1 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, 
                        interaction.depth = 20, shrinkage = 0.01, cv.folds = 1)
yhat21 <- predict(gbm_500_20_001_1, test_nolog, n.trees = 500) 
t21<- table(ifelse(yhat21>0,1,0), test_nolog$mt50K)

gbm_500_20_001_3 <- gbm(mt50K ~ . ,data=train_nolog, distribution = "bernoulli", n.trees = 500, 
                        interaction.depth = 20, shrinkage = 0.01, cv.folds = 3)
yhat22 <- predict(gbm_500_20_001_3, test_nolog, n.trees = 500) 
t22<- table(ifelse(yhat21>0,1,0), test_nolog$mt50K)

t11
t12
t21
t22
t4

?gbm

library(class)
?knn
str(train_nolog$mt50K)
train_nolog$mt50K <- as.factor(train_nolog$mt50K)
test_nolog$mt50K <- as.factor(test_nolog$mt50K)

cdf2 <- cdf

cdf2$work_class <- as.numeric(cdf2$work_class)
cdf2$education <- as.numeric(cdf2$education )
cdf2$mar_stat <- as.numeric(cdf2$mar_stat)
cdf2$occupation <- as.numeric(cdf2$occupation)
cdf2$relationship <- as.numeric(cdf2$relationship)
cdf2$race <- as.numeric(cdf2$race)
cdf2$sex <- as.numeric(cdf2$sex)
cdf2$nat_ctry <- as.numeric(cdf2$nat_ctry)
cdf2$rnd <- NULL
#cdf2$mt50K <- as.factor(cdf2$mt50K)
train_nolog2 <- cdf2[0:round( nrow(cdf2) * 0.7 ),]
test_nolog2 <- cdf2[(round( nrow(cdf2) * 0.7 )+1) : nrow(cdf2),]


str(fit2)
?knn
?predict



fit2 <- knn(train_nolog2[,1:13], test_nolog2[,1:13], train_nolog2$mt50K, k = 2, prob=TRUE)
pander(table(test_nolog2$mt50K,fit2))
prob<- attributes(fit2)$prob
rocr_obj_2nn1 <- prediction(ifelse(prob>0.5,1,0), test_nolog$mt50K)
auc_2nn1 <- performance(rocr_obj_2nn1, "auc")@y.values[[1]]

fit7 <- knn(train_nolog2[,1:13], test_nolog2[,1:13], train_nolog2$mt50K, k = 7, prob=TRUE)
pander(table(test_nolog2$mt50K,fit7))
yhat_knn7 <- fit7@values

prob  <- attr(fit7, "prob")
prob <- 2*ifelse(fit7 == "-1", 1-prob, prob) - 1

rocr_obj_7nn1 <- prediction(prob, test_nolog$mt50K)
auc_7nn1 <- performance(rocr_obj_7nn1, "auc")@y.values[[1]]


rocr_obj_gbm1 <- prediction(yhat, test_nolog$mt50K)
auc_gbm1 <- performance(rocr_obj_gbm1, "auc")@y.values[[1]]

str(rocr_obj_7nn1)

?prediction

fit13 <- knn(train_nolog2[,1:13], test_nolog2[,1:13], train_nolog2$mt50K, k = 13, prob=TRUE)
t13nn <- table(test_nolog2$mt50K,fit13)
prob13<- attributes(fit13)$prob
rocr_obj_13nn1 <- prediction(prob13, test_nolog$mt50K)
auc_13nn1 <- performance(rocr_obj_13nn1, "auc")@y.values[[1]]


library(e1071)
set.seed(123)

#rfmod_t100l <- randomForest(mt50K ~ .,data=train_log,ntree=100, importance=TRUE)
#phat5 <- predict(rfmod_t500n5, test_nolog, type = "prob")[,"1"]
#pander(table(ifelse(phat5>0.5,1,0),test_nolog$mt50K))
#mod_rf_t500_nolog_var5_errorrate <- sum(ifelse(phat5>0.5,1,0)!=test_nolog$mt50K)/nrow(test_nolog)
#rocr_obj5 <- prediction(phat5, test_nolog$mt50K)
#auc5 <- performance(rocr_obj5, "auc")@y.values[[1]] 

?svm
svm_base <- svm(mt50K ~ ., data = train_nolog,
          kernel = "radial", gamma = 1/(ncol(train_nolog)-1), cost = 1)

yhat_svm1 <- predict(svm_base, newdata = test_nolog)
sum(yhat_svm1!=test_nolog$mt50K)/nrow(test_nolog)

rocr_obj_svm1 <- prediction(as.numeric(yhat_svm1)-1, test_nolog$mt50K)
auc_svm1 <- performance(rocr_obj_svm1, "auc")@y.values[[1]]


#gamma = 2^seq(-7,-3,2), cost = 2^seq(1,5,2))
svm_2 <- svm(mt50K ~ ., data = train_nolog,
                kernel = "radial", gamma = 2^-7, cost = 2)
yhat_svm2 <- predict(svm_2, newdata = test_nolog)
sum(yhat_svm2!=test_nolog$mt50K)/nrow(test_nolog)

rocr_obj_svm2 <- prediction(as.numeric(yhat_svm2)-1, test_nolog$mt50K)
auc_svm2 <- performance(rocr_obj_svm2, "auc")@y.values[[1]]
auc_svm2


svm_3 <- svm(mt50K ~ ., data = train_nolog,
             kernel = "radial", gamma = 2^-3, cost = 2)
yhat_svm3 <- predict(svm_3, newdata = test_nolog)
sum(yhat_svm3!=test_nolog$mt50K)/nrow(test_nolog)
tsvm3<- table(ifelse(as.numeric(yhat_svm3)-1>0,1,0), test_nolog$mt50K)
rocr_obj_svm3 <- prediction(as.numeric(yhat_svm3)-1, test_nolog$mt50K)
auc_svm3 <- performance(rocr_obj_svm3, "auc")@y.values[[1]]
auc_svm3

svm_4 <- svm(mt50K ~ ., data = train_nolog,
             kernel = "radial", gamma = 2^-1, cost = 2)
yhat_svm4 <- predict(svm_4, newdata = test_nolog)
sum(yhat_svm4!=test_nolog$mt50K)/nrow(test_nolog)
rocr_obj_svm4 <- prediction(as.numeric(yhat_svm4)-1, test_nolog$mt50K)
auc_svm4 <- performance(rocr_obj_svm4, "auc")@y.values[[1]]
auc_svm4

svm_5 <- svm(mt50K ~ ., data = train_nolog,
             kernel = "radial", gamma = 2^-3, cost = 1)
yhat_svm5 <- predict(svm_5, newdata = test_nolog)
sum(yhat_svm5!=test_nolog$mt50K)/nrow(test_nolog)
rocr_obj_svm5 <- prediction(as.numeric(yhat_svm5)-1, test_nolog$mt50K)
auc_svm5 <- performance(rocr_obj_svm5, "auc")@y.values[[1]]
auc_svm5

svm_6 <- svm(mt50K ~ ., data = train_nolog,
             kernel = "radial", gamma = 2^-3, epsilon = 0.2)
yhat_svm6 <- predict(svm_6, newdata = test_nolog)
sum(yhat_svm6!=test_nolog$mt50K)/nrow(test_nolog)
rocr_obj_svm6 <- prediction(as.numeric(yhat_svm6)-1, test_nolog$mt50K)
auc_svm6 <- performance(rocr_obj_svm6, "auc")@y.values[[1]]
auc_svm6

# grid search
system.time({
  svm_base_grid <- tune.svm(mt50K ~ ., data = train_nolog,
                  gamma = 10^(-1:1), cost = 10^(-1:1), 
                  tunecontrol = tune.control(cross = 5))
})

?tune.svm

mds
summary(mds)
names(mds)
round(xtabs(error ~ gamma + cost, mds$performances)*100,2)
class(mds$best.model)
class(mds)

yhat <- predict(mds$best.model, newdata = d_test)
sum(yhat!=d_test$spam)/nrow(d_test)
