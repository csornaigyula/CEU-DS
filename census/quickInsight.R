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




names(df)[names(df)=="X..50K"] <- 'mt50k'
names(df)
library(randomForest)
randomForest(mt50k ~ .,data=df,ntree=100, importance=TRUE)


install.packages('gbm')
library(gbm)
df$isMT50k <- ifelse(df$mt50k == '<=50K',1,0)
head(subsetdf)
df$mt50k <- NULL
gbm <- gbm(isMT50k ~ .,data=df, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01)
install.packages('e1071')
library(e1071)
md <- svm(isMT50k ~ ., data = df,
          kernel = "radial", gamma = 'default', cost = 1)
?svm