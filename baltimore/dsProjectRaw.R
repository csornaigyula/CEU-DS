rm(list=ls())

# CHECK WORKING DIRECTORY
setwd("..\\temp\\rWorkDir")
getwd()
library(ggplot2)
library(pander)
library(stringr)
df_orig<- read.csv(".\\ds\\BPD_Arrests.csv")

#########################################
#global vars
barPalette <- "Dark2"
dateFormat <- "%m/%d/%Y"
#########################################

#Metadata
#Arrest-ID  Age Sex Race
#ArrestDate  ArrestTime  ArrestLocation
#IncidentOffense IncidentLocation  Charge
#ChargeDescription District  Post
#Neighborhood  Location1(Location Coordinates)
summary(df_orig)
df <- df_orig

#offense
sort(table(df$IncidentOffense),decreasing=T)[1:15]
df$IncidentOffenseUC <- as.factor(toupper( as.character(df$IncidentOffense)))
##############################################################
#offense lookup
sort(table(df$IncidentOffenseUC),decreasing=T)
################################################################
df_old <- df
df <- subset(df, df$IncidentOffenseUC !="UNKNOWN OFFENSE")
#we have only 45000 observations, that can be used for modeling
#it is a big loss, but still a decent size
#let's see categories
pander(table(df$incOffCategory))


patternN <- "[0-9]{1,4}"
df$incOffCodeMain <-as.numeric(stringr::str_extract(df$IncidentOffenseUC,patternN))
df$incOffCodeMain[is.na(df$incOffCodeMain)]<- -1 

#categories 
df$incOffCategory <- 0 
#1- Risk of fire or explosion: Bomb, gas, many life in danger
patternExplosive <- "BOMB"
patternExplosive2 <- "GAS"
patternExplosive3 <- "BURNING"
patternExplosive4 <- "WAR"
df$incOffCategory[stringr::str_detect(df$IncidentOffenseUC,patternExplosive)] <- 1 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternExplosive2)] <- 1 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternExplosive3)] <- 1 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternExplosive4)] <- 1 
unique(subset(df,df$incOffCategory ==1)$IncidentOffenseUC)



#2- Risk of armed criminal: Fiream, knife, death, life in danger
patternArmed1 <- "GUN"
patternArmed2 <- "FIREARM"
patternArmed3 <- "ARMED"
patternArmed4 <- "MURDER"
patternArmed5 <- "FIREA"
patternArmed6 <- "KNIFE"
patternArmed7 <- "ASSAULT"
patternArmed8 <- "ASSLT"
patternArmed9 <- "WPN"
patternArmed10 <- "DEAD"
patternArmed11 <- "DEATH"
patternArmed12 <- "MANSL"
patternArmed13 <- "WEAP"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed1)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed2)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed3)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed4)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed5)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed6)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed7)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed8)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed9)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed10)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed11)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed12)] <- 2 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternArmed13)] <- 2 
unique(subset(df,df$incOffCategory ==2)$IncidentOffenseUC)



#3- narcotics - potential healthcare issues, higher possibility of firearms and violence
patternNarcotic1 <- "NARCOT"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternNarcotic1)] <- 3 
unique(subset(df,df$incOffCategory ==3)$IncidentOffenseUC)


#4- child offense, child abuse: health and psyche in risk
patternChild1 <- "CHILD ABUSE"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternChild1)] <- 4 
unique(subset(df,df$incOffCategory ==4)$IncidentOffenseUC)

#5- sexual offense and abuse: grownup health and psyche in risk
patternSexa1 <- "SEX"
patternSexa2 <- "RAPE"
patternSexa3 <- "FAMILY DISTURB"
patternSexa4 <- "PERV"
patternSexa5 <- "CARNAL KNOWLEDGE"
patternSexa6 <- "ABDU"
patternSexa7 <- "PLACING HANDS"
patternSexa8 <- "HUMAN TRAFF"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa1)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa2)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa3)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa4)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa5)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa6)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa7)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa8)] <- 5 
unique(subset(df,df$incOffCategory ==5)$IncidentOffenseUC)

#6- mental disorder: expert presence needed, armed forces maybe not
patternMental1 <- "MENTAL"
patternMental2 <- "DISORD"
patternMental3 <- "SUICIDE"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMental1)] <- 6 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMental2)] <- 6 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMental3)] <- 6 
unique(subset(df,df$incOffCategory ==6)$IncidentOffenseUC)


#7- fire: firefighter support may be needed, armed forces maybe nit
patternFire1 <- "FIRE"
patternFire2 <- "ARSON"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternFire1)] <- 7 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternFire2)] <- 7 
unique(subset(df,df$incOffCategory ==7)$IncidentOffenseUC)

#8- indjury, incident: medic needed, armed forces not necessary
patternMedic1 <- "INJ"
patternMedic2 <- "SICK"
patternMedic3  <- "CHILD NEGLECT"
patternMedic4 <- "BITE"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMedic1)] <- 8 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMedic2)] <- 8
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMedic3)] <- 8 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMedic4)] <- 8 
unique(subset(df,df$incOffCategory ==8)$IncidentOffenseUC)

#9- robbery, thieving: value estimator needed, securing of traces needed
patternRobb1 <- "ROBB"
patternRobb2 <- "BURG"
patternRobb3 <- "LARC"
patternRobb4 <- "STOL"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternRobb1)] <- 9
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternRobb2)] <- 9 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternRobb3)] <- 9 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternRobb4)] <- 9 
unique(subset(df,df$incOffCategory ==9)$IncidentOffenseUC)

#10- general, everything else - regular police force is OK
df$incOffCategory[df$incOffCategory == 0 ] <- 10
unique(subset(df,df$incOffCategory ==10)$IncidentOffenseUC)

sort(
  table(
    subset(df,df$incOffCategory ==10
    )$IncidentOffenseUC
  ),decreasing=T
)

df$incOffCategoryChar[df$incOffCategory == 1]<- as.character("RISK OF EXPLOSIVE")
df$incOffCategoryChar[df$incOffCategory == 2]<- as.character("RISK OF ARMS/WEAPONS")
df$incOffCategoryChar[df$incOffCategory == 3]<- as.character("NARCOTICS")
df$incOffCategoryChar[df$incOffCategory == 4]<- as.character("CHILD ABUSE")
df$incOffCategoryChar[df$incOffCategory == 5]<- as.character("SEXUAL OFFENSE")
df$incOffCategoryChar[df$incOffCategory == 6]<- as.character("MENTAL DISORDER")
df$incOffCategoryChar[df$incOffCategory == 7]<- as.character("FIRE/ARSON")
df$incOffCategoryChar[df$incOffCategory == 8]<- as.character("MEDICAL ISSUE")
df$incOffCategoryChar[df$incOffCategory == 9]<- as.character("ROBBERY/BURGLARY")
df$incOffCategoryChar[df$incOffCategory == 10]<- as.character("OTHER COMMON ISSUES")
df$incOffCategoryFactor <- as.factor(df$incOffCategoryChar)

ggplot(df)+aes(x=incOffCategoryFactor )+
  geom_bar(fill='olivedrab1', col='olivedrab')+
  labs(
    x="Distribution of categories",
    y="Number of issues in the category",
    title="Distribution of issues based on category"
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0))

#Age
df <- subset(df,!is.na(df$Age) )
zeroAge <- as.numeric(length(subset(df,df$Age == 0)))
df <- subset(df,df$Age >0 )
head(summary(df$Age))
table(summary(df$Age))
ggplot(df)+aes(x=Age)+
  geom_histogram(bins=((max(df$Age)-min(df$Age))/5),
                  fill='olivedrab1', col='olivedrab'
                 )+
  xlim(0,max(df$Age))+
  labs(
    x="Distribution of age",
    y="Number of criminals in the category",
    title="Age distribution of criminals with non-zero age"
  )+
  theme_bw()

df$AgeGroup[df$Age < 18 ] <- as.character("CHILD")
df$AgeGroup[df$Age >= 18 & df$Age < 25] <- as.character("YOUNG")
df$AgeGroup[df$Age >= 25 & df$Age < 45] <- as.character("ADULT")
df$AgeGroup[df$Age >= 45 & df$Age < 60] <- as.character("MIDAGE")
df$AgeGroup[df$Age >= 60 ] <- as.character("SENIOR")
df$AgeGroup <- as.factor(df$AgeGroup)

ggplot(df)+aes(x=AgeGroup, fill=AgeGroup)+
  geom_bar()+
  labs(
    x="Distribution of age groups",
    y="Number of criminals in the category",
    title="Distribution of criminals based on age group"
  )+
  scale_fill_brewer(palette=barPalette)+
  scale_x_discrete(limits=c('CHILD','YOUNG','ADULT','MIDAGE','SENIOR'))+
  theme_bw()

#Palette selection: RColorBrewer::display.brewer.all()
pander(table(df$Sex))
summary(df$Sex)
ggplot(df)+aes(x=Sex, fill=Sex)+
  geom_bar()+
  labs(
    x="Distribution of sex",
    y="Number of criminals in the category",
    title="Distribution of criminals based on sex"
  )+
  scale_fill_brewer(palette=barPalette)+
  theme_bw()

pander(table(df$Race))
summary(df$Race)
ggplot(df)+aes(x=Race, fill=Race)+
  geom_bar()+
  labs(
    x="Distribution of race",
    y="Number of criminals in the category",
    title="Distribution of criminals based on race"
  )+
  scale_fill_brewer(palette=barPalette)+
  theme_bw()


## will not use date directly
df$ArrestWeekOfYear <- as.factor(format(strptime(df$ArrestDate,dateFormat),"%V"))
summary(df$ArrestWeekOfYear)
ggplot(df)+aes(x=ArrestWeekOfYear )+
  geom_bar(fill='olivedrab1', col='olivedrab')+
  labs(
    x="week of the year",
    y="Number of criminals in the category",
    title="Distribution crime issues per week of year"
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0))

df$ArrestDayOfWeekNumeric <- as.numeric(format(strptime(df$ArrestDate,dateFormat),"%w"))
summary(df$ArrestDayOfWeekNumeric)
df$ArrestDayOfWeek <- as.factor(format(strptime(df$ArrestDate,dateFormat),"%A"))

summary(df$ArrestDayOfWeek)
ggplot(df)+aes(x=ArrestDayOfWeek,fill=ArrestDayOfWeek)+
  geom_bar()+
  labs(
    x="day of week",
    y="Number of criminals in the category",
    title="Distribution crime issues per day of week"
  )+
  scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))+
  scale_fill_brewer(palette=barPalette)+
  theme_bw()


##btw, this cannot be used well, as the year in the scenario is not past
df$ArrestYear <- as.factor(format(strptime(df$ArrestDate,dateFormat),"%Y"))
summary(df$ArrestYear)
ggplot(df)+aes(x=ArrestYear,fill=ArrestYear)+
  geom_bar()+
  labs(
    x="year",
    y="Number of criminals in the category",
    title="Distribution crime issues per year"
  )+
  scale_fill_brewer(palette=barPalette)+
  theme_bw()

##part of day
patternHour <- "[0-9]{1,2}"
df$ArrestHour <- as.factor(as.numeric(stringr::str_match(df$ArrestTime,patternHour)))
summary(df$ArrestHour)
ggplot(df)+aes(x=ArrestHour)+
  geom_bar( fill='olivedrab1', col='olivedrab')+
  labs(
    x="hour",
    y="Number of criminals in the category",
    title="Distribution crime issues per year"
  )+
  theme_bw()



pander(table(df$District))
summary(df$District)
df$DistrictChar <- as.character(df$District)
df$DistrictChar[(df$DistrictChar=="")|(df$DistrictChar==" ")|is.na(df$DistrictChar)]<-"UNKNOWN"
df <- subset(df, df$DistrictChar!="UNKNOWN")
df$District <- as.factor(df$DistrictChar)
ggplot(df)+aes(x=District, fill=District)+
  geom_bar()+
  labs(
    x="Distribution of districts",
    y="Number of criminals in the category",
    title="Distribution of criminals based on district"
  )+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0))



unique(df$Neighborhood)
df$NeighborhoodUC <- toupper(df$Neighborhood)
unique(df$NeighborhoodUC)

ggplot(df)+aes(x=NeighborhoodUC)+
  geom_bar(fill='olivedrab1', col='olivedrab')+
  labs(
    x="Distribution of neighborhoods",
    y="Number of issues in the category",
    title="Distribution of issues based on neighborhood"
  )+
  theme_bw()+
  theme(axis.text.x=element_blank())

df$NeighborhoodUCF <- as.factor(df$NeighborhoodUC)

#incident location
sort(table(df$IncidentLocation),decreasing=T)
df$IncidentLocationChar <- as.character(df$IncidentLocation)
df <- (subset(df, nchar(df$IncidentLocationChar)!=0))
df$IncidentLocationChar <- toupper(df$IncidentLocationChar)
patternHouseNumber <- "[0-9]{1,6}"

#separating house numbers, if any
df$IncidentLocationHN <-  as.character(stringr::str_extract(df$ArrestLocation,patternHouseNumber))
table(df$IncidentLocationHN)
sort(
  table(
    subset(df,nchar(df$IncidentLocationHN) ==0
    )$IncidentLocationChar
  ),decreasing=T
)

#separating streets
df$IncidentLocationPLC <- substring(df$IncidentLocationChar,
                                    nchar(df$IncidentLocationHN)+2,
                                    nchar(df$IncidentLocationChar))

df$IncidentLocationPLC <- gsub(" AVE"," AV",df$IncidentLocationPLC)
df$IncidentLocationPLC <- gsub(" WAY"," WY",df$IncidentLocationPLC)

df$IncidentLocationHN <- as.factor(df$IncidentLocationHN)
df$IncidentLocationPLC <- as.factor(df$IncidentLocationPLC)

df<-subset(df, !is.na(df$IncidentLocationHN)&!is.na(df$IncidentLocationPLC)) 

dfm <- df
dfm$Arrest <- NULL
dfm$Age <- NULL
dfm$ArrestLocation <- NULL
dfm$IncidentLocationChar <- NULL
dfm$incOffCodeMain <- NULL
dfm$Post <- NULL
dfm$incOffCategoryChar <- NULL
dfm$ArrestDayOfWeekNumeric <- NULL
dfm$Charge <- NULL #this comes after arrest
dfm$ChargeDescription <- NULL #this comes after arrest
dfm$ArrestLocationChar <- NULL
dfm$Location.1 <- NULL
dfm$ArrestLocationLength <- NULL
dfm$IncidentLocation <- NULL
dfm$ArrestDate <- NULL
dfm$DistrictChar <- NULL
dfm$INCCODE <- as.factor(dfm$incOffCategory)
dfm$incOffCategory <- NULL
dfm$ArrestTime <- NULL
dfm$IncidentOffense <- NULL
dfm$IncidentOffenseUC <- NULL
dfm$Neighborhood <- NULL
dfm$NeighborhoodUC <- NULL
dfm$ArrestYear <- NULL

ggplot(dfm)+aes(x=IncidentLocationHN)+geom_bar()+theme(axis.text.x=element_text(angle=-90, hjust=0))
dfm$HNFN <- round(as.numeric(dfm$IncidentLocationHN ) / 10)
ggplot(dfm)+aes(x=HNFN)+geom_histogram()
dfm$IncidentLocationHN10 <- as.factor(dfm$HNFN)
dfm$IncidentLocationHN <- NULL
dfm$HNFN <- NULL
dfm$Offense <- dfm$incOffCategoryFactor
dfm$incOffCategoryFactor <- NULL
dfm$IncidentLocationPLC <- NULL

table(dfm$NeighborhoodUCF)


ggplot(dfm)+aes(x=ArrestWeekOfYear, y=ArrestHour)+
  geom_count(col='indianred')+
  labs(
    title="Distribution of issues in different part of the year")+
  theme_bw()

ggplot(dfm)+aes(x=ArrestHour, y=District)+
  geom_count(col='indianred')+
  labs(
    title="Distribution of issues in")+
  theme_bw()

ggplot(dfm)+aes(x=ArrestHour, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")

ggplot(dfm)+aes(x=Neighborhood, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(axis.text.x=element_blank())

ggplot(dfm)+aes(x=ArrestWeekOfYear, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0))+
  scale_fill_brewer(palette="Set3")


raredfm <- subset(dfm, dfm$Offense!="NARCOTICS" & 
                    dfm$Offense!="RISK OF ARMS/WEAPONS" &
                    dfm$Offense!="OTHER COMMON ISSUES" &
                    dfm$Offense!="ROBBERY/BURGLARY")


ggplot(raredfm)+aes(x=ArrestWeekOfYear, y=ArrestHour)+
  geom_count(col='indianred')+
  labs(
    title="Distribution of RARE issues in different part of the year")+
  theme_bw()

ggplot(raredfm)+aes(x=ArrestHour, y=District)+
  geom_count(col='indianred')+
  labs(
    title="Distribution of issues in")+
  theme_bw()

ggplot(raredfm)+aes(x=ArrestHour, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")

ggplot(raredfm)+aes(x=Neighborhood, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(axis.text.x=element_blank())

ggplot(raredfm)+aes(x=ArrestWeekOfYear, fill=Offense)+
  geom_bar(position = "stack")+
  labs(
    title="Distribution of issues in")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0))+
  scale_fill_brewer(palette="Set3")
#this world is disgusting: outstanding child abuses around Christmas


### Data munging ends when Rome did
## Here comes the modeling and machine learning
##Planned: RF, GBM, KNN, LogReg, Lasso
dfm$rndid <-runif(dim(dfm)[1]) 
dfm <- dfm[order(dfm$rndid),]

#50-25-25 splitup
trainS <- dfm[1:(round((dim(dfm)[1])*0.5)),]
validS <- dfm[(round((dim(dfm)[1])*0.5)+1):(round((dim(dfm)[1])*0.75)),]
testS <- dfm[(round((dim(dfm)[1])*0.75)+1):(dim(dfm)[1]),]

trainS$rndid <- NULL
testS$rndid <- NULL
validS$rndid <- NULL
trainS$Offense <- NULL
testS$Offense <- NULL
validS$Offense <- NULL

#random forest
#install.packages("randomForest")
library(randomForest)
summary(trainS)
#rfModel <- randomForest(Offense ~ .,data=trainS,ntree=100)
#ok, so random forest cannot handle more than 53 levels of a category
#for the moment I need to drop Neiborhood, as that cannot be 
#coupled with substring of the place, but in this form it cannot be used
trainS$NeighborhoodUCF <- NULL
testS$NeighborhoodUCF <- NULL
validS$NeighborhoodUCF <- NULL



rfModel <- randomForest(INCCODE ~ .,data=trainS,ntree=100, importance=TRUE)
?varImpPlot
summary(rfModel)
plot(rfModel)
varImpPlot(rfModel)


phat <- predict(rfModel, testS,type="prob")
phat2 <- predict(rfModel, testS)
phat2
?predict
str(phat)

install.packages("caret")
library(caret)
confusionMatrix(data=phat2,
                reference=testS$INCCODE,
                positive='yes')

cm = as.matrix(table(Actual = testS$INCCODE, Predicted = phat))

