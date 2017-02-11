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

df <- subset(df_orig,!is.na(df_orig$Age) )
zeroAge <- as.numeric(length(subset(df,df$Age == 0)))
df <- subset(df,df$Age >0 )

#Age
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
df$ArrestWeekOfYear <- as.numeric(format(strptime(df$ArrestDate,dateFormat),"%V"))
summary(df$ArrestWeekOfYear)
ggplot(df)+aes(x=ArrestWeekOfYear )+
  geom_bar(fill='olivedrab1', col='olivedrab')+
  labs(
    x="week of the year",
    y="Number of criminals in the category",
    title="Distribution crime issues per week of year"
  )+
  theme_bw()

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

## Location - street only, with regular expressions
summary(df$ArrestLocation)
summary(nchar(as.character( df$ArrestLocation)))
df$ArrestLocationLength <- nchar(as.character( df$ArrestLocation))
df$ArrestLocation[is.na(df$ArrestLocation)] <- as.factor("UNKNOWN")

df$ArrestLocationChar <- as.character(df$ArrestLocation)
df$ArrestLocationChar[(df$ArrestLocationChar=="")|is.na(df$ArrestLocation)]<-"UNKNOWN" 
df$ArrestLocation <- as.factor(df$ArrestLocationChar)
summary(df$ArrestLocation)

sort(table(df$ArrestLocation),decreasing=T)[1:15]

pattern <- "[^0-9]+"
pattern2 <- "[0-9]{1,6}"

df$ArrestLocationStr0 <- regmatches(df$ArrestLocation,regexpr(pattern,df$ArrestLocation))




df$ArrestLocationStr1 <- as.factor(stringr::str_extract(df$ArrestLocation,pattern2))
summary(df$ArrestLocationStr1)
df$ArrestLocationStr2 <- as.factor(gsub(" ", "",df$ArrestLocationStr0))
summary(df$ArrestLocationStr2)

#offense
sort(table(df$IncidentOffense),decreasing=T)[1:15]
df$IncidentOffenseUC <- as.factor(toupper( as.character(df$IncidentOffense)))
##############################################################
#offense lookup
sort(table(df$IncidentOffenseUC),decreasing=T)
################################################################
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
patternArmed5 <- "FIREAR"
patternArmed6 <- "KNIFE"
patternArmed7 <- "ASSAULT"
patternArmed8 <- "ASSLT"
patternArmed9 <- "WPN"
patternArmed10 <- "DEAD"
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
patternSexa3 <- "FAMILY DISTURBANCE"
patternSexa4 <- "PERV"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa1)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa2)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa3)] <- 5 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternSexa4)] <- 5 

unique(subset(df,df$incOffCategory ==5)$IncidentOffenseUC)

#6- mental disorder: expert presence needed
patternMental1 <- "MENTAL"
patternMental2 <- "DISORDERLY PERSON"
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMental1)] <- 6 
df$incOffCategory[df$incOffCategory == 0 & stringr::str_detect(df$IncidentOffenseUC,patternMental2)] <- 6 
unique(subset(df,df$incOffCategory ==6)$IncidentOffenseUC)

sort(
  table(
    subset(df,df$incOffCategory ==0
    )$IncidentOffenseUC
  ),decreasing=T
)


?str_detect

as.numeric(stringr::str_extract(df$IncidentOffenseUC,patternN))





