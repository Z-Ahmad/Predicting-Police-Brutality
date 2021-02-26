fatal_enc<-read.csv("~/Desktop/EAS 345/Term Project/fatal_encounters_dot_org.csv",stringsAsFactors=FALSE)

#shootings_wp<-read.csv(file.choose())
summary(fatal_enc)
fatal_enc$URL.of.image.of.deceased=NULL #not needed
fatal_enc[1]=NULL#UniqueID not needed
fatal_enc$Unique.ID.formula
fatal_enc[14:15]=NULL #latitude and longitude not needed
fatal_enc[13]=NULL
fatal_enc$Subject.s.race #many Race unspecified

#replace empty age with 0
library(dplyr)

fatal_enc$Subject.s.age
fatal_enc$Subject.s.age<-ifelse(fatal_enc$Subject.s.age=='',0,fatal_enc$Subject.s.age) 
fatal_enc$Subject.s.age

#replace unspecified race with imputated race, if imputation probabilty is >0.5
fatal_enc$Subject.s.race
summary(fatal_enc)
fatal_enc$Subject.s.race<-ifelse(fatal_enc$Subject.s.race=='Race unspecified' & fatal_enc$Imputation.probability > 0.5,fatal_enc$Subject.s.race.with.imputations,fatal_enc$Subject.s.race)
fatal_enc$Subject.s.race

fatal_enc<-fatal_enc[!(fatal_enc$Subject.s.race=='Race unspecified'),] #delete rows with race still unspecified, cant use that data
fatal_enc<-fatal_enc[!(is.na(fatal_enc$Subject.s.race)),]#delete any NA values, race data should be clean now

age<-as.numeric(fatal_enc$Subject.s.age)
age
hist(age, main="Age Histogram",  xlab= "Age",col=rainbow(19))
boxplot(age,main="Age Box Plot")
summary(age)

race<-fatal_enc$Subject.s.race
race
summary(race)
numOfWhite=sum(race=='European-American/White')
numOfBlack=sum(race=='African-American/Black')
numOfAsian=sum(race=='Asian/Pacific Islander')
numOfHisp=sum(race=='Hispanic/Latino')
numOfOther=sum(race!='European-American/White' & race!='African-American/Black' & race != 'Asian/Pacific Islander' & race != 'Hispanic/Latino')
numOfWhite
numOfBlack
numOfAsian
numOfHisp
numOfOther
raceTotals<-c(numOfWhite,numOfBlack,numOfHisp,numOfAsian,numOfOther)
summary(raceTotals)
names(raceTotals)<-c('European-American/White','African-American/Black','Hispanic/Latino','Asian/Pacific Islander','Other')
head(raceTotals)
raceTotals
pie(raceTotals,col=rainbow(5))


library(ggplot2)
ggplot(fatal_enc, mapping = aes(Location.of.death..state.,Subject.s.race)) +geom_count(color="darkblue") +ggtitle("Total Number of People Killed by Police by Race and State")+
  xlab("States") + ylab("Race")

KilledPerYear<-c()
y=2000
while(y<=2020){
  KilledPerYear<-append(KilledPerYear,sum(fatal_enc$Subject.s.race=='African-American/Black' & fatal_enc$Date..Year==y))
  y=y+1
}

KilledPerYear
years<-c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
blackDF<-data.frame(KilledPerYear,years)
blackDF

ggplot(blackDF,mapping=aes(years,KilledPerYear)) +geom_point(color='darkblue') +ggtitle("Number of Black People Killed by the Police Per Year")+
  xlab("Years") + ylab("Number of Black People Killed")
plot(KilledPerYear ~ years, data=blackDF)
predict.Deaths = lm(KilledPerYear ~ years, data=blackDF)
coefficients(predict.Deaths)
abline(predict.Deaths)

newDF<-data.frame(years=c(2020,2021,2022,2023,2024,2025))
predict(predict.Deaths,newDF)
summary(predict.Deaths)
plot(predict.Deaths)





police_killings<-read.csv("~/Desktop/EAS 345/Term Project/police_killings_MPV.csv",stringsAsFactors=FALSE)
police_killings<-police_killings[!(police_killings$Victim.s.race=='Unknown race'),]
police_killings$Victim.s.race
police_killings$Victim.s.age<-ifelse(police_killings$Victim.s.age=='Unknown',0,police_killings$Victim.s.age) 
police_killings$Victim.s.age

uniqueNames<-unique(c(fatal_enc$Subject.s.name,police_killings$Victim.s.name))
uniqueNames
c(fatal_enc$Subject.s.name,police_killings$Victim.s.name)
states<-unique(fatal_enc$Location.of.death..state.)
states<-sort(states)
states<-tail(states,-1)
blackDF
blackDF[,1]

