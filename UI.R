library(shiny)
{
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
  races<-c('European-American/White','African-American/Black','Hispanic/Latino','Asian/Pacific Islander','Other')
} 
ui <- fluidPage(
  headerPanel('Number of People Killed by the Police per Year'),
  sidebarPanel(
    selectInput('xcol', 'Race', races))
  ,
  mainPanel(
    plotOutput('plot1')
  )
  
)
server <- function(input,output){
  KilledPerYear<-c()
  y=2000
  while(y<2020){
    KilledPerYear<-append(KilledPerYear,sum(fatal_enc$Subject.s.race==input & fatal_enc$Date..Year==y))
    y=y+1
  }
  KilledPerYear
  years<-c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
  DF<-data.frame(KilledPerYear,years)
  plot(KilledPerYear ~ years, data=blackDF)
  predict.Deaths = lm(KilledPerYear ~ years, data=blackDF)
  coefficients(predict.Deaths)
  abline(predict.Deaths)
}
shinyApp(ui = ui, server = server)