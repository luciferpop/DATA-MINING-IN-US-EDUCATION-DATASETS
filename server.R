library(RSQLite)
library(dplyr)
library(ggvis)
library(shiny)
library(magrittr)
library(ggplot2)
library(tidyr)

# connect to the database
db <- dbConnect(dbDriver("SQLite"), "database.sqlite")
dbGetQuery(db, "PRAGMA temp_store=2;")

# read csv file
df <- read.csv("MERGED2013_PP.csv", na.strings = "NULL")

getSAT <- function() {
    sat <- dbGetQuery(db, "
    SELECT INSTNM College,
           SATMTMID Math,
           SATVRMID Verbal,
           SATWRMID Writing
    FROM Scorecard
    WHERE Year=2013
          AND SATMTMID IS NOT NULL
          AND SATMTMID != 'PrivacySuppressed'
          AND SATVRMID IS NOT NULL
          AND SATVRMID != 'PrivacySuppressed'
          AND SATWRMID IS NOT NULL
          AND SATWRMID != 'PrivacySuppressed'")

    return(sat)
}

plotSAT <- function(sat) {
    ggplot(sat %>% gather(Section, Score, -College), aes(x=Score, color=Section, fill=Section, group=Section)) +
    geom_density(alpha=0.3) +
    theme_light(base_size=16) +
    xlab("SAT Score") +
    ylab("")
}

printSAT <- function(sat) {
  math <- summary(sat$Math)
  verbal <- summary(sat$Verbal)
  writing <- summary(sat$Writing)
  print("Math")
  print(math)
  print("Verbal")
  print(verbal)
  print("Writing")
  print(writing)
}

getACT <- function() {
  act <- dbGetQuery(db, "
  SELECT INSTNM College,
         ACTCMMID Cumulative,
         ACTENMID English,
         ACTMTMID Math,
         ACTWRMID Writing
  FROM Scorecard
  WHERE Year=2013
         AND ACTCMMID IS NOT NULL
         AND ACTCMMID != 'PrivacySuppressed'
         AND ACTENMID IS NOT NULL
         AND ACTENMID != 'PrivacySuppressed'
         AND ACTMTMID IS NOT NULL
         AND ACTMTMID != 'PrivacySuppressed'
         AND ACTWRMID IS NOT NULL
         AND ACTWRMID != 'PrivacySuppressed'
         ")
}

plotACT <- function(act) {
  ggplot(act %>% gather(Section, Score, -College), aes(x=Score, color=Section, fill=Section, group=Section)) +
    geom_density(alpha=0.3) +
    theme_light(base_size=16) +
    xlab("ACT Score") +
    ylab("")
}

printACT <- function(act) {
  cm <- summary(act$Cumulative)
  en <- summary(act$English)
  mt <- summary(act$Math)
  wr <- summary(act$Writing)
  print("Cumulative")
  print(cm)
  print("English")
  print(en)
  print("Math")
  print(mt)
  print("Writing")
  print(wr)
}

getADM <- function() {
  adm <- dbGetQuery(db, "
    SELECT  INSTNM College,
            ADM_RATE Admission
    FROM Scorecard
    WHERE Year = 2013
    AND ADM_RATE IS NOT NULL
    ")

  return(adm)
}

plotADM <- function(adm) {
    ggplot(adm %>% gather(Section, Score, -College), aes(x=Score, color=Section, fill=Section, group=Section)) +
    geom_density(alpha=0.3) +
    theme_light(base_size=16) +
    xlab("Admission Rate") +
    ylab("")
}

printADM <- function(adm) {
  rate = summary(adm$Admission)
  print("Admission Rate")
  print(rate)
}

getENRO <- function() {
  enrollment <- dbGetQuery(db, "
  SELECT INSTNM College,
         UGDS UndergradEnrollment,
         CONTROL CollegeType
  FROM Scorecard
  WHERE Year=2013
        AND PREDDEG='Predominantly bachelor''s-degree granting'
        AND CCBASIC NOT LIKE '%Special Focus%'
        AND UGDS IS NOT NULL
        AND UGDS>0
  ORDER BY UGDS DESC")

  enrollment <- cbind(Rank=1:nrow(enrollment), enrollment)
  enrollment$College <- paste(enrollment$Rank, enrollment$College, sep=". ")
  enrollment$College <- factor(enrollment$College, levels=rev(enrollment$College))

  return(enrollment)
}

plotENRO <- function(enro) {
  ggplot(enro, aes(x=UndergradEnrollment, color=CollegeType, fill=CollegeType, group=CollegeType)) +
    geom_density(alpha=0.3) +
    theme_light(base_size=16) +
    xlab("Undergraduate Enrollment") + ylab("") +
    xlim(0, 20000)
}

printENRO <- function() {
  public <- dbGetQuery(db, "
  SELECT INSTNM College, 
     UGDS UndergradEnrollment
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Public'
    AND PREDDEG = 'Predominantly bachelor''s-degree granting'
      AND CCBASIC NOT LIKE '%Special Focus%'
      AND UGDS IS NOT NULL
      AND UGDS>0
  ")

  private_p <- dbGetQuery(db, "
  SELECT INSTNM College, 
     UGDS UndergradEnrollment
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Private for-profit'
    AND PREDDEG = 'Predominantly bachelor''s-degree granting'
      AND CCBASIC NOT LIKE '%Special Focus%'
      AND UGDS IS NOT NULL
      AND UGDS>0
  ")

  private_np <- dbGetQuery(db, "
  SELECT INSTNM College, 
     UGDS UndergradEnrollment
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Private nonprofit'
    AND PREDDEG = 'Predominantly bachelor''s-degree granting'
      AND CCBASIC NOT LIKE '%Special Focus%'
      AND UGDS IS NOT NULL
      AND UGDS>0
  ")

  p = summary(public)
  pp = summary(private_p)
  pnp = summary(private_np)

  print("Public")
  print(p)
  print("Private for-profit")
  print(pp)
  print("Private nonprofit")
  print(pnp)
}

getCOST <- function() {
  cost <- dbGetQuery(db, "
  SELECT INSTNM College,
         COSTT4_A Cost,
         CONTROL CollegeType
  FROM Scorecard
  WHERE Year=2013
    AND PREDDEG='Predominantly bachelor''s-degree granting'
    AND CCBASIC NOT LIKE '%Special Focus%'
    AND COSTT4_A IS NOT NULL
  ORDER BY COSTT4_A DESC")
  cost <- cbind(Rank=1:nrow(cost), cost)
  cost$College <- paste(cost$Rank, cost$College, sep=". ")
  cost$College <- factor(cost$College, levels=rev(cost$College))

  return(cost)
}

plotCOST <- function(cost) {
  ggplot(cost, aes(x=Cost, color=CollegeType, fill=CollegeType, group=CollegeType)) +
    geom_density(alpha=0.3) +
    theme_light(base_size=16) +
    xlab("Cost of Attendance") + ylab("")
}

printCOST <- function() {
  public <- dbGetQuery(db, "
  SELECT INSTNM College, 
     COSTT4_A Cost
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Public'
    AND PREDDEG='Predominantly bachelor''s-degree granting'
    AND CCBASIC NOT LIKE '%Special Focus%'
    AND COSTT4_A IS NOT NULL
  ")

  private_p <- dbGetQuery(db, "
  SELECT INSTNM College, 
     COSTT4_A Cost
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Private for-profit'
    AND PREDDEG='Predominantly bachelor''s-degree granting'
    AND CCBASIC NOT LIKE '%Special Focus%'
    AND COSTT4_A IS NOT NULL
  ")

  private_np <- dbGetQuery(db, "
  SELECT INSTNM College, 
     COSTT4_A Cost
  FROM Scorecard
  WHERE Year = 2013
    AND CONTROL = 'Private nonprofit'
    AND PREDDEG='Predominantly bachelor''s-degree granting'
    AND CCBASIC NOT LIKE '%Special Focus%'
    AND COSTT4_A IS NOT NULL
  ")

  p = summary(public)
  pp = summary(private_p)
  pnp = summary(private_np)

  print("Public")
  print(p)
  print("Private for-profit")
  print(pp)
  print("Private nonprofit")
  print(pnp) 
}

plotEARN <- function() {
  earnings <- dbGetQuery(db, "
  SELECT s11.INSTNM College,
         s11.CONTROL CollegeType,
         s11.md_earn_wne_p10 e50,
         s11.pct10_earn_wne_p10 e10,
         s11.pct25_earn_wne_p10 e25,
         s11.pct75_earn_wne_p10 e75,
         s11.pct90_earn_wne_p10 e90
  FROM Scorecard s11
  -- We have to do a self-join because the CCBASIC metadata is only attached to 2013 data
  -- And 2013 data has no 10 year out earnings data
  INNER JOIN Scorecard s13 ON s11.UNITID=s13.UNITID
  WHERE s11.Year=2011
    AND s13.Year=2013
    AND s11.pct75_earn_wne_p10 IS NOT NULL
    AND s11.pct75_earn_wne_p10 != 'PrivacySuppressed'
    AND s11.PREDDEG = 'Predominantly bachelor''s-degree granting'
  --Filter out medical schools and the like that are mislabeled as predominantly bachelor's-degree granting
    AND s13.CCBASIC NOT LIKE '%Special%'
  ORDER BY s11.pct75_earn_wne_p10 DESC")
  earnings <- cbind(Rank=1:nrow(earnings), earnings)
  earnings$College <- paste(earnings$Rank, earnings$College, sep=". ")
  earnings$College <- factor(earnings$College, levels=rev(earnings$College))

  ggplot(earnings[1:15,], aes(x=College, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="#0099ff") + 
  geom_text(aes(x=College, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  coord_flip() +
  xlab("") + ylab("")
}

fetchDATA <- function() {
    schools <- dplyr::select(df,
                         INSTNM,         ## institution name
                         CITY,           ## institution city
                         STABBR,         ## institution state abbrev
                         ZIP,            ## institution zip
                         PREDDEG,        ## predominate degree
                         CURROPER,       ## currently operating flag
                         CONTROL,        ## type of school
                         TUITIONFEE_IN,  ## in-state tuition and fees
                         DISTANCEONLY,   ## distance only flag
                         LATITUDE,       ## latitude
                         LONGITUDE,      ## longitude
                         GRAD_DEBT_MDN   ## median debt
  ) 
  return(schools)
}

plotMAP <- function(schools) {
  uniInfo <- paste(schools[['INSTNM']], "<br>", schools[['CITY']], ", ", 
                   schools[['STABBR']], schools[['ZIP']], "<br> Median debt: $", 
                   schools[['GRAD_DEBT_MDN']], sep='')

  schools$info <- uniInfo

  ## filter data
  schools<-filter(schools,
                  PREDDEG==3 &                        ## Predominate degree is BS
                  CURROPER==1 &                       ## Currently operating
                  DISTANCEONLY==0 &                   ## Not distance
                  is.na(TUITIONFEE_IN)==FALSE &       ## Key measurements aren't missing
                  is.na(LATITUDE)==FALSE &
                  is.na(LONGITUDE)==FALSE &
                  LATITUDE>20 & LATITUDE<50 &         ## Location is US 48
                  LONGITUDE>(-130) & LONGITUDE<(-60)
                  )

  map <- leaflet(schools) %>% 
            setView(-93.65, 42.0285, zoom = 4) %>%
            addTiles() %>%
            addMarkers(~LONGITUDE, ~LATITUDE, popup=~info,
                       options = popupOptions(closeButton = TRUE),
                       clusterOptions = markerClusterOptions())

  map
}



shinyServer(function(input, output) {
    schools <- fetchDATA()
    output$plot <- renderPlot({
        switch(input$plot,
         "SAT Scores" = {
            sat <<- getSAT()
            plotSAT(sat)
          },
         "ACT Scores" = {
            act <<- getACT()
            plotACT(act)
          },
         "Admission Rate" = {
            adm <<- getADM()
            plotADM(adm)
          },
          "Undergraduate Enrollment" = {
            enro <<- getENRO()
            plotENRO(enro)
          },
          "Cost of Attendance" = {
            cost <<- getCOST()
            plotCOST(cost)
          },
          "Top 15 meadian earnings" = {
            plotEARN()
          }
    )})

    output$summary <- renderPrint({
      switch(input$plot,
         "SAT Scores" = printSAT(sat),
         "ACT Scores" = printACT(act),
         "Admission Rate" = printADM(adm),
         "Undergraduate Enrollment" = printENRO(),
         "Cost of Attendance" = printCOST()
         )
      })

    output$map <- renderLeaflet({
      plotMAP(schools)
      })
})