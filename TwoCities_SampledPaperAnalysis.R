data <- read.csv("./Data/FullReadingAssignments_Cleaned_150308.csv", header = T)

# community number reminders: 
  # 1 is ecology
  # 2 is general bio
  # 3 is vet

# basic info
table(data$Keep, data$JournalCommunity)
data$NumericParams <- as.numeric(as.character(data$NumberParams))
data$TotalData <- as.numeric(as.character(data$NHostIndivids)) + 
  as.numeric(as.character(data$NLocations)) + 
  as.numeric(as.character(data$NDiseaseTests)) + 
  as.numeric(as.character(data$NStepsInTimeseries)) + 
  as.numeric(as.character(data$NSites))
data$YearBlock <- ifelse(data$PubYear < 1997, 1, ifelse(data$PubYear >=1997 & data$PubYear <= 2003, 2, ifelse(data$PubYear >= 2010, 4, 3)))

  # threw out a lot in community 1 relative to the other two communities
data <- subset(data, Keep != 0)
comm1 <- subset(data, JournalCommunity == 1)
comm2 <- subset(data, JournalCommunity == 2)
comm3 <- subset(data, JournalCommunity == 3)

comm1.numparams <- subset(comm1, is.na(NumericParams) == F)
comm2.numparams <- subset(comm2, is.na(NumericParams) == F)
comm3.numparams <- subset(comm3, is.na(NumericParams) == F)


table(data$NeedsSecondReader, data$JournalCommunity)
  # need a lot of second readers in community 1; a few each in 2 and 3. 


boxplot((data$NumericParams + 1) ~ data$JournalCommunity, log = "y")
plot((data$NumericParams + 1) ~ data$PubYear, pch = data$JournalCommunity, log = "y", col = data$JournalCommunity)
lines(lowess(as.numeric(as.character((comm1.numparams$NumericParams + 1))) ~ comm1.numparams$PubYear), col = "black")
lines(lowess(as.numeric(as.character((comm2.numparams$NumericParams + 1))) ~ comm2.numparams$PubYear), col = "red")
lines(lowess(as.numeric(as.character((comm3.numparams$NumericParams + 1))) ~ comm3.numparams$PubYear), col = "green")

# data used
comm1.numdata <- subset(comm1, is.na(TotalData) == F)
comm2.numdata <- subset(comm2, is.na(TotalData) == F)
comm3.numdata <- subset(comm3, is.na(TotalData) == F)

table(data$DataIncorporated, data$JournalCommunity, data$YearBlock)
boxplot(data$DataIncorporated ~ data$JournalCommunity + data$YearBlock)
table(data$NewEmpiricalData, data$JournalCommunity)

boxplot((data$TotalData + 1) ~ data$JournalCommunity, log = "y")
plot((data$TotalData + 1) ~ data$PubYear, pch = data$JournalCommunity, log = "y", col = data$JournalCommunity)
lines(lowess(as.numeric(as.character((comm1.numdata$TotalData + 1))) ~ comm1.numdata$PubYear), col = "black")
lines(lowess(as.numeric(as.character((comm2.numdata$TotalData + 1))) ~ comm2.numdata$PubYear), col = "red")
lines(lowess(as.numeric(as.character((comm3.numdata$TotalData + 1))) ~ comm3.numdata$PubYear), col = "green")
