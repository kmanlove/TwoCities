# data <- read.csv("./Data/FullReadingAssignments_Cleaned_150308.csv", header = T)
data <- read.csv("./Data/FullReadingAssignmentsCompleted_Clean2.csv", sep = "\t", header = T)

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

# recode data$NumericParams 

boxplot((data$NumericParams + 1) ~ data$JournalCommunity, 
        names = c("ecology", "gen biol", "vet"), col = "grey80")
plot((data$NumericParams + 1) ~ data$PubYear, pch = data$JournalCommunity, log = "y", col = data$JournalCommunity)
lines(lowess(as.numeric(as.character((comm1.numparams$NumericParams + 1))) ~ comm1.numparams$PubYear), col = "black")
lines(lowess(as.numeric(as.character((comm2.numparams$NumericParams + 1))) ~ comm2.numparams$PubYear), col = "red")
lines(lowess(as.numeric(as.character((comm3.numparams$NumericParams + 1))) ~ comm3.numparams$PubYear), col = "green")

# data used
comm1.numdata <- subset(comm1, is.na(TotalData) == F)
comm2.numdata <- subset(comm2, is.na(TotalData) == F)
comm3.numdata <- subset(comm3, is.na(TotalData) == F)

# data incorporated through time
table(data$DataIncorporated, data$JournalCommunity, data$YearBlock)
boxplot(data$DataIncorporated ~ data$JournalCommunity + data$YearBlock)
table(data$NewEmpiricalData, data$JournalCommunity)

boxplot((data$TotalData + 1) ~ data$JournalCommunity, log = "y")
plot((data$TotalData + 1) ~ data$PubYear, pch = data$JournalCommunity, log = "y", col = data$JournalCommunity)
lines(lowess(as.numeric(as.character((comm1.numdata$TotalData + 1))) ~ comm1.numdata$PubYear), 
      col = "black", lwd = 2)
lines(lowess(as.numeric(as.character((comm2.numdata$TotalData + 1))) ~ comm2.numdata$PubYear), 
      col = "red", lwd = 2)
lines(lowess(as.numeric(as.character((comm3.numdata$TotalData + 1))) ~ comm3.numdata$PubYear), 
      col = "green", lwd = 2)
leg.text <- c("ecol", "gen biol", "vet")
legend("topleft", leg.text, col = c("black", "red", "green"), lty = c(1, 1, 1), lwd = c(2, 2, 2), bty = "n")

# is shift toward more data in ecol and gen bio real?
require(lme4)
ecol.shift <- glm(as.numeric(as.character((comm1.numdata$TotalData + 1))) ~ comm1.numdata$PubYear, family = "poisson")
genbiol.shift <- glm(as.numeric(as.character((comm2.numdata$TotalData + 1))) ~ comm2.numdata$PubYear, family = "poisson")
vet.shift <- glm(as.numeric(as.character((comm3.numdata$TotalData + 1))) ~ comm3.numdata$PubYear, family = "poisson")

# ratio of number parameters to number of data
plot((data$TotalData + 1)~ (data$NumericParams + 1), col = data$JournalCommunity, log = "xy", pch = 16)



#  question 2: do communities differ in their modeling objectives?
data <- subset(data, Keep == 1)
appbasman <- subset(data, ModelObjectiveGeneral %in% c("applied science", "basic science", "management"))
appbasman$ModelObjectiveGeneral <- factor(appbasman$ModelObjectiveGeneral)
barplot(table(appbasman$ModelObjectiveGeneral, appbasman$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 80))

# question 2: do communities differ in predictive/descriptiveness?
PredDesc <- subset(data, PredictiveDescriptive %in% c("both", "predictive", "descriptive"))
PredDesc$PredictiveDescriptive <- factor(PredDesc$PredictiveDescriptive)
barplot(table(PredDesc$PredictiveDescriptive, PredDesc$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70))

# question 2: do communities differ in their study systems?
dataHAWP <- subset(data, HumanAgWildPlant %in% c("ag", "human", "hypothetical", "plant", "wild"))
dataHAWP$HumanAgWildPlant <- factor(dataHAWP$HumanAgWildPlant)
barplot(table(dataHAWP$HumanAgWildPlant, dataHAWP$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70),
        args.legend = list(x = 6, y = 70, bty = "n"))

# question 2: who looks at evolution / who looks at climate change?
par(mfrow = c(1, 2))
barplot(table(data$Evolutionary, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70),
        legend.text = c("Evolution", "No Evolution"),
        args.legend = list(x = 7, y = 95, bty = "n"))
barplot(table(data$Climate, data$JournalCommunity), beside = T, 
         names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70),
        legend.text = c("Climate", "No Climate"),
        args.legend = list(x = 7, y = 95, bty = "n"))


#  question 3: do communities differ in how they model?
barplot(table(data$DataIncorporated, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70), 
        legend.text = c("No data", "Data incorporated"), 
        args.legend = list(x = 4, y = 70, bty = "n"))

dataFit <- subset(data, FitAssessed..2...qualitative. %in% c(0, 1, 2))
barplot(table(dataFit$FitAssessed..2...qualitative, dataFit$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("Not assessed", "Qualitative", "Quantitative"), 
        args.legend = list(x = 4, y = 60, bty = "n"))

dataNew <- subset(data, NewEmpiricalData %in% c(0, 1))
dataNew$NewEmpiricalData <- factor(dataNew$NewEmpiricalData)
barplot(table(dataNew$NewEmpiricalData, dataNew$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("No new data", "New data"), 
        args.legend = list(x = 4, y = 60, bty = "n"))
