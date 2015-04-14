# data <- read.csv("./Data/FullReadingAssignments_Cleaned_150308.csv", header = T)
data <- read.csv("./Data/FullReadingAssignments_Clean3.csv", sep = "\t", header = T)
data <- subset(data, Keep == 1)

require(Hmisc)

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
        names = c("ecology", "gen biol", "vet"), col = "grey80", log = "y")
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

# build binomial confidence interval for each proportion
mod.obj.tab <- table(appbasman$ModelObjectiveGeneral, appbasman$JournalCommunity)
app.sci.1 <- binconf(x = mod.obj.tab[1, 1], n = sum(mod.obj.tab[, 1])) * sum(mod.obj.tab[ , 1])
bas.sci.1 <- binconf(x = mod.obj.tab[2, 1], n = sum(mod.obj.tab[, 1])) * sum(mod.obj.tab[ , 1])
man.sci.1 <- binconf(x = mod.obj.tab[3, 1], n = sum(mod.obj.tab[, 1])) * sum(mod.obj.tab[ , 1])
app.sci.2 <- binconf(x = mod.obj.tab[1, 2], n = sum(mod.obj.tab[, 2])) * sum(mod.obj.tab[ , 2])
bas.sci.2 <- binconf(x = mod.obj.tab[2, 2], n = sum(mod.obj.tab[, 2])) * sum(mod.obj.tab[ , 2])
man.sci.2 <- binconf(x = mod.obj.tab[3, 2], n = sum(mod.obj.tab[, 2])) * sum(mod.obj.tab[ , 2])
app.sci.3 <- binconf(x = mod.obj.tab[1, 3], n = sum(mod.obj.tab[, 3])) * sum(mod.obj.tab[ , 3])
bas.sci.3 <- binconf(x = mod.obj.tab[2, 3], n = sum(mod.obj.tab[, 3])) * sum(mod.obj.tab[ , 3])
man.sci.3 <- binconf(x = mod.obj.tab[3, 3], n = sum(mod.obj.tab[, 3])) * sum(mod.obj.tab[ , 3])

pred.desc.tab <- table(PredDesc$PredictiveDescriptive, PredDesc$JournalCommunity)
both.sci.1 <- binconf(x = pred.desc.tab[1, 1], n = sum(pred.desc.tab[, 1])) * sum(pred.desc.tab[ , 1])
desc.sci.1 <- binconf(x = pred.desc.tab[2, 1], n = sum(pred.desc.tab[, 1])) * sum(pred.desc.tab[ , 1])
pred.sci.1 <- binconf(x = pred.desc.tab[3, 1], n = sum(pred.desc.tab[, 1])) * sum(pred.desc.tab[ , 1])
both.sci.2 <- binconf(x = pred.desc.tab[1, 2], n = sum(pred.desc.tab[, 2])) * sum(pred.desc.tab[ , 2])
desc.sci.2 <- binconf(x = pred.desc.tab[2, 2], n = sum(pred.desc.tab[, 2])) * sum(pred.desc.tab[ , 2])
pred.sci.2 <- binconf(x = pred.desc.tab[3, 2], n = sum(pred.desc.tab[, 2])) * sum(pred.desc.tab[ , 2])
both.sci.3 <- binconf(x = pred.desc.tab[1, 3], n = sum(pred.desc.tab[, 3])) * sum(pred.desc.tab[ , 3])
desc.sci.3 <- binconf(x = pred.desc.tab[2, 3], n = sum(pred.desc.tab[, 3])) * sum(pred.desc.tab[ , 3])
pred.sci.3 <- binconf(x = pred.desc.tab[3, 3], n = sum(pred.desc.tab[, 3])) * sum(pred.desc.tab[ , 3])

hawp.tab <- table(dataHAWP$HumanAgWildPlant, dataHAWP$JournalCommunity)
ag.1 <- binconf(x = hawp.tab[1, 1], n = sum(hawp.tab[, 1])) * sum(hawp.tab[ , 1])
hum.1 <- binconf(x = hawp.tab[2, 1], n = sum(hawp.tab[, 1])) * sum(hawp.tab[ , 1])
hyp.1 <- binconf(x = hawp.tab[3, 1], n = sum(hawp.tab[, 1])) * sum(hawp.tab[ , 1])
plant.1 <- binconf(x = hawp.tab[4, 1], n = sum(hawp.tab[, 1])) * sum(hawp.tab[ , 1])
wild.1 <- binconf(x = hawp.tab[5, 1], n = sum(hawp.tab[, 1])) * sum(hawp.tab[ , 1])
ag.2 <- binconf(x = hawp.tab[1, 2], n = sum(hawp.tab[, 2])) * sum(hawp.tab[ , 2])
hum.2 <- binconf(x = hawp.tab[2, 2], n = sum(hawp.tab[, 2])) * sum(hawp.tab[ , 2])
hyp.2 <- binconf(x = hawp.tab[3, 2], n = sum(hawp.tab[, 2])) * sum(hawp.tab[ , 2])
plant.2 <- binconf(x = hawp.tab[4, 2], n = sum(hawp.tab[, 2])) * sum(hawp.tab[ , 2])
wild.2 <- binconf(x = hawp.tab[5, 2], n = sum(hawp.tab[, 2])) * sum(hawp.tab[ , 2])
ag.3 <- binconf(x = hawp.tab[1, 3], n = sum(hawp.tab[, 3])) * sum(hawp.tab[ , 3])
hum.3 <- binconf(x = hawp.tab[2, 3], n = sum(hawp.tab[, 3])) * sum(hawp.tab[ , 3])
hyp.3 <- binconf(x = hawp.tab[3, 3], n = sum(hawp.tab[, 3])) * sum(hawp.tab[ , 3])
plant.3 <- binconf(x = hawp.tab[4, 3], n = sum(hawp.tab[, 3])) * sum(hawp.tab[ , 3])
wild.3 <- binconf(x = hawp.tab[5, 3], n = sum(hawp.tab[, 3])) * sum(hawp.tab[ , 3])

par(mfrow = c(1, 3))
barplot(table(appbasman$ModelObjectiveGeneral, appbasman$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60),
        args.legend = list(x = 8, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = app.sci.1[2], y1 = app.sci.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = bas.sci.1[2], y1 = bas.sci.1[3], lwd = 2)
segments(x0 = 3.5, x1 = 3.5, y0 = man.sci.1[2], y1 = man.sci.1[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = app.sci.2[2], y1 = app.sci.2[3], lwd = 2)
segments(x0 = 6.5, x1 = 6.5, y0 = bas.sci.2[2], y1 = bas.sci.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = man.sci.2[2], y1 = man.sci.2[3], lwd = 2)
segments(x0 = 9.5, x1 = 9.5, y0 = app.sci.3[2], y1 = app.sci.3[3], lwd = 2)
segments(x0 = 10.5, x1 = 10.5, y0 = bas.sci.3[2], y1 = bas.sci.3[3], lwd = 2)
segments(x0 = 11.5, x1 = 11.5, y0 = man.sci.3[2], y1 = man.sci.3[3], lwd = 2)

barplot(table(PredDesc$PredictiveDescriptive, PredDesc$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60),
        args.legend = list(x = 6, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = both.sci.1[2], y1 = both.sci.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = desc.sci.1[2], y1 = desc.sci.1[3], lwd = 2)
segments(x0 = 3.5, x1 = 3.5, y0 = pred.sci.1[2], y1 = pred.sci.1[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = both.sci.2[2], y1 = both.sci.2[3], lwd = 2)
segments(x0 = 6.5, x1 = 6.5, y0 = desc.sci.2[2], y1 = desc.sci.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = pred.sci.2[2], y1 = pred.sci.2[3], lwd = 2)
segments(x0 = 9.5, x1 = 9.5, y0 = both.sci.3[2], y1 = both.sci.3[3], lwd = 2)
segments(x0 = 10.5, x1 = 10.5, y0 = desc.sci.3[2], y1 = desc.sci.3[3], lwd = 2)
segments(x0 = 11.5, x1 = 11.5, y0 = pred.sci.3[2], y1 = pred.sci.3[3], lwd = 2)

barplot(table(dataHAWP$HumanAgWildPlant, dataHAWP$JournalCommunity), beside = T, 
        legend = T, names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70),
        args.legend = list(x = 10, y = 70, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = ag.1[2], y1 = ag.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = hum.1[2], y1 = hum.1[3], lwd = 2)
segments(x0 = 3.5, x1 = 3.5, y0 = hyp.1[2], y1 = hyp.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = plant.1[2], y1 = plant.1[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = wild.1[2], y1 = wild.1[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = ag.2[2], y1 = ag.2[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = hum.2[2], y1 = hum.2[3], lwd = 2)
segments(x0 = 9.5, x1 = 9.5, y0 = hyp.2[2], y1 = hyp.2[3], lwd = 2)
segments(x0 = 10.5, x1 = 10.5, y0 = plant.2[2], y1 = plant.2[3], lwd = 2)
segments(x0 = 11.5, x1 = 11.5, y0 = wild.2[2], y1 = wild.2[3], lwd = 2)
segments(x0 = 13.5, x1 = 13.5, y0 = ag.3[2], y1 = ag.3[3], lwd = 2)
segments(x0 = 14.5, x1 = 14.5, y0 = hum.3[2], y1 = hum.3[3], lwd = 2)
segments(x0 = 15.5, x1 = 15.5, y0 = hyp.3[2], y1 = hyp.3[3], lwd = 2)
segments(x0 = 16.5, x1 = 16.5, y0 = plant.3[2], y1 = plant.3[3], lwd = 2)
segments(x0 = 17.5, x1 = 17.5, y0 = wild.3[2], y1 = wild.3[3], lwd = 2)


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


#-------------------------------------------#
#-- data and fit assessment ----------------#
#-------------------------------------------#
incdata.tab <- table(data$DataIncorporated, data$JournalCommunity)
nodat.1 <- binconf(x = incdata.tab[1, 1], n = sum(incdata.tab[, 1])) * sum(incdata.tab[ , 1])
dat.1   <- binconf(x = incdata.tab[2, 1], n = sum(incdata.tab[, 1])) * sum(incdata.tab[ , 1])
nodat.2 <- binconf(x = incdata.tab[1, 2], n = sum(incdata.tab[, 2])) * sum(incdata.tab[ , 2])
dat.2   <- binconf(x = incdata.tab[2, 2], n = sum(incdata.tab[, 2])) * sum(incdata.tab[ , 2])
nodat.3 <- binconf(x = incdata.tab[1, 3], n = sum(incdata.tab[, 3])) * sum(incdata.tab[ , 3])
dat.3   <- binconf(x = incdata.tab[2, 3], n = sum(incdata.tab[, 3])) * sum(incdata.tab[ , 3])

dataFit <- subset(data, FitAssessed..2...qualitative. %in% c(0, 1, 2))
fit.tab <- table(dataFit$FitAssessed..2...qualitative, dataFit$JournalCommunity)
nofit.1    <- binconf(x = fit.tab[1, 1], n = sum(fit.tab[, 1])) * sum(fit.tab[ , 1])
quantfit.1 <- binconf(x = fit.tab[2, 1], n = sum(fit.tab[, 1])) * sum(fit.tab[ , 1])
qualfit.1  <- binconf(x = fit.tab[3, 1], n = sum(fit.tab[, 1])) * sum(fit.tab[ , 1])
nofit.2    <- binconf(x = fit.tab[1, 2], n = sum(fit.tab[, 2])) * sum(fit.tab[ , 2])
quantfit.2 <- binconf(x = fit.tab[2, 2], n = sum(fit.tab[, 2])) * sum(fit.tab[ , 2])
qualfit.2  <- binconf(x = fit.tab[3, 2], n = sum(fit.tab[, 2])) * sum(fit.tab[ , 2])
nofit.3    <- binconf(x = fit.tab[1, 3], n = sum(fit.tab[, 3])) * sum(fit.tab[ , 3])
quantfit.3 <- binconf(x = fit.tab[2, 3], n = sum(fit.tab[, 3])) * sum(fit.tab[ , 3])
qualfit.3  <- binconf(x = fit.tab[3, 3], n = sum(fit.tab[, 3])) * sum(fit.tab[ , 3])

dataNew <- subset(data, NewEmpiricalData %in% c(0, 1))
dataNew$NewEmpiricalData <- factor(dataNew$NewEmpiricalData)
new.tab <- table(dataNew$NewEmpiricalData, dataNew$JournalCommunity)
nonew.1 <- binconf(x = new.tab[1, 1], n = sum(new.tab[, 1])) * sum(new.tab[ , 1])
new.1   <- binconf(x = new.tab[2, 1], n = sum(new.tab[, 1])) * sum(new.tab[ , 1])
nonew.2 <- binconf(x = new.tab[1, 2], n = sum(new.tab[, 2])) * sum(new.tab[ , 2])
new.2   <- binconf(x = new.tab[2, 2], n = sum(new.tab[, 2])) * sum(new.tab[ , 2])
nonew.3 <- binconf(x = new.tab[1, 3], n = sum(new.tab[, 3])) * sum(new.tab[ , 3])
new.3   <- binconf(x = new.tab[2, 3], n = sum(new.tab[, 3])) * sum(new.tab[ , 3])

#----------------------------------------------------------------------------#
#  question 3: do communities differ in how they evaluate/truth their model?
#----------------------------------------------------------------------------#

par(mfrow = c(2, 2), oma = c(2, 2, 2, 2), mex = 1, bty = "n")
boxplot((data$NumericParams + 1) ~ data$JournalCommunity, 
        names = c("ecology", "gen biol", "vet"), col = "grey80", log = "y", 
        ylab = "Number of parameters", xaxs = "i", yaxs = "i", 
        ylim = c(1, max(na.omit(data$NumericParams))))

barplot(table(data$DataIncorporated, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 70), 
        legend.text = c("No data", "Data incorporated"), 
        args.legend = list(x = 6, y = 70, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nodat.1[2], y1 = nodat.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = dat.1[2], y1 = dat.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = nodat.2[2], y1 = nodat.2[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = dat.2[2], y1 = dat.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = nodat.3[2], y1 = nodat.3[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = dat.3[2], y1 = dat.3[3], lwd = 2)


barplot(table(dataFit$FitAssessed..2...qualitative, dataFit$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("Not assessed", "Qualitative", "Quantitative"), 
        args.legend = list(x = 7, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nofit.1[2], y1 = nofit.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = quantfit.1[2], y1 = quantfit.1[3], lwd = 2)
segments(x0 = 3.5, x1 = 3.5, y0 = qualfit.1[2], y1 = qualfit.1[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = nofit.2[2], y1 = nofit.2[3], lwd = 2)
segments(x0 = 6.5, x1 = 6.5, y0 = quantfit.2[2], y1 = quantfit.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = qualfit.2[2], y1 = qualfit.2[3], lwd = 2)
segments(x0 = 9.5, x1 = 9.5, y0 = nofit.3[2], y1 = nofit.3[3], lwd = 2)
segments(x0 = 10.5, x1 = 10.5, y0 = quantfit.3[2], y1 = quantfit.3[3], lwd = 2)
segments(x0 = 11.5, x1 = 11.5, y0 = qualfit.3[2], y1 = qualfit.3[3], lwd = 2)

barplot(table(dataNew$NewEmpiricalData, dataNew$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("No new data", "New data"), 
        args.legend = list(x = 5, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nonew.1[2], y1 = nonew.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = new.1[2], y1 = new.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = nonew.2[2], y1 = nonew.2[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = new.2[2], y1 = new.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = nonew.3[2], y1 = nonew.3[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = new.3[2], y1 = new.3[3], lwd = 2)

#-----------------------------------------------------#
# do communities differ in how them implement models?
#-----------------------------------------------------#

# stochasticity
data$StochDeter <- factor(data$StochDeter)
table(data$StochDeter)

dataStochDeter <- subset(data, StochDeter %in% c("both", "deter", "stoch"))
dataStochDeter$StochDeter <- factor(dataStochDeter$StochDeter)

# confidence limits
stodet.tab <- table(dataStochDeter$StochDeter, dataStochDeter$JournalCommunity)
bothsd.1 <- binconf(x = stodet.tab[1, 1], n = sum(stodet.tab[, 1])) * sum(stodet.tab[ , 1])
det.1    <- binconf(x = stodet.tab[2, 1], n = sum(stodet.tab[, 1])) * sum(stodet.tab[ , 1])
sto.1    <- binconf(x = stodet.tab[3, 1], n = sum(stodet.tab[, 1])) * sum(stodet.tab[ , 1])
bothsd.2 <- binconf(x = stodet.tab[1, 2], n = sum(stodet.tab[, 2])) * sum(stodet.tab[ , 2])
det.2    <- binconf(x = stodet.tab[2, 2], n = sum(stodet.tab[, 2])) * sum(stodet.tab[ , 2])
sto.2    <- binconf(x = stodet.tab[3, 2], n = sum(stodet.tab[, 2])) * sum(stodet.tab[ , 2])
bothsd.3 <- binconf(x = stodet.tab[1, 3], n = sum(stodet.tab[, 3])) * sum(stodet.tab[ , 3])
det.3    <- binconf(x = stodet.tab[2, 3], n = sum(stodet.tab[, 3])) * sum(stodet.tab[ , 3])
sto.3    <- binconf(x = stodet.tab[3, 3], n = sum(stodet.tab[, 3])) * sum(stodet.tab[ , 3])

sim.tab <- table(data$Simulation, data$JournalCommunity)
nosim.1 <- binconf(x = sim.tab[1, 1], n = sum(sim.tab[, 1])) * sum(sim.tab[ , 1])
sim.1   <- binconf(x = sim.tab[2, 1], n = sum(sim.tab[, 1])) * sum(sim.tab[ , 1])
nosim.2 <- binconf(x = sim.tab[1, 2], n = sum(sim.tab[, 2])) * sum(sim.tab[ , 2])
sim.2   <- binconf(x = sim.tab[2, 2], n = sum(sim.tab[, 2])) * sum(sim.tab[ , 2])
nosim.3 <- binconf(x = sim.tab[1, 3], n = sum(sim.tab[, 3])) * sum(sim.tab[ , 3])
sim.3   <- binconf(x = sim.tab[2, 3], n = sum(sim.tab[, 3])) * sum(sim.tab[ , 3])

sens.tab <- table(data$Sensitivity, data$JournalCommunity)
nosens.1 <- binconf(x = sens.tab[1, 1], n = sum(sens.tab[, 1])) * sum(sens.tab[ , 1])
sens.1   <- binconf(x = sens.tab[2, 1], n = sum(sens.tab[, 1])) * sum(sens.tab[ , 1])
nosens.2 <- binconf(x = sens.tab[1, 2], n = sum(sens.tab[, 2])) * sum(sens.tab[ , 2])
sens.2   <- binconf(x = sens.tab[2, 2], n = sum(sens.tab[, 2])) * sum(sens.tab[ , 2])
nosens.3 <- binconf(x = sens.tab[1, 3], n = sum(sens.tab[, 3])) * sum(sens.tab[ , 3])
sens.3   <- binconf(x = sens.tab[2, 3], n = sum(sens.tab[, 3])) * sum(sens.tab[ , 3])

math.tab <- table(data$MathAssessment, data$JournalCommunity)
nomath.1 <- binconf(x = math.tab[1, 1], n = sum(math.tab[, 1])) * sum(math.tab[ , 1])
math.1   <- binconf(x = math.tab[2, 1], n = sum(math.tab[, 1])) * sum(math.tab[ , 1])
nomath.2 <- binconf(x = math.tab[1, 2], n = sum(math.tab[, 2])) * sum(math.tab[ , 2])
math.2   <- binconf(x = math.tab[2, 2], n = sum(math.tab[, 2])) * sum(math.tab[ , 2])
nomath.3 <- binconf(x = math.tab[1, 3], n = sum(math.tab[, 3])) * sum(math.tab[ , 3])
math.3   <- binconf(x = math.tab[2, 3], n = sum(math.tab[, 3])) * sum(math.tab[ , 3])

# figure
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mex = 1)
barplot(table(dataStochDeter$StochDeter, dataStochDeter$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 40), 
        legend.text = c("both", "deterministic", "stochastic"), 
        args.legend = list(x = 5, y = 45, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = bothsd.1[2], y1 = bothsd.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = det.1[2], y1 = det.1[3], lwd = 2)
segments(x0 = 3.5, x1 = 3.5, y0 = sto.1[2], y1 = sto.1[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = bothsd.2[2], y1 = bothsd.2[3], lwd = 2)
segments(x0 = 6.5, x1 = 6.5, y0 = det.2[2], y1 = det.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = sto.2[2], y1 = sto.2[3], lwd = 2)
segments(x0 = 9.5, x1 = 9.5, y0 = bothsd.3[2], y1 = bothsd.3[3], lwd = 2)
segments(x0 = 10.5, x1 = 10.5, y0 = det.3[2], y1 = det.3[3], lwd = 2)
segments(x0 = 11.5, x1 = 11.5, y0 = sto.3[2], y1 = sto.3[3], lwd = 2)

# simulation
barplot(table(data$Simulation, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("no simulation", "simulation"), 
        args.legend = list(x = 4, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nosim.1[2], y1 = nosim.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = sim.1[2], y1 = sim.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = nosim.2[2], y1 = nosim.2[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = sim.2[2], y1 = sim.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = nosim.3[2], y1 = nosim.3[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = sim.3[2], y1 = sim.3[3], lwd = 2)

# sensitivty
barplot(table(data$Sensitivity, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("no sensitivity", "sensitivity"), 
        args.legend = list(x = 4, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nosens.1[2], y1 = nosens.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = sens.1[2], y1 = sens.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = nosens.2[2], y1 = nosens.2[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = sens.2[2], y1 = sens.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = nosens.3[2], y1 = nosens.3[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = sens.3[2], y1 = sens.3[3], lwd = 2)

# math assessment
barplot(table(data$MathAssessment, data$JournalCommunity), beside = T, 
        names = c("Ecology", "General Bio", "Vet"), ylim = c(0, 60), 
        legend.text = c("no math assessment", "math assessment"), 
        args.legend = list(x = 5, y = 60, bty = "n"))
segments(x0 = 1.5, x1 = 1.5, y0 = nomath.1[2], y1 = nomath.1[3], lwd = 2)
segments(x0 = 2.5, x1 = 2.5, y0 = math.1[2], y1 = math.1[3], lwd = 2)
segments(x0 = 4.5, x1 = 4.5, y0 = nomath.2[2], y1 = nomath.2[3], lwd = 2)
segments(x0 = 5.5, x1 = 5.5, y0 = math.2[2], y1 = math.2[3], lwd = 2)
segments(x0 = 7.5, x1 = 7.5, y0 = nomath.3[2], y1 = nomath.3[3], lwd = 2)
segments(x0 = 8.5, x1 = 8.5, y0 = math.3[2], y1 = math.3[3], lwd = 2)

#--------------------------------------------------#
#-- Highly cited papers ---------------------------#
#--------------------------------------------------#
dataAnnualized <- subset(data, is.na(AnnualizedCitationRate) == F & is.na(NumericParams) == F)

par(mfrow = c(1, 2))
plot(dataAnnualized$AnnualizedCitationRate ~ dataAnnualized$NumericParams, log = "xy",
     xlab = "Number of parameters", ylab = "Citations per year", col = "black")
lines(lowess(as.numeric(as.character(dataAnnualized$AnnualizedCitationRate, f = 1/2)) ~ as.numeric(as.character(dataAnnualized$NumericParams))))
boxplot((data$AnnualizedCitationRate + 1) ~ factor(data$DataIncorporated), 
        log = "y", col = "grey60", names = c("No Data", "Data Incorporated"), 
        ylab = "Citations per year")

boxplot((data$AnnualizedCitationRate + 1) ~ factor(data$DataIncorporated) + data$JournalCommunity, 
        log = "y", col = rep(c("grey80", "grey30"), 3), xaxt = "n", ylim = c(1, 100),
        ylab = "Citations per year"
        )
legend("topright", c("No Data", "Data Incorporated"), fill = c("grey80", "grey30"), bty = "n")

axis(side = 1, at = c(1.5, 3.5, 5.5), labels = c("Ecology", "Gen Bio", "Vet"))

plot((dataAnnualized$AnnualizedCitationRate + 1) ~ dataAnnualized$NumericParams, log = "xy",
     xlab = "Number of parameters", ylab = "Citations per year", col = "black")
#lines(lowess(as.numeric(as.character(dataAnnualized$AnnualizedCitationRate, f = 5/6)) ~ as.numeric(as.character(dataAnnualized$NumericParams))))



# add annualized citation ~ author centrality and author diversity
