# load required packages and source functions
require(igraph)
require(ape)
require(lme4)
require(vegan)
require(graphics)
require(arcdiagram)
require(RColorBrewer)
require(mgcv)
require(Hmisc)

source("./R/TwoCities_SourceFunctions.R")
source("./R/BuildAuthorGraph.R")
source("./R/BuildCitationFrame.R")
source("./R/BuildAssocMat.R")

#---------------------------------------------------------------------------# 
#-- Data have two unique keys: ---------------------------------------------#
#-- Paper.Number = papers in FULL SET (accepted + rejected = 2258 papers) --#
#-- AuthorID = unique author identifier: last name <space> first initial ---#
#---------------------------------------------------------------------------#
data.frame <- read.csv("./Data/FullDataFrame_AuthorInstsAndAffils_20150928.csv", 
                       header = T)

#--------------------------#
#-- Additional data prep --#
#--------------------------#
all.authors <- BuildAuthorFrame(data.frame.in = data.frame)

# add lead author ID to data.frame 
affils.test <- GetAuthorAffils(author.frame = all.authors)
affils.test$CheckAffils <- rep(NA, dim(affils.test)[1])
for(i in 1:dim(affils.test)[1]){
  k <- factor(subset(affils.test, Paper.Number == affils.test$Paper.Number[i])$FullAffil)
  affils.test$CheckAffils[i] <- ifelse(length(k) == 1, 0, 
                                       ifelse(length(levels(k)) == 1, 1, 0)
  )
#  print(i)
}

data.frame$AuMath <- data.frame$AuStat <- data.frame$AuEcol <- data.frame$AuEvol <- rep(NA, dim(data.frame)[1])
data.frame$AuBiol <- data.frame$AuEpi <- data.frame$AuVet <- data.frame$AuMed <- rep(NA, dim(data.frame)[1])
data.frame$NumberAuthors <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  k <- subset(affils.test, Paper.Number == data.frame$Paper.Number[i])
  data.frame$AuMath[i] <- sum(k$Math)
  data.frame$AuStat[i] <- sum(k$Stat)
  data.frame$AuEcol[i] <- sum(k$Ecol)
  data.frame$AuEvol[i] <- sum(k$Evol)
  data.frame$AuBiol[i] <- sum(k$Biol)
  data.frame$AuEpi[i] <- sum(k$Epi)
  data.frame$AuVet[i] <- sum(k$Vet)
  data.frame$AuMed[i] <- sum(k$Med)
  data.frame$NumberAuthors[i] <- dim(k)[1]
#  print(i)
}

# calculate author shannon diversity for each paper
data.frame$AuShannonDiv <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  data.frame$AuShannonDiv[i] <- diversity(subset(data.frame[i, ], 
                                                 select = c("AuMath", "AuStat",
                                                            "AuEcol", "AuEvol",
                                                            "AuBiol", "AuEpi",
                                                            "AuVet", "AuMed")))
#  print(i)                    
}

# store all papers from each community in their own objects
comm1 <- subset(data.frame, Journal.Community == 1)
comm2 <- subset(data.frame, Journal.Community == 2)
comm3 <- subset(data.frame, Journal.Community == 3)

# get within paper-bank citation data
cite.list <- BuildCitationFrame(data.frame.in = data.frame)

assoc.mat <- BuildAssocMat(data.frame.in = data.frame, 
                           cite.frame = cite.list)


data.frame$citations.within <- data.frame$citations.between <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  data.frame$citations.within[i] <- sum(na.omit(assoc.mat[i, 
                                                          which(data.frame$Journal.Community == data.frame$Journal.Community[i])]))
  data.frame$citations.between[i] <- sum(na.omit(assoc.mat[i, 
                                                           which(data.frame$Journal.Community != data.frame$Journal.Community[i])]))
#  print(i)
}
data.frame$between.to.tot <- data.frame$citations.between / (data.frame$citations.within + data.frame$citations.between)

full.citation.frame <- do.call("rbind", cite.list) # 69905 total refs
paper.graph <- BuildPaperGraph(assoc.mat, data.frame)
paper.diags <- NetworkDiagnostics(graph.in = paper.graph, seed.in = 123)

journal.data <- ExtractJournalData(data.frame)
data.frame$Source <- ifelse(data.frame$Source == "PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES",
                            "PHILOSOPHICAL TRANSACTIONS B", as.character(data.frame$Source))
data.frame$Source  <- ifelse(data.frame$Source == "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA",
                             "PNAS", as.character(data.frame$Source))
data.frame$Source  <- ifelse(data.frame$Source == "PROCEEDINGS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES",
                             "PROCEEDINGS OF THE ROYAL SOCIETY B", as.character(data.frame$Source))
data.frame$Source  <- ifelse(data.frame$Source == "TRANSACTIONS OF THE ROYAL SOCIETY OF TROPICAL MEDICINE AND HYGIENE",
                             "TRANS ROY SOC TROPICAL MEDICINE AND HYGIENE", as.character(data.frame$Source))

journal.graph <- BuildJournalGraph(data.frame.in = data.frame,
                                   assoc.mat.in = assoc.mat)


#-----------------------------#
#-- prep sampled paper data --#
#-----------------------------#

# build sample of papers for detailed reading (key isn't static, so sample will vary)
source("./R/BuildReadingSampleFun.R")
reading.assignments <- BuildReadingSampleFun(data.frame)

paper.data <- read.csv("./Data/FullReadingAssignments_CleanAndReduced_V5.csv", sep = ",", header = T)
paper.data <- subset(paper.data, Keep == 1)

# basic info
table(data$Keep, data$JournalCommunity)
paper.data$NumericParams <- as.numeric(as.character(paper.data$NumberParams))
paper.data$TotalData <- as.numeric(as.character(paper.data$NHostIndivids)) + 
  as.numeric(as.character(paper.data$NLocations)) + 
  as.numeric(as.character(paper.data$NDiseaseTests)) + 
  as.numeric(as.character(paper.data$NStepsInTimeseries)) + 
  as.numeric(as.character(paper.data$NSites))
paper.data$YearBlock <- ifelse(paper.data$PubYear < 1997, 1, 
                               ifelse(paper.data$PubYear >= 1997 & paper.data$PubYear <= 2003, 2, 
                                      ifelse(paper.data$PubYear >= 2010, 4, 3)))

paper.data <- subset(paper.data, Keep != 0)

#------------#
#-- Tables --#
#------------#
# List from section S8: top 10 most-cited papers per community
comm1.sm <- subset(comm1, select = c("Title", 
                                     "Source", 
                                     "Authors", 
                                     "PubYear", 
                                     "TimesCited"))
comm1.sm[order(comm1.sm$TimesCited, decreasing = T), ][1:10, 1:4]

comm2.sm <- subset(comm2, select = c("Title", 
                                     "Source", 
                                     "Authors", 
                                     "PubYear", 
                                     "TimesCited"))
comm2.sm[order(comm2.sm$TimesCited, decreasing = T), ][1:10, 1:4]

comm3.sm <- subset(comm3, select = c("Title", 
                                     "Source", 
                                     "Authors", 
                                     "PubYear", 
                                     "TimesCited"))
comm3.sm[order(comm3.sm$TimesCited, decreasing = T), ][1:10, 1:4]

# Table S5: contents built on line 189 of GAMPlot.R

# Table S6: contents built on line 117 of GAMPlot.R

# Table S7: 
source("./R/TableS7Code.R")
TableS7Code(data.frame) 
# no returns, but all information for table S7 is sitting in this function. 

# Table S8: contents built on line 15 of CitationRateModel.R

#-------------#
#-- Figures --#
#-------------#
# Fig S1
source("./R/PaperJournalNumberThroughTime.R")
PaperJournalNumberThroughTime(data.frame)

# Fig S2
source("./R/SampledPaperDataFitPlot.R")
SampledPaperDataFitPlot(paper.data)

# Fig S3
source("./R/SampledPaperMathPlot.R")
SampledPaperMathPlot(paper.data)

# Fig S4

# Fig S5
source("./R/AuthorDiversityPlots.R")
AuthorDiversityPlots(data.frame = data.frame)

# Fig S6

# Fig 1
ArcDiagramPlot(journal.graph)

# Fig 2a (left panel has line widths for legend)
source("./R/CommunityNetworkPlot.R")
CommunityNetworkPlot(data.frame)

# Fig 2b
source("./R/AuthorAffiliationByJoCommunityPlot.R")
AuthorAffiliationByJoCommunityPlot(data.frame)

# Fig 3
source("./R/SampledPaperObjectivesFun.R")
SampledPaperObjectivesFun(paper.data)

# Fig 4
source("./R/GAMPlot.R")
GAMPlot(data.frame)

# Fig 5 (cross-hatches built by hand in inkscape)
source("./R/CitationRateModel.R")
CitationRateModel(data.frame = data.frame)