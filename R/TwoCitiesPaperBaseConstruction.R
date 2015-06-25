
# what journals are included?
journal.list <- levels(factor(graph.data.frame$SO))
journal.tab <- as.data.frame(table(factor(graph.data.frame$SO)))

# journal rankings by topic
agbiosci <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/AgBioSciFull.csv", header = T)

agbio.top20s <- subset(agbiosci, RankInClass <= 20)
length(levels(factor(agbio.top20s$Title)))

vetjos <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/Vet_All.csv", header = T)
vet.top20s <- subset(vetjos, RankInClass <= 20)
length(levels(factor(vet.top20s$Title)))

fulljo <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/FullJournalList.csv", header = T)

top25.full <- subset(fulljo, RankInClass <= 25)
ClassesToOmit <- c("Forestry")
forestry.journals <- subset(top25.full, Class %in% ClassesToOmit)$Title
foodsci.journals <- subset(top25.full, Class %in% "FoodScience")$Title
table(factor(foodsci.journals))
virology.journals <- subset(top25.full, Class %in% "Virology")$Title
table(factor(virology.journals))
aqsci.journals <- subset(top25.full, Class %in% "AquaticSci")$Title
table(factor(aqsci.journals))

fulljos <- levels(factor(top25.full$Title))
write.csv(fulljos, "~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/Top25JoList.csv", row.names = F)

MostRelevantCategories <- c("AgBiosciMisc", "AnimSciZool", "EcologicalModeling", "Ecology", "EEBS", "Med_Epi", "Med_InfectDisease", "NatureLandscapeConservation", "")
mostrelevant.top25 <- subset(top25.full, Class %in% MostRelevantCategories)
mostrelevantjos <- levels(factor(mostrelevant.top25$Title))
write.csv(mostrelevantjos, "~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/MostRelevantTop25JoList.csv", row.names = F)

addons <- c( "AgBiosciMisc")
addon.dat <- subset(top25.full, Class %in% addons)
addonjos <- levels(factor(addon.dat$Title))
write.csv(addonjos, "~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/AddOnJos.csv", row.names = F)

most.relevant.quotes <- subset(mostrelevant.top25, Class == "")
levels(factor(most.relevant.quotes$Title))

most.relevant <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/MostRelevant/Strategy3PaperList_04Nov2014.csv", header = T, sep = "\t")
table(most.relevant$SO)
length(levels(factor(most.relevant$SO)))
common.sources <- levels(factor(most.relevant$SO))[which(table(most.relevant$SO) >= 10)]

mostrel.joswithmultcites <- subset(most.relevant, as.character(SO) %in% common.sources)
table(factor(mostrel.joswithmultcites$SO))

# subsets of papers by journal
ecol.let <- subset(most.relevant, as.character(SO) == "ECOLOGY LETTERS", select = c("AU", "TI"))
write.csv(ecol.let, "~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/MostRelevant/Strategy3ExEcolLetters.csv")

prsb <- subset(most.relevant, as.character(SO) == "PROCEEDINGS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES", select = c("AU", "TI"))
write.csv(prsb, "~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search2/MostRelevant/Strategy3ExPRSB.csv")

