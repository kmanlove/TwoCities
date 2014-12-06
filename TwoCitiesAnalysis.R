# load required package igraph
require(igraph)
#require(degreenet)

# read in papers dataset
# keep track of where you saved the dataset; file.choose() will open a navigation window that should let you browse to the dataset's location.
# file.choose is UNTESTED ON MACS, so if it doesn't work, specify the path to the datafile by hand. 


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




#------------------------------------------------------------------------------------------# 
#-- Data are set up with two unique keys: -------------------------------------------------#
#-- Paper.Number = papers in the FULL SET (accepted + rejected = 2258 papers) -------------#
#-- AuthorID = unique author identified: all lower case. last name <space> first initial --#
#------------------------------------------------------------------------------------------#


#-------------------------------------------#
#-- Preliminary description of papers ------#
#-------------------------------------------#
data.frame <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Searches/04Nov2014/Search3_NoPathogens/FullPaperBank_30Nov2014.csv", header = T, sep = "\t")
names(data.frame)[6] <- "KRMKeep"
data.frame <- subset(data.frame, KRMKeep == 1)
dim(data.frame) # 1605 papers

# histogram of distribution of papers among journals
length(levels(factor(data.frame$Source))) # 108 journals represented
data.frame$Source <- factor(data.frame$Source)
table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# order command reorders levels so that they appear in descending frequency
par(mfrow = c(1, 1), las = 2, mar = c(15, 4, 1, 1))
plot(order.source, type = "h", xaxt = "n", ylab = "Frequency", ylim = c(0, 180), xlab = "")
axis(side = 1, at = c(1:108), labels = names(order.source), las = 2, cex.axis = .5)

# histogram of distribution of citations
data.frame$AnnualizedCitationRate <- data.frame$TimesCited / (2014 - data.frame$PubYear)
par(mfrow = c(2, 2))
hist(log(data.frame$TimesCited + 1), col = "grey80", main = "", xaxt = "n", xlab = "log(Total Citations + 1)")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
hist(log(data.frame$AnnualizedCitationRate + 1), col = "grey80", xlab = "log(Annualized Citations + 1)", main = "", xaxt = "n")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ data.frame$PubYear, yaxt = "n", ylab = "log(Annualized citations + 1)", xlab = "year")
axis(side = 2, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))

# aggregated annualized citations of database papers by journal
journal.total.cites <- journal.total.papers <- journal.total.years <- journal.agg.annual.cites <- rep(NA, 108)
journal.subsets <- vector("list", 108)
for(i in 1:108){ # in this loop, calculate annualized citation rate averaged over all papers from each journal
  journal.subsets[[i]] <- subset(data.frame, Source == levels(data.frame$Source)[i])
  journal.total.cites[i] <- sum(journal.subsets[[i]]$TimesCited)
  journal.total.years[i] <- sum(2014 - journal.subsets[[i]]$PubYear)
  journal.agg.annual.cites[i] <- journal.total.cites[i] / journal.total.years[i]
  journal.total.papers[i] <- dim(journal.subsets[[i]])[1]
}

#levels(data.frame$Source)[order(journal.agg.annual.cites, decreasing = T)]
order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# order command reorders levels so that they appear in descending frequency
par(mfrow = c(1, 1), las = 2, mar = c(15, 6, 1, 1))
plot(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)] ~ c(1:108), type = "h", xaxt = "n", ylab = "Avg. annualize citation rate for \n all papers in dataset", xlab = "", lwd = journal.total.papers[order(journal.agg.annual.cites, decreasing = T)] * .5)
text(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)][1:20] ~ c(seq(1:20) + 0.25), labels = paste("(", journal.total.papers[order(journal.agg.annual.cites, decreasing = T)][1:20], ")", sep = ""), cex = .6)
axis(side = 1, at = c(1:108), labels = levels(data.frame$Source)[order(journal.agg.annual.cites, decreasing = T)]
     , las = 2, cex.axis = .5)

#--------------------------------------------------------------------------#
#-- Extract author information and build author info storage structures ---#
#--------------------------------------------------------------------------#
# extract author institution information from data.frame$C1
institution.list <- institution.list2 <- author.institutions <- author.institutions.expanded <- author.list <- author.lastonly <- author.list.out <- First <- Last <- author.frame <- data.list.au <- vector("list", length = dim(data.frame)[1])
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # trim cuts leading whitespace off of any character string
for(i in 2:dim(data.frame)[1]){ # loop starts at 2 because paper 1 is authored by Anonymous and there's no info in the author fields
  # extract authors from data.frame$Author field
  author.list[[i]] <- strsplit(x = as.character(data.frame[i, ]$Authors), split = ";")[[1]]
  for(j in 1:length(author.list[[i]])){
    author.list[[i]][j] <- trim(author.list[[i]][j]) # trim cuts leading whitespace off of any character string
    author.lastonly[[i]][j] <- strsplit(x = (author.list[[i]][j]), split = ",", fixed = T)[[1]][1] # pulls of first name/initials from author.list elements
  }
  # extract institutions for each author from C1 and merge with authorlist
  institution.list[[i]] <- strsplit(x = as.character(data.frame[i, ]$C1), split = "; [", fixed = T)[[1]] # splits apart all institutions listed in data.frame$C1
  if(length(institution.list[[i]]) != 0){ # if there are institutions listed
    author.institutions.expanded[[i]] <- vector("list", length(institution.list[[i]])) # build a list that is length-institutions long.
    if(length(institution.list[[i]]) == 1){ # if everyone's in the same institution
      author.list.out[[i]] <- cbind(author.list[[i]], rep(institution.list[[i]][1], length(author.list[[i]])), rep(data.frame$Paper.Number[i], length(author.list[[i]])), rep(as.character(data.frame$Title[i]), length(author.list[[i]])))
      # cbind author names, institution (there's only one) rep'd number-of-authors times.
    } else { # if there are multiple institutions
      for(k in 1:length(institution.list[[i]])){ # loop over the different institutions
        author.institutions[[i]][k] <- strsplit(x = as.character(institution.list[[i]][k]), split = "]", fixed = T)[[1]][1]
        # extract the authors listed before the institutions (authors listed in [] and sep'd by ";")
        if(k == 1){ # for first element in list, character string is different ([authors] inst;), so clean up first element of author.institutions[[i]] 
          author.institutions[[i]][k] <- strsplit(x = as.character(author.institutions[[i]][k]), split = "[", fixed = T)[[1]][2]
        }
        institution.list2[[i]][k] <- strsplit(x = as.character(institution.list[[i]][k]), split = "]", fixed = T)[[1]][2]
        author.institutions.expanded[[i]][[k]] <- cbind((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]), rep(institution.list2[[i]][k], length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]))) , rep(data.frame$Paper.Number[i], length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]))), rep(as.character(data.frame$Title[i]), length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]))))
      }
      # merge authors in author.list and institutions in institution.list2
      author.list.out[[i]] <- do.call("rbind", author.institutions.expanded[[i]])
    }
  }
  else{
    author.list.out[[i]] <- cbind(author.list[[i]], rep(NA, length(author.list[[i]])), rep(data.frame$Paper.Number[i], length(author.list[[i]])), rep(as.character(data.frame$Title[i]), length(author.list[[i]])))
  }
}

author.full.frame <- as.data.frame(do.call("rbind", author.list.out)) # unlist author.list.out and store as one large dataframe
names(author.full.frame) <- c("FullName", "FullAffil", "Paper.Number", "PaperTitle")

# parse text in FullAffil field to build unique author ID and get indicators for presence of different keywords in author affiliation
author.full.frame$LastName <- author.full.frame$FirstName <- author.full.frame$AuthorID <- author.full.frame$AllInits <- author.full.frame$OneInit <- author.full.frame$Dept <- author.full.frame$Sch <- author.full.frame$Univ <- author.full.frame$Math <- author.full.frame$Ecol <- author.full.frame$Epi <- author.full.frame$Evol <- author.full.frame$Stat <- author.full.frame$Vet <- author.full.frame$Ctr <- author.full.frame$Biol <- author.full.frame$Med <- rep(NA, dim(author.full.frame)[1])
for(i in 1:dim(author.full.frame)[1]){
  # split names into first and last vectors
  author.full.frame$LastName[i] <- tolower(strsplit(x = as.character((author.full.frame[i, 1])), split = ",", fixed = T)[[1]][1]) # strip off last name; make all characters lower-case
  author.full.frame$FirstName[i] <- trim(strsplit(x = as.character((author.full.frame[i, 1])), split = ",", fixed = T)[[1]][2]) # strip off first name; remove leading whitespace with "trim"
  # refine FirstName so that it's only initials
  author.full.frame$FirstName[i] <- gsub("[.]", "", author.full.frame$FirstName[i])[[1]] # remove periods
  author.full.frame$AllInits[i] <- gsub("[a-z]", "", author.full.frame$FirstName[i])[[1]] # remove lower-case letters
  author.full.frame$AllInits[i] <- tolower(gsub(" ", "", author.full.frame$AllInits[i])[[1]]) # remove spaces; change initials to lower-case
  author.full.frame$OneInit[i] <- substring(author.full.frame$AllInits[i], 1, 1) # remove all characters past first one
  author.full.frame$AuthorID[i] <- trim(paste(author.full.frame$LastName[i], " ", author.full.frame$OneInit[i], sep = ""))
  # create indicators for the strings "Univ", "Sch Med" or "Med Sch", "Ctr", "Math", "Stat", "Ecol", "Evol", "Epi", "Vet", "Biol" appearing in FullAffil
  author.full.frame$Univ[i] <- ifelse(length(grep("Univ", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Math[i] <- ifelse(length(grep("Math", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Stat[i] <- ifelse(length(grep("Stat", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Ecol[i] <- ifelse(length(grep("Ecol", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Evol[i] <- ifelse(length(grep("Evol", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Biol[i] <- ifelse(length(grep("Bio", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Epi[i] <- ifelse(length(grep("Epi", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Ctr[i] <- ifelse(length(grep("Ctr", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Vet[i] <- ifelse(length(grep("Vet", author.full.frame$FullAffil[i])) >= 1, 1, 0)
  author.full.frame$Med[i] <- ifelse(((length(grep("Med Sch", author.full.frame$FullAffil[i])) >= 1) | (length(grep("Sch Med", author.full.frame$FullAffil[i])) >= 1)), 1, 0)
  #  print(i)
}

# merge multiple records for same author
unique.authors <- length(levels(factor(author.full.frame$AuthorID))) #  unique authors
author.papers <- vector("list", unique.authors)
author.unique.frame <- as.data.frame(matrix(NA, nrow = unique.authors, ncol = 11))
names(author.unique.frame) <- c("AuthorID", "TotPapers", "TotUniv", "TotMath", "TotStat", "TotEcol", "TotEvol", "TotEpi", "TotMed", "TotVet", "TotBiol")

for(i in 1:unique.authors){ # build dataframe with one row per author, and indicators author math, stat, ecol, evol, etc. affiliations over ALL affiliations for that author
  # extract all references for a given AuthorID
  author.papers[[i]] <- subset(author.full.frame, AuthorID == levels(factor(author.full.frame$AuthorID))[i])
  author.unique.frame$AuthorID[i] <- trim(levels(factor(author.full.frame$AuthorID))[i])
  author.unique.frame$TotPapers[i] <- length(levels(factor(author.papers[[i]]$Paper.Number)))
  author.unique.frame$TotUniv[i] <- ifelse(sum(author.papers[[i]]$Univ) >= 1, 1, 0)
  author.unique.frame$TotMath[i] <- ifelse(sum(author.papers[[i]]$Math) >= 1, 1, 0)
  author.unique.frame$TotStat[i] <- ifelse(sum(author.papers[[i]]$Stat) >= 1, 1, 0)
  author.unique.frame$TotEcol[i] <- ifelse(sum(author.papers[[i]]$Ecol) >= 1, 1, 0)
  author.unique.frame$TotEvol[i] <- ifelse(sum(author.papers[[i]]$Evol) >= 1, 1, 0)
  author.unique.frame$TotBiol[i] <- ifelse(sum(author.papers[[i]]$Biol) >= 1, 1, 0)
  author.unique.frame$TotEpi[i] <- ifelse(sum(author.papers[[i]]$Epi) >= 1, 1, 0)
  author.unique.frame$TotMed[i] <- ifelse(sum(author.papers[[i]]$Med) >= 1, 1, 0)
  author.unique.frame$TotVet[i] <- ifelse(sum(author.papers[[i]]$Vet) >= 1, 1, 0)
  author.unique.frame$TotCtr[i] <- ifelse(sum(author.papers[[i]]$Ctr) >= 1, 1, 0)
  #  print(i)
}

#-----------------------------------------#
#-- Author network -----------------------#
#-----------------------------------------#

authors <- trim(levels(factor(author.unique.frame$AuthorID)))
author.mat <- matrix(0, nrow = length(authors), ncol = length(authors))
for(i in 1:length(authors)){
  author.refs <- subset(author.full.frame, AuthorID == authors[i])
  paper.numbers <- as.numeric(as.character(unique(author.refs$Paper.Number)))
  coauthors <- unique(as.data.frame((subset(author.full.frame, Paper.Number %in% paper.numbers, select = c(AuthorID, Paper.Number)))))
  author.edgeweights <- as.vector(table(coauthors$AuthorID))
  addvals <- which(authors %in% names(table(coauthors$AuthorID)))
  author.mat[i, addvals] <- author.edgeweights
  print(i)
}

diag(author.mat) <- rep(0, dim(author.mat)[1])
author.vertex.size <- apply(author.mat, 1, sum) 

# description of author graph
author.graph <- graph.adjacency(author.mat, mode = "undirected")
author.compos <- clusters(author.graph)
author.compos$csize[which.max(author.compos$csize[-1])] # get size of second-largest component
table(author.compos$csize == 1) # table isolated nodes
avg.path.length.au <- average.path.length(author.graph)
avg.degree.au <- mean(degree(author.graph))
diam.au <- diameter(author.graph)
author.unique.frame$degrees <- degree(author.graph)
author.unique.frame$closeness <- centralization.closeness(author.graph)$res
author.unique.frame$betweenness <- centralization.betweenness(author.graph, directed = F)$res
power.law.fit.au <- power.law.fit(author.degrees)

# specify some node attributes and begin plotting
V(author.graph)$size <- author.vertex.size
V(author.graph)$name <- authors
par(mfrow = c(1, 1))
plot(author.graph, margin = c(-.75, -.75, 0, 0), vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)


# author communities
walktr.au <- walktrap.community(author.graph, steps = 20)
membership(walktr.au)
sizes(walktr.au)
component1.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 1]
component2.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 2]
component3.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 3]
plot(walktr.au, margin = c(-.75, -.75, 0, 0), author.graph, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = 3)

# cut down to just authors in communities of 10 or more
communities.to.include.au <- which(sizes(walktr.au) >= 20)
vertex.inclusion.ind.au <- ifelse(membership(walktr.au) %in% communities.to.include.au, 1, 0)
author.graph.small <- delete.vertices(author.graph, which(vertex.inclusion.ind.au == 0))
multi.au <- multilevel.community(author.graph.small)

vertex.labels.verysmall <- rep(NA, length(V(author.graph.small)$name))
for(i in 1:length(vertex.labels.verysmall)){
  vertex.labels.verysmall[i] <- ifelse (i %in% which(V(author.graph.small)$size >= 50), toupper(gsub(" .", "", V(author.graph.small)$name[i])), "")
}

#label.vertices <- which(V(author.graph.small)$size >= 75)
labeled.scientists <- V(author.graph.small)$name[which(V(author.graph.small)$size >= 50)]
labeled.scientists

#plot(walktr2.au, vertex.frame.color = rgb(red = 255, blue = 255, green = 255, alpha = 1, maxColorValue = 255), vertex.label = vertex.labels.verysmall,  vertex.label.cex = 1.2, vertex.label.color = "black", margin = c(-.85, -.85, -.2, -.6), author.graph.small, layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = rgb(red = 100, green = 100, blue = 100, alpha = .50, maxColorValue = 255), edge.arrow.size = .3, vertex.label = "", vertex.size = V(author.graph.small)$size / 12)
plot(author.graph.small, vertex.frame.color = "grey70", vertex.label = vertex.labels.verysmall,  vertex.label.cex = .5, vertex.label.color = "black", margin = c(-.9, -.95, -.3, -.7), layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = "grey80", edge.arrow.size = 0, vertex.label = "", vertex.size = V(author.graph.small)$size / 12, vertex.color = "grey80")
plot(multi.au, author.graph.small, vertex.frame.color = "grey70", vertex.label = vertex.labels.verysmall,  vertex.label.cex = .5, vertex.label.color = "black", margin = c(-.9, -.95, -.3, -.7), layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = "grey80", edge.arrow.size = 0, vertex.label = "", vertex.size = V(author.graph.small)$size / 12, vertex.color = "grey80")


#---------------------------------#
#-- paper network ----------------#
#---------------------------------# 
# 1) extract citations for each paper and store in citation.list
trim.leading <- function (x)  sub("^\\s+", "", x)
citation.list <- citation.frame <- citation.frame.small <- vector("list", dim(data.frame)[1])
first.author <- pub.year <- rep(NA, dim(data.frame)[1])
papers.with.cites <- c(1:dim(data.frame)[1])[-c(1, 484, 591, 843)]
for(i in papers.with.cites){
  citation.list[[i]] <- strsplit(x = as.character(data.frame$CitedRefs)[i], split = ";")[[1]]
  citation.frame[[i]] <- matrix(NA, nrow = length(citation.list[[i]]), ncol = 15)
  citation.frame.small[[i]] <-  matrix(NA, nrow = length(citation.list[[i]]), ncol = 12)
  first.author[i] <- strsplit(as.character(data.frame$Authors[i]), split = ";")[[1]][1]
  pub.year[i] <- data.frame$PubYear[i]
  for(j in 1:length(citation.list[[i]])){
    citation.frame.small[[i]][j, ] <- trim.leading(c(strsplit(citation.list[[i]][j], split = ",")[[1]], rep(NA, 12 - length(strsplit(citation.list[[i]][j], split = ",")[[1]]))))
    if(is.na(as.numeric(citation.frame.small[[i]][j, 1])) == F){
      citation.frame.small[[i]][j, ] <- c(NA, citation.frame.small[[i]][j, -12])
    }
    citation.frame[[i]][j, ] <- c(as.character(data.frame$DOI)[i], as.character(first.author[i]), pub.year[i], citation.frame.small[[i]][j, ])
    citation.frame[[i]][j, 1] <- paste("DOI ", citation.frame[[i]][j, 1], sep = "")    
    citation.frame[[i]][j, 2] <- tolower(trim.leading(citation.frame[[i]][j, 2]))
    citation.frame[[i]][j, 4] <- tolower(trim.leading(citation.frame[[i]][j, 4]))
  }
}

full.citation.frame <- do.call("rbind", citation.frame) # 68651 total refs

# loop over papers. build relations into 1605 X 1605 matrix
# rows are cited paper, cols are new papers
assoc.mat <- matrix(NA, nrow = 1605, ncol = 1605)
#assoc.list <- vector("list", 1605)
for(i in papers.with.cites){
  for(j in papers.with.cites){
    if(i == j){
      assoc.mat[i, j] <- NA
    } else{
      assoc.mat[i, j] <- ifelse(((citation.frame[[i]][1, 1] %in% citation.frame[[j]][ ,9]) | (paste(citation.frame[[i]][1 ,2], " ", citation.frame[[i]][1, 3]) %in% paste(citation.frame[[j]][ , 4], " ", citation.frame[[j]][, 5]))), 1, 0)
    }
  }
  print(i)
}

paper.graph <- graph.adjacency(assoc.mat)

# description of paper graph
paper.compos <- clusters(paper.graph, mode = "weak")
paper.compos$csize[which.max(paper.compos$csize[-which.max(paper.compos$csize)])] # get size of second-largest component
table(paper.compos$csize == 1) # table isolated nodes
avg.path.length.paper <- average.path.length(paper.graph)
avg.degree.paper <- mean(degree(paper.graph, mode = "in"))
diam.paper <- diameter(paper.graph)
power.law.fit.paper <- power.law.fit(degree(paper.graph, mode = "all"))

# plots for paper graph
par(mfrow = c(1, 1))
plot(paper.graph, vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)
paper.degree.dist <- degree(paper.graph, mode = "in")
paper.betweenness <- betweenness(paper.graph)
paper.hierarchy <- hierarchy(paper.graph)
paper.optim.community <- optimal.community(paper.graph)



#-----------------------------------------#
#-- Journal network ----------------------#
#-----------------------------------------#
journal1 <- journal2 <- matrix(NA, nrow = 1605, ncol = 1605)
for(i in papers.with.cites){
  for(j in papers.with.cites){
    if(is.na(assoc.mat[i, j]) == F & (assoc.mat[i, j] == 1) == T){
      journal1[i, j] <- as.character(data.frame[i, ]$Source)
      journal2[i, j] <- as.character(data.frame[j, ]$Source)
    } else{
      journal1[i, j] <- NA
      journal2[i, j] <- NA
    }
  }
}

journal1.vec <- as.character(na.omit(as.vector(journal1)))
journal2.vec <- as.character(na.omit(as.vector(journal2)))
journal.frame <- as.data.frame(cbind(journal1.vec, journal2.vec))
journal.frame$journal1.vec <- factor(journal.frame$journal1.vec, levels = levels(factor(c(journal1.vec, journal2.vec))))
journal.frame$journal2.vec <- factor(journal.frame$journal2.vec, levels = levels(factor(c(journal1.vec, journal2.vec))))

journal.crosstab <- table(journal.frame$journal1.vec, journal.frame$journal2.vec) # 95 journals connected via edges
journal.graph <- graph.adjacency(journal.crosstab, mode = "directed", weighted = T)

# description of journal graph
journal.compos <- clusters(journal.graph, mode = "weak")
journal.compos$csize[which.max(journal.compos$csize[-which.max(journal.compos$csize)])] # get size of second-largest component
table(journal.compos$csize == 1) # table isolated nodes
avg.path.length.journal <- average.path.length(journal.graph)
avg.degree.journal <- mean(degree(journal.graph, mode = "in"))
diam.journal <- diameter(journal.graph)
power.law.fit.journal <- power.law.fit(degree(journal.graph, mode = "all"))

journal.graph.small <- delete.vertices(journal.graph, which(degree(journal.graph) < 1) - 1)
V(journal.graph.small)$name
plot(journal.graph.small, layout = layout.fruchterman.reingold, vertex.size = log(table(journal.frame$journal1.vec) + 1) * 2, vertex.label = NA, edge.arrow.size = .05)

# journal communities
walktr <- walktrap.community(journal.graph, steps = 6)
membership(walktr)
sizes(walktr)
component1.journals <- vertex.attributes(journal.graph)$name[membership(walktr) == 1]
component2.journals <- vertex.attributes(journal.graph)$name[membership(walktr) == 2]
component3.journals <- vertex.attributes(journal.graph.small)$name[membership(walktr) == 3]
plot(walktr, journal.graph, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = 3)

#-- cut down to just the journals in communities of 2 or more, and cut number of steps in walk --#
communities.to.include <- which(sizes(walktr) >= 2)
vertex.inclusion.ind <- ifelse(membership(walktr) %in% communities.to.include, 1, 0)
journal.graph.very.small <- delete.vertices(journal.graph.small, which(vertex.inclusion.ind == 0))
walktr2 <- walktrap.community(journal.graph.very.small, steps = 8)
plot(walktr2, journal.graph.very.small, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = 3)

sizes(walktr2)
grp1 <- vertex.attributes(journal.graph.very.small)$name[membership(walktr2) == 1]
grp2 <- vertex.attributes(journal.graph.very.small)$name[membership(walktr2) == 2]
grp3 <- vertex.attributes(journal.graph.very.small)$name[membership(walktr2) == 3]
grp4 <- vertex.attributes(journal.graph.very.small)$name[membership(walktr2) == 4]

# Qu: which journals are excluded?
disconnected.journals <- levels(factor(data.frame$Source))[!(levels(factor(data.frame$Source)) %in% levels(journal.frame$journal1.vec))]

# get distribution of author disciplines for each community

#-------------------------------------------------------------------------------------#
#-- regression of avg. author closeness and betweenness on annualized citation rate --#
#-------------------------------------------------------------------------------------#
# to get paper's authorship diversity, 
# 1) extract authors from author.unique.frame by looping over paper numbers in author full frame, extracting authorID, and subsetting on authorID in author.unique.frame
# 2) sum (math, stat, ecol, evol, epi, med) across all paper authors

data.frame$avg.author.degree <- data.frame$avg.author.between <- data.frame$avg.author.close <- data.frame$author.diversity <- data.frame$total.author.ctrs <- data.frame$num.authors.in.ctrs <- data.frame$num.authors <- data.frame$math.author <- data.frame$epi.author <- data.frame$ecoevo.author <- data.frame$biol.author <- data.frame$med.author <- data.frame$vet.author <- data.frame$stat.author <- data.frame$discipline.class <- rep(NA, dim(data.frame)[1])

for(i in 1:dim(data.frame)[1]){
  k <- subset(author.full.frame, as.numeric(as.character(Paper.Number)) == as.numeric(as.character(data.frame$Paper.Number))[i])
  if(dim(k)[1] >= 1){
    AuthorIDs <- trim(levels(factor(k$AuthorID)))
    author.subset <- subset(author.unique.frame, AuthorID %in% AuthorIDs)
    data.frame$stat.author[i] <- ifelse(sum(author.subset$TotStat) >= 1, 1, 0)  
    data.frame$epi.author[i] <- ifelse(sum(author.subset$TotEpi) >= 1, 1, 0)  
    data.frame$ecoevo.author[i] <- ifelse((sum(author.subset$TotEcol) >= 1 |sum(author.subset$TotEvol) >= 1) , 1, 0) 
    data.frame$biol.author[i] <- ifelse(sum(author.subset$TotBiol) >= 1, 1, 0) 
    data.frame$med.author[i] <- ifelse(sum(author.subset$TotMed) >= 1, 1, 0) 
    data.frame$math.author[i] <- ifelse(sum(author.subset$TotMath) >= 1, 1, 0) 
    data.frame$vet.author[i] <- ifelse(sum(author.subset$TotVet) >= 1, 1, 0) 
    data.frame$author.diversity[i] <- data.frame$stat.author[i] + data.frame$ecoevo.author[i] + data.frame$biol.author[i] + data.frame$med.author[i] + data.frame$math.author[i] + data.frame$math.author[i] + data.frame$vet.author[i]   
    data.frame$total.author.ctrs[i] <- sum(author.subset$TotCtr)
    data.frame$num.authors.in.ctrs[i] <- length(which(author.subset$TotCtr >= 1))
    data.frame$num.authors[i] <- dim(k)[1]
    data.frame$avg.author.degree[i] <- mean(author.subset$degree)
    data.frame$avg.author.between[i] <- mean(author.subset$betweenness)
    data.frame$avg.author.close[i] <- mean(author.subset$closeness)
    data.frame$discipline.class[i] <- ifelse((data.frame$math.author[i] == 1 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "1-math",
                                             ifelse((data.frame$math.author[i] == 0 & (data.frame$ecoevo.author[i] == 1 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "1-bio", 
                                                    ifelse((data.frame$math.author[i] == 0 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "1-stat",
                                                           ifelse((data.frame$math.author[i] == 0 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 0), "1-med",
                                                                  ifelse((data.frame$math.author[i] == 0 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 1), "1-vet",  
                                                                         ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 1 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "2-mathbio",  
                                                                                ifelse((data.frame$math.author[i] == 1 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "2-mathstat",   
                                                                                       ifelse((data.frame$math.author[i] == 1 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 0), "2-mathmed", 
                                                                                              ifelse((data.frame$math.author[i] == 1 & data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0 & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 1), "2-mathvet",
                                                                                                     ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 0), "3-mathbiostat",
                                                                                                            ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 0), "3-mathbiomed", 
                                                                                                                   ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 1), "3-mathbiovet",
                                                                                                                          ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 0), "3-mathstatmed",
                                                                                                                                 ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 1), "3-mathstatvet",
                                                                                                                                        ifelse((data.frame$math.author[i] == 1 & (data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 1), "3-mathmedvet",
                                                                                                                                               ifelse((data.frame$math.author[i] == 0 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 0), "3-biostatmed",
                                                                                                                                                      ifelse((data.frame$math.author[i] == 0 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 0 & data.frame$vet.author[i] == 1), "3-biostatvet", 
                                                                                                                                                             ifelse((data.frame$math.author[i] == 0 & (data.frame$ecoevo.author[i] == 0 | data.frame$biol.author[i] == 1) & data.frame$stat.author[i] == 0 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 1), "3-biomedvet",  
                                                                                                                                                                    ifelse((data.frame$math.author[i] == 0 & (data.frame$ecoevo.author[i] == 0 & data.frame$biol.author[i] == 0) & data.frame$stat.author[i] == 1 & data.frame$med.author[i] == 1 & data.frame$vet.author[i] == 1), "3-statmedvet", 
                                                                                                                                                                           "FourDiscip")))))))))))))))))))
    print(i)
  }
}

# PLOTS: author discipline diversity and Number center affiliations by annualized citation rate
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2), oma = c(1, 1, 0, 0))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$author.diversity), xaxt = "n", yaxt = "n", xlab = "Author discipline diversity", ylab = "Annualized citation rate", col = "grey80")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, at = c(log(1), log(5), log(10), log(50)), labels = c("1", "5", "10", "50"), cex.axis = .7)
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$num.authors.in.ctrs), xaxt = "n", yaxt = "n", xlab = "Number of authors with center affiliations", ylab = "Annualized citation rate", col = "grey80")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, at = c(log(1), log(5), log(10), log(50)), labels = c("1", "5", "10", "50"), cex.axis = .7)
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$num.authors), xaxt = "n", yaxt = "n", xlab = "Number of authors", ylab = "Annualized citation rate", col = "grey80")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, at = c(log(1), log(5), log(10), log(50)), labels = c("1", "5", "10", "50"), cex.axis = .7)
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$discipline.class), xaxt = "n", yaxt = "n", xlab = "Discipline class", ylab = "Annualized citation rate", col = "grey80")
axis(side = 1, at = seq(1:length(levels(factor(data.frame$discipline.class)))), labels = levels(factor(data.frame$discipline.class)), las = 2, cex.axis = .7)
axis(side = 2, at = c(log(1), log(5), log(10), log(50)), labels = c("1", "5", "10", "50"), cex.axis = .7)

papers.with.citrate <- subset(data.frame, is.na(AnnualizedCitationRate) == F & is.infinite(AnnualizedCitationRate) == F)
close.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.close[-1], data = papers.with.citrate)
between.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.between[-1], data = papers.with.citrate)
degree.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.degree[-1], data = papers.with.citrate)
cor(subset(papers.with.citrate[-1, ], select = c(avg.author.degree, avg.author.between, avg.author.close)))
require(lme4)
saturated.fit <- lmer(log(AnnualizedCitationRate + 1) ~ log(avg.author.between + 1) + (1 | Source), data = papers.with.citrate[-1, ])

# extract papers with very high residuals in saturated model
papers.with.citrate$sat.resids[2:1455] <- residuals(saturated.fit)
sat.quantiles <- quantile(papers.with.citrate$sat.resids[-1], c(0.025, 0.975))
outliers <- which(papers.with.citrate$sat.resids <= sat.quantiles[1] | papers.with.citrate$sat.resids >= sat.quantiles[2])
outlier.papers <- papers.with.citrate[outliers, ]

outlier.view.sub <- subset(outlier.papers, select = c("Authors", "Title", "Paper.Number", "Source", "AnnualizedCitationRate", "avg.author.close", "avg.author.between", "sat.resids"))
outlier.view.sub[21:40, ]

#------------------------------------#
#-- OLD -----------------------------#
#------------------------------------#

ref.data.frame.out <- as.data.frame(do.call(rbind, data.frame))
names(ref.data.frame.out) <- c("OrigDOI", "Author", "Year", "Journal", "Var4", "Var5", "Var6", rep(NA, 8))
sorted.refs <- ref.data.frame.out[order(ref.data.frame.out$Year), ]
write.csv(ref.data.frame.out, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/SortedReferenceDataFrame_16Sept2014.csv")
table(ref.data.frame.out$Year)

DOI1.vec <- unlist(DOI1)
DOI2.vec <- unlist(DOI2)

length(DOI1.vec)
length(DOI2.vec)

edgelist.dataframe <- as.data.frame(cbind(DOI1.vec, DOI2.vec))
edgelist.data.frame <- edgelist.dataframe[complete.cases(edgelist.dataframe), ]
doi.data.frame <- subset(edgelist.data.frame, DOI2.vec %in% DOI1.vec)
doi.data.frame$Journal1 <- doi.data.frame$Journal2 <- rep(NA, dim(doi.data.frame)[1])
for(i in 1:dim(doi.data.frame)[1]){
  ref1 <- subset(graph.data.frame, as.character(DI) == as.character(doi.data.frame[i, 1]))
  ref2 <- subset(graph.data.frame, as.character(DI) == as.character(doi.data.frame[i, 2]))
  doi.data.frame$Journal1[i] <- as.character(ref1$SO[1])
  doi.data.frame$Journal2[i] <- as.character(ref2$SO[1])
}



#doi.data.frame$Journal1 <- as.factor(doi.data.frame$Journal1)
#doi.data.frame$Journal2 <- as.factor(doi.data.frame$Journal2)
all.journals <- c(as.character(doi.data.frame$Journal1), as.character(doi.data.frame$Journal2))
all.journals.small <- levels(factor(all.journals))
doi.data.frame$Journal1 <- factor(doi.data.frame$Journal1, levels = all.journals.small)
doi.data.frame$Journal2 <- factor(doi.data.frame$Journal2, levels = all.journals.small)

#levels(doi.data.frame$Journal1) <- all.journals.small
#levels(doi.data.frame$Journal2) <- all.journals.small

journal.crosstab <- table(doi.data.frame$Journal1, doi.data.frame$Journal2)
journal.graph <- graph.adjacency(journal.crosstab, mode = "directed", weighted = T)
V(journal.graph)$size <- table(doi.data.frame$Journal2)

# #-- which journals are disconnected from all others? --#
# journals.to.drop <- which(times.cited == 1)
# journal.crosstab.small <- journal.crosstab[- journals.to.drop, - journals.to.drop]
# journal.graph <- graph.adjacency(journal.crosstab.small, mode = "directed")
# plot(journal.graph)
# plot(journal.graph, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = log(times.cited[which(times.cited != 0)] + 1) * 2)

journal.graph.small <- delete.vertices(journal.graph, which(degree(journal.graph) < 1) - 1)

#-- betweenness in journal graph --#
journal.ebc <- edge.betweenness.community(journal.graph.small)
journal.fg <- fastgreedy.community(journal.graph.small)
#journal.lp <- label.propagation.community(journal.graph.small) -- crashed R
journal.spinglass <- spinglass.community(journal.graph.small)
# mods <- sapply(0:ecount(journal.graph.small), 
#   function(i){
#     journal.graph.small2 <- delete.edges(journal.graph.small, journal.ebc$removed.edges[seq(length = i)])
#     journal.clusters <- clusters(journal.graph.small2)$membership
#     modularity(journal.graph.small, journal.clusters)
#   }
# )
# 
# plot(mods, pch = 20)
# 
# journal.graph2 <- delete.edges(journal.graph, journal.ebc$removed.edges[seq(length = which.max(mods) - 1)])
# V(journal.graph2)$color = clusters(journal.graph2)$membership
# journal.graph$layout <- layout.fruchterman.reingold
# plot(journal.graph2, vertex.label = NA)
# 
# #-- fast-greedy communities --#
# fc <- fastgreedy.community(journal.graph.small)
# com <- community.to.membership(journal.graph, fc$merges, steps = which.max(fc$modularity) - 1)
# V(journal.graph)$color <- com$membership+1
# journal.graph$layout <- layout.fruchterman.reingold
# plot(journal.graph, vertex.label = NA)

#-- walktrap communities --#
walktr <- walktrap.community(journal.graph, steps = 6)
membership(walktr)
sizes(walktr)
component6.journals <- vertex.attributes(journal.graph)$name[membership(walktr) == 6]
component10.journals <- vertex.attributes(journal.graph)$name[membership(walktr) == 10]
component11.journals <- vertex.attributes(journal.graph.small)$name[membership(walktr) == 11]
component12.journals <- vertex.attributes(journal.graph)$name[membership(walktr) == 12]
component2.journals <- vertex.attributes(journal.graph.small)$name[membership(walktr) == 2]
plot(walktr, journal.graph, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = 3)

#-- cut down to just the journals in communities of 2 or more, and cut number of steps in walk --#
communities.to.include <- which(sizes(walktr) >= 2)
vertex.inclusion.ind <- ifelse(membership(walktr) %in% communities.to.include, 1, 0)
journal.graph.small <- delete.vertices(journal.graph, which(vertex.inclusion.ind == 0))
walktr2 <- walktrap.community(journal.graph.small, steps = 3)
plot(walktr2, journal.graph.small, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = "", vertex.size = 3)

sizes(walktr2)
#grp1 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 1]
#grp2 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 2]
grp3 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 3]
grp4 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 4]
grp5 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 5]
grp9 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 9]
grp12 <- vertex.attributes(journal.graph.small)$name[membership(walktr2) == 12]

#-- cut down to just the journals in the five largest communities --#
communities.to.include2 <- c(3, 4, 5, 9, 12)
vertex.inclusion.ind2 <- ifelse(membership(walktr2) %in% communities.to.include2, 1, 0)
journal.graph.verysmall <- delete.vertices(journal.graph.small, which(vertex.inclusion.ind2 == 0))
walktr3 <- walktrap.community(journal.graph.verysmall, steps = 4)
sizes(walktr3)
grp1 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 1]
grp2 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 2]
grp3 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 3]
grp4 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 4]
grp5 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 5]
grp6 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 6]
grp7 <- vertex.attributes(journal.graph.verysmall)$name[membership(walktr3) == 7]
#-- label every 10th node --#
vertex.labels.verysmall <- vertex.lab.colors <- rep(NA, length(V(journal.graph.verysmall)$name))
for(i in 1:length(vertex.labels.verysmall)){
  vertex.labels.verysmall[i] <- ifelse (i %% 4 != 0, "", V(journal.graph.verysmall)$name[i])
  vertex.lab.colors[i] <- ifelse(membership(walktr3)[i] == 1, "red3", ifelse(membership(walktr3)[i] == 2, "darkorange",
                                                                             ifelse(membership(walktr3)[i] == 3, "forestgreen", 
                                                                                    ifelse(membership(walktr3)[i] == 4, "aquamarine4",
                                                                                           ifelse(membership(walktr3)[i] == 5, "blue", ifelse(membership(walktr3)[i] == 6, "purple",  "black"))))))
}



makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

edge.col <- makeTransparent("black", alpha = 30)

plot(walktr3, journal.graph.verysmall, layout = layout.fruchterman.reingold, edge.arrow.size = .3, vertex.label = vertex.labels.verysmall, vertex.size = (V(journal.graph.verysmall)$size + 1) / 10, vertex.label.cex = .8, vertex.label.color = vertex.lab.colors, margin = c(-.25, -.75, -.25, -.75), edge.color = edge.col)
legend(x = -1.3, y = -.75, bty = "n", fill = c("red3", "darkorange", "forestgreen", "aquamarine3", "purple", "blue"), c("General Biology", "Micro/Food Sci.", "Quantitative Biology", "Stats/Biostats", "Biotech", "Veterinary"))

write.csv(grp1, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/GeneralBiol.csv")
write.csv(grp2, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/MicroFoodSci.csv")
write.csv(grp3, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/QuantBiol.csv")
write.csv(grp4, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/Stats.csv")
write.csv(grp5, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/Vet.csv")
write.csv(grp6, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/Biotech.csv")

included.journals <- V(journal.graph.verysmall)$name
all.journals.short <- levels(factor(all.journals))
excluded.journals <- all.journals.short[!(all.journals.short %in% included.journals)]

write.csv(excluded.journals, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalGroupMemberships/ExcludedJournals.csv")

#----------------------------------------#
#-- END build journal citation network --#
#----------------------------------------#

DOI1.vec <- unlist(DOI1)
DOI2.vec <- unlist(DOI2)

length(DOI1.vec)
length(DOI2.vec)

edgelist.dataframe <- as.data.frame(cbind(DOI1.vec, DOI2.vec))
edgelist.data.frame <- edgelist.dataframe[complete.cases(edgelist.dataframe), ]

#----------------------------------------------#
#-- get author list ---------------------------#
#----------------------------------------------#

author.list <- First <- Last <- author.frame <- data.list.au <- vector("list", length = dim(graph.data.frame)[1])
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
for(i in 1:dim(graph.data.frame)[1]){
  author.list[[i]] <- strsplit(x = as.character(graph.data.frame[i, ]$AF), split = ";")[[1]]
  #  author.frame[[i]] <- matrix(nrow = length(author.list[[i]]), ncol = 2)
  # if(length(author.list[[i]]) >=2){
  for(j in 1:length(author.list[[i]])){
    author.list[[i]][j] <- trim(author.list[[i]][j])
    #      author.frame[[i]][j, ] <- strsplit(as.character(author.list[[i]][j]), split = ",")[[1]]
    #    }
  }
}

author.vec <- unlist(author.list)
author.frame <- matrix(NA, nrow = length(author.vec), ncol = 2)
to.use <- seq(1:dim(author.frame)[1])[-c(246, 742, 770, 974, 981)]
for(i in to.use){
  author.frame[i, ] <- strsplit(author.vec[i], ",")[[1]]
}
#--
length(author.vec)
length(unique(author.vec))
levels(factor(author.vec))

#-- edits on author.vec --#
author.vec <- ifelse(author.vec == "Aerts, J. M.", "Aerts, Jean-Marie", author.vec)
author.vec <- ifelse(author.vec == "Alexander, H. K.", "Alexander, Helen K.", author.vec)
author.vec <- ifelse(author.vec == "Barrios, J. M.", "Barrios, Jose Miguel", author.vec)


#----------------------------------------------#
#-- plot publication network ------------------#
#----------------------------------------------#

graph.obj <- graph.data.frame(edgelist.data.frame, directed = T)
V(graph.obj)$label.cex <- .2
plot(graph.obj, layout=layout.fruchterman.reingold, vertex.label = "", vertex.size = 0.6)

# cut down to only records that were selected for
edgelist.data.frame.reduced <- subset(edgelist.data.frame, DOI2.vec %in% DOI1.vec)
graph.obj.reduced <- graph.data.frame(edgelist.data.frame.reduced, directed = T)
plot(graph.obj.reduced, layout = layout.fruchterman.reingold, vertex.label = "", vertex.size = 2)

# pull in journals for all papers in DOI1 --#
journal <- times.cited <- rep(NA, length(V(graph.obj.reduced)))
for(i in 1:length(journal)){
  k <- subset(graph.data.frame, as.character(DI) == as.character(V(graph.obj.reduced)$name)[i])
  journal[i] <- as.character(k$SO[1])
  times.cited[i] <- k$TC[1]
}

write.csv(levels(factor(journal)), "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalList_21Aug.csv")

#-- sort journals into topical groups (current vsn. arbitrary) --#
levels(factor(journal))
gen.bio <- levels(factor(journal))[c(5, 22, 39, 42, 45, 46, 48)]
topical.bio <- levels(factor(journal))[c(6, 18, 27, 30, 34, 37)]
eco <- levels(factor(journal))[c(12, 13, 14, 20, 47, 51)]
epi.eid <- levels(factor(journal))[c(7, 8, 10, 11, 15, 16, 19, 38, 49, 50, 52, 53)]
vet.med <- levels(factor(journal))[c(2, 17, 40, 54, 55)]
mathstat <- levels(factor(journal))[c(1, 3, 4, 9, 21, 23, 24, 26, 28, 29, 32, 33, 43)]
crossdiscpmodeling <- levels(factor(journal))[c(25, 35, 36)]
big <- levels(factor(journal))[c(31, 41, 44)]

genbio <- levels(factor(journal))[gen.bio]

vert.color <- ifelse(journal %in% gen.bio, "red", 
                     ifelse(journal %in% topical.bio, "grey",
                            ifelse(journal %in% eco, "green",
                                   ifelse(journal %in% epi.eid, "yellow",
                                          ifelse(journal %in% vet.med, "blue",
                                                 ifelse(journal %in% mathstat, "orange",
                                                        ifelse(journal %in% crossdiscpmodeling, "pink",
                                                               ifelse(journal %in% big, "black", "white"))))))))

plot(graph.obj.reduced, layout = layout.fruchterman.reingold, vertex.label = "", vertex.size = log(times.cited + 1) * 3, vertex.color = vert.color)
