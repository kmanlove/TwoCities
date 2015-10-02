# load required packages and source functions
require(igraph)
require(ape)
require(lme4)
require(vegan)
require(graphics)

source("./R/AuthorDiversityPlots.R")
source("./R/CitationRateModel.R")
source("./R/TwoCities_SourceFunctions.R")
source("./R/GAMPlot.R")

GAMPlot(data.frame)

#------------------------------------------------------------------------------------------# 
#-- Data are set up with two unique keys: -------------------------------------------------#
#-- Paper.Number = papers in the FULL SET (accepted + rejected = 2258 papers) -------------#
#-- AuthorID = unique author identified: all lower case. last name <space> first initial --#
#------------------------------------------------------------------------------------------#

#-------------------------------------------#
#-- Preliminary description of papers ------#
#-------------------------------------------#
# data.frame <- read.csv("./Data/IncludedPaperBank_Final.csv", header = T, sep = "\t")
# data.frame <- DataPrep(data.frame)
# # jo.comms <- read.csv("./Data/JournalCommunityVector_16Dec2014.csv")
# jo.comms.2 <- read.csv("./Data/FullPaperBank_FinalWithCommunities.csv", header = T)
# # jo.comms <- jo.comms[-c(1, 494, 603, 858), 2] # four papers removed in DataPrep function
# data.frame$Journal.Community <- data.frame$NumberAuthors <- data.frame$AuthInstAffil <- rep(NA, dim(data.frame)[1])
# for(i in 1:dim(data.frame)[1]){
#   k <- subset(jo.comms.2, Paper.Number == data.frame$Paper.Number[i])
#   data.frame$Journal.Community[i] <- k$JoCommunityShifted[1]
#   data.frame$NumberAuthors[i] <- k$NumberAuthors[1]
#   data.frame$AuthInstAffil[i] <- as.character(k$AuthInstAffil)[1]
#   print(i)
# }

#data.frame <- read.csv("./Data/FullDataFrame_AuthorInstsAndAffils.csv", header = T)
data.frame <- read.csv("./Data/FullDataFrame_AuthorInstsAndAffils_20150928.csv", header = T)

# # jo.comms
# comm1 <- subset(data.frame, Journal.Community == 1)
# comm2 <- subset(data.frame, Journal.Community == 2)
# comm3 <- subset(data.frame, Journal.Community == 3)
# 
# JournalFreqHist(data.frame)
# TimesCitedHists(data.frame)

#-- prep all data --#

all.authors <- BuildAuthorFrame(data.frame.in = data.frame)

# add lead author ID to data.frame 

affils.test <- GetAuthorAffils(author.frame = all.authors)
affils.test$CheckAffils <- rep(NA, dim(affils.test)[1])
for(i in 1:dim(affils.test)[1]){
  k <- factor(subset(affils.test, Paper.Number == affils.test$Paper.Number[i])$FullAffil)
  affils.test$CheckAffils[i] <- ifelse(length(k) == 1, 0, 
                                ifelse(length(levels(k)) == 1, 1, 0)
                                )
  print(i)
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
  print(i)
}


# jo.comms
comm1 <- subset(data.frame, Journal.Community == 1)
comm2 <- subset(data.frame, Journal.Community == 2)
comm3 <- subset(data.frame, Journal.Community == 3)

# get top 10 most-cited papers per community
comm1.sm <- subset(comm1, select = c("Title", "Source", "Authors", "PubYear", "TimesCited"))
comm1.sm[order(comm1.sm$TimesCited, decreasing = T), ][1:10, 1:4]

comm2.sm <- subset(comm2, select = c("Title", "Source", "Authors", "PubYear", "TimesCited"))
comm2.sm[order(comm2.sm$TimesCited, decreasing = T), ][1:10, 1:4]

comm3.sm <- subset(comm3, select = c("Title", "Source", "Authors", "PubYear", "TimesCited"))
comm3.sm[order(comm3.sm$TimesCited, decreasing = T), ][1:10, 1:4]

data.frame$AuShannonDiv <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  data.frame$AuShannonDiv[i] <- diversity(subset(data.frame[i, ], 
                                                 select = c("AuMath", "AuStat",
                                                            "AuEcol", "AuEvol",
                                                            "AuBiol", "AuEpi",
                                                            "AuVet", "AuMed")))
  print(i)                    
}

# citation diversity
cite.list <- BuildCitationFrame(data.frame.in = data.frame)

assoc.mat <- BuildAssocMat(data.frame.in = data.frame, 
                           cite.frame = cite.list)

data.frame$citations.within <- data.frame$citations.between <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  data.frame$citations.within[i] <- sum(na.omit(assoc.mat[i, which(data.frame$Journal.Community == data.frame$Journal.Community[i])]))
  data.frame$citations.between[i] <- sum(na.omit(assoc.mat[i, which(data.frame$Journal.Community != data.frame$Journal.Community[i])]))
  print(i)
}
data.frame$between.to.tot <- data.frame$citations.between / (data.frame$citations.within + data.frame$citations.between)

#-------------------------------------------------#
#-- fit author diversity models and build Fig 6 --#
#-------------------------------------------------#
require(mgcv)

source("./R/AuthorDiversityPlots.R")
AuthorDiversityPlots(data.frame = data.frame)

CitationRateModel(data.frame = data.frame)

plot(data.frame$NumberAuthors ~ data.frame$AuShannonDiv)

affil.inst.tab <- table(data.frame$LeadAuthorAffil, data.frame$AuthInstAffil)
inst.affil.tab <- table(data.frame$LeadAuthorAffil)

# write.csv(affils.test, "./Data/AllAuthorAffils_28Apr2015_2.csv")
unique.authors <- AuthorMerge(author.frame = affils.test)
author.graph <- BuildAuthorGraph(all.authors = affils.test, unique.authors)

auth.adj <- get.adjacency (author.graph)
author.edgelist <- get.edgelist(author.graph, names = F)
author.edgelist <- as.data.frame (cbind(author.edgelist, seq(1:dim(author.edgelist)[1])))
names(author.edgelist) <- c("node.in", "node.out", "edge.ind")
author.edgeweights <- E(author.graph)$weight

auth.full.edge.data <- cbind(author.edgelist, author.edgeweights)
auth.reduced.edgelist <- subset(auth.full.edge.data, auth.cut.edge == 0)

author.nodes <- V(author.graph)$name 
auth.node.size <- diag(auth.adj)
auth.node.number <- seq(1:length(author.nodes))
auth.node.data <- cbind(author.nodes, auth.node.size, auth.node.number)

# write.csv(auth.reduced.edgelist, "./Data/AuthorEdges.csv")
# write.csv(auth.node.data, "./Data/AuthorNodes.csv")


author.diags <- NetworkDiagnostics(graph.in = author.graph, seed.in = 123)
unique.authors.new <- as.data.frame(cbind(unique.authors, author.diags$out.unique.frame))

# extract affiliation of lead author and add that to data.frame
# data.frame$LeadAuthorAffil <- rep(NA, dim (data.frame)1[])
# for(i in 1:dim(data.frame)[1]){
#   k <- subset(affils.test, Paper.Number == data.frame$Paper.Number[i])
#   data.frame$LeadAuthorAffil[i] <- ifelse(k$Math[1] == 1, "Math",
#                                    ifelse(k$Stat[1] == 1, "Stat",
#                                    ifelse(k$Ecol[1] == 1 | k$Evol[1] == 1, "Ecol",
#                                    ifelse(k$Biol[1] == 1, "Biol", 
#                                    ifelse(k$Epi[1] == 1, "Epi",
#                                    ifelse(k$Vet[1] == 1, "Vet",
#                                    ifelse(k$Med[1] == 1, "Med", "other")))))))
#   print(i)
# }

comm1.citelist <- cite.list[which(data.frame$Journal.Community == 1)]
assoc.mat.comm1 <- BuildAssocMat(data.frame.in = comm1, 
                           cite.frame = comm1.citelist)

# get within-community and between-community edgeweights
comm1 <- subset(data.frame, Journal.Community == 1)
comm1.edgeswithin <- assoc.mat[which(data.frame$Journal.Community == 1), which(data.frame$Journal.Community == 1)]
comm1.edgesbetween <- assoc.mat[which(data.frame$Journal.Community == 1), which(data.frame$Journal.Community != 1)]
comm1.edges.comm2 <- assoc.mat[which(data.frame$Journal.Community == 1), which(data.frame$Journal.Community == 2)]
comm1.edges.comm3 <- assoc.mat[which(data.frame$Journal.Community == 1), which(data.frame$Journal.Community == 3)]
quantile(na.omit(as.vector(comm1.edgeswithin)), c(0.025, 0.5, 0.975))
sum(na.omit(comm1.edgeswithin))
sum(na.omit(comm1.edgesbetween))
table(comm1.edgeswithin)
table(comm1.edgesbetween)
table(comm1.edges.comm2)
table(comm1.edges.comm3)

comm2.edgeswithin <- assoc.mat[which(data.frame$Journal.Community == 2), which(data.frame$Journal.Community == 2)]
comm2.edgesbetween <- assoc.mat[which(data.frame$Journal.Community == 2), which(data.frame$Journal.Community != 2)]
comm2.edges.comm1 <- assoc.mat[which(data.frame$Journal.Community == 2), which(data.frame$Journal.Community == 1)]
comm2.edges.comm3 <- assoc.mat[which(data.frame$Journal.Community == 2), which(data.frame$Journal.Community == 3)]
quantile(na.omit(as.vector(comm2.edgeswithin)), c(0.025, 0.5, 0.975))
table(comm2.edgeswithin)
table(comm2.edgesbetween)
table(comm2.edges.comm1)
table(comm2.edges.comm3)

comm3.edges <- assoc.mat[which(data.frame$Journal.Community == 3), ]
comm3.edgeswithin <- comm3.edges[ , which(data.frame$Journal.Community == 3)]
comm3.edgesbetween <- comm3.edges[ , which(data.frame$Journal.Community != 3)]
comm3.edges.comm1 <- comm3.edges[ , which(data.frame$Journal.Community == 1)]
comm3.edges.comm2 <- comm3.edges[ , which(data.frame$Journal.Community == 2)]
quantile(na.omit(as.vector(comm3.edgeswithin)), c(0.025, 0.5, 0.975))
table(comm3.edgeswithin)
table(comm3.edgesbetween)
table(comm3.edges.comm1)
table(comm3.edges.comm2)

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

require(arcdiagram)
require(RColorBrewer)
source("./R/ArcDiagramPlot.R")
svg("./Figures/Figure1_V5_20150928.svg")
ArcDiagramPlot(journal.graph)
dev.off()

# # get edgelist
# edgelist = get.edgelist(journal.graph)
# 
# # clean up the really long names
# 
# 
# # get vertex labels
# #arcdiagram orders by edgelist not by igraph vertex labels- so doesn't include those species that don't have edges.
# nam=character()
# for(i in 1:dim(edgelist)[1]){
#   nam=c(nam, edgelist[i,])  
# }
# vlabels = levels(factor(nam))
# 
# 
# # batcomsMatlab=read.csv("batcomsMatlab.csv")
# # bc=sort(unique(unlist(batcomsMatlab)))
# # batcoms=list()
# # for(i in bc){
# #   batcoms[[i]]=rownames(batsharedvirusesmat)[which(batcoms0matlab==i)]
# # }
# # 
# # # vertex groups (communities)
# # b=character()
# # com=numeric()
# # for(i in 1:length(batcoms)){
# #   b=c(b,as.character(batcoms[[i]]))
# #   com=c(com,rep(i,length(batcoms[[i]])))
# #}
# com <- rep(NA, length(vlabels))
# for(i in 1:length(vlabels)){
#   k <- subset(data.frame, as.character(Source) == vlabels[i])
#   com[i] <- ifelse(length(k$Journal.Community[which(is.na(k$Journal.Community) == F)]) == 0, NA,
#                    k$Journal.Community[which(is.na(k$Journal.Community) == F)][1])    
# }
# com <- as.numeric(as.character(com))
# com <- ifelse(com == 1, 4, com)
# com <- ifelse(com == 2, 5, com)
# com <- ifelse(com == 4, 2, com)
# com <- ifelse(com == 5, 1, com)
# com <- ifelse(is.na(com), 4, com)
# #vgroups = com[match(vlabels, b)]
# vgroups = com
# #rename groups so that largest communities first
# x=sort(unique(vgroups))
# lx=numeric()
# for(i in 1:length(x)){
#   lx [i]=length(which(vgroups==x[i]))
# }
# y=x[order(lx,decreasing=TRUE)]
# vgroups2=numeric()
# for(i in 1:length(vgroups)){
#   vgroups2[i] <- ifelse(is.na(vgroups[i]) == T, NA, which(y==vgroups[i]))
#   print(i)
# }
# 
# # get vertex fill color
# col1=brewer.pal(12,"Paired")
# cols=rep(col1[c(1,3,5,7,9,11)],3)
# cols <- c(rgb(255/255, 215/255, 153/255, alpha = .3), 
#           rgb(255/255, 153/255, 153/255, alpha = .3), 
#           rgb(153/255, 204/255, 255/255, alpha = .3), 
#           rgb(153/255, 204/255, 153/255, alpha = .3))
# vfill = rep("gray",length(vgroups2))
# for(i in 1:length(y)){
#   vfill[which(vgroups2==i)]=cols[i]
# }
# # get vertex border color
# #cols=rep(col1[c(2,4,6,8,10,12)],3)
# cols=c(rgb(255/255, 215/255, 0), "red", "blue", "green")
# vborders = rep("gray",length(vgroups2))
# for(i in 1:length(y)){
#   vborders[which(vgroups2==i)]=cols[i]
# }
# 
# 
# # get vertex degree
# # degrees=batdata.network.njab$degree[match(vlabels,batdata.network.njab$binomial)]
# jo.degrees=degree(journal.graph)
# 
# # get edges value
# # values = get.edge.attribute(batsharedvirusesgraph.nr.njab, "weight")
# values = get.edge.attribute(journal.graph, "weight")
# 
# #sort by community, then degree
# ord=order(vgroups2,max(jo.degrees)-jo.degrees)
# 
# #with species labels 
# par(mar=c(20,0,2,0))
# arcplot(edgelist, 
#         sorted = T,
#         ordering=ord, 
# #        labels=vlabels, 
#         cex.labels=0.6,
#         show.nodes=TRUE, 
#         col.nodes=vborders, 
#         bg.nodes=vfill,
#         cex.nodes = jo.degrees/max(jo.degrees)+1, 
#         pch.nodes=21,
#         col.labels="grey30",
#         lwd.nodes = 1.5, 
#         line= -0.1, 
#         lwd.arcs = .5 * values,
#         col.arcs = rgb(160/255, 160/255, 160/255, alpha = .2))
# # col.arcs = "#5998ff30"
# #--- END angie code ---#

journal.adj <- get.adjacency(journal.graph)
# build 1492 x 2 matrix of edges
journal.edgelist <- which(journal.adj == 1, arr.ind = T)
journal.edgelist <- as.data.frame (cbind(journal.edgelist, seq(1:dim(journal.edgelist)[1])))
names(journal.edgelist) <- c("node.in", "node.out", "edge.ind")
# to get edgeweights, extract i, j and j, i from edgelist, and get weights for each. 
journal.edgelist.reduced <- rep(NA, dim(journal.edgelist)[1])
symm.edge <- compiled.weight <- cut.edge <- rep(NA, dim(journal.edgelist)[1])
journal.edgeweights <- E(journal.graph)$weight

for(i in 1:dim(journal.edgelist)[1]){
  # check for symmetry
  test.sym <- subset(journal.edgelist, node.in == journal.edgelist$node.out[i] & journal.edgelist$node.out == journal.edgelist$node.in[i])
  symm.edge[i] <- ifelse(dim(test.sym)[1] == 0, 0, test.sym$edge.ind)
  cut.edge[i] <- ifelse(dim(test.sym)[1] == 0, 0, ifelse(test.sym$edge.ind <= i, 0, 1))
  compiled.weight[i] <- ifelse(symm.edge[i] == 0, journal.edgeweights[i], journal.edgeweights[i] + journal.edgeweights[test.sym$edge.ind])
}
full.edge.data <- cbind(journal.edgelist, journal.edgeweights, symm.edge, cut.edge, compiled.weight)
reduced.edgelist <- subset(full.edge.data, cut.edge == 0)

# journal.edgeweights.long <- c(journal.edgeweights, journal.edgeweights)
# journal.edges <- cbind(journal.edgelist, journal.edgeweights.long[1:1492])
journal.nodes <- V(journal.graph)$name 
journal.comm <- journal.size <- rep(NA, length(journal.nodes))
for(i in 1:length(journal.comm)){
  k <- subset(data.frame, Source == journal.nodes[i])
  journal.comm[i] <- k$Journal.Community[1]
  journal.size[i] <- dim(k)[1]
}  
node.number <- seq(1:length(journal.nodes))
node.data <- cbind(journal.nodes, journal.comm, journal.size, node.number)

# write.csv(reduced.edgelist, "./Data/JournalEdges_2.csv")
# write.csv(node.data, "./Data/JournalNames.csv")
# 26 journals get cut when constructing journal.graph....


paper.graph <- BuildPaperGraph(assoc.mat, data.frame)
paper.nodes <- cbind(seq(1:length(V(paper.graph)$name)), V(paper.graph)$name, V(paper.graph)$size, 
                     data.frame$Journal.Community)

paper.edgelist <- get.edgelist(paper.graph, names = F)
paper.edgelist <- as.data.frame (cbind(paper.edgelist, seq(1:dim(paper.edgelist)[1])))
names(paper.edgelist) <- c("node.in", "node.out", "edge.ind")

#write.csv(paper.nodes, "./Data/PaperNodes.csv")
#write.csv(paper.edgelist, "./Data/PaperEdges.csv")

#---------------------------#
#----- Figure S1 Code ------#
#---------------------------#
source("./R/PaperJournalNumberThroughTime.R")
PaperJournalNumberThroughTime(data.frame)

# papers.per.year <- table(factor(data.frame$PubYear, levels = seq(1980, 2014)))
# 
# journals.per.year <- rep(NA, 35)
# for(i in 1:35){
#   k <- subset(data.frame, PubYear == 1979 + i)
#   journals.per.year[i] <- length(levels(factor(k$Source)))
# }

within.to.between.refs.comm1 <- rep(NA, 35)
within.to.between.refs.comm2 <- rep(NA, 35)
within.to.between.refs.comm3 <- rep(NA, 35)

comm1.edgeswithin.byyear <- comm1.edgesbetween.byyear <- vector("list", 35)
comm1.quants.byyear <- comm1.edges.between <- comm1.edges.tot <- rep(NA,35)

comm2.edgeswithin.byyear <- comm2.edgesbetween.byyear <- vector("list", 35)
comm2.quants.byyear <- comm2.edges.between <- comm2.edges.tot  <- rep(NA,35)

comm3.edgeswithin.byyear <- comm3.edgesbetween.byyear <- vector("list", 35)
comm3.quants.byyear <- comm3.edges.between <- comm3.edges.tot  <- rep(NA,35)

for(i in 1:35){
#  comm1.byyear[[i]] <- subset(data.frame, Journal.Community == 1 & PubYear == 1979 + i)
#   comm1.edgeswithin.byyear[[i]] <- assoc.mat[which(data.frame$Journal.Community == 1 & data.frame$PubYear == 1979 + i), 
#                                              which(data.frame$Journal.Community == 1 & 
#                                                      data.frame$PubYear <= 1979 + i)]
  comm1.edgeswithin.byyear[[i]] <- assoc.mat[ 
                                             which(data.frame$Journal.Community == 1 & 
                                                     data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 1 & data.frame$PubYear == 1979 + i)]
  comm1.edgesbetween.byyear[[i]] <- assoc.mat[
                                              which(data.frame$Journal.Community != 1 & 
                                                      data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 1 
                                                                                             & data.frame$PubYear == 1979 + i)]
  comm1.quants.byyear[i] <- sum(na.omit(as.vector(comm1.edgesbetween.byyear[[i]]))) / 
    (sum(na.omit(as.vector(comm1.edgesbetween.byyear[[i]]))) + 
       sum(na.omit(as.vector(comm1.edgeswithin.byyear[[i]]))))
  comm1.edges.between[i] <- sum(na.omit(as.vector(comm1.edgesbetween.byyear[[i]]))) 
  comm1.edges.tot[i] <-  (sum(na.omit(as.vector(comm1.edgesbetween.byyear[[i]]))) + 
       sum(na.omit(as.vector(comm1.edgeswithin.byyear[[i]]))))
  
  comm2.edgeswithin.byyear[[i]] <- assoc.mat[ 
                                             which(data.frame$Journal.Community == 2 & 
                                                     data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 2 
                                                                                            & data.frame$PubYear == 1979 + i)]
  comm2.edgesbetween.byyear[[i]] <- assoc.mat[ 
                                              which(data.frame$Journal.Community != 2 & 
                                                      data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 2 
                                                                                             & data.frame$PubYear == 1979 + i)]
  comm2.quants.byyear[i] <- sum(na.omit(as.vector(comm2.edgesbetween.byyear[[i]]))) / 
    (sum(na.omit(as.vector(comm2.edgesbetween.byyear[[i]]))) + 
       sum(na.omit(as.vector(comm2.edgeswithin.byyear[[i]]))))
  comm2.edges.between[i] <- sum(na.omit(as.vector(comm2.edgesbetween.byyear[[i]]))) 
  comm2.edges.tot[i] <- (sum(na.omit(as.vector(comm2.edgesbetween.byyear[[i]]))) + 
     sum(na.omit(as.vector(comm2.edgeswithin.byyear[[i]]))))
  
  comm3.edgeswithin.byyear[[i]] <- assoc.mat[ 
                                             which(data.frame$Journal.Community == 3 & 
                                                     data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 3 
                                                                                            & data.frame$PubYear == 1979 + i)]
  comm3.edgesbetween.byyear[[i]] <- assoc.mat[ 
                                              which(data.frame$Journal.Community != 3 & 
                                                      data.frame$PubYear <= 1979 + i), which(data.frame$Journal.Community == 3 
                                                                                             & data.frame$PubYear == 1979 + i)]
  comm3.quants.byyear[i] <- sum(na.omit(as.vector(comm3.edgesbetween.byyear[[i]]))) / 
    (sum(na.omit(as.vector(comm3.edgesbetween.byyear[[i]]))) + 
       sum(na.omit(as.vector(comm3.edgeswithin.byyear[[i]]))))
  comm3.edges.between[i] <- sum(na.omit(as.vector(comm3.edgesbetween.byyear[[i]]))) 
  comm3.edges.tot[i] <- (sum(na.omit(as.vector(comm3.edgesbetween.byyear[[i]]))) + 
                           sum(na.omit(as.vector(comm3.edgeswithin.byyear[[i]]))))
  
}

# Shannon's diversity of lead author affiliation through time in each community
require(vegan)
comm1.byyear <- comm2.byyear <- comm3.byyear <- vector("list", 35)
pi.1 <- pi.2 <- pi.3 <- vector("list", 35)
shannon.comm1 <- shannon.comm2 <- shannon.comm3 <- rep(NA, 35)

for(i in 1:35){
  comm1.byyear[[i]] <- subset(data.frame, Journal.Community == 1 
                              & PubYear == 1979 + i)
  pi.1[[i]] <- table(comm1.byyear[[i]]$AuthInstAffil) / dim(comm1.byyear[[i]])
  shannon.comm1[i] <- diversity(pi.1[[i]])

  comm2.byyear[[i]] <- subset(data.frame, Journal.Community == 2 
                              & PubYear == 1979 + i)
  pi.2[[i]] <- table(comm2.byyear[[i]]$AuthInstAffil) / dim(comm2.byyear[[i]])
  shannon.comm2[i] <- diversity(pi.2[[i]])

  comm3.byyear[[i]] <- subset(data.frame, Journal.Community == 3 
                              & PubYear == 1979 + i)
  pi.3[[i]] <- table(comm3.byyear[[i]]$AuthInstAffil) / dim(comm3.byyear[[i]])
  shannon.comm3[i] <- diversity(pi.3[[i]])
}

# number authors through time
comm1.authsthroughtime <- tapply(comm1$NumberAuthors, as.factor(comm1$PubYear), median)
comm2.authsthroughtime <- tapply(comm2$NumberAuthors, as.factor(comm2$PubYear), median)
comm3.authsthroughtime <- tapply(comm3$NumberAuthors, as.factor(comm3$PubYear), median)

par(mfrow = c(1, 2), mar = c(2, 6, 1, 1), oma = c(1, 1, 1, 1))
plot(as.numeric(papers.per.year) ~ seq(1980, 2014), xlab = "", 
     ylab = "Number included in search", pch = 16, 
     xlim = c(1990, 2015), 
     ylim = c(0, 250))
points(journals.per.year ~ seq(1980, 2014), xlab = "", 
     ylab = "Journals with \n Included Papers", pch = 1)
leg.text <- c("Papers", "Journals")
legend("topleft", leg.text, pch = c(1, 16), bty = "n")
plot(comm1.quants.byyear[- (1:10)] ~ seq(1990, 2014), xlab = "", 
     ylab = "Proportion between-community citations", 
     ylim = c(0, 1), pch = 16, col = "gold", xlim = c(1990, 2015))
points(comm2.quants.byyear[-c(1:10)] ~ seq(1990, 2014), col = "red", pch = 1)
points(comm3.quants.byyear[-c(1:10)] ~ seq(1990, 2014), col = "blue", pch = 2)
lines((comm1.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "gold", lwd = 2)
lines((comm2.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "red", lwd = 2)
lines((comm3.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "blue", lwd = 2)
leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
legend("topright", leg.text, col = c("gold", "red", "blue"), bty = "n", 
       pch = c(16, 1, 2), 
       lty = c(1, 1, 1),
       lwd = c(2, 2, 2))

# GAM fits
require(mgcv)
data.frame$Journal.Community <- factor(data.frame$Journal.Community)
auth.div.mod <- gam(AuShannonDiv ~ s(PubYear) + 
                      s(PubYear, by = (Journal.Community)) +
                      factor(Journal.Community), 
                    data = data.frame)

# Between-community citations
cites.between <- c(comm1.edges.between[-c(1:14)], 
                   comm2.edges.between[-c(1:15)], 
                   comm3.edges.between[-c(1:17)])
cites.within <- c(comm1.edges.tot[-c(1:14)] - comm1.edges.between[-c(1:14)], 
                  comm2.edges.tot[-c(1:15)] - comm2.edges.between[-c(1:15)], 
                  comm2.edges.tot[-c(1:17)] - comm3.edges.between[-c(1:17)])
years <- 1994:2014
years.full <- c(years,
                years[-1],
                years[-c(1:3)])
community <- c(rep(1, length(1994:2014)), 
               rep(2, length(1994:2014) - 1), 
               rep(3, length(1994:2014) - 3)
               )
full.gam.fit <- gam(cbind(cites.between, cites.within) ~ s(years.full) + 
                      s(years.full, by = factor(community)) +
                      factor(community), 
                    family = "binomial")
reduced.gam.fit <- gam(cbind(cites.between, cites.within) ~ s(years.full) +
                      factor(community), 
                    family = "binomial")
summary(full.gam.fit)
AIC(full.gam.fit)
AIC(reduced.gam.fit)


cites.gam <- gam(cites ~ s(year) + 
                   s(year, by = factor(community)) +
                   factor(community))
years <- 1994:2014
gam1.dat <- data.frame(cites = comm1.quants.byyear[-c(1:14)],
                       years = years)
comm1.gam <- gam(cbind(comm1.edges.between[-c(1:14)], comm1.edges.tot[-c(1:14)] - comm1.edges.between[-c(1:14)]) ~ s(years), 
                 family = "binomial")
comm2.gam <- gam(cbind(comm2.edges.between[-c(1:15)], comm2.edges.tot[-c(1:15)] - comm2.edges.between[-c(1:15)]) ~ s(years[-1]), 
                 family = "binomial")
comm3.gam <- gam(cbind(comm3.edges.between[-c(1:17)], comm3.edges.tot[-c(1:17)] - comm3.edges.between[-c(1:17)]) ~ s(years[-c(1:3)]), 
                 family = "binomial")
preds.comm1 <- predict(comm1.gam, type = "response", se.fit = TRUE)
fit.comm1 <- preds.comm1$fit 
fit.up95.comm1 <-fit.comm1 - 1.96*preds.comm1$se.fit 
fit.low95.comm1 <-fit.comm1 + 1.96*preds.comm1$se.fit

preds.comm2 <- predict(comm2.gam, type="response", 
               se.fit = TRUE)
fit.comm2 <- preds.comm2$fit
fit.up95.comm2 <-fit.comm2 - 1.96*preds.comm2$se.fit
fit.low95.comm2 <-fit.comm2 + 1.96*preds.comm2$se.fit

preds.comm3 <- predict(comm3.gam, type="response", 
                       se.fit = TRUE)
fit.comm3 <- preds.comm3$fit
fit.up95.comm3 <-fit.comm3 - 1.96*preds.comm3$se.fit
fit.low95.comm3 <-fit.comm3 + 1.96*preds.comm3$se.fit

par(mfrow = c(1, 2), mar = c(2, 6, 1, 1), oma = c(1, 1, 1, 1))
plot(as.numeric(papers.per.year) ~ seq(1980, 2014), xlab = "", 
     ylab = "Number included in search", pch = 16, 
     xlim = c(1990, 2015), 
     ylim = c(0, 250))
points(journals.per.year ~ seq(1980, 2014), xlab = "", 
       ylab = "Journals with \n Included Papers", pch = 1)
leg.text <- c("Papers", "Journals")
legend("topleft", leg.text, pch = c(1, 16), bty = "n", cex = .8)
plot(x = 0, y = 0, xlab = "", 
     ylab = "Proportion between-community citations", 
     ylim = c(0, 1), pch = 16, col = "gold", xlim = c(1994, 2015))

# plot(x = 0, y = 0, cex = 0, ylim = c(0, 1), xlim = c(1994, 2015), 
#      xaxs = "i", yaxs = "i")
polygon(c(years, rev(years)), 
        c(fit.low95.comm1, rev(fit.up95.comm1)),
        border=NA, col = rgb(255/255,215/255,0, alpha = .3))
polygon(c(years[-1], rev(years[-1])), 
        c(fit.low95.comm2, rev(fit.up95.comm2)), col = rgb(1,0,0, alpha = .3),
        border=NA)
polygon(c(years[-c(1:3)], rev(years[-c(1:3)])), 
        c(fit.low95.comm3, rev(fit.up95.comm3)), col = rgb(0,0,1, alpha = .3),
        border=NA)
points(comm1.quants.byyear[-c(1:10)] ~ seq(1990, 2014), col = "gold", pch = 1)
points(comm2.quants.byyear[-c(1:15)] ~ seq(1995, 2014), col = "red", pch = 1)
points(comm3.quants.byyear[-c(1:10)] ~ seq(1990, 2014), col = "blue", pch = 2)
lines((comm1.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "gold", lwd = 2)
lines((comm2.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "red", lwd = 2)
lines((comm3.quants.byyear[- (1:10)] ~ seq(1990, 2014)), col = "blue", lwd = 2)
leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
legend("topright", leg.text, col = c("gold", "red", "blue"), bty = "n", 
       pch = c(16, 1, 2), 
       lty = c(1, 1, 1),
       lwd = c(2, 2, 2), cex = .8)

# plot(shannon.comm1[- (1:10)] ~ seq(1990, 2014), xlab = "", 
#      ylab = "Lead author diversity", 
#      ylim = c(0, 3), pch = 16, col = "gold", type = "l", lwd = 2, xlim = c(1990, 2015))
# points(shannon.comm1[-c(1:10)] ~ seq(1990, 2014), col = "gold", pch = 16, lwd = 2)
# lines(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", lwd = 2)
# lines(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", lwd = 2)
# points(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", pch = 1)
# points(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", pch = 2)
# leg.text <- c("Ecol", "Epi", "Vet")
# legend("topleft", leg.text, col = c("gold", "red", "blue"), bty = "n", 
#        pch = c(16, 1, 2), 
#        lty = c(1, 1, 1),
#        lwd = c(2, 2, 2))
# 
# par(new = T)
# plot(lowess(comm1.authsthroughtime ~ as.numeric(as.character(names(comm1.authsthroughtime)))), 
#        col = "gold", type = "l", yaxt = "n", ylab = "", xlab = "",
#      ylim = c(0, 7))
# axis(side = 4, at = c(0:7), labels = c(0:7))
# mtext("Median number authors", side = 4, las = 0, cex = .8, line = 2)
# lines(lowess(comm2.authsthroughtime ~ as.numeric(as.character(names(comm2.authsthroughtime)))), 
#      col = "red")
# lines(lowess(comm3.authsthroughtime ~ as.numeric(as.character(names(comm3.authsthroughtime)))), 
#       col = "blue")

#-------------------------------#
#-- diversity model ------------#
#-------------------------------#
div.model <- lm(AuShannonDiv ~ PubYear * as.factor(Journal.Community) + NumberAuthors, data = data.frame)
plot(div.model)
summary(div.model)

#-----------------------------------------#
#-- Author network -----------------------#
#-----------------------------------------#
# par(mfrow = c(1, 1))
# plot(author.graph, margin = c(-.75, -.75, 0, 0), 
#      vertex.size = 1, 
#      vertex.label = NA, 
#      edge.arrow.size = .05)

# cut down to just giant component
author.clusters <- clusters(author.graph)
giant.compo.aus <- delete.vertices(author.graph, which(clusters(author.graph)$membership != 1))

# plot(giant.compo.aus, 
#      margin = c(-.85, -.75, -.15, -.5), 
#      vertex.size = 1, 
#      vertex.label = NA, 
#      edge.arrow.size = .05)

# stored.layout <- layout.fruchterman.reingold(giant.compo.aus)
# margin.dims <- c(-1.0, -1.0, -.15, -.9)
giant.compo.au.walktr <- walktrap.community(giant.compo.aus, steps = 4)
giant.compo.au.walktr$membership
table(giant.compo.au.walktr$membership)
# largest communities get unique colors; all communities <= 20 are "grey40"
small.communities.au <- as.numeric(as.character(names(table(giant.compo.au.walktr$membership))
                                                [which(table(giant.compo.au.walktr$membership) <= 50)]))
au.vertex.ind <- factor(ifelse(giant.compo.au.walktr$membership %in% small.communities.au, 
                               "small", giant.compo.au.walktr$membership))
color.vec.au <- c("red", "blue", "green", "yellow", "deeppink", "darkseagreen4", "purple", "grey70")
au.vertex.colors <- color.vec.au[au.vertex.ind]

au.comm.top10 <- vector("list", length(levels(au.vertex.ind)))
for(i in 1:length(levels(au.vertex.ind))){
  au.comm.i <- V(giant.compo.aus)[as.numeric(au.vertex.ind) == i]
  au.comm.top10[[i]] <- au.comm.i$name[order(au.comm.i$size, decreasing = T)][1:10]
}

# plot(giant.compo.aus, 
#      vertex.frame.color = as.numeric(walktr.au.giant$membership), 
#      vertex.label = vertex.labels.giantcompos,  
#      vertex.label.cex = .5, 
#      vertex.label.color = "black", 
#      margin = c(-.9, -.95, -.3, -.7), 
#      layout = layout.fruchterman.reingold, 
#      edge.width = 0.25, 
#      edge.color = "grey50", 
#      edge.arrow.size = 0, 
#      vertex.label = "", 
#      vertex.size = V(giant.compo.aus)$size / 12, 
#      vertex.color = as.numeric(walktr.au.giant$membership))
#
# stored.layout.au <- layout.fruchterman.reingold(giant.compo.aus)
#
# plot(giant.compo.aus, 
#      margin = margin.dims, 
#      vertex.frame.color = "black", 
#      vertex.label = "",  
#      vertex.label.cex = .6, 
#      vertex.label.color = "black", 
#      layout = stored.layout.au, 
#      edge.width = 0.25, 
#      edge.color = "grey50", 
#      edge.arrow.size = 0, 
#      vertex.label = "", 
#      vertex.size = V(giant.compo.aus)$size / 12, 
#      vertex.color = au.vertex.colors)

au.dendro <- as.dendrogram(giant.compo.au.walktr)
height.of.leafs <- dendrapply(au.dendro, function(e) attr(e, "height"))
quantile(unlist(height.of.leafs), c(0.9, 0.95, 0.975))
au.dendro.height2700 <- cut(au.dendro, h = 2700)
# plot(au.dendro.height2700$upper)

quantile(V(giant.compo.aus)$size, 0.95)
V(giant.compo.aus)$name[which(V(giant.compo.aus)$size >= 24)]
which(V(giant.compo.aus)$size >= 24)[1]

#---------------------------------#
#-- citation network -------------#
#---------------------------------# 
# plots for paper graph
# par(mfrow = c(1, 1))
# plot(paper.graph, vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)
paper.degree.dist <- degree(paper.graph, mode = "in")
paper.betweenness <- betweenness(paper.graph)
paper.hierarchy <- hierarchy(paper.graph)
# paper.optim.community <- optimal.community(paper.graph)

# plot of giant component only
giant.compo.papers <- delete.vertices(paper.graph, 
                                      which(clusters(paper.graph)$membership != 4))
giant.compo.papers.walktr <- walktrap.community(giant.compo.papers, steps = 4)
giant.compo.papers.walktr$membership
table(giant.compo.papers.walktr$membership)
# largest communities get unique colors; all communities <= 10 are "grey40"
small.communities <- as.numeric(as.character(names(table(giant.compo.papers.walktr$membership))
                                             [which(table(giant.compo.papers.walktr$membership) <= 20)]))
paper.vertex.ind <- factor(ifelse(giant.compo.papers.walktr$membership %in% small.communities, 
                                  "small", 
                                  giant.compo.papers.walktr$membership))
color.vec <- c("red", "blue", "green", "yellow", "deeppink", "darkseagreen4", "darkturquoise", "purple", "grey70")
paper.vertex.colors <- color.vec[paper.vertex.ind]

data.frame$PaperCommunity <- ifelse(clusters(paper.graph)$membership != 4, "NotGiant", NA
                                    )
data.frame$PaperCommunity[which(clusters(paper.graph)$membership == 4)] <- as.character(paper.vertex.ind)
# write.csv(data.frame, "./Data/PaperDataFrameWithPaperCommunities_072515.csv")

paper.comm.top10 <- vector("list", length(levels(paper.vertex.ind)))
for(i in 1:length(levels(paper.vertex.ind))){
  paper.comm.i <- V(giant.compo.papers)[as.character(paper.vertex.ind) == levels(paper.vertex.ind)[i]]
  paper.comm.top10[[i]] <- as.data.frame(cbind(paper.comm.i$name[order(paper.comm.i$size, decreasing = T)], 
                                 paper.comm.i$JoComm[order(paper.comm.i$size, decreasing = T)], 
                                 as.character(paper.comm.i$Source)[order(paper.comm.i$size, decreasing = T)], 
                                 as.character(paper.comm.i$PaperNumber)[order(paper.comm.i$size, decreasing = T)],
                                 rep(levels(paper.vertex.ind)[i], length(paper.comm.i))
                                 ))
  names(paper.comm.top10[[i]]) <- c("Paper", "JournalCommunity", "JournalName", "PaperNumber", "PaperCommunity")
}

# dput(paper.comm.top10, "./Data/PaperCommunityMembership_072515")

table(paper.comm.top10[[1]]$JournalCommunity) / dim(paper.comm.top10[[1]])[1]
table(paper.comm.top10[[2]]$JournalCommunity) / dim(paper.comm.top10[[2]])[1]
table(paper.comm.top10[[3]]$JournalCommunity) / dim(paper.comm.top10[[3]])[1]
table(paper.comm.top10[[4]]$JournalCommunity) / dim(paper.comm.top10[[4]])[1]
table(paper.comm.top10[[5]]$JournalCommunity) / dim(paper.comm.top10[[5]])[1]
table(paper.comm.top10[[6]]$JournalCommunity) / dim(paper.comm.top10[[6]])[1]
table(paper.comm.top10[[7]]$JournalCommunity) / dim(paper.comm.top10[[7]])[1]
table(paper.comm.top10[[8]]$JournalCommunity) / dim(paper.comm.top10[[8]])[1]
table(paper.comm.top10[[9]]$JournalCommunity) / dim(paper.comm.top10[[9]])[1]
table(paper.comm.top10[[10]]$JournalCommunity) / dim(paper.comm.top10[[10]])[1]

# margin.dims = c(0, 0, 0, 0)
# plot(giant.compo.papers, 
#      margin = margin.dims, 
#      vertex.frame.color = "black", 
#      vertex.label = "",  
#      vertex.label.cex = .5, 
#      vertex.label.color = "black", 
#      layout = fixed.paper.layout, 
#      edge.width = 0.25, 
#      edge.color = "grey50", 
#      edge.arrow.size = 0, 
#      vertex.label = "", 
#      vertex.size = V(giant.compo.papers)$size / 30, 
#      vertex.color = paper.vertex.colors)

#-----------------------------------------#
#-- Journal network ----------------------#
#-----------------------------------------#

# order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# # order command reorders levels so that they appear in descending frequency
# par(mfrow = c(1, 1), las = 2, mar = c(15, 6, 1, 1))
# plot(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)] ~ c(1:112), 
#      type = "h", 
#      xaxt = "n", 
#      ylab = "Avg. annualize citation rate for \n all papers in dataset", 
#      xlab = "", 
#      lwd = journal.total.papers[order(journal.agg.annual.cites, decreasing = T)] * .5)
# text(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)][1:20] ~ c(seq(1:20) + 0.25), 
#      labels = paste("(", journal.total.papers[order(journal.agg.annual.cites, decreasing = T)][1:20], ")", sep = ""), 
#      cex = .6)
# axis(side = 1, las = 2, cex.axis = .5,
#      at = c(1:112), 
#      labels = levels(data.frame$Source)[order(journal.agg.annual.cites, decreasing = T)])
# 
# 
# plot(journal.graph)
# Qu: which journals are excluded?
# disconnected.journals <- levels(factor(data.frame$Source))
# [!(levels(factor(data.frame$Source)) %in% levels(journal.frame$journal1.vec))]
# V(journal.graph)$size <- table(data.frame$Source)[!(names(table(data.frame$Source)) %in% disconnected.journals)]

# # description of journal graph
# journal.compos <- clusters(journal.graph, mode = "weak")
# # get size of second-largest component
# journal.compos$csize[which.max(journal.compos$csize[-which.max(journal.compos$csize)])] 
# table(journal.compos$csize == 1) # table isolated nodes
# avg.path.length.journal <- average.path.length(journal.graph)
# avg.degree.journal <- mean(degree(journal.graph, mode = "in"))
# diam.journal <- diameter(journal.graph)
# power.law.fit.journal <- power.law.fit(degree(journal.graph, mode = "all"))


journal.degree.best <- degree(journal.graph)[c(33, 35, 60, 74, 77, 80)]
range(journal.degree.best)
journal.degree.other <- degree(journal.graph)[-c(33, 35, 60, 74, 77, 80)]
range(journal.degree.other)
journal.degree.all <- degree(journal.graph, mode = "out")
best.not.best <- rep(0, length(journal.degree.all))
best.not.best[-c(33, 35, 60, 74, 77, 80)] <- 1
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1), oma = c(2, 2, 2, 2))
boxplot((journal.degree.all)~ best.not.best, col = "grey80", 
        names = c("\n Most Diverse \n Authorship", "Other Journals"),
        ylab = "Journal Out-Degree")


# which(degree(journal.graph) < 1)
comm1.jos <- levels(factor(subset(data.frame, Journal.Community == 1)$Source))
comm2.jos <- levels(factor(subset(data.frame, Journal.Community == 2)$Source))
comm3.jos <- levels(factor(subset(data.frame, Journal.Community == 3)$Source))

journal.graph.small <- delete.vertices(journal.graph, which(degree(journal.graph) < 1))
vertex.sizes <- vertex.community <- rep(NA, length(V(journal.graph.small)))
for(i in 1:length(vertex.sizes)){
  k <- subset(journal.data, journal == V(journal.graph.small)$name[i])
  vertex.sizes[i] <- as.numeric(as.character(k$total.papers))[1]
  vertex.community[i] <- ifelse(V(journal.graph.small)$name[i] %in% comm1.jos, 1,
                                ifelse(V(journal.graph.small)$name[i] %in% comm2.jos, 2,
                                       ifelse(V(journal.graph.small)$name[i] %in% comm3.jos, 3, 4)))
}


V(journal.graph.small)$name
plot(journal.graph.small, 
     layout = layout.fruchterman.reingold, 
#     vertex.size = log(table(journal.frame$journal1.vec) + 1) * 2, 
     vertex.size = log(vertex.sizes + 1) * 2, 
     vertex.label = NA, 
     edge.arrow.size = .05)

# # journal communities
# set.seed(123)
# walktr.jo <- walktrap.community(journal.graph.small, steps = 4)
# walktr.jo$membership
# V(journal.graph.small)$name[walktr.jo$membership == 3]
# table(walktr.jo$membership)
# # largest communities get unique colors; all communities <= 10 are "grey40"
# small.communities.jo <- as.numeric(as.character(names(table(walktr.jo$membership))
#                                                 [which(table(walktr.jo$membership) <= 3)]))
# jo.vertex.ind <- factor(ifelse(walktr.jo$membership %in% small.communities.jo, 
#                                "small", 
#                                walktr.jo$membership))



vet.vertices <- c(5, 52, 79, 89, 92, 94, 95)
ecol.vertices <- c(72, 26, 50, 6, 29, 28, 48, 37, 23, 70)  
genbiol.vertices <- c(77, 59, 65, 60, 81, 91, 80, 76, 57, 35)

jo.vertex.ind <- rep(4, length(V(journal.graph.small)))
jo.vertex.ind[vet.vertices] <- 3
jo.vertex.ind[ecol.vertices] <- 2
jo.vertex.ind[genbiol.vertices] <- 1

# jo.vertex.ind <- journal
color.vec.jo <- c("pink", "lightblue", "gold1", "grey70")
jo.vertex.colors <- color.vec.jo[vertex.community]
V(journal.graph.small)$size <- ifelse(is.finite(V(journal.graph.small)$size) == T, 
                                      V(journal.graph.small)$size, 0)

# jo.comm.top10 <- vector("list", length(levels(jo.vertex.ind)))
# for(i in 1:length(levels(jo.vertex.ind))){
#   jo.comm.i <- V(journal.graph.small)[as.numeric(jo.vertex.ind) == i]
#   jo.comm.top10[[i]] <- jo.comm.i$name[order(jo.comm.i$size, decreasing = T)][1:10]
# }

fixed.jo.layout <- layout.fruchterman.reingold(journal.graph.small)
par(mfrow = c(1, 1))
margin.dims = c(-1.0, -.75, 0, -.25)

# cherry-pick journal names to label
vertex.labels <- rep("", 96)
vertex.labels[which(vertex.sizes >= 30)] <- V(journal.graph.small)$name[which(vertex.sizes >= 30)]
vertex.labels[6] <- "Am Nat"
vertex.labels[27] <- "Ecol Monogs"
vertex.labels[28] <- "Ecology"
vertex.labels[51] <- "J Clin Micro"
vertex.labels[48] <- "J Anim Ecol"
vertex.labels[58] <- "J Med Ent"
vertex.labels[60] <- "Interface"
vertex.labels[61] <- "J Wildlife Dis"
vertex.labels[62] <- "J Wildlife Manag"
vertex.labels[66] <- "Med Vet Ent"
vertex.labels[73] <- "PLoS Biol"
vertex.labels[75] <- "PLoS Med"
vertex.labels[77] <- "PLoS One"
vertex.labels[78] <- "Pop Health Metrics"
vertex.labels[79] <- "Prev Vet Med"
vertex.labels[80] <- "PNAS"
vertex.labels[81] <- "PRSB"
vertex.labels[82] <- "Science"
vertex.labels[85] <- "Stats in Med"
vertex.labels[91] <- "Vaccine"
vertex.labels[96] <- "Vet Parasitol"


plot(journal.graph.small, 
     margin = margin.dims, 
     vertex.frame.color = "black", 
     vertex.label = "",  
     vertex.label.cex = .7, 
     vertex.label.color = "black", 
     layout = fixed.jo.layout, 
     edge.width = 0.25, 
     edge.color = "grey50", 
     edge.arrow.size = 0, 
#     vertex.label = V(journal.graph.small)$name, 
     vertex.size = log(vertex.sizes + 1) * 3, 
     vertex.color = jo.vertex.colors)
legend(x = -1, y = -.6, c("Ecology", "General Bio", "Vet", "Other"), bty = "n", fill = c("pink", "lightblue", "gold1", "grey70"))

jo.dendro <- as.dendrogram(walktr.jo)
in.colbar <- c("forestgreen", 
               "navyblue", 
               "red", 
               "black", 
               "green", 
               "seagreen4", 
               "yellow", 
               "turquoise", 
               "purple", 
               "gold", 
               "magenta", 
               "grey40", 
               "orange", 
               "deeppink", 
               "blue")
par(mar = c(0, 0, 0, 0), cex.axis = .7)
dendPlot(walktr.jo, 
         mode = "phylo", 
         label.offset = .5, 
         cex = .8, 
         colbar = in.colbar, 
         direction = "downwards")

# extract journal community membership for each paper
journal.community.membership <- data.frame(cbind(jo.vertex.ind, V(journal.graph.small)$name))
names(journal.community.membership) <- c("Community", "JournalName")
data.frame$JournalCommunity <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  journal.source <- subset(journal.community.membership, 
                           as.character(JournalName) == as.character(data.frame$Source[i]))
  data.frame$JournalCommunity[i] <- as.character(journal.source$Community[1])
}

#write.csv(data.frame$JournalCommunity, 
#   "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalCommunityVector_16Dec2014.csv")

#---------------------------------------------#
#-- Stratified sampling set-up ---------------#
#---------------------------------------------#
comm1 <- subset(data.frame, JournalCommunity == 1) # Ecology (310 papers)
table(comm1$PubYear %in% seq(1998, 2002)) # 40 in 1998-2002
table(comm1$PubYear %in% seq(2003, 2007)) # 67 in 2003-2007
table(comm1$PubYear %in% seq(2008, 2012)) # 118 in 2008-2012

comm2 <- subset(data.frame, JournalCommunity == 2) # General Bio (1043 papers)
table(comm2$PubYear %in% seq(1998, 2002)) # 81 in 1998-2002
table(comm2$PubYear %in% seq(2003, 2007)) # 188 in 2003-2007
table(comm2$PubYear %in% seq(2008, 2012)) # 453 in 2008-2012

comm3 <- subset(data.frame, JournalCommunity == 3) # Vet (198 papers)
table(comm3$PubYear %in% seq(1998, 2002)) # 23 in 1998-2002
table(comm3$PubYear %in% seq(2003, 2007)) # 44 in 2003-2007
table(comm3$PubYear %in% seq(2008, 2012)) # 81 in 2008-2012

levels(factor(comm1$Source))
levels(factor(comm2$Source))
levels(factor(comm3$Source))

# assume 10 readers; 3 papers / reader
Comm1ReaderList <- ReaderAssignmentSpecificGrp(data.frame, 
                                               number.readers = 4, 
                                               papers.per.reader = 20, 
                                               fixed.seed = 123, 
                                               community.to.use = 1, 
                                               quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))
Comm2ReaderList <- ReaderAssignmentSpecificGrp(data.frame, 
                                               number.readers = 4, 
                                               papers.per.reader = 20, 
                                               fixed.seed = 123, 
                                               community.to.use = 2, 
                                               quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))
Comm3ReaderList <- ReaderAssignmentSpecificGrp(data.frame, 
                                               number.readers = 4, 
                                               papers.per.reader = 20, 
                                               fixed.seed = 123, 
                                               community.to.use = 3, 
                                               quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))

readers <- c("Bande", 
             "Craft", 
             "Cross", 
             "Huyvaert", 
             "Joseph", 
             "Manlove", 
             "Miller", 
             "Nol", 
             "OBrien", 
             "Patyk", 
             "Walker", 
             "Walsch")
comm1.reader.frame <- do.call("rbind", Comm1ReaderList)
comm1.reader.frame$Reader <- rep(readers[1:4], each = 18)
comm2.reader.frame <- do.call("rbind", Comm2ReaderList)
comm2.reader.frame$Reader <- rep(readers[5:8], each = 18)
comm3.reader.frame <- do.call("rbind", Comm3ReaderList)
comm3.reader.frame$Reader <- rep(readers[9:12], each = 18)

full.reader.assignments <- as.data.frame(rbind(comm1.reader.frame, 
                                               comm2.reader.frame, 
                                               comm3.reader.frame))
# write.csv(full.reader.assignments, 
#  "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/FullReadingAssignments_11Jan2015.csv")

#-------------------------------------------------------------------------------------#
#-- regression of avg. author closeness and betweenness on annualized citation rate --#
#-------------------------------------------------------------------------------------#
# to get paper's authorship diversity, 
# 1) extract authors from author.unique.frame by looping over paper numbers in author 
#    full frame, extracting authorID, and subsetting on authorID in author.unique.frame
# 2) sum (math, stat, ecol, evol, epi, med) across all paper authors
data.frame.full <- DataFrameAddons(data.frame.in = data.frame, all.authors = affils.test, unique.authors)
comm1.full <- subset(data.frame.full, Journal.Community == 1)
comm1.vet <- sum(comm1.full$vet.tot)
comm1.ecoevo <- sum(comm1.full$ecoevo.tot)
comm1.biol <- sum(comm1.full$biol.tot)
comm1.math <- sum(comm1.full$math.tot)
comm1.stat <- sum(comm1.full$stat.tot)
comm1.epi <- sum(comm1.full$epi.tot)
comm1.med <- sum(comm1.full$med.tot)
comm1.author.counts <- c(comm1.vet,
                         comm1.ecoevo,
                         comm1.biol,
                         comm1.math,
                         comm1.stat,
                         comm1.epi,
                         comm1.med)
comm2.full <- subset(data.frame.full, Journal.Community == 2)
comm2.vet <- sum(na.omit(comm2.full$vet.tot))
comm2.ecoevo <- sum(na.omit(comm2.full$ecoevo.tot))
comm2.biol <- sum(na.omit(comm2.full$biol.tot))
comm2.math <- sum(na.omit(comm2.full$math.tot))
comm2.stat <- sum(na.omit(comm2.full$stat.tot))
comm2.epi <- sum(na.omit(comm2.full$epi.tot))
comm2.med <- sum(na.omit(comm2.full$med.tot))
comm2.author.counts <- c(comm2.vet,
                         comm2.ecoevo,
                         comm2.biol,
                         comm2.math,
                         comm2.stat,
                         comm2.epi,
                         comm2.med)
comm3.full <- subset(data.frame.full, Journal.Community == 3)
comm3.vet <- sum(comm3.full$vet.tot)
comm3.ecoevo <- sum(comm3.full$ecoevo.tot)
comm3.biol <- sum(comm3.full$biol.tot)
comm3.math <- sum(comm3.full$math.tot)
comm3.stat <- sum(comm3.full$stat.tot)
comm3.epi <- sum(comm3.full$epi.tot)
comm3.med <- sum(comm3.full$med.tot)
comm3.author.counts <- c(comm3.vet,
                         comm3.ecoevo,
                         comm3.biol,
                         comm3.math,
                         comm3.stat,
                         comm3.epi,
                         comm3.med)

comm1.00 <- subset(comm1.full, PubYear == 2000)
sum(comm1.00$vet)
comm1.00.vet <- sum(comm1.00$vet.tot)
comm1.00.ecoevo <- sum(comm1.00$ecoevo.tot)
comm1.00.biol <- sum(comm1.00$biol.tot)
comm1.00.math <- sum(comm1.00$math.tot)
comm1.00.stat <- sum(comm1.00$stat.tot)
comm1.00.epi <- sum(comm1.00$epi.tot)
comm1.00.med <- sum(comm1.00$med.tot)
comm1.00.tot <- sum(comm1.00.vet, comm1.00.ecoevo, comm1.00.biol, comm1.00.math, 
                 comm1.00.stat, comm1.00.epi, comm1.00.med)

comm1.02 <- subset(comm1.full, PubYear == 2002)
sum(comm1.02$vet)
comm1.02.vet <- sum(comm1.02$vet.tot)
comm1.02.ecoevo <- sum(comm1.02$ecoevo.tot)
comm1.02.biol <- sum(comm1.02$biol.tot)
comm1.02.math <- sum(comm1.02$math.tot)
comm1.02.stat <- sum(comm1.02$stat.tot)
comm1.02.epi <- sum(comm1.02$epi.tot)
comm1.02.med <- sum(comm1.02$med.tot)
comm1.02.tot <- sum(comm1.02.vet, 
                    comm1.02.ecoevo, 
                    comm1.02.biol, 
                    comm1.02.math,
                    comm1.02.stat, 
                    comm1.02.epi, 
                    comm1.02.med)

comm1.04 <- subset(comm1.full, PubYear == 2004)
sum(comm1.04$vet)
comm1.04.vet <- sum(comm1.04$vet.tot)
comm1.04.ecoevo <- sum(comm1.04$ecoevo.tot)
comm1.04.biol <- sum(comm1.04$biol.tot)
comm1.04.math <- sum(comm1.04$math.tot)
comm1.04.stat <- sum(comm1.04$stat.tot)
comm1.04.epi <- sum(comm1.04$epi.tot)
comm1.04.med <- sum(comm1.04$med.tot)
comm1.04.tot <- sum(comm1.04.vet, 
                    comm1.04.ecoevo, 
                    comm1.04.biol, 
                    comm1.04.math,
                    comm1.04.stat, 
                    comm1.04.epi, 
                    comm1.04.med)

comm1.06 <- subset(comm1.full, PubYear == 2006)
sum(comm1.06$vet)
comm1.06.vet <- sum(comm1.06$vet.tot)
comm1.06.ecoevo <- sum(comm1.06$ecoevo.tot)
comm1.06.biol <- sum(comm1.06$biol.tot)
comm1.06.math <- sum(comm1.06$math.tot)
comm1.06.stat <- sum(comm1.06$stat.tot)
comm1.06.epi <- sum(comm1.06$epi.tot)
comm1.06.med <- sum(comm1.06$med.tot)
comm1.06.tot <- sum(comm1.06.vet, 
                    comm1.06.ecoevo, 
                    comm1.06.biol, 
                    comm1.06.math,
                    comm1.06.stat, 
                    comm1.06.epi, 
                    comm1.06.med)

comm1.08 <- subset(comm1.full, PubYear == 2008)
sum(comm1.08$vet)
comm1.08.vet <- sum(comm1.08$vet.tot)
comm1.08.ecoevo <- sum(comm1.08$ecoevo.tot)
comm1.08.biol <- sum(comm1.08$biol.tot)
comm1.08.math <- sum(comm1.08$math.tot)
comm1.08.stat <- sum(comm1.08$stat.tot)
comm1.08.epi <- sum(comm1.08$epi.tot)
comm1.08.med <- sum(comm1.08$med.tot)
comm1.08.tot <- sum(comm1.08.vet, 
                    comm1.08.ecoevo, 
                    comm1.08.biol, 
                    comm1.08.math,
                    comm1.08.stat, 
                    comm1.08.epi, 
                    comm1.08.med)

comm1.10 <- subset(comm1.full, PubYear == 2010)
sum(comm1.10$vet)
comm1.10.vet <-    sum(comm1.10$vet.tot)
comm1.10.ecoevo <- sum(comm1.10$ecoevo.tot)
comm1.10.biol <-   sum(comm1.10$biol.tot)
comm1.10.math <-   sum(comm1.10$math.tot)
comm1.10.stat <-   sum(comm1.10$stat.tot)
comm1.10.epi <-    sum(comm1.10$epi.tot)
comm1.10.med <-    sum(comm1.10$med.tot)
comm1.10.tot <-    sum(comm1.10.vet, 
                    comm1.10.ecoevo, 
                    comm1.10.biol, 
                    comm1.10.math,
                    comm1.10.stat, 
                    comm1.10.epi, 
                    comm1.10.med)

comm1.12 <- subset(comm1.full, PubYear == 2012)
sum(comm1.12$vet)
comm1.12.vet <-    sum(comm1.12$vet.tot)
comm1.12.ecoevo <- sum(comm1.12$ecoevo.tot)
comm1.12.biol <-   sum(comm1.12$biol.tot)
comm1.12.math <-   sum(comm1.12$math.tot)
comm1.12.stat <-   sum(comm1.12$stat.tot)
comm1.12.epi <-    sum(comm1.12$epi.tot)
comm1.12.med <-    sum(comm1.12$med.tot)
comm1.12.tot <-    sum(comm1.12.vet, 
                       comm1.12.ecoevo, 
                       comm1.12.biol, 
                       comm1.12.math,
                       comm1.12.stat, 
                       comm1.12.epi, 
                       comm1.12.med)

comm1.14 <- subset(comm1.full, PubYear == 2014)
sum(comm1.14$vet)
comm1.14.vet <-    sum(comm1.14$vet.tot)
comm1.14.ecoevo <- sum(comm1.14$ecoevo.tot)
comm1.14.biol <-   sum(comm1.14$biol.tot)
comm1.14.math <-   sum(comm1.14$math.tot)
comm1.14.stat <-   sum(comm1.14$stat.tot)
comm1.14.epi <-    sum(comm1.14$epi.tot)
comm1.14.med <-    sum(comm1.14$med.tot)
comm1.14.tot <-    sum(comm1.14.vet, 
                       comm1.14.ecoevo, 
                       comm1.14.biol, 
                       comm1.14.math,
                       comm1.14.stat, 
                       comm1.14.epi, 
                       comm1.14.med)

comm1.eco.time <- c(comm1.00.ecoevo / comm1.00.tot,
                    comm1.02.ecoevo / comm1.02.tot,
                    comm1.04.ecoevo / comm1.04.tot,
                    comm1.06.ecoevo / comm1.06.tot,
                    comm1.08.ecoevo / comm1.08.tot,
                    comm1.10.ecoevo / comm1.10.tot,
                    comm1.12.ecoevo / comm1.12.tot,
                    comm1.14.ecoevo / comm1.14.tot
                    )

yr <- c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)
plot(comm1.eco.time ~ yr, pch = 16, type = "b", ylim = c(0, 1))

par(mfrow = c(1, 1))
barplot((rbind(comm1.author.counts, comm2.author.counts, comm3.author.counts)), 
        xlab = "Author affiliation", ylab = "Number of authors",
        names = c("vet", "ecoevo", "biol", "math", "stat", "epi", "med"), 
        col = c("lightblue", "pink", "gold"), 
        legend.text = c("Vet Journal", "Gen Biol Journal", "Ecology Journal"),
        args.legend = list("topleft",
                           fill = c("lightblue", "pink", "gold"), bty = "n")
        )

relative.props <- cbind(comm1.author.counts / sum(comm1.author.counts), 
                        comm2.author.counts / sum(comm2.author.counts), 
                        comm3.author.counts / sum(comm3.author.counts))
barplot(relative.props, 
        legend = T)

# PLOTS: author discipline diversity and Number center affiliations by annualized citation rate
AuthorFactorsByCitesPlot(data.frame = data.frame.full)
AuthorFactorsByCitesPlot(data.frame = comm1.full)
AuthorFactorsByCitesPlot(data.frame = comm2.full)
AuthorFactorsByCitesPlot(data.frame = comm3.full)

AuthorDiversByCitesComparPlot(comm1.dat = comm1.full, comm2.dat = comm2.full,
                              comm3.dat = comm3.full)

# does author centrality for all previous papers predict author centrality on this paper?


# plot(comm1.full$author.diversity ~ as.factor(comm1.full$PubYear), pch = 16, col = "lightblue")
# plot(comm2.full$author.diversity ~ as.factor(comm2.full$PubYear), pch = 16, col = "pink")
# plot(comm3.full$author.diversity ~ as.factor(comm3.full$PubYear), pch = 16, col = "gold")

# author centrality through time figure
par(mfrow = c(1, 1))
plot(data.frame.full$author.diversity ~ as.factor(data.frame.full$PubYear), 
     pch = 16, col = "grey60", xlab = "", ylab = "author diversity", xaxt = "n")
axis(side = 1, at = c(2, 7, 12, 17, 22), labels = c("1990", "1995", "2000", "2005", "2010"))


papers.with.citrate <- subset(data.frame, is.na(AnnualizedCitationRate) == F & 
                                is.infinite(AnnualizedCitationRate) == F)
close.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.close[-1], 
                data = papers.with.citrate)
between.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.between[-1], 
                  data = papers.with.citrate)
degree.fit <- lm(log(AnnualizedCitationRate[-1] + 1) ~ avg.author.degree[-1], 
                 data = papers.with.citrate)
cor(subset(papers.with.citrate[-1, ], 
           select = c(avg.author.degree, avg.author.between, avg.author.close)))
saturated.fit <- lmer(log(AnnualizedCitationRate + 1) ~ log(avg.author.between + 1) + 
                        (1 | Source), data = papers.with.citrate[-1, ])

# extract papers with very high residuals in saturated model
papers.with.citrate$sat.resids[2:1455] <- residuals(saturated.fit)
sat.quantiles <- quantile(papers.with.citrate$sat.resids[-1], c(0.025, 0.975))
outliers <- which(papers.with.citrate$sat.resids <= sat.quantiles[1] | 
                    papers.with.citrate$sat.resids >= sat.quantiles[2])
outlier.papers <- papers.with.citrate[outliers, ]

outlier.view.sub <- subset(outlier.papers, select = c("Authors", 
                                                      "Title", 
                                                      "Paper.Number", 
                                                      "Source", 
                                                      "AnnualizedCitationRate", 
                                                      "avg.author.close", 
                                                      "avg.author.between", 
                                                      "sat.resids"))
outlier.view.sub[21:40, ]

#--------------------------------------------#
#-- regenerate networks through time --------#
#--------------------------------------------#

# between-subgraph edges
large.comms.subgraph <- induced.subgraph(journal.graph.95, vids = which(walktr.jo.95$membership %in% comms.to.include))
between.edges <- length(E(large.comms.subgraph)) - within.edges

within.edges <- between.edges <- n.comms <- rep(NA, length(1995:2014))
test.year <- comm.members <- within.edges.vec <- between.edges.vec <- vector("list", length(1995:2014))
for(i in ((1995:2014) - 1994))
{
  test.year[[i]] <- YearSpecJournalEdgeWeightRatio(data.frame = data.frame, in.year = i + 1994)
  within.edges[i] <- test.year[[i]]$within.edges
  between.edges[i] <- test.year[[i]]$between.edges
  n.comms[i] <- test.year[[i]]$n.comms
  comm.members[[i]] <- test.year[[i]]$member.list
  within.edges.vec[[i]] <- test.year[[i]]$within.edges.vec
  between.edges.vec[[i]] <- test.year[[i]]$between.edges.vec
  print(i)
}

save(test.year, file = "NetworksByYear_17Feb2015.R")

matrix.compare <- vector("list", 20)
eco.old <- biol.old <- vet.old <- ento.old <- rep(NA, 20)
eco.prop.over <- biol.prop.over <- ento.prop.over <- vet.prop.over <- rep(NA, 20)
eco.within.prop <- biol.within.prop <- vet.within.prop <- ento.within.prop <- rep(NA, 20)
for(i in 1:20)
{
  comm.members[[i]]
  matrix.compare[[i]] <- matrix(NA, nrow = 4, ncol = n.comms[i])
  for(j in 1:n.comms[i])
  {
    matrix.compare[[i]][1, j] <- length(which(comm.members[[i]][[j]] %in% comm.members[[20]][[1]]))
    matrix.compare[[i]][2, j] <- length(which(comm.members[[i]][[j]] %in% comm.members[[20]][[2]]))
    matrix.compare[[i]][3, j] <- length(which(comm.members[[i]][[j]] %in% comm.members[[20]][[3]]))
    matrix.compare[[i]][4, j] <- length(which(comm.members[[i]][[j]] %in% comm.members[[20]][[4]]))
  }
  eco.old[i] <- which.max(matrix.compare[[i]][1, ])
  biol.old[i] <- which.max(matrix.compare[[i]][2, ])
  vet.old[i] <- which.max(matrix.compare[[i]][3, ])
  ento.old[i] <- which.max(matrix.compare[[i]][4, ])
  eco.prop.over[i] <- max(matrix.compare[[i]][1, ]) / sum(matrix.compare[[i]][1, ])
  biol.prop.over[i] <- max(matrix.compare[[i]][2, ]) / sum(matrix.compare[[i]][2, ])
  vet.prop.over[i] <- max(matrix.compare[[i]][3, ]) / sum(matrix.compare[[i]][3, ])
  ento.prop.over[i] <- max(matrix.compare[[i]][4, ]) / sum(matrix.compare[[i]][4, ])

  eco.within.prop[i] <- within.edges.vec[[i]][which.max(matrix.compare[[i]][1, ])] / (within.edges.vec[[i]][which.max(matrix.compare[[i]][1, ])] + between.edges.vec[[i]][which.max(matrix.compare[[i]][1, ])])
  biol.within.prop[i] <- within.edges.vec[[i]][which.max(matrix.compare[[i]][2, ])] / (within.edges.vec[[i]][which.max(matrix.compare[[i]][2, ])] + between.edges.vec[[i]][which.max(matrix.compare[[i]][2, ])])
  vet.within.prop[i] <- within.edges.vec[[i]][which.max(matrix.compare[[i]][3, ])] / (within.edges.vec[[i]][which.max(matrix.compare[[i]][3, ])] + between.edges.vec[[i]][which.max(matrix.compare[[i]][3, ])])
  ento.within.prop[i] <- within.edges.vec[[i]][which.max(matrix.compare[[i]][4, ])] / (within.edges.vec[[i]][which.max(matrix.compare[[i]][4, ])] + between.edges.vec[[i]][which.max(matrix.compare[[i]][4, ])])
}

within.edges <- within.edges[1995:2014]
between.edges <- between.edges[1995:2014]

par(mfrow = c(2, 2), oma = c(3, 2, 3, 3), mar = c(4, 5, 2, 2))
plot(n.comms ~ c(1995:2014), xlim = c(1994, 2015), ylim = c(0, 6), xlab = "year", ylab = "# communities detected", pch = 16)
plot(within.edges / (within.edges + between.edges) ~ c(1995:2014), xlim = c(1994, 2015), ylim = c(0, 1), xlab = "year", ylab = "% within-community references", pch = 16)
plot(eco.prop.over ~ c(1995:2014), col = "red", pch = 16, ylim = c(0, 1), xlab = "", ylab = "Proportion overlapping with \n 2014 community membership")
points(biol.prop.over ~ c(1995:2014), col = "blue", pch = 16)
points(vet.prop.over ~ c(1995:2014), col = "green", pch = 16)
points(ento.prop.over ~ c(1995:2014), col = "black")
leg.text <- c("Ecol", "GenBiol/Epi", "Vet", "Ento")
legend("bottomright", leg.text, pch = c(16, 16, 16, 1), col = c("red", "blue", "green", "black"), bty = "n")
plot(eco.within.prop ~ c(1995:2014), col = "red", pch = 16, ylim = c(0, 1), xlab = "", ylab = "Proportion of total edges \n within community")
points(biol.within.prop ~ c(1995:2014), col = "blue", pch = 16)
points(vet.within.prop ~ c(1995:2014), col = "green", pch = 16)
points(ento.within.prop ~ c(1995:2014), col = "black")
leg.text <- c("Ecol", "GenBiol/Epi", "Vet", "Ento")
legend("topright", leg.text, pch = c(16, 16, 16, 1), col = c("red", "blue", "green", "black"), bty = "n")


#----------------------------------#
#-- author networks through time --#
#----------------------------------#
# build storage
unique.authors.new.yr <- vector("list", 18)
for(i in 1:18)
{
  data.frame.yr <- subset(data.frame, PubYear %in% (1994 + i):(1994 + i + 2))
  data.frame.yr$Authors <- factor(data.frame.yr$Authors)
  all.authors.yr <- BuildAuthorFrame(data.frame.in = data.frame.yr)
  affils.test.yr.old <- GetAuthorAffils(author.frame = all.authors.yr)
  unique.authors.yr <- AuthorMerge(author.frame = affils.test.yr.old)
  author.graph.yr <- BuildAuthorGraph(all.authors = affils.test.yr.old, unique.authors.yr)
  author.diags.yr <- NetworkDiagnostics(graph.in = author.graph.yr, seed.in = 123)
  unique.authors.new.yr[[i]] <- as.data.frame(cbind(unique.authors.yr, author.diags.yr$out.unique.frame))
  year.between <- betweenness(author.graph.yr, normalized = T)
  author.names <- V(author.graph.yr)$name
  for(j in 1:dim(unique.authors.new.yr[[i]])[1]){
     unique.authors.new.yr[[i]]$betweenness[j] <- year.between[which(as.character(unique.authors.new.yr[[i]]$AuthorID) == as.character(author.names[j]))]
  } #j
  print(i)
} #i

names(unique.authors.new.yr[[1]])
mean.between.discip <- matrix(NA, nrow = 18, ncol = 8)
med.between.discip <- matrix(NA, nrow = 18, ncol = 8)
for(i in 1:18){
  # math
  math <- subset(unique.authors.new.yr[[i]], TotMath == 1)
  mean.between.discip[i, 1] <- mean(math$betweenness)
  med.between.discip[i, 1] <- median(math$betweenness)
  # stat
  stat <- subset(unique.authors.new.yr[[i]], TotStat == 1)
  mean.between.discip[i, 2] <- mean(stat$betweenness)
  med.between.discip[i, 2] <- median(stat$betweenness)
  # Ecol
  ecol <- subset(unique.authors.new.yr[[i]], TotEcol == 1)
  mean.between.discip[i, 3] <- mean(ecol$betweenness)
  med.between.discip[i, 3] <- median(ecol$betweenness)
  # Evol
  evol <- subset(unique.authors.new.yr[[i]], TotEvol == 1)
  mean.between.discip[i, 4] <- mean(evol$betweenness)
  med.between.discip[i, 4] <- median(evol$betweenness)
  # Epi
  epi <- subset(unique.authors.new.yr[[i]], TotEpi == 1)
  mean.between.discip[i, 5] <- mean(epi$betweenness)
  med.between.discip[i, 5] <- median(epi$betweenness)
  # Vet
  vet <- subset(unique.authors.new.yr[[i]], TotVet == 1)
  mean.between.discip[i, 6] <- mean(vet$betweenness)
  med.between.discip[i, 6] <- median(vet$betweenness)
  # Med
  med <- subset(unique.authors.new.yr[[i]], TotMed == 1)
  mean.between.discip[i, 7] <- mean(med$betweenness)
  med.between.discip[i, 7] <- median(med$betweenness)
  # Biol
  biol <- subset(unique.authors.new.yr[[i]], TotBiol == 1)
  mean.between.discip[i, 8] <- mean(biol$betweenness)
  med.between.discip[i, 8] <- median(biol$betweenness)
} #i

par(mfrow = c(1, 1), oma = c(4, 4, 4, 4), mar = c(4, 4, 4, 4))
plot(mean.between.discip[, 1] ~ c(1996:2013), col = "red", type = "l", ylim = c(0, .003), xlab = "", ylab = "mean logged betweenness")
lines(mean.between.discip[, 2] ~ c(1996:2013), col = "blue")
lines(mean.between.discip[, 3] ~ c(1996:2013), col = "green")
lines(mean.between.discip[, 4] ~ c(1996:2013), col = "orange")
lines(mean.between.discip[, 5] ~ c(1996:2013), col = "purple")
lines(mean.between.discip[, 6] ~ c(1996:2013), col = "turquoise")
lines(mean.between.discip[, 7] ~ c(1996:2013), col = "hotpink")
lines(mean.between.discip[, 8] ~ c(1996:2013), col = "darkblue")

par(mfrow = c(4, 4))
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotMath)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotBiol)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotEvol)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotEpi)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotVet)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotEcol)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotMed)
boxplot(unique.authors.new.yr[[2]]$betweenness ~ unique.authors.new.yr[[2]]$TotStat)
