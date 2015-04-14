# load required packages and source functions
require(igraph)
require(ape)
require(lme4)
source("./R/TwoCities_SourceFunctions.R")

#------------------------------------------------------------------------------------------# 
#-- Data are set up with two unique keys: -------------------------------------------------#
#-- Paper.Number = papers in the FULL SET (accepted + rejected = 2258 papers) -------------#
#-- AuthorID = unique author identified: all lower case. last name <space> first initial --#
#------------------------------------------------------------------------------------------#

#-------------------------------------------#
#-- Preliminary description of papers ------#
#-------------------------------------------#
data.frame <- read.csv("./Data/IncludedPaperBank_Final.csv", header = T, sep = "\t")
data.frame <- DataPrep(data.frame)
jo.comms <- read.csv("./Data/JournalCommunityVector_16Dec2014.csv")
jo.comms <- jo.comms[-c(1, 494, 603, 858), 2] # four papers removed in DataPrep function
data.frame$Journal.Community <- jo.comms
comm1 <- subset(data.frame, Journal.Community == 1)
comm2 <- subset(data.frame, Journal.Community == 2)
comm3 <- subset(data.frame, Journal.Community == 3)

JournalFreqHist(data.frame)
TimesCitedHists(data.frame)

#-- prep all data --#

all.authors <- BuildAuthorFrame(data.frame.in = data.frame)
affils.test <- GetAuthorAffils(author.frame = all.authors)
unique.authors <- AuthorMerge(author.frame = affils.test)
author.graph <- BuildAuthorGraph(all.authors = affils.test, unique.authors)
author.diags <- NetworkDiagnostics(graph.in = author.graph, seed.in = 123)
unique.authors.new <- as.data.frame(cbind(unique.authors, author.diags$out.unique.frame))

cite.list <- BuildCitationFrame(data.frame.in = data.frame)
assoc.mat <- BuildAssocMat(data.frame.in = data.frame, 
                           cite.frame = cite.list)
full.citation.frame <- do.call("rbind", cite.list) # 69905 total refs
paper.graph <- BuildPaperGraph(assoc.mat, data.frame)
paper.diags <- NetworkDiagnostics(graph.in = paper.graph, seed.in = 123)

journal.data <- ExtractJournalData(data.frame)
journal.graph <- BuildJournalGraph(data.frame.in = data.frame,
                                   assoc.mat.in = assoc.mat)
# 26 journals get cut when constructing journal.graph....

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
paper.optim.community <- optimal.community(paper.graph)

# plot of giant component only
giant.compo.papers <- delete.vertices(paper.graph, 
                                      which(clusters(paper.graph)$membership != 5))
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

paper.comm.top10 <- vector("list", length(levels(paper.vertex.ind)))
for(i in 1:length(levels(paper.vertex.ind))){
  paper.comm.i <- V(giant.compo.papers)[as.numeric(paper.vertex.ind) == i]
  paper.comm.top10[[i]] <- paper.comm.i$name[order(paper.comm.i$size, decreasing = T)][1:10]
}

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
comm2.full <- subset(data.frame.full, Journal.Community == 2)
comm3.full <- subset(data.frame.full, Journal.Community == 3)
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
