# load required packages and source functions
require(igraph)
require(ape)
source("./TwoCities_SourceFunctions.R")

#------------------------------------------------------------------------------------------# 
#-- Data are set up with two unique keys: -------------------------------------------------#
#-- Paper.Number = papers in the FULL SET (accepted + rejected = 2258 papers) -------------#
#-- AuthorID = unique author identified: all lower case. last name <space> first initial --#
#------------------------------------------------------------------------------------------#

#-------------------------------------------#
#-- Preliminary description of papers ------#
#-------------------------------------------#
data.frame <- read.csv("~/work/Kezia/Research/EcologyPapers/TwoCities/Data/IncludedPaperBank_CirculatedV1.csv", header = T, sep = "\t")
names(data.frame)[5] <- "Keep"
data.frame <- subset(data.frame, Keep == 1)
dim(data.frame) # 1632 papers
data.frame$Source <- factor(data.frame$Source)
data.frame$AnnualizedCitationRate <- data.frame$TimesCited / (2014 - data.frame$PubYear)

# histogram of distribution of papers among journals
length(levels(factor(data.frame$Source))) # 112 journals represented
table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# order command reorders levels so that they appear in descending frequency
par(mfrow = c(1, 1), las = 2, mar = c(15, 4, 1, 1))
plot(order.source, type = "h", xaxt = "n", ylab = "Frequency", ylim = c(0, 180), xlab = "")
axis(side = 1, at = c(1:112), labels = names(order.source), las = 2, cex.axis = .5)

# histogram of distribution of citations
par(mfrow = c(2, 2), mex = 1, oma = c(2, 0, 0, 0))
hist(log(data.frame$TimesCited + 1), col = "grey80", main = "", xaxt = "n", xlab = "log(Total Citations + 1)")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
hist(log(data.frame$AnnualizedCitationRate + 1), col = "grey80", xlab = "log(Annual Citations + 1)", main = "", xaxt = "n")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ data.frame$PubYear, yaxt = "n", ylab = "log(Annual citations + 1)", xlab = "year of publication")
axis(side = 2, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))

#-----------------------------------------#
#-- Author network -----------------------#
#-----------------------------------------#
all.authors <- BuildAuthorFrame(data.frame.in = data.frame)
affils.test <- GetAuthorAffils(author.frame = all.authors)
unique.authors <- AuthorMerge(author.frame = affils.test)
author.graph <- BuildAuthorGraph(all.authors = affils.test, unique.authors)
author.diags <- NetworkDiagnostics(graph.in = author.graph, seed.in = 123)
unique.authors.new <- as.data.frame(cbind(unique.authors, author.diags$out.unique.frame))

par(mfrow = c(1, 1))
plot(author.graph, margin = c(-.75, -.75, 0, 0), vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)

# # author communities
# walktr.au <- walktrap.community(author.graph, steps = 20)
# membership(walktr.au)
# sizes(walktr.au)
# component1.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 1]
# component2.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 2]
# component3.au <- vertex.attributes(author.graph)$name[membership(walktr.au) == 3]
# plot(walktr.au, 
#      margin = c(-.75, -.75, 0, 0), 
#      author.graph, 
#      layout = layout.fruchterman.reingold, 
#      edge.arrow.size = .3, 
#      vertex.label = "", 
#      vertex.size = 3)
# 
# # cut down to just authors in communities of 10 or more
# communities.to.include.au <- which(sizes(walktr.au) >= 20)
# vertex.inclusion.ind.au <- ifelse(membership(walktr.au) %in% communities.to.include.au, 1, 0)
# author.graph.small <- delete.vertices(author.graph, which(vertex.inclusion.ind.au == 0))
# multi.au <- multilevel.community(author.graph.small)
# 
# vertex.labels.verysmall <- rep(NA, length(V(author.graph.small)$name))
# for(i in 1:length(vertex.labels.verysmall)){
#   vertex.labels.verysmall[i] <- ifelse (i %in% which(V(author.graph.small)$size >= 50), 
#                                         toupper(gsub(" .", "", V(author.graph.small)$name[i])), 
#                                         "")
# }
# 
# labeled.scientists <- V(author.graph.small)$name[which(V(author.graph.small)$size >= 50)]
# 
# plot(author.graph.small, 
#      vertex.frame.color = "grey70", 
#      vertex.label = vertex.labels.verysmall,  
#      vertex.label.cex = .5, 
#      vertex.label.color = "black", 
#      margin = c(-.9, -.95, -.3, -.7), 
#      layout = layout.fruchterman.reingold, 
#      edge.width = 0.25, 
#      edge.color = "grey80", 
#      edge.arrow.size = 0, 
#      vertex.label = "", 
#      vertex.size = V(author.graph.small)$size / 12, 
#      vertex.color = "grey80")
# 
# plot(multi.au, 
#      author.graph.small, 
#      vertex.frame.color = "grey70", 
#      vertex.label = vertex.labels.verysmall,  
#      vertex.label.cex = .5, 
#      vertex.label.color = "black", 
#      margin = c(-.9, -.95, -.3, -.7), 
#      layout = layout.fruchterman.reingold, 
#      edge.width = 0.25, 
#      edge.color = "grey80", 
#      edge.arrow.size = 0, 
#      vertex.label = "", 
#      vertex.size = V(author.graph.small)$size / 12, 
#      vertex.color = "grey80")

# cut down to just giant component
author.clusters <- clusters(author.graph)
giant.compo.aus <- delete.vertices(author.graph, which(clusters(author.graph)$membership != 1))

plot(giant.compo.aus, 
     margin = c(-.85, -.75, -.15, -.5), 
     vertex.size = 1, 
     vertex.label = NA, 
     edge.arrow.size = .05)

stored.layout <- layout.fruchterman.reingold(giant.compo.aus)
margin.dims <- c(-1.0, -1.0, -.15, -.9)
giant.compo.au.walktr <- walktrap.community(giant.compo.aus, steps = 4)
giant.compo.au.walktr$membership
table(giant.compo.au.walktr$membership)
# largest communities get unique colors; all communities <= 20 are "grey40"
small.communities.au <- as.numeric(as.character(names(table(giant.compo.au.walktr$membership))[which(table(giant.compo.au.walktr$membership) <= 50)]))
au.vertex.ind <- factor(ifelse(giant.compo.au.walktr$membership %in% small.communities.au, 
                               "small", giant.compo.au.walktr$membership))
color.vec.au <- c("red", "blue", "green", "yellow", "deeppink", "darkseagreen4", "purple", "grey70")
au.vertex.colors <- color.vec.au[au.vertex.ind]

au.comm.top10 <- vector("list", length(levels(au.vertex.ind)))
for(i in 1:length(levels(au.vertex.ind))){
  au.comm.i <- V(giant.compo.aus)[as.numeric(au.vertex.ind) == i]
  au.comm.top10[[i]] <- au.comm.i$name[order(au.comm.i$size, decreasing = T)][1:10]
}

plot(giant.compo.aus, 
     vertex.frame.color = as.numeric(walktr.au.giant$membership), 
     vertex.label = vertex.labels.giantcompos,  
     vertex.label.cex = .5, 
     vertex.label.color = "black", 
     margin = c(-.9, -.95, -.3, -.7), 
     layout = layout.fruchterman.reingold, 
     edge.width = 0.25, 
     edge.color = "grey50", 
     edge.arrow.size = 0, 
     vertex.label = "", 
     vertex.size = V(giant.compo.aus)$size / 12, 
     vertex.color = as.numeric(walktr.au.giant$membership))

stored.layout.au <- layout.fruchterman.reingold(giant.compo.aus)

plot(giant.compo.aus, 
     margin = margin.dims, 
     vertex.frame.color = "black", 
     vertex.label = "",  
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     layout = stored.layout.au, 
     edge.width = 0.25, 
     edge.color = "grey50", 
     edge.arrow.size = 0, 
     vertex.label = "", 
     vertex.size = V(giant.compo.aus)$size / 12, 
     vertex.color = au.vertex.colors)

au.dendro <- as.dendrogram(giant.compo.au.walktr)
height.of.leafs <- dendrapply(au.dendro, function(e) attr(e, "height"))
quantile(unlist(height.of.leafs), c(0.9, 0.95, 0.975))
au.dendro.height2700 <- cut(au.dendro, h = 2700)
plot(au.dendro.height2700$upper)

quantile(V(giant.compo.aus)$size, 0.95)
V(giant.compo.aus)$name[which(V(giant.compo.aus)$size >= 24)]
which(V(giant.compo.aus)$size >= 24)[1]

#---------------------------------#
#-- citation network -------------#
#---------------------------------# 
no.cite.papers.in = c(1, 494, 603, 858)

cite.list <- BuildCitationFrame(data.frame.in = data.frame, no.cite.papers = no.cite.papers.in)
assoc.mat <- BuildAssocMat(data.frame.in = data.frame[-no.cite.papers.in, -no.cite.papers.in], cite.frame = cite.list[-no.cite.papers.in])
full.citation.frame <- do.call("rbind", cite.list) # 69905 total refs

paper.graph <- BuildPaperGraph(assoc.mat, data.frame[-no.cite.papers.in, ])
paper.diags <- NetworkDiagnostics(graph.in = paper.graph, seed.in = 123)

# plots for paper graph
par(mfrow = c(1, 1))
plot(paper.graph, vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)
paper.degree.dist <- degree(paper.graph, mode = "in")
paper.betweenness <- betweenness(paper.graph)
paper.hierarchy <- hierarchy(paper.graph)
paper.optim.community <- optimal.community(paper.graph)

# plot of giant component only
giant.compo.papers <- delete.vertices(paper.graph, which(clusters(paper.graph)$membership != 5))
giant.compo.papers.walktr <- walktrap.community(giant.compo.papers, steps = 4)
giant.compo.papers.walktr$membership
table(giant.compo.papers.walktr$membership)
# largest communities get unique colors; all communities <= 10 are "grey40"
small.communities <- as.numeric(as.character(names(table(giant.compo.papers.walktr$membership))[which(table(giant.compo.papers.walktr$membership) <= 20)]))
paper.vertex.ind <- factor(ifelse(giant.compo.papers.walktr$membership %in% small.communities, "small", giant.compo.papers.walktr$membership))
color.vec <- c("red", "blue", "green", "yellow", "deeppink", "darkseagreen4", "darkturquoise", "purple", "grey70")
paper.vertex.colors <- color.vec[paper.vertex.ind]

paper.comm.top10 <- vector("list", length(levels(paper.vertex.ind)))
for(i in 1:length(levels(paper.vertex.ind))){
  paper.comm.i <- V(giant.compo.papers)[as.numeric(paper.vertex.ind) == i]
  paper.comm.top10[[i]] <- paper.comm.i$name[order(paper.comm.i$size, decreasing = T)][1:10]
}

margin.dims = c(0, 0, 0, 0)
plot(giant.compo.papers, 
     margin = margin.dims, 
     vertex.frame.color = "black", 
     vertex.label = "",  
     vertex.label.cex = .5, 
     vertex.label.color = "black", 
     layout = fixed.paper.layout, 
     edge.width = 0.25, 
     edge.color = "grey50", 
     edge.arrow.size = 0, 
     vertex.label = "", 
     vertex.size = V(giant.compo.papers)$size / 30, 
     vertex.color = paper.vertex.colors)

#-----------------------------------------#
#-- Journal network ----------------------#
#-----------------------------------------#
# aggregated annualized citations of database papers by journal
journal.data <- ExtractJournalData(data.frame)

order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# order command reorders levels so that they appear in descending frequency
par(mfrow = c(1, 1), las = 2, mar = c(15, 6, 1, 1))
plot(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)] ~ c(1:112), 
     type = "h", 
     xaxt = "n", 
     ylab = "Avg. annualize citation rate for \n all papers in dataset", 
     xlab = "", 
     lwd = journal.total.papers[order(journal.agg.annual.cites, decreasing = T)] * .5)
text(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)][1:20] ~ c(seq(1:20) + 0.25), 
     labels = paste("(", journal.total.papers[order(journal.agg.annual.cites, decreasing = T)][1:20], ")", sep = ""), 
     cex = .6)
axis(side = 1, at = c(1:112), labels = levels(data.frame$Source)[order(journal.agg.annual.cites, decreasing = T)]
     , las = 2, cex.axis = .5)

journal.graph <- BuildJournalGraph(data.frame.in = data.frame[-no.cite.papers, -no.cite.papers], assoc.mat.in = assoc.mat)

plot(journal.graph)
# Qu: which journals are excluded?
disconnected.journals <- levels(factor(data.frame$Source))[!(levels(factor(data.frame$Source)) %in% levels(journal.frame$journal1.vec))]
V(journal.graph)$size <- table(data.frame$Source)[!(names(table(data.frame$Source)) %in% disconnected.journals)]

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
plot(journal.graph.small, 
     layout = layout.fruchterman.reingold, 
     vertex.size = log(table(journal.frame$journal1.vec) + 1) * 2, 
     vertex.label = NA, 
     edge.arrow.size = .05)

# journal communities
set.seed(123)
walktr.jo <- walktrap.community(journal.graph.small, steps = 4)
walktr.jo$membership
table(walktr.jo$membership)
# largest communities get unique colors; all communities <= 10 are "grey40"
small.communities.jo <- as.numeric(as.character(names(table(walktr.jo$membership))[which(table(walktr.jo$membership) <= 3)]))


jo.vertex.ind <- factor(ifelse(walktr.jo$membership %in% small.communities.jo, "small", walktr.jo$membership))
color.vec.jo <- c("deeppink", "blue", "gold1", "grey70")
jo.vertex.colors <- color.vec.jo[jo.vertex.ind]
V(journal.graph.small)$size <- ifelse(is.finite(V(journal.graph.small)$size) == T, V(journal.graph.small)$size, 0)

jo.comm.top10 <- vector("list", length(levels(jo.vertex.ind)))
for(i in 1:length(levels(jo.vertex.ind))){
  jo.comm.i <- V(journal.graph.small)[as.numeric(jo.vertex.ind) == i]
  jo.comm.top10[[i]] <- jo.comm.i$name[order(jo.comm.i$size, decreasing = T)][1:10]
}

fixed.jo.layout <- layout.fruchterman.reingold(journal.graph.small)
margin.dims = c(0, 0, 0, 0)

#svg("~/work/Kezia/Research/EcologyPapers/TwoCities/Plots/JournalsGC_06Dec2014.svg")
#fixed.paper.layout <- layout.fruchterman.reingold(giant.compo.papers)
plot(journal.graph.small, margin = margin.dims, vertex.frame.color = "black", vertex.label = "",  vertex.label.cex = .5, vertex.label.color = "black", layout = fixed.jo.layout, edge.width = 0.25, edge.color = "grey50", edge.arrow.size = 0, vertex.label = "", vertex.size = (V(journal.graph.small)$size + 1) / 10 + 2, vertex.color = jo.vertex.colors)
#dev.off()

jo.dendro <- as.dendrogram(walktr.jo)
in.colbar <- c("forestgreen", "navyblue", "red", "black", "green", "seagreen4", "yellow", "turquoise", "purple", "gold", "magenta", "grey40", "orange", "deeppink", "blue")
par(mar = c(0, 0, 0, 0), cex.axis = .7)
dendPlot(walktr.jo, mode = "phylo", label.offset = .5, cex = .8, colbar = in.colbar, direction = "downwards")


# extract journal community membership for each paper
journal.community.membership <- data.frame(cbind(jo.vertex.ind, V(journal.graph.small)$name))
names(journal.community.membership) <- c("Community", "JournalName")
data.frame$JournalCommunity <- rep(NA, dim(data.frame)[1])
for(i in 1:dim(data.frame)[1]){
  journal.source <- subset(journal.community.membership, as.character(JournalName) == as.character(data.frame$Source[i]))
  data.frame$JournalCommunity[i] <- as.character(journal.source$Community[1])
}

#write.csv(data.frame$JournalCommunity, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/JournalCommunityVector_16Dec2014.csv")

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
Comm1ReaderList <- ReaderAssignmentSpecificGrp(data.frame, number.readers = 4, papers.per.reader = 20, fixed.seed = 123, community.to.use = 1, quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))
Comm2ReaderList <- ReaderAssignmentSpecificGrp(data.frame, number.readers = 4, papers.per.reader = 20, fixed.seed = 123, community.to.use = 2, quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))
Comm3ReaderList <- ReaderAssignmentSpecificGrp(data.frame, number.readers = 4, papers.per.reader = 20, fixed.seed = 123, community.to.use = 3, quantiles.to.use = c(0.33, 0.34, 0.66, 0.67))

readers <- c("Bande", "Craft", "Cross", "Huyvaert", "Joseph", "Manlove", "Miller", "Nol", "OBrien", "Patyk", "Walker", "Walsch")
comm1.reader.frame <- do.call("rbind", Comm1ReaderList)
comm1.reader.frame$Reader <- rep(readers[1:4], each = 18)
comm2.reader.frame <- do.call("rbind", Comm2ReaderList)
comm2.reader.frame$Reader <- rep(readers[5:8], each = 18)
comm3.reader.frame <- do.call("rbind", Comm3ReaderList)
comm3.reader.frame$Reader <- rep(readers[9:12], each = 18)

full.reader.assignments <- as.data.frame(rbind(comm1.reader.frame, comm2.reader.frame, comm3.reader.frame))
# write.csv(full.reader.assignments, "~/work/Kezia/Research/EcologyPapers/TwoCities/Data/FullReadingAssignments_11Jan2015.csv")

#-------------------------------------------------------------------------------------#
#-- regression of avg. author closeness and betweenness on annualized citation rate --#
#-------------------------------------------------------------------------------------#
# to get paper's authorship diversity, 
# 1) extract authors from author.unique.frame by looping over paper numbers in author full frame, extracting authorID, and subsetting on authorID in author.unique.frame
# 2) sum (math, stat, ecol, evol, epi, med) across all paper authors
data.frame.full <- DataFrameAddons(data.frame.in = data.frame, all.authors, unique.authors)

# PLOTS: author discipline diversity and Number center affiliations by annualized citation rate
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2), oma = c(1, 1, 0, 0))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$author.diversity), 
     xaxt = "n", yaxt = "n", 
     xlab = "Author discipline diversity", 
     ylab = "Annualized citation rate", 
     col = "grey80")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, cex.axis = .7,
     at = c(log(1), log(5), log(10), log(50)), 
     labels = c("1", "5", "10", "50"))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$num.authors.in.ctrs), 
     xaxt = "n", yaxt = "n", col = "grey80",
     xlab = "Number of authors with center affiliations", 
     ylab = "Annualized citation rate")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, cex.axis = .7,
     at = c(log(1), log(5), log(10), log(50)), 
     labels = c("1", "5", "10", "50"))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$num.authors), 
     xaxt = "n", 
     yaxt = "n", 
     xlab = "Number of authors", 
     ylab = "Annualized citation rate", 
     col = "grey80")
axis(side = 1, las = 1, cex.axis = .7)
axis(side = 2, 
     at = c(log(1), log(5), log(10), log(50)), 
     labels = c("1", "5", "10", "50"), 
     cex.axis = .7)
plot(log(data.frame$AnnualizedCitationRate + 1) ~ as.factor(data.frame$discipline.class), 
     xaxt = "n", 
     yaxt = "n", 
     xlab = "Discipline class", 
     ylab = "Annualized citation rate", 
     col = "grey80")
axis(side = 1, 
     at = seq(1:length(levels(factor(data.frame$discipline.class)))), 
     labels = levels(factor(data.frame$discipline.class)), 
     las = 2, 
     cex.axis = .7)
axis(side = 2, 
     at = c(log(1), log(5), log(10), log(50)), 
     labels = c("1", "5", "10", "50"), 
     cex.axis = .7)

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
