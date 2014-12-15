# load required packages and source functions
require(igraph)
require(ape)

#------------------------------------------------------------#
#-- download data from Google Drive:-------------------------#
#-- Two Cities / Data / IncludedPaperBank_CirculatedV1.csv --#
#------------------------------------------------------------#

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
par(mfrow = c(2, 2))
hist(log(data.frame$TimesCited + 1), col = "grey80", main = "", xaxt = "n", xlab = "log(Total Citations + 1)")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
hist(log(data.frame$AnnualizedCitationRate + 1), col = "grey80", xlab = "log(Annualized Citations + 1)", main = "", xaxt = "n")
axis(side = 1, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))
plot(log(data.frame$AnnualizedCitationRate + 1) ~ data.frame$PubYear, yaxt = "n", ylab = "log(Annualized citations + 1)", xlab = "year")
axis(side = 2, at = c(log(1), log(5), log(10), log(50), log(100), log(500), log(1000)), labels = c("1", "5", "10", "50", "100", "500", "1000"))


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
length(authors) # 4309 authors
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
power.law.fit.au <- power.law.fit(author.unique.frame$degrees)

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

labeled.scientists <- V(author.graph.small)$name[which(V(author.graph.small)$size >= 50)]

plot(author.graph.small, vertex.frame.color = "grey70", vertex.label = vertex.labels.verysmall,  vertex.label.cex = .5, vertex.label.color = "black", margin = c(-.9, -.95, -.3, -.7), layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = "grey80", edge.arrow.size = 0, vertex.label = "", vertex.size = V(author.graph.small)$size / 12, vertex.color = "grey80")
plot(multi.au, author.graph.small, vertex.frame.color = "grey70", vertex.label = vertex.labels.verysmall,  vertex.label.cex = .5, vertex.label.color = "black", margin = c(-.9, -.95, -.3, -.7), layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = "grey80", edge.arrow.size = 0, vertex.label = "", vertex.size = V(author.graph.small)$size / 12, vertex.color = "grey80")

# cut down to just giant component
author.clusters <- clusters(author.graph)
giant.compo.aus <- delete.vertices(author.graph, which(clusters(author.graph)$membership != 1))
#author.graph[clusters(author.graph)$membership == 1, clusters(author.graph)$membership == 1]
plot(giant.compo.aus, margin = c(-.85, -.75, -.15, -.5), vertex.size = 1, vertex.label = NA, edge.arrow.size = .05)

stored.layout <- layout.fruchterman.reingold(giant.compo.aus)
margin.dims <- c(-1.0, -1.0, -.15, -.9)
giant.compo.au.walktr <- walktrap.community(giant.compo.aus, steps = 4)
giant.compo.au.walktr$membership
table(giant.compo.au.walktr$membership)
# largest communities get unique colors; all communities <= 20 are "grey40"
small.communities.au <- as.numeric(as.character(names(table(giant.compo.au.walktr$membership))[which(table(giant.compo.au.walktr$membership) <= 50)]))
au.vertex.ind <- factor(ifelse(giant.compo.au.walktr$membership %in% small.communities.au, "small", giant.compo.au.walktr$membership))
color.vec.au <- c("red", "blue", "green", "yellow", "deeppink", "darkseagreen4", "purple", "grey70")
au.vertex.colors <- color.vec.au[au.vertex.ind]

au.comm.top10 <- vector("list", length(levels(au.vertex.ind)))
for(i in 1:length(levels(au.vertex.ind))){
  au.comm.i <- V(giant.compo.aus)[as.numeric(au.vertex.ind) == i]
  au.comm.top10[[i]] <- au.comm.i$name[order(au.comm.i$size, decreasing = T)][1:10]
}

plot(giant.compo.aus, vertex.frame.color = as.numeric(walktr.au.giant$membership), vertex.label = vertex.labels.giantcompos,  vertex.label.cex = .5, vertex.label.color = "black", margin = c(-.9, -.95, -.3, -.7), layout = layout.fruchterman.reingold, edge.width = 0.25, edge.color = "grey50", edge.arrow.size = 0, vertex.label = "", vertex.size = V(giant.compo.aus)$size / 12, vertex.color = as.numeric(walktr.au.giant$membership))

stored.layout.au <- layout.fruchterman.reingold(giant.compo.aus)
#svg("~/work/Kezia/Research/EcologyPapers/TwoCities/Plots/AuthorsGC_06Dec2014.svg")
plot(giant.compo.aus, margin = margin.dims, vertex.frame.color = "black", vertex.label = "",  vertex.label.cex = .6, vertex.label.color = "black", layout = stored.layout.au, edge.width = 0.25, edge.color = "grey50", edge.arrow.size = 0, vertex.label = "", vertex.size = V(giant.compo.aus)$size / 12, vertex.color = au.vertex.colors)
#dev.off()

au.dendro <- as.dendrogram(giant.compo.au.walktr)
height.of.leafs <- dendrapply(au.dendro, function(e) attr(e, "height"))
quantile(unlist(height.of.leafs), c(0.9, 0.95, 0.975))
au.dendro.height2700 <- cut(au.dendro, h = 2700)
plot(au.dendro.height2700$upper)

quantile(V(giant.compo.aus)$size, 0.95)
V(giant.compo.aus)$name[which(V(giant.compo.aus)$size >= 24)]
which(V(giant.compo.aus)$size >= 24)[1]

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
V(paper.graph)$size <- data.frame$TimesCited
V(paper.graph)$name <- rep(NA, 1605)
for(i in 2:1605){
  first.i <- strsplit(strsplit(as.character(data.frame$Authors)[i], split = ";")[[1]][1], split = ",")[[1]][1]
  V(paper.graph)$name[i] <- paste(first.i, " ", as.character(data.frame$PubYear[i]), " ", as.character(data.frame$Title[i]), sep = "")
}

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
#svg("~/work/Kezia/Research/EcologyPapers/TwoCities/Plots/PapersGC_06Dec2014.svg")
#fixed.paper.layout <- layout.fruchterman.reingold(giant.compo.papers)
plot(giant.compo.papers, margin = margin.dims, vertex.frame.color = "black", vertex.label = "",  vertex.label.cex = .5, vertex.label.color = "black", layout = fixed.paper.layout, edge.width = 0.25, edge.color = "grey50", edge.arrow.size = 0, vertex.label = "", vertex.size = V(giant.compo.papers)$size / 30, vertex.color = paper.vertex.colors)
#dev.off()



#-----------------------------------------#
#-- Journal network ----------------------#
#-----------------------------------------#
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

order.source <- table(data.frame$Source)[order(table(data.frame$Source), decreasing = T)]
# order command reorders levels so that they appear in descending frequency
par(mfrow = c(1, 1), las = 2, mar = c(15, 6, 1, 1))
plot(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)] ~ c(1:108), type = "h", xaxt = "n", ylab = "Avg. annualize citation rate for \n all papers in dataset", xlab = "", lwd = journal.total.papers[order(journal.agg.annual.cites, decreasing = T)] * .5)
text(journal.agg.annual.cites[order(journal.agg.annual.cites, decreasing = T)][1:20] ~ c(seq(1:20) + 0.25), labels = paste("(", journal.total.papers[order(journal.agg.annual.cites, decreasing = T)][1:20], ")", sep = ""), cex = .6)
axis(side = 1, at = c(1:108), labels = levels(data.frame$Source)[order(journal.agg.annual.cites, decreasing = T)]
     , las = 2, cex.axis = .5)

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
plot(journal.graph.small, layout = layout.fruchterman.reingold, vertex.size = log(table(journal.frame$journal1.vec) + 1) * 2, vertex.label = NA, edge.arrow.size = .05)

# journal communities
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
