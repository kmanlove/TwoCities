as.dendrogram.igraph.walktrap <- function (object, hang=-1, use.modularity=FALSE, ...){
  .memberDend <- function(x) {
    r <- attr(x,"x.member")
    if(is.null(r)) {
      r <- attr(x,"members")
      if(is.null(r)) r <- 1:1
    }
    r
  }
      stopifnot(nrow(object$merges)> 0)
      storage.mode(object$merges) <- "integer"
      object$merges <- object$merges + 1L
      if (is.null(object$labels))
          object$labels <- 1:(nrow(object$merges)+1)-1
      z <- list()
      if (!use.modularity || is.null(object$modularity)) {
          object$height <- 1:nrow(object$merges)
        } else {
            object$height <- object$modularity[-1]
            object$height <- cumsum(object$height - min(object$height))
          }
      nMerge <- length(oHgt <- object$height)
      if (nMerge != nrow(object$merges))
          stop("'merge' and 'height' do not fit!")
      hMax <- oHgt[nMerge]
      one <- 1:1;
      two <- 2:2 # integer!
      leafs <- nrow(object$merges)+1
      for (k in 1:nMerge) {
          x <- object$merges[k, ]# no sort() anymore!
          if (any(neg <- x < leafs+1))
              h0 <- if (hang < 0) 0 else max(0, oHgt[k] - hang * hMax)
          if (all(neg)) {                  # two leaves
              zk <- as.list(x)
              attr(zk, "members") <- two
              attr(zk, "midpoint") <- 0.5 # mean( c(0,1) )
              objlabels <- object$labels[x]
              attr(zk[[1]], "label") <- objlabels[1]
              attr(zk[[2]], "label") <- objlabels[2]
              attr(zk[[1]], "members") <- attr(zk[[2]], "members") <- one
              attr(zk[[1]], "height") <- attr(zk[[2]], "height") <- h0
              attr(zk[[1]], "leaf") <- attr(zk[[2]], "leaf") <- TRUE
            }
          else if (any(neg)) {            # one leaf, one node
              X <- as.character(x)
              ## Originally had "x <- sort(..) above => leaf always left, x[1];
                ## don't want to assume this
                isL <- x[1] < leafs+1 ## is leaf left?
              zk <- if(isL) list(x[1], z[[X[2]]])
                else    list(z[[X[1]]], x[2])
              attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + one
              attr(zk, "midpoint") <- (igraph:::.memberDend(zk[[1]]) + attr(z[[X[1 + isL]]], "midpoint"))/2
              attr(zk[[2 - isL]], "members") <- one
              attr(zk[[2 - isL]], "height") <- h0
              attr(zk[[2 - isL]], "label") <- object$labels[x[2 - isL]]
              attr(zk[[2 - isL]], "leaf") <- TRUE
              }
          else {                        # two nodes
              x <- as.character(x)
              zk <- list(z[[x[1]]], z[[x[2]]])
              attr(zk, "members") <- attr(z[[x[1]]], "members") + attr(z[[x[2]]], "members")
              attr(zk, "midpoint") <- (attr(z[[x[1]]], "members") + attr(z[[x[1]]], "midpoint") + attr(z[[x[2]]], "midpoint"))/2
          }
          attr(zk, "height") <- oHgt[k]
          z[[k <- as.character(k+leafs)]] <- zk
        }
      z <- z[[k]]
      class(z) <- "dendrogram"
      z
}

#-------------------------------------------#
#-- reader assignment function -------------#
#-------------------------------------------#
ReaderAssignmentFull <- function(number.readers, papers.per.reader, fixed.seed, number.communities, quantiles.to.use){
  comm.list <- vector("list", number.communities)
  comm.quantiles <- matrix(NA, nrow = number.communities, ncol = length(quantiled.to.use))
  for(i in 1:number.communities){
    comm[[i]] <- subset(data.frame, JournalCommunity == i)
    comm.quantiles[i, ] <- quantile(comm[[i]]$AnnualizedCitationRate, quantiles.to.use, na.rm = T)
  } 
}

ReaderAssignmentSpecificGrp <- function(data.frame, number.readers, papers.per.reader, fixed.seed, community.to.use, quantiles.to.use){
  sampling.pattern <- cbind(c(1, 5, 9), c(2, 6, 7), c(3, 4, 8))
  comm <- subset(data.frame, JournalCommunity == community.to.use)
  comm.quantiles <- quantile(comm$AnnualizedCitationRate, quantiles.to.use, na.rm = T)
  # sample uniformly over quantiles -- take to 25, middle 25, bottom 25 papers in each...
  sampled.papers <- comm[c(
         sample(x = which(comm$PubYear <= 2002 & comm$AnnualizedCitationRate <= comm.quantiles[1]), 
                min(number.readers * 2, length(which(comm$PubYear <= 2002 & comm$AnnualizedCitationRate <= comm.quantiles[1]))), 
                rep = F), 
         sample(x = which(comm$PubYear <= 2002 & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]), 
                min(number.readers * 2, length(which(comm$PubYear <= 2002 & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]))), 
                rep = F),
         sample(x = which(comm$PubYear <= 2002  & comm$AnnualizedCitationRate >= comm.quantiles[4]), 
                min(number.readers * 2, length(which(comm$PubYear <= 2002 & comm$AnnualizedCitationRate >= comm.quantiles[4]))), 
                rep = F),
         sample(x = which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate <= comm.quantiles[1]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate <= comm.quantiles[1]))), 
                rep = F), 
         sample(x = which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]))), 
                rep = F),
         sample(x = which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate >= comm.quantiles[4]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2003, 2007) & comm$AnnualizedCitationRate >= comm.quantiles[4]))), 
                rep = F),
         sample(x = which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate <= comm.quantiles[1]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate <= comm.quantiles[1]))), 
                rep = F), 
         sample(x = which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate >= comm.quantiles[2] & comm$AnnualizedCitationRate <= comm.quantiles[3]))), 
                rep = F),
         sample(x = which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate >= comm.quantiles[4]), 
                min(number.readers * 2, length(which(comm$PubYear %in% seq(2008, 2012) & comm$AnnualizedCitationRate >= comm.quantiles[4]))), 
                rep = F)
         ), ]
 reader.paper.list <- reader.paper.list.reduced <- vector("list", number.readers)
 for(i in 1:number.readers){
   reader.paper.list[[i]] <- sampled.papers[which(1:dim(sampled.papers)[1] %% number.readers == (i - 1)),]
#   reader.paper.list.reduced[[i]] <- reader.paper.list[[i]][sampling.pattern[, ((i %% number.readers) %% 3) + 1], ]
 } 
 return(reader.paper.list)
}

#--------------------------------------------------------------------------#
#-- Extract author information and build author info storage structures ---#
#--------------------------------------------------------------------------#

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # trim cuts leading whitespace off of any character string


# extract author institution information from data.frame$C1
BuildAuthorFrame <- function(data.frame.in)
{
  institution.list <- institution.list2 <- author.institutions <- vector("list", length = dim(data.frame.in)[1])
  author.institutions.expanded <- author.list <- author.lastonly <- vector("list", length = dim(data.frame.in)[1])
  author.list.out <- First <- Last <- author.frame <- data.list.au <- vector("list", length = dim(data.frame.in)[1])

  # loop to build author data list
  for(i in 2:dim(data.frame.in)[1])
  { # loop starts at 2 because paper 1 is authored by Anonymous and there's no info in the author fields
    # extract authors from data.frame$Author field
    author.list[[i]] <- strsplit(x = as.character(data.frame.in[i, ]$Authors), split = ";")[[1]]
    for(j in 1:length(author.list[[i]]))
    {
      author.list[[i]][j] <- trim(author.list[[i]][j]) # trim cuts leading whitespace off of any character string
      # pulls of first name/initials from author.list elements
      author.lastonly[[i]][j] <- strsplit(x = (author.list[[i]][j]), split = ",", fixed = T)[[1]][1]     } # j
      # extract institutions for each author from C1 and merge with authorlist
      # splits apart all institutions listed in data.frame$C1
      institution.list[[i]] <- strsplit(x = as.character(data.frame.in[i, ]$C1), split = "; [", fixed = T)[[1]]     
    if(length(institution.list[[i]]) != 0)
    { # if there are institutions listed
      # build a list that is length-institutions long.
      author.institutions.expanded[[i]] <- vector("list", length(institution.list[[i]]))
      if(length(institution.list[[i]]) == 1)
      { # if everyone's in the same institution
        author.list.out[[i]] <- cbind(author.list[[i]], 
                                      rep(institution.list[[i]][1], length(author.list[[i]])), 
                                      rep(data.frame$Paper.Number[i], length(author.list[[i]])), 
                                      rep(as.character(data.frame$Title[i]), length(author.list[[i]]))
                                      )
        # cbind author names, institution (there's only one) rep'd number-of-authors times.
      } # if
      else 
        { # if there are multiple institutions
        for(k in 1:length(institution.list[[i]]))
        { # loop over the different institutions
          author.institutions[[i]][k] <- strsplit(x = as.character(institution.list[[i]][k]), split = "]", fixed = T)[[1]][1]
          # extract the authors listed before the institutions (authors listed in [] and sep'd by ";")
          if(k == 1)
          { # for first element in list, character string is different ([authors] inst;). clean up first element of author.institutions[[i]] 
            author.institutions[[i]][k] <- strsplit(x = as.character(author.institutions[[i]][k]), split = "[", fixed = T)[[1]][2]
          }
          institution.list2[[i]][k] <- strsplit(x = as.character(institution.list[[i]][k]), split = "]", fixed = T)[[1]][2]
          author.institutions.expanded[[i]][[k]] <- cbind((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]), 
                                                          rep(institution.list2[[i]][k], length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]))),
                                                          rep(data.frame$Paper.Number[i], length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]]))), 
                                                          rep(as.character(data.frame$Title[i]), length((strsplit(x = author.institutions[[i]][k], split = ";", fixed = T)[[1]])))
                                                          )
        } # k
        # merge authors in author.list and institutions in institution.list2
        author.list.out[[i]] <- do.call("rbind", author.institutions.expanded[[i]])
      } # END else
    } # END if there are institutions listed
    else
    {
      author.list.out[[i]] <- cbind(author.list[[i]], 
                                  rep(NA, length(author.list[[i]])), 
                                  rep(data.frame$Paper.Number[i], length(author.list[[i]])), 
                                  rep(as.character(data.frame$Title[i]), length(author.list[[i]]))
                                  )
    } # END else
  } # i
  
  # unlist author.list.out and store as one large dataframe
  author.full.frame <- as.data.frame(do.call("rbind", author.list.out))   
  names(author.full.frame) <- c("FullName", "FullAffil", "Paper.Number", "PaperTitle")

  return(author.full.frame)
}

# function to extract author affiliations
# parse text in FullAffil field to build unique author ID and get indicators for presence of different keywords in author affiliation
GetAuthorAffils <- function(author.frame)
{
  LastName <- FirstName <- AuthorID <- AllInits <- OneInit <- Dept <- Sch <- Univ <- rep(NA, dim(author.frame)[1])
  Math <- Ecol <- Epi <- Evol <- Stat <- Vet <- Ctr <- Biol <- Med <- rep(NA, dim(author.frame)[1])
  for(i in 1:dim(author.full.frame)[1])
  {
    # split names into first and last vectors: strip off last name; make all characters lower-case
    LastName[i] <- tolower(strsplit(x = as.character((author.full.frame[i, 1])), split = ",", fixed = T)[[1]][1]) 
    # strip off first name; remove leading whitespace with "trim"
    FirstName[i] <- trim(strsplit(x = as.character((author.full.frame[i, 1])), split = ",", fixed = T)[[1]][2]) 
    # refine FirstName so that it's only initials
    # remove periods
    FirstName[i] <- gsub("[.]", "", FirstName[i])[[1]] 
    # remove lower-case letters
    AllInits[i] <- gsub("[a-z]", "", FirstName[i])[[1]] 
    # remove spaces; change initials to lower-case
    AllInits[i] <- tolower(gsub(" ", "", AllInits[i])[[1]]) 
    # remove all characters past first one
    OneInit[i] <- substring(AllInits[i], 1, 1) 
    AuthorID[i] <- trim(paste(LastName[i], " ", OneInit[i], sep = ""))
    # create indicators for the strings "Univ", "Sch Med" or "Med Sch", "Ctr", "Math", "Stat", "Ecol", "Evol", "Epi", "Vet", "Biol" appearing in FullAffil
    Univ[i] <- ifelse(length(grep("Univ", author.frame$FullAffil[i])) >= 1, 1, 0)
    Math[i] <- ifelse(length(grep("Math", author.frame$FullAffil[i])) >= 1, 1, 0)
    Stat[i] <- ifelse(length(grep("Stat", author.frame$FullAffil[i])) >= 1, 1, 0)
    Ecol[i] <- ifelse(length(grep("Ecol", author.frame$FullAffil[i])) >= 1, 1, 0)
    Evol[i] <- ifelse(length(grep("Evol", author.frame$FullAffil[i])) >= 1, 1, 0)
    Biol[i] <- ifelse(length(grep("Bio", author.frame$FullAffil[i])) >= 1, 1, 0)
    Epi[i] <- ifelse(length(grep("Epi", author.frame$FullAffil[i])) >= 1, 1, 0)
    Ctr[i] <- ifelse(length(grep("Ctr", author.frame$FullAffil[i])) >= 1, 1, 0)
    Vet[i] <- ifelse(length(grep("Vet", author.frame$FullAffil[i])) >= 1, 1, 0)
    Med[i] <- ifelse(((length(grep("Med Sch", author.frame$FullAffil[i])) >= 1) | (length(grep("Sch Med", author.frame$FullAffil[i])) >= 1)), 1, 0)
    #  print(i)
  }
  author.affil.frame <- cbind(author.frame, LastName, FirstName, AllInits, OneInit, AuthorID, Univ, Math, Stat, Ecol, Evol, Biol, Epi, Ctr, Vet, Med)
  names(author.affil.frame) <- c(names(author.frame), "LastName", "FirstName", "AllInits", 
                                       "OneInit", "AuthorID", "Univ", "Math", "Stat", 
                                       "Ecol", "Evol", "Biol", "Epi", "Ctr", "Vet", "Med"
                                 )
  return(author.affil.frame)
}

# function to merge multiple records for same author
AuthorMerge <- function(author.frame)
{
  unique.authors <- length(levels(factor(author.frame$AuthorID))) #  unique authors
  author.papers <- vector("list", unique.authors)
  author.unique.frame <- as.data.frame(matrix(NA, nrow = unique.authors, ncol = 11))
  names(author.unique.frame) <- c("AuthorID", "TotPapers", "TotUniv", "TotMath", 
                                  "TotStat", "TotEcol", "TotEvol", "TotEpi", "TotMed", 
                                  "TotVet", "TotBiol")
  
  for(i in 1:unique.authors)
  { # build dataframe with one row per author, and indicators author math, stat, ecol, evol, etc. affiliations over ALL affiliations for that author
    # extract all references for a given AuthorID
    author.papers[[i]] <- subset(author.frame, AuthorID == levels(factor(author.frame$AuthorID))[i])
    author.unique.frame$AuthorID[i] <- trim(levels(factor(author.frame$AuthorID))[i])
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
  } #i
  
  return(author.unique.frame)
}

# build author matrix for coauthorship network
BuildAuthorGraph <- function(all.authors, unique.authors)
{
  authors <- trim(levels(factor(unique.authors$AuthorID)))
  length(authors) # 4309 authors
  author.mat <- matrix(0, nrow = length(authors), ncol = length(authors))
  for(i in 1:length(authors)){
    author.refs <- subset(all.authors, AuthorID == authors[i])
    paper.numbers <- as.numeric(as.character(unique(author.refs$Paper.Number)))
    coauthors <- unique(as.data.frame((subset(all.authors, 
                                              Paper.Number %in% paper.numbers, 
                                              select = c(AuthorID, Paper.Number))))
                        )
    author.edgeweights <- as.vector(table(coauthors$AuthorID))
    addvals <- which(authors %in% names(table(coauthors$AuthorID)))
    author.mat[i, addvals] <- author.edgeweights
    print(i)
  } # i
  
  diag(author.mat) <- rep(0, dim(author.mat)[1])
  author.vertex.size <- apply(author.mat, 1, sum) 
  
  # description of author graph
  author.graph <- graph.adjacency(author.mat, mode = "undirected")
  return(author.graph)
}


all.authors <- BuildAuthorFrame(data.frame.in = data.frame)
affils.test <- GetAuthorAffils(author.frame = all.authors)
unique.authors <- AuthorMerge(author.frame = affils.test)
author.graph.test <- BuildAuthorGraph(all.authors = affils.test, unique.authors)

#-------------------------------#
#-- Build network functions ----#
#-------------------------------#
BuildAssocMat <- function(cite.list)
{
  assoc.mat <- matrix(NA, nrow = length(cite.list), ncol = length(cite.list))
  for(i in 1:length(cite.list))
  {
    if(is.null(cite.list[[i]]) == T)
    {
      assoc.mat
    } # END if is.null (cite.list[[i]])
    for(j in 1:length(cite.list))
    {
      if(i == j)
      {
        assoc.mat[i, j] <- NA
      } # END if i == j
      else
        {
        assoc.mat[i, j] <- ifelse(((cite.list[[i]][1, 1] %in% cite.list[[j]][ , 9]) 
                                  | (paste(cite.list[[i]][1 ,2], " ", cite.list[[i]][1, 3]) %in% paste(cite.list[[j]][ , 4], " ", cite.list[[j]][, 5]))), 
                                  1, 
                                  0
                                  )
      } # END else (i neq j)
    } # j
    print(i)
  } # i
  return(assoc.mat)
}

citation.list.in <- citation.list[papers.with.cites]
assoc.test <- BuildAssocMat(citation.list.in)

BuildJournalGraph <- function(data.frame.in)
{
  journal1 <- journal2 <- matrix(NA, nrow = dim(data.frame.in)[1], ncol = dim(data.frame.in)[1])
  for(i in dim(data.frame.in)[1])
  {
    for(j in dim(data.frame.in)[1])
    {
      if(is.na(assoc.mat[i, j]) == F & (assoc.mat[i, j] == 1) == T)
      {
        journal1[i, j] <- as.character(data.frame[i, ]$Source)
        journal2[i, j] <- as.character(data.frame[j, ]$Source)
      } else
        {
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
  
  return()
}