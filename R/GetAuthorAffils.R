GetAuthorAffils <- function(author.frame)
{
  LastName <- FirstName <- AuthorID <- AllInits <- OneInit <- Dept <- Sch <- Univ <- rep(NA, dim(author.frame)[1])
  Math <- Ecol <- Epi <- Evol <- Stat <- Vet <- Ctr <- Biol <- Med <- rep(NA, dim(author.frame)[1])
  for(i in 1:dim(author.frame)[1])
  {
    # split names into first and last vectors: strip off last name; make all characters lower-case
    LastName[i] <- tolower(strsplit(x = as.character((author.frame[i, 1])), split = ",", fixed = T)[[1]][1]) 
    # strip off first name; remove leading whitespace with "trim"
    FirstName[i] <- trim(strsplit(x = as.character((author.frame[i, 1])), split = ",", fixed = T)[[1]][2]) 
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