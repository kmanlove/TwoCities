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