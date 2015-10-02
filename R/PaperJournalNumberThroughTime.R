PaperJournalNumberThroughTime <- function(data.frame){
  papers.per.year <- table(factor(data.frame$PubYear, levels = seq(1980, 2014)))
  
  journals.per.year <- rep(NA, 35)
  for(i in 1:35){
    k <- subset(data.frame, PubYear == 1979 + i)
    journals.per.year[i] <- length(levels(factor(k$Source)))
  }
  
  plot(as.numeric(papers.per.year) ~ seq(1980, 2014), xlab = "", 
       ylab = "Number included in search", pch = 16, 
       xlim = c(1990, 2015), 
       ylim = c(0, 250))
  points(journals.per.year ~ seq(1980, 2014), xlab = "", 
         ylab = "Journals with \n Included Papers", pch = 1)
  leg.text <- c("Papers", "Journals")
  legend("topleft", leg.text, pch = c(1, 16), bty = "n", cex = .8)
}
