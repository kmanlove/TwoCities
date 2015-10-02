AuthorDiversityPlots <- function(data.frame){
  data.frame$BlockedYears <- cut(data.frame$PubYear, breaks = seq(1990, 2014, length.out = 13))
  comm1 <- subset(data.frame, Journal.Community == 1)
  comm2 <- subset(data.frame, Journal.Community == 2)
  comm3 <- subset(data.frame, Journal.Community == 3)
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
  # author diversity model
  lead.shannon <- c(shannon.comm1, shannon.comm2, shannon.comm3)
  lead.year <- rep(1980:2014, times = 3)
  lead.community <- rep(1:3, each = 35)
  
  require(mgcv)
  LeadDiv.fit <- lm(lead.shannon ~ lead.year * factor(lead.community))
  LeadDiv.gam <- gam(lead.shannon ~ s(lead.year) + 
                       s(lead.year, by = factor(lead.community)) +
                       factor(lead.community))
  LeadDivCommonTrajectory.gam <- gam(lead.shannon ~ s(lead.year) + 
                       factor(lead.community))
  AIC(LeadDiv.fit)
  AIC(LeadDiv.gam)
  AIC(LeadDivCommonTrajectory.gam)
  summary(LeadDiv.fit)
  
  lead.yr <- 1980:2014
  
  comm1.leadgam <- gam(shannon.comm1 ~ s(lead.yr))
  comm2.leadgam <- gam(shannon.comm2 ~ s(lead.yr))
  comm3.leadgam <- gam(shannon.comm3 ~ s(lead.yr))
  leadpreds.comm1 <- predict(comm1.leadgam, type = "response", se.fit = TRUE)
  leadfit.comm1 <- leadpreds.comm1$fit 
  leadfit.up95.comm1 <-leadfit.comm1 - 1.96*leadpreds.comm1$se.fit 
  leadfit.low95.comm1 <-leadfit.comm1 + 1.96*leadpreds.comm1$se.fit
  
  leadpreds.comm2 <- predict(comm2.leadgam, type="response", se.fit = TRUE)
  leadfit.comm2 <- leadpreds.comm2$fit
  leadfit.up95.comm2 <- leadfit.comm2 - 1.96*leadpreds.comm2$se.fit
  leadfit.low95.comm2 <- leadfit.comm2 + 1.96*leadpreds.comm2$se.fit
  
  leadpreds.comm3 <- predict(comm3.leadgam, type="response", se.fit = TRUE)
  leadfit.comm3 <- leadpreds.comm3$fit
  leadfit.up95.comm3 <-leadfit.comm3 - 1.96*leadpreds.comm3$se.fit
  leadfit.low95.comm3 <-leadfit.comm3 + 1.96*leadpreds.comm3$se.fit
# plot(x = 0, y = 0, xlab = "", 
#      ylab = "Diversity of lead author affiliation", 
#      ylim = c(0, 3.5), pch = 16, col = "gold", xlim = c(1994, 2015))


  
  data.frame.no4 <- subset(data.frame, Journal.Community != 4)
  data.frame.no4$plotcol <- ifelse(data.frame.no4$Journal.Community == 1, "gold", 
                                   ifelse(data.frame.no4$Journal.Community == 2, "red", 
                                          "blue"))
  data.frame.no4$plotpch <- ifelse(data.frame.no4$Journal.Community == 1, 16, 
                                   ifelse(data.frame.no4$Journal.Community == 2, 1, 
                                          2))
  
par(mfrow = c(1, 2), mar = c(2, 6, 1, 1), oma = c(1, 1, 1, 1), cex.axis = 1.2, cex.lab = 1.2)
plot(data.frame.no4$AuShannonDiv ~ data.frame.no4$PubYear, 
       col = data.frame.no4$plotcol,
       pch = data.frame.no4$plotpch,
       xlim = c(1990, 2015), cex = 1, 
       ylab = "Author diversity within papers",
       xaxt = "n", xlab = "")
  lines(lowess(comm1$AuShannonDiv ~ comm1$PubYear), col = "gold", lwd = 3)
  lines(lowess(comm2$AuShannonDiv ~ comm2$PubYear), col = "red", lwd = 2)
  lines(lowess(comm3$AuShannonDiv ~ comm3$PubYear), col = "blue", lwd = 2)
  mtext("(a)", side = 3, line = 0.5, at = 1990)
  leg.text <- c("Ecology", "Human epi", "Veterinary")
  legend("topleft", leg.text, col = c("gold", "red", "blue"),
         lty = c(1, 1, 1),
         pch = c(16, 1, 2), 
         cex = c(1, 1, 1),
         lwd = c(2, 2, 2),
         bty = "n")
  axis(side = 1, at = c(1990, 1995, 2000, 2005, 2010, 2015), 
       labels = c("1990", "1995", "2000", "2005", "2010", "2015"), las = 1, cex.axis = 1)
  
  # plot proportion of papers in each community with NO diversity thru time
#   comm1.divtab <- as.vector(table(comm1[which(comm1$AuShannonDiv == 0),]$PubYear)) / 
#     as.vector(table(comm1$PubYear))
#   
#   comm2.divtab <- as.vector(table(comm2[which(comm2$AuShannonDiv == 0),]$PubYear)) / 
#     as.vector(table(comm2$PubYear))
#   
#   comm3.divtab <- as.vector(table(comm3[which(comm3$AuShannonDiv == 0),]$PubYear)) / 
#     as.vector(table(comm3$PubYear))
  
  comm1.divtab <- as.vector(table(comm1[which(comm1$AuShannonDiv == 0),]$BlockedYears)) / 
    as.vector(table(comm1$BlockedYears))
  
  comm2.divtab <- as.vector(table(comm2[which(comm2$AuShannonDiv == 0),]$BlockedYears)) / 
    as.vector(table(comm2$BlockedYears))
  
  comm3.divtab <- as.vector(table(comm3[which(comm3$AuShannonDiv == 0),]$BlockedYears)) / 
    as.vector(table(comm3$BlockedYears))
  
  
  plot((comm1.divtab ~ seq(1992, 2014, length.out = 12)), 
       type = "l", ylab = "P(Author diversity = 0)", xaxt = "n", 
       col = "gold", lwd = 2, ylim = c(0, 1), xlab = "", xlim = c(1990, 2015))
  axis(side = 1, at = seq(1990, 2015, length.out = 6), 
       labels = seq(1990, 2015, length.out = 6), 
       las = 1, cex.axis = 1)
#   lines(lowess(comm2.divtab ~ seq(1990:2014), f = 1/3), type = "l", col = "red",
#         lwd = 2)
#   lines(lowess(c(NA, comm3.divtab) ~ seq(1990:2014), f = 1/3), type = "l", 
#         col = "blue", lwd = 2)
  lines((comm2.divtab ~ seq(1992, 2014, length.out = 12)), type = "l", col = "red",
        lwd = 2)
  lines((c(comm3.divtab) ~ seq(1992, 2014, length.out = 12)), type = "l", 
        col = "blue", lwd = 2)
  points(comm1.divtab ~ seq(1992, 2014, length.out = 12), pch = 16, cex = 1, col = "gold")
  points(comm2.divtab ~ seq(1992, 2014, length.out = 12), pch = 1, cex = 1, col = "red")
  points(c(comm3.divtab) ~ seq(1992, 2014, length.out = 12), pch = 2, cex = 1, col = "blue")
leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
legend("bottomleft", leg.text, col = c("gold", "red", "blue"),
         lty = c(1, 1, 1),
         pch = c(16, 1, 2), 
         cex = c(1, 1, 1),
         lwd = c(2, 2, 2),
         bty = "n")
  mtext("(b)", side = 3, line = .5, at = 1990)  
  
#   # plot ratio of citations within to citations between by author diversity
#   plot(shannon.comm1[- (1:10)] ~ seq(1990, 2014), 
#        ylab = "Diversity of lead author affiliations", xlab = "Year",
#        ylim = c(0, 3), pch = 16, col = "gold", type = "p", lwd = 2, 
#        xlim = c(1990, 2015))
# polygon(c(lead.yr, rev(lead.yr)), 
#         c(leadfit.low95.comm1, rev(leadfit.up95.comm1)),
#         border=NA, col = rgb(255/255,215/255,0, alpha = .3))
# polygon(c(lead.yr, rev(lead.yr)), 
#         c(leadfit.low95.comm2, rev(leadfit.up95.comm2)), col = rgb(1,0,0, alpha = .3),
#         border=NA)
# polygon(c(lead.yr, rev(lead.yr)), 
#         c(leadfit.low95.comm3, rev(leadfit.up95.comm3)), col = rgb(0,0,1, alpha = .3),
#         border=NA)
# 
#   points(shannon.comm1[-c(1:10)] ~ seq(1990, 2014), col = "gold", pch = 16, lwd = 2)
# #  lines(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", lwd = 2)
# #  lines(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", lwd = 2)
#   points(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", pch = 1)
#   points(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", pch = 2)
#   lines(leadfit.comm1[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "gold")
#   lines(leadfit.comm2[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "red")
#   lines(leadfit.comm3[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "blue")
#   leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
#   legend("topleft", leg.text, col = c("gold", "red", "blue"), bty = "n", 
#          pch = c(16, 1, 2), 
#          lty = c(1, 1, 1),
#          lwd = c(2, 2, 2))
# #   plot((comm1$epi.jos / comm1$tot.refs) ~ comm1$AuShannonDiv, col = "gold",
# #        xlab = "Author diversity", ylab = "References within community : Total references",
# #        pch = 16, 
# #        cex = .7,
# #        ylim = c(0, 1.1))
# #   lines(lowess((comm1$epi.jos / comm1$tot.refs) ~ comm1$AuShannonDiv), col = "gold", lwd = 2)
# #   points((comm2$epi.jos / comm2$tot.refs) ~ comm2$AuShannonDiv, col = "red",
# #          pch = 16, 
# #          cex = .7)
# #   lines(lowess((comm2$epi.jos / comm2$tot.refs) ~ comm2$AuShannonDiv), col = "red", lwd = 2)
# #   points((comm3$epi.jos / comm3$tot.refs) ~ comm3$AuShannonDiv, col = "blue",
# #          pch = 16, 
# #          cex = .7)
# #   lines(lowess((comm3$epi.jos / comm3$tot.refs) ~ comm3$AuShannonDiv), col = "blue", lwd = 2)
# #   leg.text <- c("Ecology", "Human epi", "Veterinary")
# #   legend("top", leg.text, col = c("gold", "red", "blue"),
# #          lty = c(1, 1, 1),
# #          pch = c(1, 1, 1), 
# #          cex = c(.8, .8, .8),
# #          bty = "n", ncol = 3)
#   mtext("(c)",side = 3, line = .5, at = 0)
}