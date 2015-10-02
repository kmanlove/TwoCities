GAMPlot <- function(data.frame){
  # GAM fits
  
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
  
  
#  cites.gam <- gam(cites ~ s(year) + 
#                     s(year, by = factor(community)) +
#                     factor(community))
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
  plot(shannon.comm1[- (1:10)] ~ seq(1990, 2014), 
       ylab = "Lead author affiliation diversity", xlab = "Year",
       ylim = c(0, 3), pch = 16, col = "gold", type = "p", lwd = 2, 
       xlim = c(1995, 2015))
  polygon(c(lead.yr, rev(lead.yr)), 
          c(leadfit.low95.comm1, rev(leadfit.up95.comm1)),
          border=NA, col = rgb(255/255,215/255,0, alpha = .3))
  polygon(c(lead.yr, rev(lead.yr)), 
          c(leadfit.low95.comm2, rev(leadfit.up95.comm2)), col = rgb(1,0,0, alpha = .3),
          border=NA)
  polygon(c(lead.yr, rev(lead.yr)), 
          c(leadfit.low95.comm3, rev(leadfit.up95.comm3)), col = rgb(0,0,1, alpha = .3),
          border=NA)
  
  points(shannon.comm1[-c(1:10)] ~ seq(1990, 2014), col = "gold", pch = 16, lwd = 2)
  #  lines(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", lwd = 2)
  #  lines(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", lwd = 2)
  points(shannon.comm2[-c(1:10)] ~ seq(1990, 2014), col = "red", pch = 1)
  points(shannon.comm3[-c(1:10)] ~ seq(1990, 2014), col = "blue", pch = 2)
  lines(leadfit.comm1[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "gold")
  lines(leadfit.comm2[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "red")
  lines(leadfit.comm3[-c(1:10)] ~ seq(1990, 2014), lwd = 2, col = "blue")
  leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
  legend("topleft", leg.text, col = c("gold", "red", "blue"), bty = "n", 
         pch = c(16, 1, 2), 
         lty = c(1, 1, 1),
         lwd = c(2, 2, 2), cex = .8)
  mtext("(a)", line = 0.5, at = 1995)
  
#   plot(as.numeric(papers.per.year) ~ seq(1980, 2014), xlab = "", 
#        ylab = "Number included in search", pch = 16, 
#        xlim = c(1990, 2015), 
#        ylim = c(0, 250))
#   points(journals.per.year ~ seq(1980, 2014), xlab = "", 
#          ylab = "Journals with \n Included Papers", pch = 1)
#   leg.text <- c("Papers", "Journals")
#   legend("topleft", leg.text, pch = c(1, 16), bty = "n", cex = .8)
plot(x = 0, y = 0, xlab = "", 
     ylab = "Proportion of out-citations", 
     ylim = c(0, 1), pch = 16, col = "gold", xlim = c(1994, 2015), type = "p")

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
lines(fit.comm1 ~ years, col = "gold", lwd = 2)
lines(fit.comm2 ~ years[-1], col = "red", lwd = 2)
lines(fit.comm3 ~ years[-c(1:3)], col = "blue", lwd = 2)
leg.text <- c("Ecology journals", "Human epi journals", "Veterinary journals")
legend("topright", leg.text, col = c("gold", "red", "blue"), bty = "n", 
       pch = c(16, 1, 2), 
       lty = c(1, 1, 1),
       lwd = c(2, 2, 2), cex = .8)
mtext("(b)", line = 0.5, at = 1995)

}