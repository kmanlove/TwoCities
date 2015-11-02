CitationRateModel <- function(data.frame){
  data.frame$LnAnnualizedCites <- log(data.frame$AnnualizedCitationRate + 1)
  data.no2014 <- subset(data.frame, PubYear != 2014 & Journal.Community != 4)
  Citation.fit <- lm(LnAnnualizedCites ~ AuShannonDiv 
                     * factor(Journal.Community)
                     + PubYear, data = data.no2014)
  
  PoisCitation.fit <- glm(TimesCited ~ AuShannonDiv 
                          * factor(Journal.Community)
                          + PubYear, 
                          offset = (2014 - PubYear),
                          data = data.no2014,
                          family = "poisson")

  PoisCitation.fit2 <- glm(TimesCited ~ AuShannonDiv 
                          * factor(Journal.Community) + between.to.tot * factor(Journal.Community)
                          + PubYear, 
                          offset = (2014 - PubYear),
                          data = data.no2014,
                          family = "poisson")
  AuShannonDiv.new <- c(1, 1, 1, 0, 0, 0)
  Journal.Community.new <- c(1, 2, 3, 1, 2, 3)
  between.to.tot.new <- c(1, 1, 1, 1, 1, 1)
  PubYear.new <- rep(2014, times = 6)
  pred.data <- as.data.frame(cbind(AuShannonDiv = AuShannonDiv.new, 
                                   Journal.Community = Journal.Community.new,
                                   between.to.tot = between.to.tot.new, 
                                   PubYear = PubYear.new))
  predict(PoisCitation.fit2, newdata = pred.data, type = "response", se.fit = T)
  require(sandwich)
  cov.m1 <- vcovHC(PoisCitation.fit2, type="HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate= coef(PoisCitation.fit2), "Robust SE" = std.err,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(PoisCitation.fit2)/std.err), lower.tail=FALSE),
                 LL = coef(PoisCitation.fit2) - 1.96 * std.err,
                 UL = coef(PoisCitation.fit2) + 1.96 * std.err)
  
  
  summary(Citation.fit)
  
  # RE on journal
  data.no2014$Source <- factor(data.no2014$Source)
  Citation.fit.jore <- lmer(LnAnnualizedCites ~ AuShannonDiv 
                            * factor(Journal.Community)
                            + PubYear + (1 | Source), data = data.no2014)
  
  coef.table <- summary(Citation.fit.jore)$coef
  coef.table.reorder <- coef.table[rev(c(5, 2, 3, 4, 6, 7)), ]
  poiscoef.table <- summary(PoisCitation.fit)$coef
  poiscoef.table.reorder <- poiscoef.table[rev(c(5, 2, 3, 4, 6, 7)), ]

  poiscoef2.table <- summary(PoisCitation.fit2)$coef
  poiscoef2.table.reorder <- poiscoef2.table[rev(c(6, 1, 3, 4, 2, 5, 7, 8, 9, 10)), ]
  
  au.div.effects <-c(exp(poiscoef2.table[2, 1]),
                     exp(poiscoef2.table[2, 1] + poiscoef2.table[7, 1]),
                     exp(poiscoef2.table[2, 1] + poiscoef2.table[8, 1])    )
  au.div.effects.lbs <-c(exp(r.est[2, 4]),
                     exp(r.est[2, 4] + poiscoef2.table[7, 4]),
                     exp(r.est[2, 4] + poiscoef2.table[8, 4]))
  au.div.effects.ubs <-c(exp(r.est[2, 5]),
                         exp(r.est[2, 5] + r.est[7, 5]),
                         exp(r.est[2, 5] + r.est[8, 5])    )
  cite.div.effects <-c(exp(poiscoef2.table[5, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[9, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[10, 1]))
  cite.div.effects.lbs <- c(exp(r.est[5, 4]),
                       exp(r.est[5, 4] + r.est[9, 4]),
                       exp(r.est[5, 4] + r.est[10, 4]))
  cite.div.effects.ubs <- c(exp(r.est[5, 5]),
                            exp(r.est[5, 5] + r.est[9, 5]),
                            exp(r.est[5, 5] + r.est[10, 5]))
  
  vcov.fit2 <- vcov(PoisCitation.fit2)
  
  cite.div.effects <-c(1,
                     exp(poiscoef2.table[5, 1]),
                     exp(poiscoef2.table[3, 1]),
                     exp(poiscoef2.table[3, 1] + poiscoef2.table[5, 1] + poiscoef2.table[9, 1]),
                     exp(poiscoef2.table[4, 1]),
                     exp(poiscoef2.table[4, 1] + poiscoef2.table[5, 1] + poiscoef2.table[10, 1])    
                     )
  
  cite.div.effects.ubs <-c(1 ,
                       exp(poiscoef2.table[5, 1] + sqrt(vcov.fit2[5, 5]) * 1.96),
                       exp(poiscoef2.table[3, 1] + sqrt(vcov.fit2[3, 3]) * 1.96),
                       exp(poiscoef2.table[3, 1] + poiscoef2.table[5, 1] + poiscoef2.table[9, 1]
                           + sqrt(vcov.fit2[3, 3] + vcov.fit2[5, 5] + vcov.fit2[9, 9] 
                                  - 2 * (vcov.fit2[3, 5] + vcov.fit2[3, 9] + vcov.fit2[5, 9])) * 1.96),
                       exp(poiscoef2.table[4, 1] + sqrt(vcov.fit2[4, 4]) * 1.96),
                       exp(poiscoef2.table[4, 1] + poiscoef2.table[5, 1] + poiscoef2.table[10, 1]
                           + sqrt(vcov.fit2[4, 4] + vcov.fit2[5, 5] + vcov.fit2[10, 10] 
                                  - 2 * (vcov.fit2[4, 5] + vcov.fit2[4, 10] + vcov.fit2[5, 10])) * 1.96)    
  )

  cite.div.effects.lbs <-c(1 ,
                           exp(poiscoef2.table[5, 1] - sqrt(vcov.fit2[5, 5]) * 1.96),
                           exp(poiscoef2.table[3, 1] - sqrt(vcov.fit2[3, 3]) * 1.96),
                           exp(poiscoef2.table[3, 1] + poiscoef2.table[5, 1] + poiscoef2.table[9, 1]
                               - sqrt(vcov.fit2[3, 3] + vcov.fit2[5, 5] + vcov.fit2[9, 9] 
                                      - 2 * (vcov.fit2[3, 5] + vcov.fit2[3, 9] + vcov.fit2[5, 9])) * 1.96),
                           exp(poiscoef2.table[4, 1] - sqrt(vcov.fit2[4, 4]) * 1.96),
                           exp(poiscoef2.table[4, 1] + poiscoef2.table[5, 1] + poiscoef2.table[10, 1]
                               - sqrt(vcov.fit2[4, 4] + vcov.fit2[5, 5] + vcov.fit2[10, 10] 
                                      - 2 * (vcov.fit2[4, 5] + vcov.fit2[4, 10] + vcov.fit2[5, 10])) * 1.96)    
  )
  
  au.div.effects <-c(1,
                     exp(poiscoef2.table[2, 1]),
                     exp(poiscoef2.table[3, 1]),
                     exp(poiscoef2.table[3, 1] + poiscoef2.table[2, 1] + poiscoef2.table[7, 1]),
                     exp(poiscoef2.table[4, 1]),
                     exp(poiscoef2.table[4, 1] + poiscoef2.table[2, 1] + poiscoef2.table[8, 1])    
  )

  au.div.effects.ubs <-c(1,
                     exp(poiscoef2.table[2, 1] + sqrt(vcov.fit2[2, 2]) * 1.96),
                     exp(poiscoef2.table[3, 1] + sqrt(vcov.fit2[3, 3]) * 1.96),
                     exp(poiscoef2.table[3, 1] + poiscoef2.table[2, 1] + poiscoef2.table[7, 1]
                         + sqrt(vcov.fit2[3, 3] + vcov.fit2[2, 2] + vcov.fit2[7, 7] 
                                - 2 * (vcov.fit2[3, 2] + vcov.fit2[3, 7] + vcov.fit2[2, 7])) * 1.96),
                     exp(poiscoef2.table[4, 1] + sqrt(vcov.fit2[4, 4]) * 1.96),
                     exp(poiscoef2.table[4, 1] + poiscoef2.table[2, 1] + poiscoef2.table[8, 1]
                         + sqrt(vcov.fit2[4, 4] + vcov.fit2[2, 2] + vcov.fit2[8, 8] 
                                - 2 * (vcov.fit2[4, 2] + vcov.fit2[4, 8] + vcov.fit2[2, 8])) * 1.96)    
  )

  au.div.effects.lbs <-c(1,
                         exp(poiscoef2.table[2, 1] - sqrt(vcov.fit2[2, 2]) * 1.96),
                         exp(poiscoef2.table[3, 1] - sqrt(vcov.fit2[3, 3]) * 1.96),
                         exp(poiscoef2.table[3, 1] + poiscoef2.table[2, 1] + poiscoef2.table[7, 1]
                             - sqrt(vcov.fit2[3, 3] + vcov.fit2[2, 2] + vcov.fit2[7, 7] 
                                    - 2 * (vcov.fit2[3, 2] + vcov.fit2[3, 7] + vcov.fit2[2, 7])) * 1.96),
                         exp(poiscoef2.table[4, 1] - sqrt(vcov.fit2[4, 4]) * 1.96),
                         exp(poiscoef2.table[4, 1] + poiscoef2.table[2, 1] + poiscoef2.table[8, 1]
                             - sqrt(vcov.fit2[4, 4] + vcov.fit2[2, 2] + vcov.fit2[8, 8] 
                                    - 2 * (vcov.fit2[4, 2] + vcov.fit2[4, 8] + vcov.fit2[2, 8])) * 1.96)    
  )
  
  my.blue <- rgb(0/255, 128 / 255, 255/255)
  muted.red <- rgb(255/255, 102/255, 102/255, alpha = .5)
  
  # alternative plot a la Raina revised
#  poly.col <- c("gold", "gold", muted.red, "red", "lightblue", my.blue)
  poly.col <- c("white", "gold", "white", "red", "white", my.blue)
  border.col <- c("gold", "black", "red", "black", my.blue, "black")
# svg("./Figures/Figure6_20151008.svg", height = 4, width = 4)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(2, 6, 1, 1))
  plot(x = 0, y = 0, cex = 0, xlim = c(0, 13), ylim = c(0, 2), 
       xlab = "",
       ylab = "Multiplicative change in citation rate",
       xaxt = "n")
  for(i in 1:6){
    segments(x0 = i - .4, x1 = i - .4, y0 = 1, y1 = au.div.effects[i], lty = 1)
    segments(x0 = i + .4, x1 = i + .4, y0 = 1, y1 = au.div.effects[i], lty = 1)
    segments(x0 = i - .4, x1 = i + .4, y0 = au.div.effects[i], y1 = au.div.effects[i], lty = 1)
    segments(x0 = i - .4, x1 = i + .4, y0 = 1, y1 = 1, lty = 1)
    polygon(x = c(i - .4, i - .4, i + .4, i + .4), y = c(1, au.div.effects[i], au.div.effects[i], 1),
            col = poly.col[i], border = border.col[i], lwd = 2)
    segments(x0 = i, x1 = i, y0 = au.div.effects.lbs[i], y1 = au.div.effects.ubs[i], lwd = 2)
  }
  for(i in 7:12){
    segments(x0 = i - .4, x1 = i - .4, y0 = 1, y1 = cite.div.effects[i - 6], lty = 1)
    segments(x0 = i + .4, x1 = i + .4, y0 = 1, y1 = cite.div.effects[i - 6], lty = 1)
    segments(x0 = i - .4, x1 = i + .4, y0 = cite.div.effects[i - 6], y1 = cite.div.effects[i - 6], lty = 1)
    segments(x0 = i - .4, x1 = i + .4, y0 = 1, y1 = 1, lty = 1)
    polygon(x = c(i - .4, i - .4, i + .4, i + .4), y = c(1, cite.div.effects[i - 6], cite.div.effects[i - 6], 1),
            col = poly.col[i - 6], border = border.col[i - 6], lwd = 2)
    segments(x0 = i, x1 = i, y0 = cite.div.effects.lbs[i - 6], y1 = cite.div.effects.ubs[i - 6], lwd = 2)
  }
  abline(h = 1)
  abline(v = 6.5)
  legend("bottomleft", c("Ecology", "Human Epi", "Veterinary"), fill = c("gold", "red", my.blue), bty = "n")
  legend("bottomright", c("Low diversity", "High diversity"), fill = c("white","grey20"), bty = "n")
  text(x = 3, y = 2, "Author diversity")
  text(x = 10, y = 2, "Citation diversity")
#  dev.off()

#   # alternative a la Raina original
#   poly.col <- c("gold", "red", "blue")
#   par(mfrow = c(1, 1))
#   plot(x = 0, y = 0, cex = 0, xlim = c(0, 7), ylim = c(0, 2), 
#        xlab = "",
#        ylab = "Multiplicative change in citation rate",
#        xaxt = "n")
#   for(i in 1:3){
#     segments(x0 = i - .4, x1 = i - .4, y0 = 1, y1 = au.div.effects[i], lty = 1)
#     segments(x0 = i + .4, x1 = i + .4, y0 = 1, y1 = au.div.effects[i], lty = 1)
#     segments(x0 = i - .4, x1 = i + .4, y0 = au.div.effects[i], y1 = au.div.effects[i], lty = 1)
#     segments(x0 = i - .4, x1 = i + .4, y0 = 1, y1 = 1, lty = 1)
#     polygon(x = c(i - .4, i - .4, i + .4, i + .4), y = c(1, au.div.effects[i], au.div.effects[i], 1),
#             col = poly.col[i])
#     segments(x0 = i, x1 = i, y0 = au.div.effects.lbs[i], y1 = au.div.effects.ubs[i])
#   }
#   for(i in 4:6){
#   segments(x0 = i - .4, x1 = i - .4, y0 = 1, y1 = cite.div.effects[i - 3], lty = 1)
#   segments(x0 = i + .4, x1 = i + .4, y0 = 1, y1 = cite.div.effects[i - 3], lty = 1)
#   segments(x0 = i - .4, x1 = i + .4, y0 = cite.div.effects[i - 3], y1 = cite.div.effects[i - 3], lty = 1)
#   segments(x0 = i - .4, x1 = i + .4, y0 = 1, y1 = 1, lty = 1)
#   polygon(x = c(i - .4, i - .4, i + .4, i + .4), y = c(1, cite.div.effects[i - 3], cite.div.effects[i - 3], 1),
#           col = poly.col[i - 3])
#   segments(x0 = i, x1 = i, y0 = cite.div.effects.lbs[i - 3], y1 = cite.div.effects.ubs[i - 3])
#   
#   }
#   abline(h = 1)
#   legend("bottomleft", c("Ecology", "Human Epidemiology", "Veterinary"), fill = c("gold", "red", "blue"), bty = "n")
#   
#   # effect sizes plot
#   par(mfrow = c(1, 1), mar = c(4, 8, 1, 1))
# #   plot(x = 0, y = 0, 
# #        ylim = c(0, 7), 
# #        xlim = c(-1, 1), 
# #        cex = 0, 
# #        xaxt = "n",
# #        yaxt = "n", 
# #        ylab = "",
# #        xlab = "Multiplicative change in annual citations \n (relative to ecology in 1990)")
# #   abline(v = 0, lty = 2, col = "grey40")
# #   for(i in 1:6){
# #     segments(y0 = i, y1 = i, x0 = coef.table.reorder[i, 1] - 1.96 * coef.table.reorder[i, 2], 
# #              x1 = coef.table.reorder[i, 1] + 1.96 * coef.table.reorder[i, 2], lwd = 2)
# #     segments(y0 = i + 0.1, y1 = i - 0.1, x0 = coef.table.reorder[i, 1], 
# #              x1 = coef.table.reorder[i, 1], lwd = 2)
# #   }
# #   axis(side = 2, at = c(1:6), labels = c("Diversity in Vet", "Diversity in Human Epi", 
# #                                          "Veterinary", "Human Epi","Author Diversity", 
# #                                          "Publication Year" 
# #   ))
# #   axis(side = 1, at = c(-1, -.5, 0, .5, 1), labels = c(-1, -.5, 0, .5, 1), las = 1)
# 
# seg.cols <- c("blue", "red", "blue", "red", "blue", "red", "gold", "gold", "black")
# plot(x = .0001, y = 0, 
#      ylim = c(0, 10), 
#      xlim = c(.2, 5), log = "x",
#      cex = 0, 
#      xaxs = "i", yaxs = "i",
#      xaxt = "n",
#      yaxt = "n", 
#      ylab = "",
#      xlab = "Multiplicative change in citations \n (relative to Ecology journal community papers in 1990)", cex.lab = .8)
# abline(v = 1, lty = 2, col = "grey40")
# for(i in 1:9){
#   segments(y0 = i, y1 = i, x0 = exp(poiscoef2.table.reorder[i, 1] - 1.96 * poiscoef2.table.reorder[i, 2]), 
#            x1 = exp(poiscoef2.table.reorder[i, 1] + 1.96 * poiscoef2.table.reorder[i, 2]), lwd = 2, col = seg.cols[i])
#   segments(y0 = i + 0.1, y1 = i - 0.1, x0 = exp(poiscoef2.table.reorder[i, 1]), 
#            x1 = exp(poiscoef2.table.reorder[i, 1]), lwd = 2, col = seg.cols[i])
# }
# axis(side = 2, at = c(1:9), labels = c("Cite diversity -- vet", "Cite diversity -- epi", "Au diversity -- vet", "Au diversity -- epi", 
#                                        "Veterinary", "Human epi","Citation diversity", "Author diversity", 
#                                        "Publication year" 
# ), las = 1)
# axis(side = 1, at = c(.2, .45, 1, 2.25, 5), labels = c(.2, .45, 1, 2.25, 5), las = 1)

return(list(Citation.fit.jore = Citation.fit.jore,
              coef.table = coef.table)
         )
}