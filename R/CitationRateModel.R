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
  au.div.effects.lbs <-c(exp(poiscoef2.table[2, 1]),
                     exp(poiscoef2.table[2, 1] + poiscoef2.table[7, 1]),
                     exp(poiscoef2.table[2, 1] + poiscoef2.table[8, 1])    )
  au.div.effects.ubs <-c(exp(poiscoef2.table[2, 1]),
                         exp(poiscoef2.table[2, 1] + poiscoef2.table[7, 1]),
                         exp(poiscoef2.table[2, 1] + poiscoef2.table[8, 1])    )
  cite.div.effects <-c(exp(poiscoef2.table[5, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[9, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[10, 1]))
  cite.div.effects.lbs <-c(exp(poiscoef2.table[5, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[9, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[10, 1]))
  cite.div.effects.ubs <-c(exp(poiscoef2.table[5, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[9, 1]),
                       exp(poiscoef2.table[5, 1] + poiscoef2.table[10, 1]))
  
  # effect sizes plot
  par(mfrow = c(1, 1), mar = c(4, 8, 1, 1))
#   plot(x = 0, y = 0, 
#        ylim = c(0, 7), 
#        xlim = c(-1, 1), 
#        cex = 0, 
#        xaxt = "n",
#        yaxt = "n", 
#        ylab = "",
#        xlab = "Multiplicative change in annual citations \n (relative to ecology in 1990)")
#   abline(v = 0, lty = 2, col = "grey40")
#   for(i in 1:6){
#     segments(y0 = i, y1 = i, x0 = coef.table.reorder[i, 1] - 1.96 * coef.table.reorder[i, 2], 
#              x1 = coef.table.reorder[i, 1] + 1.96 * coef.table.reorder[i, 2], lwd = 2)
#     segments(y0 = i + 0.1, y1 = i - 0.1, x0 = coef.table.reorder[i, 1], 
#              x1 = coef.table.reorder[i, 1], lwd = 2)
#   }
#   axis(side = 2, at = c(1:6), labels = c("Diversity in Vet", "Diversity in Human Epi", 
#                                          "Veterinary", "Human Epi","Author Diversity", 
#                                          "Publication Year" 
#   ))
#   axis(side = 1, at = c(-1, -.5, 0, .5, 1), labels = c(-1, -.5, 0, .5, 1), las = 1)

seg.cols <- c("blue", "red", "blue", "red", "blue", "red", "gold", "gold", "black")
plot(x = .0001, y = 0, 
     ylim = c(0, 10), 
     xlim = c(.2, 5), log = "x",
     cex = 0, 
     xaxs = "i", yaxs = "i",
     xaxt = "n",
     yaxt = "n", 
     ylab = "",
     xlab = "Multiplicative change in citations \n (relative to Ecology journal community papers in 1990)", cex.lab = .8)
abline(v = 1, lty = 2, col = "grey40")
for(i in 1:9){
  segments(y0 = i, y1 = i, x0 = exp(poiscoef2.table.reorder[i, 1] - 1.96 * poiscoef2.table.reorder[i, 2]), 
           x1 = exp(poiscoef2.table.reorder[i, 1] + 1.96 * poiscoef2.table.reorder[i, 2]), lwd = 2, col = seg.cols[i])
  segments(y0 = i + 0.1, y1 = i - 0.1, x0 = exp(poiscoef2.table.reorder[i, 1]), 
           x1 = exp(poiscoef2.table.reorder[i, 1]), lwd = 2, col = seg.cols[i])
}
axis(side = 2, at = c(1:9), labels = c("Cite diversity -- vet", "Cite diversity -- epi", "Au diversity -- vet", "Au diversity -- epi", 
                                       "Veterinary", "Human epi","Citation diversity", "Author diversity", 
                                       "Publication year" 
), las = 1)
axis(side = 1, at = c(.2, .45, 1, 2.25, 5), labels = c(.2, .45, 1, 2.25, 5), las = 1)

return(list(Citation.fit.jore = Citation.fit.jore,
              coef.table = coef.table)
         )
}