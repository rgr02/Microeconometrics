
# libraries
library(ggplot2)
library(mgcv)


source("multiplot.R")
source("controls.R")

# Adjustment for HH Size and Proportion
adj.gam1 <- gam(trimmed.final$fdsh ~ trimmed.final$lsize + proportions)
adj.gam2 <- gam(trimmed.final$mksh ~ trimmed.final$lsize + proportions)
adj.gam3 <- gam(trimmed.final$adsh ~ trimmed.final$lsize + proportions)
adj.gam4 <- gam(trimmed.final$chsh ~ trimmed.final$lsize + proportions)

# Adding adjusted variables to dataframe
trimmed.final$adj.fdsh <- adj.gam1$coefficients[[1]] + adj.gam1$residuals
trimmed.final$adj.mksh <- adj.gam2$coefficients[[1]] + adj.gam2$residuals
trimmed.final$adj.adsh <- adj.gam3$coefficients[[1]] + adj.gam3$residuals
trimmed.final$adj.chsh <- adj.gam4$coefficients[[1]] + adj.gam4$residuals

gam1 <- gam(trimmed.final$adj.fdsh ~ s(trimmed.final$lpcexp))
g1.upper <- gam1$fitted.values + (1.96 * sd(trimmed.final$adj.fdsh/sqrt(length(trimmed.final))))
g1.lower <- gam1$fitted.values - (1.96 * sd(trimmed.final$adj.fdsh/sqrt(length(trimmed.final))))

gam2 <- gam(trimmed.final$adj.mksh ~ s(trimmed.final$lpcexp))
g2.upper <- gam2$fitted.values + (1.96 * sd(trimmed.final$adj.mksh/sqrt(length(trimmed.final))))
g2.lower <- gam2$fitted.values - (1.96 * sd(trimmed.final$adj.mksh/sqrt(length(trimmed.final))))

gam3 <- gam(trimmed.final$adj.adsh ~ s(trimmed.final$lpcexp))
g3.upper <- gam3$fitted.values + (1.96 * sd(trimmed.final$adj.adsh/sqrt(length(trimmed.final))))
g3.lower <- gam3$fitted.values - (1.96 * sd(trimmed.final$adj.adsh/sqrt(length(trimmed.final))))

gam4 <- gam(trimmed.final$adj.chsh ~ s(trimmed.final$lpcexp))
g4.upper <- gam4$fitted.values + (1.96 * sd(trimmed.final$adj.chsh/sqrt(length(trimmed.final))))
g4.lower <- gam4$fitted.values - (1.96 * sd(trimmed.final$adj.chsh/sqrt(length(trimmed.final))))

gam.plot.fdsh <- ggplot(trimmed.final) +
  geom_line(aes(lpcexp, g1.lower)) +
  geom_line(aes(trimmed.final$lpcexp, gam1$fitted.values), col = "blue", size = 1.5, alpha = 1) +
  geom_line(aes(lpcexp, g1.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = g1.lower, ymax = g1.upper), alpha = 0.1, col = "grey") +
  xlim(4.0,7.2) +
  ylim(0.25, 0.6) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Food") +
  ggtitle("Adjusted Food Share - Nonparametric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.mksh <- ggplot(trimmed.final) +
  geom_line(aes(lpcexp, g2.lower)) +
  geom_line(aes(trimmed.final$lpcexp, gam2$fitted.values), col = "green", size = 1.5, alpha = 1) +
  geom_line(aes(lpcexp, g2.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = g2.lower, ymax = g2.upper), alpha = 0.1, col = "grey") +
  xlim(4.0,7.2) +
  ylim(0.04, 0.175) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Milk") +
  ggtitle("Adjusted Milk Share - Nonparametric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.adsh <- ggplot(data = trimmed.final) +
  geom_line(aes(lpcexp, g3.lower)) +
  geom_line(aes(trimmed.final$lpcexp, gam3$fitted.values), col = "red", size = 1.5, alpha = 1) +
  geom_line(aes(lpcexp, g3.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = g3.lower, ymax = g3.upper), alpha = 0.1, col = "grey") +
  xlim(4.0,7.2) +
  ylim(0.01, 0.045) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Adult Goods") +
  ggtitle("Adjusted Adult Good Share - Nonparametric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.chsh <- ggplot(data = trimmed.final) +
  geom_line(aes(lpcexp, g4.lower)) +
  geom_line(aes(trimmed.final$lpcexp, gam4$fitted.values), col = "orange", size = 1.5, alpha = 1) +
  geom_line(aes(lpcexp, g4.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = g4.lower, ymax = g4.upper), alpha = 0.1, col = "grey") +
  xlim(4,7.2) +
  ylim(-0.02, 0.01) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Child Goods") +
  ggtitle("Adjusted Child Good Share - Nonparametric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# export manually 
multiplot(gam.plot.fdsh, gam.plot.chsh, gam.plot.mksh, gam.plot.adsh, cols = 2)



