
# libraries
library(ggplot2)
library(mgcv)
library(KernSmooth)

source("multiplot.R")
source("controls.R")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Adjusting *share for HH Size and Composition
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# Adjustment for HH Size and Proportion
adj.lm1 <- lm(trimmed.final$fdsh ~ trimmed.final$lsize + proportions)
adj.lm2 <- lm(trimmed.final$mksh ~ trimmed.final$lsize + proportions)
adj.lm3 <- lm(trimmed.final$adsh ~ trimmed.final$lsize + proportions)
adj.lm4 <- lm(trimmed.final$chsh ~ trimmed.final$lsize + proportions)

# Adding adjusted variables to dataframe
trimmed.final$adj.fdsh <- adj.lm1$coefficients[[1]] + adj.lm1$residuals
trimmed.final$adj.mksh <- adj.lm2$coefficients[[1]] + adj.lm2$residuals
trimmed.final$adj.adsh <- adj.lm3$coefficients[[1]] + adj.lm3$residuals
trimmed.final$adj.chsh <- adj.lm4$coefficients[[1]] + adj.lm4$residuals

# Engel Curves and CI
t1 <- lm(trimmed.final$adj.fdsh ~ expenditure)
t1.upper <- t1$fitted.values + (1.96 * sd(trimmed.final$adj.fdsh/sqrt(length(trimmed.final))))
t1.lower <- t1$fitted.values - (1.96 * sd(trimmed.final$adj.fdsh/sqrt(length(trimmed.final))))

t2 <- lm(trimmed.final$adj.mksh ~ expenditure)
t2.upper <- t2$fitted.values + (1.96 * sd(trimmed.final$adj.mksh/sqrt(length(trimmed.final))))
t2.lower <- t2$fitted.values - (1.96 * sd(trimmed.final$adj.mksh/sqrt(length(trimmed.final))))

t3 <- lm(trimmed.final$adj.adsh ~ expenditure)
t3.upper <- t3$fitted.values + (1.96 * sd(trimmed.final$adj.adsh/sqrt(length(trimmed.final))))
t3.lower <- t3$fitted.values - (1.96 * sd(trimmed.final$adj.adsh/sqrt(length(trimmed.final))))

t4 <- lm(trimmed.final$adj.chsh ~ expenditure)
t4.upper <- t4$fitted.values + (1.96 * sd(trimmed.final$adj.chsh/sqrt(length(trimmed.final))))
t4.lower <- t4$fitted.values - (1.96 * sd(trimmed.final$adj.chsh/sqrt(length(trimmed.final))))


lm.plot.fdsh <- ggplot(trimmed.final) +
  xlim(4.0,7.2) +
  ylim(0.25, 0.55) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Food") +
  geom_line(aes(lpcexp, t1.lower)) +
  geom_line(aes(lpcexp, t1$fitted.values), col = "blue", size = 1.5) +
  geom_line(aes(lpcexp, t1.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = t1.lower, ymax = t1.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Food Share - Parametric") +
  geom_vline(xintercept = -(t1$coefficients[[2]]/(2*t1$coefficients[[3]])), linetype = "dotted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
lm.plot.mksh <- ggplot(trimmed.final) +
  xlim(4.0,7.2) +
  ylim(-0.0, 0.17) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Milk") +
  geom_line(aes(lpcexp, t2.lower)) +
  geom_line(aes(lpcexp, t2$fitted.values), col = "green", size = 1.5) +
  geom_line(aes(lpcexp, t2.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = t2.lower, ymax = t2.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Milk Share - Parametric") +
  geom_vline(xintercept = -(t2$coefficients[[2]]/(2*t2$coefficients[[3]])), linetype = "dotted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm.plot.adsh <- ggplot(data = trimmed.final) +
  xlim(4.0,7.2) +
  ylim(0.01, 0.045) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Adult Goods") +
  geom_line(aes(lpcexp, t3.lower)) +
  geom_line(aes(lpcexp, t3$fitted.values), col = "red", size = 1.5) +
  geom_line(aes(lpcexp, t3.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = t3.lower, ymax = t3.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Adult Good Share - Parametric") +
  geom_vline(xintercept = -(t3$coefficients[[2]]/(2*t3$coefficients[[3]])), linetype = "dotted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm.plot.chsh <- ggplot(data = trimmed.final) +
  xlim(4,7.2) +
  ylim(-0.02, 0.01) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Child Goods") +
  geom_line(aes(lpcexp, t4.lower)) +
  geom_line(aes(lpcexp, t4$fitted.values), col = "orange", size = 1.5) +
  geom_line(aes(lpcexp, t4.upper)) +
  geom_ribbon(aes(x = trimmed.final$lpcexp, ymin = t4.lower, ymax = t4.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Child Good Share - Parametric") +
  geom_vline(xintercept = -(t4$coefficients[[2]]/(2*t4$coefficients[[3]])), linetype = "dotted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# export manually 
multiplot(lm.plot.fdsh, lm.plot.chsh, lm.plot.mksh, lm.plot.adsh, cols = 2)


