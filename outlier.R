
library(mvoutlier)
library(ggplot2)
library(stargazer)

load("final.Rdata")
source("multiplot.R")
#source("helpOutlier.R")


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Defining controls for regression models
# ------------------------------------------------------------------------------------------------------------------------------------------------------

income <- cbind(final$lpcinc, final$lpcincsq)
expenditure <- cbind(final$lpcexp, final$lpcexpsq)
covariates <- cbind(final$lsize ,final$dforder, final$dmorder)

proportions <- cbind(final$prpm01, final$prpm02,final$prpm03,final$prpm04,final$prpm05,final$prpm06,final$prpm07,final$prpm08,
                     final$prpf01, final$prpf02,final$prpf03,final$prpf04,final$prpf05,final$prpf06,final$prpf07,final$prpf08,
                     final$wprpm09,final$wprpm10,final$wprpm11,final$wprpm12,final$wprpm13,final$wprpm14,final$wprpm15,
                     final$wprpf09, final$wprpf10,final$wprpf11,final$wprpf12,final$wprpf13,final$wprpf14,final$wprpf15,
                     final$dprpm09, final$dprpm10,final$dprpm11,final$dprpm12,final$dprpm13,final$dprpm14,final$dprpm15,
                     final$dprpf09, final$dprpf10,final$dprpf11,final$dprpf12,final$dprpf13,final$dprpf14,final$dprpf15,
                     final$pmwork5, final$pmwork6, final$pmwork7,
                     final$pfwork5, final$pfwork6, final$pfwork7,
                     final$m5, final$m6, final$m7,
                     final$f5, final$f6)

# Without Children
# proportions <- cbind(final$wprpm09,final$wprpm10,final$wprpm11,final$wprpm12,final$wprpm13,final$wprpm14,final$wprpm15,
#                      final$wprpf09, final$wprpf10,final$wprpf11,final$wprpf12,final$wprpf13,final$wprpf14,final$wprpf15,
#                      final$dprpm09, final$dprpm10,final$dprpm11,final$dprpm12,final$dprpm13,final$dprpm14,final$dprpm15,
#                      final$dprpf09, final$dprpf10,final$dprpf11,final$dprpf12,final$dprpf13,final$dprpf14,final$dprpf15,
#                      final$pmwork5, final$pmwork6, final$pmwork7,
#                      final$pfwork5, final$pfwork6, final$pfwork7,
#                      final$m5, final$m6, final$m7,
#                      final$f5, final$f6)

regions <- cbind(final$dp, final$dn, final$ds) # db base
seasons <- cbind(final$dq1, final$dq2, final$dq3) #dq4 base

demographics <- cbind(final$lsize ,final$dforder, final$dmorder,
                      final$prpm01, final$prpm02,final$prpm03,final$prpm04,final$prpm05,final$prpm06,final$prpm07,final$prpm08,
                      final$prpf01, final$prpf02,final$prpf03,final$prpf04,final$prpf05,final$prpf06,final$prpf07,final$prpf08,
                      final$wprpm09,final$wprpm10,final$wprpm11,final$wprpm12,final$wprpm13,final$wprpm14,final$wprpm15,
                      final$wprpf09, final$wprpf10,final$wprpf11,final$wprpf12,final$wprpf13,final$wprpf14,final$wprpf15,
                      final$dprpm09, final$dprpm10,final$dprpm11,final$dprpm12,final$dprpm13,final$dprpm14,final$dprpm15,
                      final$dprpf09, final$dprpf10,final$dprpf11,final$dprpf12,final$dprpf13,final$dprpf14,final$dprpf15,
                      final$pmwork5, final$pmwork6, final$pmwork7,
                      final$pfwork5, final$pfwork6, final$pfwork7,
                      final$m5, final$m6, final$m7,
                      final$f5, final$f6)


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Adjusting *share for HH Size and Composition
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# Adjustment for HH Size and Proportion
adj.lm1 <- lm(final$fdsh ~ final$lsize + proportions)
adj.lm2 <- lm(final$mksh ~ final$lsize + proportions)
adj.lm3 <- lm(final$adsh ~ final$lsize + proportions)
adj.lm4 <- lm(final$chsh ~ final$lsize + proportions)

# Adding adjusted variables to dataframe
final$adj.fdsh <- adj.lm1$coefficients[[1]] + adj.lm1$residuals
final$adj.mksh <- adj.lm2$coefficients[[1]] + adj.lm2$residuals
final$adj.adsh <- adj.lm3$coefficients[[1]] + adj.lm3$residuals
final$adj.chsh <- adj.lm4$coefficients[[1]] + adj.lm4$residuals

save(final, file = "finalAdjusted.RData", list = "final")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Outlier Analysis
# ------------------------------------------------------------------------------------------------------------------------------------------------------


fdsh.plot <- aq.plot(cbind(final$adj.fdsh, final$lpcexp))
mksh.plot <- aq.plot(cbind(final$adj.mksh, final$lpcexp))
adsh.plot <- aq.plot(cbind(final$adj.adsh, final$lpcexp))
chsh.plot <- aq.plot(cbind(final$adj.chsh, final$lpcexp))

fdsh.data <- cbind(final,fdsh.plot$outliers)
mksh.data <- cbind(final,mksh.plot$outliers)
adsh.data <- cbind(final,adsh.plot$outliers)
chsh.data <- cbind(final,chsh.plot$outliers)


fdsh.inlier <- subset(fdsh.data, fdsh.data$`fdsh.plot$outliers` == 0)
fdsh.outlier <- subset(fdsh.data, fdsh.data$`fdsh.plot$outliers` == 1)

mksh.inlier <- subset(mksh.data, mksh.data$`mksh.plot$outliers` == 0)
mksh.outlier <- subset(mksh.data, mksh.data$`mksh.plot$outliers` == 1)

adsh.inlier <- subset(adsh.data, adsh.data$`adsh.plot$outliers` == 0)
adsh.outlier <- subset(adsh.data, adsh.data$`adsh.plot$outliers` == 1)

chsh.inlier <- as.data.frame(subset(chsh.data, chsh.data$`chsh.plot$outliers` == 0))
chsh.outlier <- as.data.frame(subset(chsh.data, chsh.data$`chsh.plot$outliers` == 1))

str(chsh.inlier)


fdsh.plot <- ggplot() +
  geom_point(aes(fdsh.inlier$lpcexp, fdsh.inlier$adj.fdsh), col = "black", alpha = 0.5) +
  geom_point(aes(fdsh.outlier$lpcexp, fdsh.outlier$adj.fdsh), col = "blue", alpha = 0.3) +
  ylab("Adjusted Foodshare") +
  xlab("Log per Capita Expenditure") +
  ggtitle("Outlier Foodshare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
fdsh.plot

mksh.plot <- ggplot() +
  geom_point(aes(mksh.inlier$lpcexp, mksh.inlier$adj.mksh), col = "black", alpha = 0.5) +
  geom_point(aes(mksh.outlier$lpcexp,mksh.outlier$adj.mksh), col = "green", alpha = 0.3)+
  ylab("Adjusted Milkshare") +
  xlab("Log per Capita Expenditure") +
  ggtitle("Outlier Milkshare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

adsh.plot <- ggplot() +
  geom_point(aes(adsh.inlier$lpcexp, adsh.inlier$adj.adsh), col = "black", alpha = 0.5) +
  geom_point(aes(adsh.outlier$lpcexp, adsh.outlier$adj.adsh), col = "red", alpha = 0.3)+
  ylab("Adjusted Adult Good Share") +
  xlab("Log per Capita Expenditure") +
  ggtitle("Outlier Adult Good Share") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

chsh.plot <- ggplot() +
  geom_point(aes(chsh.inlier$lpcexp, chsh.inlier$adj.chsh), col = "black", alpha = 0.5) +
  geom_point(aes(chsh.outlier$lpcexp, chsh.outlier$adj.chsh), col = "orange", alpha = 0.3)+
  ylab("Adjusted Child Good Share") +
  xlab("Log per Capita Expenditure") +
  ggtitle("Outlier Child Good Share") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(fdsh.plot, chsh.plot, mksh.plot, adsh.plot, cols = 2)

# Aggregate Variable for fdsh.inlier
fdsh.proportions <- cbind(fdsh.inlier$prpm01, fdsh.inlier$prpm02,fdsh.inlier$prpm03,fdsh.inlier$prpm04,fdsh.inlier$prpm05,fdsh.inlier$prpm06,fdsh.inlier$prpm07,fdsh.inlier$prpm08,
                          fdsh.inlier$prpf01, fdsh.inlier$prpf02,fdsh.inlier$prpf03,fdsh.inlier$prpf04,fdsh.inlier$prpf05,fdsh.inlier$prpf06,fdsh.inlier$prpf07,fdsh.inlier$prpf08,
                          fdsh.inlier$wprpm09,fdsh.inlier$wprpm10,fdsh.inlier$wprpm11,fdsh.inlier$wprpm12,fdsh.inlier$wprpm13,fdsh.inlier$wprpm14,fdsh.inlier$wprpm15,
                          fdsh.inlier$wprpf09, fdsh.inlier$wprpf10,fdsh.inlier$wprpf11,fdsh.inlier$wprpf12,fdsh.inlier$wprpf13,fdsh.inlier$wprpf14,fdsh.inlier$wprpf15,
                          fdsh.inlier$dprpm09, fdsh.inlier$dprpm10,fdsh.inlier$dprpm11,fdsh.inlier$dprpm12,fdsh.inlier$dprpm13,fdsh.inlier$dprpm14,fdsh.inlier$dprpm15,
                          fdsh.inlier$dprpf09, fdsh.inlier$dprpf10,fdsh.inlier$dprpf11,fdsh.inlier$dprpf12,fdsh.inlier$dprpf13,fdsh.inlier$dprpf14,fdsh.inlier$dprpf15,
                          fdsh.inlier$pmwork5, fdsh.inlier$pmwork6, fdsh.inlier$pmwork7,
                          fdsh.inlier$pfwork5, fdsh.inlier$pfwork6, fdsh.inlier$pfwork7,
                          fdsh.inlier$m5, fdsh.inlier$m6, fdsh.inlier$m7,
                          fdsh.inlier$f5, fdsh.inlier$f6)

fdsh.regions <- cbind(fdsh.inlier$dp, fdsh.inlier$dn, fdsh.inlier$ds) # db base
fdsh.seasons <- cbind(fdsh.inlier$dq1, fdsh.inlier$dq2, fdsh.inlier$dq3) #dq4 base

# Aggregate Variable for mksh.inlier
mksh.proportions <- cbind(mksh.inlier$prpm01, mksh.inlier$prpm02,mksh.inlier$prpm03,mksh.inlier$prpm04,mksh.inlier$prpm05,mksh.inlier$prpm06,mksh.inlier$prpm07,mksh.inlier$prpm08,
                          mksh.inlier$prpf01, mksh.inlier$prpf02,mksh.inlier$prpf03,mksh.inlier$prpf04,mksh.inlier$prpf05,mksh.inlier$prpf06,mksh.inlier$prpf07,mksh.inlier$prpf08,
                          mksh.inlier$wprpm09,mksh.inlier$wprpm10,mksh.inlier$wprpm11,mksh.inlier$wprpm12,mksh.inlier$wprpm13,mksh.inlier$wprpm14,mksh.inlier$wprpm15,
                          mksh.inlier$wprpf09, mksh.inlier$wprpf10,mksh.inlier$wprpf11,mksh.inlier$wprpf12,mksh.inlier$wprpf13,mksh.inlier$wprpf14,mksh.inlier$wprpf15,
                          mksh.inlier$dprpm09, mksh.inlier$dprpm10,mksh.inlier$dprpm11,mksh.inlier$dprpm12,mksh.inlier$dprpm13,mksh.inlier$dprpm14,mksh.inlier$dprpm15,
                          mksh.inlier$dprpf09, mksh.inlier$dprpf10,mksh.inlier$dprpf11,mksh.inlier$dprpf12,mksh.inlier$dprpf13,mksh.inlier$dprpf14,mksh.inlier$dprpf15,
                          mksh.inlier$pmwork5, mksh.inlier$pmwork6, mksh.inlier$pmwork7,
                          mksh.inlier$pfwork5, mksh.inlier$pfwork6, mksh.inlier$pfwork7,
                          mksh.inlier$m5, mksh.inlier$m6, mksh.inlier$m7,
                          mksh.inlier$f5, mksh.inlier$f6)

mksh.regions <- cbind(mksh.inlier$dp, mksh.inlier$dn, mksh.inlier$ds) # db base
mksh.seasons <- cbind(mksh.inlier$dq1, mksh.inlier$dq2, mksh.inlier$dq3) #dq4 base

# Aggregate Variable for adsh.inlier
adsh.proportions <- cbind(adsh.inlier$prpm01, adsh.inlier$prpm02,adsh.inlier$prpm03,adsh.inlier$prpm04,adsh.inlier$prpm05,adsh.inlier$prpm06,adsh.inlier$prpm07,adsh.inlier$prpm08,
                          adsh.inlier$prpf01, adsh.inlier$prpf02,adsh.inlier$prpf03,adsh.inlier$prpf04,adsh.inlier$prpf05,adsh.inlier$prpf06,adsh.inlier$prpf07,adsh.inlier$prpf08,
                          adsh.inlier$wprpm09,adsh.inlier$wprpm10,adsh.inlier$wprpm11,adsh.inlier$wprpm12,adsh.inlier$wprpm13,adsh.inlier$wprpm14,adsh.inlier$wprpm15,
                          adsh.inlier$wprpf09, adsh.inlier$wprpf10,adsh.inlier$wprpf11,adsh.inlier$wprpf12,adsh.inlier$wprpf13,adsh.inlier$wprpf14,adsh.inlier$wprpf15,
                          adsh.inlier$dprpm09, adsh.inlier$dprpm10,adsh.inlier$dprpm11,adsh.inlier$dprpm12,adsh.inlier$dprpm13,adsh.inlier$dprpm14,adsh.inlier$dprpm15,
                          adsh.inlier$dprpf09, adsh.inlier$dprpf10,adsh.inlier$dprpf11,adsh.inlier$dprpf12,adsh.inlier$dprpf13,adsh.inlier$dprpf14,adsh.inlier$dprpf15,
                          adsh.inlier$pmwork5, adsh.inlier$pmwork6, adsh.inlier$pmwork7,
                          adsh.inlier$pfwork5, adsh.inlier$pfwork6, adsh.inlier$pfwork7,
                          adsh.inlier$m5, adsh.inlier$m6, adsh.inlier$m7,
                          adsh.inlier$f5, adsh.inlier$f6)

adsh.regions <- cbind(adsh.inlier$dp, adsh.inlier$dn, adsh.inlier$ds) # db base
adsh.seasons <- cbind(adsh.inlier$dq1, adsh.inlier$dq2, adsh.inlier$dq3) #dq4 base

# Aggregate Variable for chsh.inlier
chsh.proportions <- cbind(chsh.inlier$prpm01, chsh.inlier$prpm02,chsh.inlier$prpm03,chsh.inlier$prpm04,chsh.inlier$prpm05,chsh.inlier$prpm06,chsh.inlier$prpm07,chsh.inlier$prpm08,
                          chsh.inlier$prpf01, chsh.inlier$prpf02,chsh.inlier$prpf03,chsh.inlier$prpf04,chsh.inlier$prpf05,chsh.inlier$prpf06,chsh.inlier$prpf07,chsh.inlier$prpf08,
                          chsh.inlier$wprpm09,chsh.inlier$wprpm10,chsh.inlier$wprpm11,chsh.inlier$wprpm12,chsh.inlier$wprpm13,chsh.inlier$wprpm14,chsh.inlier$wprpm15,
                          chsh.inlier$wprpf09, chsh.inlier$wprpf10,chsh.inlier$wprpf11,chsh.inlier$wprpf12,chsh.inlier$wprpf13,chsh.inlier$wprpf14,chsh.inlier$wprpf15,
                          chsh.inlier$dprpm09, chsh.inlier$dprpm10,chsh.inlier$dprpm11,chsh.inlier$dprpm12,chsh.inlier$dprpm13,chsh.inlier$dprpm14,chsh.inlier$dprpm15,
                          chsh.inlier$dprpf09, chsh.inlier$dprpf10,chsh.inlier$dprpf11,chsh.inlier$dprpf12,chsh.inlier$dprpf13,chsh.inlier$dprpf14,chsh.inlier$dprpf15,
                          chsh.inlier$pmwork5, chsh.inlier$pmwork6, chsh.inlier$pmwork7,
                          chsh.inlier$pfwork5, chsh.inlier$pfwork6, chsh.inlier$pfwork7,
                          chsh.inlier$m5, chsh.inlier$m6, chsh.inlier$m7,
                          chsh.inlier$f5, chsh.inlier$f6)

chsh.regions <- cbind(chsh.inlier$dp, chsh.inlier$dn, chsh.inlier$ds) # db base
chsh.seasons <- cbind(chsh.inlier$dq1, chsh.inlier$dq2, chsh.inlier$dq3) #dq4 base



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Regressions
# ------------------------------------------------------------------------------------------------------------------------------------------------------

a <- lm(fdsh.inlier$adj.fdsh ~ fdsh.inlier$lpcexp + fdsh.inlier$lpcexpsq)
a.upper <- a$fitted.values + (1.96 * sd(fdsh.inlier$adj.fdsh/sqrt(length(fdsh.inlier))))
a.lower <- a$fitted.values - (1.96 * sd(fdsh.inlier$adj.fdsh/sqrt(length(fdsh.inlier))))


b <- lm(mksh.inlier$adj.mksh ~ mksh.inlier$lpcexp + mksh.inlier$lpcexpsq)
b.upper <- b$fitted.values + (1.96 * sd(mksh.inlier$adj.fdsh/sqrt(length(mksh.inlier))))
b.lower <- b$fitted.values - (1.96 * sd(mksh.inlier$adj.fdsh/sqrt(length(mksh.inlier))))


c <- lm(adsh.inlier$adj.adsh ~ adsh.inlier$lpcexp + adsh.inlier$lpcexpsq)
c.upper <- c$fitted.values + (1.96 * sd(adsh.inlier$adj.fdsh/sqrt(length(adsh.inlier))))
c.lower <- c$fitted.values - (1.96 * sd(adsh.inlier$adj.fdsh/sqrt(length(adsh.inlier))))

d <- lm(chsh.inlier$adj.chsh ~ chsh.inlier$lpcexp + chsh.inlier$lpcexpsq)
d.upper <- d$fitted.values + (1.96 * sd(chsh.inlier$adj.fdsh/sqrt(length(chsh.inlier))))
d.lower <- d$fitted.values - (1.96 * sd(chsh.inlier$adj.fdsh/sqrt(length(chsh.inlier))))



stargazer(a, type = "html",
          title = "Parametric Engel Curve Food Share",
          dep.var.labels = c("Adjusted Foodshare"),
          covariate.labels = c("Log Expenditure","Log Expenditure Square"),
          out = "OLS2a.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("proportions*", "regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "Engel Curve without Outliers",
          notes.align = "r")

stargazer(b, type = "html",
          title = "Parametric Engel Curve Milkshare",
          dep.var.labels = c("Adjusted Milkshare"),
          covariate.labels = c("Log Expenditure","Log Expenditure Square"),
          out = "OLS2b.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("proportions*", "regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "Engel Curve without Outliers",
          notes.align = "r")

stargazer(c, type = "html",
          title = "Parametric Engel Curve Adult Good Share",
          dep.var.labels = c("Adjusted Adult Good Share"),
          covariate.labels = c("Log Expenditure","Log Expenditure Square"),
          out = "OLS2c.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("proportions*", "regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "Engel Curve without Outliers",
          notes.align = "r")

stargazer(d, type = "html",
          title = "Parametric Engel Curve Child Good Share",
          dep.var.labels = c("Adjusted Child Good Share"),
          covariate.labels = c("Log Expenditure","Log Expenditure Square"),
          out = "OLS2d.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("proportions*", "regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "Engel Curve without Outliers",
          notes.align = "r")

lm.plot.fdsh <- ggplot(fdsh.inlier) +
  xlim(4.0,7.2) +
  ylim(0.35, 0.55) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Food") +
  geom_line(aes(fdsh.inlier$lpcexp, a.lower)) +
  geom_line(aes(fdsh.inlier$lpcexp, a$fitted.values), col = "blue", size = 1.5) +
  geom_line(aes(fdsh.inlier$lpcexp, a.upper)) +
  geom_ribbon(aes(x = fdsh.inlier$lpcexp, ymin = a.lower, ymax = a.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Food Share - Parametric") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm.plot.mksh <- ggplot(mksh.inlier) +
  xlim(4.0,7.2) +
  ylim(0.02, 0.17) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Milk") +
  geom_line(aes(mksh.inlier$lpcexp, b.lower)) +
  geom_line(aes(mksh.inlier$lpcexp, b$fitted.values), col = "green", size = 1.5) +
  geom_line(aes(mksh.inlier$lpcexp, b.upper)) +
  geom_ribbon(aes(x = mksh.inlier$lpcexp, ymin = b.lower, ymax = b.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Milk Share - Parametric") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm.plot.adsh <- ggplot(adsh.inlier) +
  xlim(4.0,7.2) +
  ylim(-0.015, 0.06) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Adult Goods") +
  geom_line(aes(adsh.inlier$lpcexp, c.lower)) +
  geom_line(aes(adsh.inlier$lpcexp, c$fitted.values), col = "red", size = 1.5) +
  geom_line(aes(adsh.inlier$lpcexp, c.upper)) +
  geom_ribbon(aes(x = adsh.inlier$lpcexp, ymin = c.lower, ymax = c.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Adult Good Share - Parametric") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

lm.plot.chsh <- ggplot(chsh.inlier) +
  xlim(4,7.2) +
  ylim(-0.04, 0.04) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Child Goods") +
  geom_line(aes(chsh.inlier$lpcexp, d.lower)) +
  geom_line(aes(chsh.inlier$lpcexp, d$fitted.values), col = "orange", size = 1.5) +
  geom_line(aes(chsh.inlier$lpcexp, d.upper)) +
  geom_ribbon(aes(x = chsh.inlier$lpcexp, ymin = d.lower, ymax = d.upper), alpha = 0.05, col = "grey") +
  ggtitle("Adjusted Child Good Share - Parametric") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# export manually 
multiplot(lm.plot.fdsh, lm.plot.chsh, lm.plot.mksh, lm.plot.adsh, cols = 2)

gam.plot.fdsh <- ggplot() +
  xlim(4,7.2) +
  #ylim(-0.015, 0.005) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Foodshare") +
  geom_smooth(data = fdsh.inlier, aes(fdsh.inlier$lpcexp, fdsh.inlier$adj.fdsh),method = "auto", col = "blue", alpha = 0.5) +
  ggtitle("Adjusted Foodshare - GAM") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.mksh <- ggplot() +
  xlim(4,7.2) +
  #ylim(-0.015, 0.005) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Milkshare") +
  geom_smooth(data = mksh.inlier, aes(mksh.inlier$lpcexp, mksh.inlier$adj.mksh),method = "auto", col = "green", alpha = 0.5) +
  ggtitle("Adjusted Milkshare - GAM") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.adsh <- ggplot() +
  xlim(4,7.2) +
  #ylim(-0.015, 0.005) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Adult Goods") +
  geom_smooth(data = adsh.inlier, aes(adsh.inlier$lpcexp, adsh.inlier$adj.adsh),method = "auto", col = "red", alpha = 0.5) +
  ggtitle("Adjusted Adult Good Share - GAM") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gam.plot.chsh <- ggplot() +
  xlim(4,7.2) +
  ylim(-0.015, 0.005) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Child Goods") +
  geom_smooth(data = chsh.inlier, aes(chsh.inlier$lpcexp, chsh.inlier$adj.chsh),method = "auto", col = "orange", alpha = 0.5) +
  ggtitle("Adjusted Child Good Share - GAM") +
  labs(caption = "Without Outliers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(gam.plot.fdsh, gam.plot.chsh, gam.plot.mksh, gam.plot.adsh, cols = 2)

