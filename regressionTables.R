# Clear workspace
rm(list = ls())

# libraries
library(stargazer)
library(dplyr)
library(ggplot2)
library(mgcv)
library(foreign)
library(KernSmooth)
library(kedd)

source("multiplot.R")

# dataset
load("final.RData")

# checking structure
str(final)
final <- as.data.frame(final)
TRUE %in% is.na(final)

# trimming density
dens <- density(final$fdsh, n = length(final$fdsh), from = min(final$fdsh), to = max(final$fdsh))
length(dens$x)
final$density <- dens$y

final$density <- sort(final$density)
plot(final$density)

final <- final[99:9741,]

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


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# OLS Regressions
# ------------------------------------------------------------------------------------------------------------------------------------------------------

lm.m1 <- lm(final$fdsh ~ expenditure + covariates + proportions + regions + seasons)
lm.m2 <- lm(final$mksh ~ expenditure + covariates + proportions + regions + seasons)
lm.m3 <- lm(final$adsh ~ expenditure + covariates + proportions + regions + seasons)
lm.m4 <- lm(final$chsh ~ expenditure + covariates + proportions + regions + seasons)

# Tables LM
stargazer(lm.m1, lm.m2, lm.m3, lm.m4, type = "html",
          title = "OLS Results",
          dep.var.labels = c("Foodshare", "Milkshare", "Adult Good Share", "Child Good Share"),
          out = "ols.html")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Semiparametric Regressions
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# Generalized Additive Models
# s for smoothing via splines
gam.m1 <- gam(final$fdsh ~ s(final$lpcexp) + covariates + proportions + regions + seasons)
gam.m2 <- gam(final$mksh ~ s(final$lpcexp) + covariates + proportions + regions + seasons)
gam.m3 <- gam(final$adsh ~ s(final$lpcexp) + covariates + proportions + regions + seasons)
gam.m4 <- gam(final$chsh ~ s(final$lpcexp) + covariates + proportions + regions + seasons)

stargazer(gam.m1,gam.m2,gam.m3,gam.m4, type = "html",
          title = "GAM Results",
          dep.var.labels = c("Foodshare", "Milkshare", "Adult Good Share", "Child Good Share"),
          out = "GAM.html")