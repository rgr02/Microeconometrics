
# Trimmed Dataset
load("trimmed.final.RData")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Defining controls for regression models
# ------------------------------------------------------------------------------------------------------------------------------------------------------

income <- cbind(trimmed.final$lpcinc, trimmed.final$lpcincsq)
expenditure <- cbind(trimmed.final$lpcexp, trimmed.final$lpcexpsq)
covariates <- cbind(trimmed.final$lsize ,trimmed.final$dforder, trimmed.final$dmorder)

proportions <- cbind(trimmed.final$prpm01, trimmed.final$prpm02,trimmed.final$prpm03,trimmed.final$prpm04,trimmed.final$prpm05,trimmed.final$prpm06,trimmed.final$prpm07,trimmed.final$prpm08,
                     trimmed.final$prpf01, trimmed.final$prpf02,trimmed.final$prpf03,trimmed.final$prpf04,trimmed.final$prpf05,trimmed.final$prpf06,trimmed.final$prpf07,trimmed.final$prpf08,
                     trimmed.final$wprpm09,trimmed.final$wprpm10,trimmed.final$wprpm11,trimmed.final$wprpm12,trimmed.final$wprpm13,trimmed.final$wprpm14,trimmed.final$wprpm15,
                     trimmed.final$wprpf09, trimmed.final$wprpf10,trimmed.final$wprpf11,trimmed.final$wprpf12,trimmed.final$wprpf13,trimmed.final$wprpf14,trimmed.final$wprpf15,
                     trimmed.final$dprpm09, trimmed.final$dprpm10,trimmed.final$dprpm11,trimmed.final$dprpm12,trimmed.final$dprpm13,trimmed.final$dprpm14,trimmed.final$dprpm15,
                     trimmed.final$dprpf09, trimmed.final$dprpf10,trimmed.final$dprpf11,trimmed.final$dprpf12,trimmed.final$dprpf13,trimmed.final$dprpf14,trimmed.final$dprpf15,
                     trimmed.final$pmwork5, trimmed.final$pmwork6, trimmed.final$pmwork7,
                     trimmed.final$pfwork5, trimmed.final$pfwork6, trimmed.final$pfwork7,
                     trimmed.final$m5, trimmed.final$m6, trimmed.final$m7,
                     trimmed.final$f5, trimmed.final$f6)

# Without Children
# proportions <- cbind(trimmed.final$wprpm09,trimmed.final$wprpm10,trimmed.final$wprpm11,trimmed.final$wprpm12,trimmed.final$wprpm13,trimmed.final$wprpm14,trimmed.final$wprpm15,
#                      trimmed.final$wprpf09, trimmed.final$wprpf10,trimmed.final$wprpf11,trimmed.final$wprpf12,trimmed.final$wprpf13,trimmed.final$wprpf14,trimmed.final$wprpf15,
#                      trimmed.final$dprpm09, trimmed.final$dprpm10,trimmed.final$dprpm11,trimmed.final$dprpm12,trimmed.final$dprpm13,trimmed.final$dprpm14,trimmed.final$dprpm15,
#                      trimmed.final$dprpf09, trimmed.final$dprpf10,trimmed.final$dprpf11,trimmed.final$dprpf12,trimmed.final$dprpf13,trimmed.final$dprpf14,trimmed.final$dprpf15,
#                      trimmed.final$pmwork5, trimmed.final$pmwork6, trimmed.final$pmwork7,
#                      trimmed.final$pfwork5, trimmed.final$pfwork6, trimmed.final$pfwork7,
#                      trimmed.final$m5, trimmed.final$m6, trimmed.final$m7,
#                      trimmed.final$f5, trimmed.final$f6)

regions <- cbind(trimmed.final$dp, trimmed.final$dn, trimmed.final$ds) # db base
seasons <- cbind(trimmed.final$dq1, trimmed.final$dq2, trimmed.final$dq3) #dq4 base

demographics <- cbind(trimmed.final$lsize ,trimmed.final$dforder, trimmed.final$dmorder,
                      trimmed.final$prpm01, trimmed.final$prpm02,trimmed.final$prpm03,trimmed.final$prpm04,trimmed.final$prpm05,trimmed.final$prpm06,trimmed.final$prpm07,trimmed.final$prpm08,
                      trimmed.final$prpf01, trimmed.final$prpf02,trimmed.final$prpf03,trimmed.final$prpf04,trimmed.final$prpf05,trimmed.final$prpf06,trimmed.final$prpf07,trimmed.final$prpf08,
                      trimmed.final$wprpm09,trimmed.final$wprpm10,trimmed.final$wprpm11,trimmed.final$wprpm12,trimmed.final$wprpm13,trimmed.final$wprpm14,trimmed.final$wprpm15,
                      trimmed.final$wprpf09, trimmed.final$wprpf10,trimmed.final$wprpf11,trimmed.final$wprpf12,trimmed.final$wprpf13,trimmed.final$wprpf14,trimmed.final$wprpf15,
                      trimmed.final$dprpm09, trimmed.final$dprpm10,trimmed.final$dprpm11,trimmed.final$dprpm12,trimmed.final$dprpm13,trimmed.final$dprpm14,trimmed.final$dprpm15,
                      trimmed.final$dprpf09, trimmed.final$dprpf10,trimmed.final$dprpf11,trimmed.final$dprpf12,trimmed.final$dprpf13,trimmed.final$dprpf14,trimmed.final$dprpf15,
                      trimmed.final$pmwork5, trimmed.final$pmwork6, trimmed.final$pmwork7,
                      trimmed.final$pfwork5, trimmed.final$pfwork6, trimmed.final$pfwork7,
                      trimmed.final$m5, trimmed.final$m6, trimmed.final$m7,
                      trimmed.final$f5, trimmed.final$f6)


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

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Dummies for Labor Market Status
# ------------------------------------------------------------------------------------------------------------------------------------------------------

trimmed.final$nr.earners <- (trimmed.final$wprpm09 + 
                              trimmed.final$wprpm10 +
                              trimmed.final$wprpm11 +
                              trimmed.final$wprpm12 +
                              trimmed.final$wprpm13 +
                              trimmed.final$wprpm14 +
                              trimmed.final$wprpm15 +
                              trimmed.final$wprpf09 +
                              trimmed.final$wprpf10 +
                              trimmed.final$wprpf11 +
                              trimmed.final$wprpf12 +
                              trimmed.final$wprpf13 +
                              trimmed.final$wprpf14 +
                              trimmed.final$wprpf15 +
                              trimmed.final$pmwork5 +
                              trimmed.final$pmwork6 +
                              trimmed.final$pmwork7 +
                              trimmed.final$pfwork5 +
                              trimmed.final$pfwork6 +
                              trimmed.final$pfwork7) * exp(trimmed.final$lsize)

trimmed.final$nr.earners <- round(trimmed.final$nr.earners)
summary(trimmed.final$nr.earners)


trimmed.final$earn1 <- ifelse(trimmed.final$nr.earners == 1, 1, 0)
trimmed.final$earn2 <- ifelse(trimmed.final$nr.earners == 2, 1, 0)
trimmed.final$earn3 <- ifelse(trimmed.final$nr.earners >= 3, 1, 0)

sum(trimmed.final$earn1)
sum(trimmed.final$earn2)
sum(trimmed.final$earn3)

# Subsetting
earn1 <- subset(trimmed.final, trimmed.final$earn1 == 1)
earn2 <- subset(trimmed.final, trimmed.final$earn2 == 1)
earn3 <- subset(trimmed.final, trimmed.final$earn3 == 1)

punjab <- subset(trimmed.final, trimmed.final$dp == 1) 
sind <- subset(trimmed.final, trimmed.final$dn == 1) 
nwfp <- subset(trimmed.final, trimmed.final$ds == 1) 
baluchistan <- subset(trimmed.final, trimmed.final$db == 1)

