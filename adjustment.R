# Adjustment for HH Size and Proportion
adj.lm1 <- lm(trimmed.final$fdsh ~ trimmed.final$lsize + proportions)
adj.lm2 <- lm(trimmed.final$mksh ~ trimmed.final$lsize + proportions)
adj.lm3 <- lm(trimmed.final$adsh ~ trimmed.final$lsize + proportions)
adj.lm4 <- lm(trimmed.final$chsh ~ trimmed.final$lsize + proportions)

# Adding adjusted variables to dataframe
final$adj.fdsh <- adj.lm1$coefficients[[1]] + adj.lm1$residuals
final$adj.mksh <- adj.lm2$coefficients[[1]] + adj.lm2$residuals
final$adj.adsh <- adj.lm3$coefficients[[1]] + adj.lm3$residuals
final$adj.chsh <- adj.lm4$coefficients[[1]] + adj.lm4$residuals

# Saving Trimmed Data
save(trimmed.final, file = "trimmed.final.RData", list = "trimmed.final")
write.csv(trimmed.final, file = "trimmed.final.csv")
write.dta(trimmed.final, file = "trimmed.final.dta")