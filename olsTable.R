
# libraries
library(stargazer)
library(survey)

source("controls.R")

# Stargazer Cheatsheet
# http://www.jakeruss.com/cheatsheets/stargazer/

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# OLS Regressions
# ------------------------------------------------------------------------------------------------------------------------------------------------------

lm1 <- lm(trimmed.final$fdsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq +
                    trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder +
                    proportions + regions + seasons)

lm2 <- lm(trimmed.final$mksh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq +
                    trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder +
                    proportions + regions + seasons)

lm3 <- lm(trimmed.final$adsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq +
                    trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder +
                    proportions + regions + seasons)

lm4 <- lm(trimmed.final$chsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq +
                    trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder +
                    proportions + regions + seasons)

# Rerunning Regressions for Wald Test
l1 <- lm(trimmed.final$fdsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq + demographics + regions + seasons)

# Extracting F and p values
l1.F.demographics <- anova(l1)$`F value`[[3]]
l1.F.regions <- anova(l1)$`F value`[[4]]
l1.F.seasons <- anova(l1)$`F value`[[5]]
l1.p.demographics <- anova(l1)$`Pr(>F)`[[3]]
l1.p.regions <- anova(l1)$`Pr(>F)`[[4]]
l1.p.seasons <- anova(l1)$`Pr(>F)`[[5]]

l2 <- lm(trimmed.final$mksh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq + demographics + regions + seasons)

l2.F.demographics <- anova(l2)$`F value`[[3]]
l2.F.regions <- anova(l2)$`F value`[[4]]
l2.F.seasons <- anova(l2)$`F value`[[5]]
l2.p.demographics <- anova(l2)$`Pr(>F)`[[3]]
l2.p.regions <- anova(l2)$`Pr(>F)`[[4]]
l2.p.seasons <- anova(l2)$`Pr(>F)`[[5]]


l3 <- lm(trimmed.final$adsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq + demographics + regions + seasons)

l3.F.demographics <- anova(l3)$`F value`[[3]]
l3.F.regions <- anova(l3)$`F value`[[4]]
l3.F.seasons <- anova(l3)$`F value`[[5]]
l3.p.demographics <- anova(l3)$`Pr(>F)`[[3]]
l3.p.regions <- anova(l3)$`Pr(>F)`[[4]]
l3.p.seasons <- anova(l3)$`Pr(>F)`[[5]]

l4 <- lm(trimmed.final$chsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq + demographics + regions + seasons)

l4.F.demographics <- anova(l4)$`F value`[[3]]
l4.F.regions <- anova(l4)$`F value`[[4]]
l4.F.seasons <- anova(l4)$`F value`[[5]]
l4.p.demographics <- anova(l4)$`Pr(>F)`[[3]]
l4.p.regions <- anova(l4)$`Pr(>F)`[[4]]
l4.p.seasons <- anova(l4)$`Pr(>F)`[[5]]


# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Elasticity
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# Redo adjusting for Engel Curve
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

engel <- lm(trimmed.final$adj.fdsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq)
nominator <- engel$coefficients[[2]]+(2*engel$coefficients[[3]]*mean(trimmed.final$lpcexp))
denominator <- engel$coefficients[[1]]+(engel$coefficients[[2]]*mean(trimmed.final$lpcexp))+(engel$coefficients[[3]]*mean(trimmed.final$lpcexpsq))
fdsh.result <- (nominator/denominator)

engel <- lm(trimmed.final$adj.mksh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq)
nominator <- engel$coefficients[[2]]+(2*engel$coefficients[[3]]*mean(trimmed.final$lpcexp))
denominator <- engel$coefficients[[1]]+(engel$coefficients[[2]]*mean(trimmed.final$lpcexp))+(engel$coefficients[[3]]*mean(trimmed.final$lpcexpsq))
mksh.result <- (nominator/denominator)

engel <- lm(trimmed.final$adj.adsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq)
nominator <- engel$coefficients[[2]]+(2*engel$coefficients[[3]]*mean(trimmed.final$lpcexp))
denominator <- engel$coefficients[[1]]+(engel$coefficients[[2]]*mean(trimmed.final$lpcexp))+(engel$coefficients[[3]]*mean(trimmed.final$lpcexpsq))
adsh.result <- (nominator/denominator)

engel <- lm(trimmed.final$adj.chsh ~ trimmed.final$lpcexp + trimmed.final$lpcexpsq)
nominator <- engel$coefficients[[2]]+(2*engel$coefficients[[3]]*mean(trimmed.final$lpcexp))
denominator <- engel$coefficients[[1]]+(engel$coefficients[[2]]*mean(trimmed.final$lpcexp))+(engel$coefficients[[3]]*mean(trimmed.final$lpcexpsq))
chsh.result <- (nominator/denominator)

stargazer(lm1,lm2,lm3,lm4, type = "html",
          title = "Table 1.a Parametric Engel Curves",
          dep.var.labels = c("Foodshare", "Milkshare", "Adult Good Share", "Child Good Share"),
          covariate.labels = c("Log Expenditure","Log Expenditure Square","Log Household Size","Birthorder Female", "Birthorder Male"),
          out = "OLS.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("proportions*", "regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "# Denotes Significance at least at the 5 percent level",
          notes.align = "r",
          add.lines = list(c("------------------------------","---------------","---------------","---------------","---------------"),
                           c("Demographics",
                             paste0("F = ",round(l1.F.demographics,digits = 2)),
                             paste0("F = ",round(l2.F.demographics,digits = 2)),
                             paste0("F = ",round(l3.F.demographics,digits = 2)),
                             paste0("F = ",round(l4.F.demographics,digits = 2))),
                           c(" ",
                             ifelse(l1.p.demographics <= 0.05, "#", paste0("p = ",round(l1.p.demographics, digits = 2))),
                             ifelse(l2.p.demographics <= 0.05, "#", paste0("p = ",round(l2.p.demographics, digits = 2))),
                             ifelse(l3.p.demographics <= 0.05, "#", paste0("p = ",round(l3.p.demographics, digits = 2))),
                             ifelse(l4.p.demographics <= 0.05, "#", paste0("p = ",round(l4.p.demographics, digits = 2)))),
                           c("Regions",
                             paste0("F = ",round(l1.F.regions, digits = 2)),
                             paste0("F = ",round(l2.F.regions, digits = 2)),
                             paste0("F = ",round(l3.F.regions, digits = 2)),
                             paste0("F = ",round(l4.F.regions, digits = 2))),
                           c(" ",
                             ifelse(l1.p.regions <= 0.05, "#", paste0("p = ",round(l1.p.regions, digits = 2))),
                             ifelse(l2.p.regions <= 0.05, "#", paste0("p = ",round(l2.p.regions, digits = 2))),
                             ifelse(l3.p.regions <= 0.05, "#", paste0("p = ",round(l3.p.regions, digits = 2))),
                             ifelse(l4.p.regions <= 0.05, "#", paste0("p = ",round(l4.p.regions, digits = 2)))),
                           c("Seasons",
                             paste0("F = ", round(l1.F.seasons, digits = 2)),
                             paste0("F = ", round(l2.F.seasons, digits = 2)),
                             paste0("F = ", round(l3.F.seasons, digits = 2)),
                             paste0("F = ", round(l4.F.seasons, digits = 2))),
                           c(" ",
                             ifelse(l1.p.seasons <= 0.05, "#", paste0("p = ",round(l1.p.seasons, digits = 2))),
                             ifelse(l2.p.seasons <= 0.05, "#", paste0("p = ",round(l2.p.seasons, digits = 2))),
                             ifelse(l3.p.seasons <= 0.05, "#", paste0("p = ",round(l3.p.seasons, digits = 2))),
                             ifelse(l4.p.seasons <= 0.05, "#", paste0("p = ",round(l4.p.seasons, digits = 2)))),
                           c("Mean of dependent Variable",
                             round(mean(trimmed.final$fdsh), digits = 2),
                             round(mean(trimmed.final$mksh), digits = 2),
                             round(mean(trimmed.final$adsh),digits = 2),
                             round(mean(trimmed.final$chsh), digits = 2)),
                           c("Y - Elasticity",round(fdsh.result, digits = 2),round(mksh.result,digits = 2),round(adsh.result, digits = 2),round(chsh.result, digits = 2)),
                           c("------------------------------","---------------","---------------","---------------","---------------")))
                           







