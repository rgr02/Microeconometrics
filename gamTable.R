
# libraries
library(stargazer)
library(survey)
library(mgcv)
library(KernSmooth)

source("main.R")

# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Semiparametric Regressions
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# Generalized Additive Models
# s for smoothing via splines
gam1 <- gam(trimmed.final$fdsh ~ s(trimmed.final$lpcexp) + trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder + proportions + regions + seasons)
gam2 <- gam(trimmed.final$mksh ~ s(trimmed.final$lpcexp) + trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder + proportions + regions + seasons)
gam3 <- gam(trimmed.final$adsh ~ s(trimmed.final$lpcexp) + trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder + proportions + regions + seasons)
gam4 <- gam(trimmed.final$chsh ~ s(trimmed.final$lpcexp) + trimmed.final$lsize + trimmed.final$dforder + trimmed.final$dmorder + proportions + regions + seasons)


# Rerunning GAM for Wald Test
gam21 <- gam(trimmed.final$fdsh ~ s(trimmed.final$lpcexp) + demographics + regions + seasons)
gam22 <- gam(trimmed.final$mksh ~ s(trimmed.final$lpcexp) + demographics + regions + seasons)
gam23 <- gam(trimmed.final$adsh ~ s(trimmed.final$lpcexp) + demographics + regions + seasons)
gam24 <- gam(trimmed.final$chsh ~ s(trimmed.final$lpcexp) + demographics + regions + seasons)

g1 <- summary.gam(gam21)
g2 <- summary.gam(gam22)
g3 <- summary.gam(gam23)
g4 <- summary.gam(gam24)

stargazer(gam1,gam2,gam3,gam4, type = "html",
          title = "Table 1.b Nonparametric Engel Curves",
          dep.var.labels = c("Foodshare", "Milkshare", "Adult Good Share", "Child Good Share"),
          covariate.labels = c("Log Household Size","Birthorder Female", "Birthorder Male"),
          out = "GAM.html",
          align = TRUE,
          style = "aer",
          report = "vct*",
          omit = c("lpc*","proportion*","regions*","seasons*", "Constant"),
          notes.append = TRUE,
          notes = "# Denotes Significance at least at the 5 percent level",
          notes.align = "r",
          add.lines = list(c("------------------------------","---------------","---------------","---------------","---------------"),
                           c("Demographics",
                             paste0("F = ",round(g1$pTerms.chi.sq[[1]], digits = 2)),
                             paste0("F = ",round(g2$pTerms.chi.sq[[1]],digits = 2)),
                             paste0("F = ",round(g3$pTerms.chi.sq[[1]],digits = 2)),
                             paste0("F = ",round(g4$pTerms.chi.sq[[1]],digits = 2))),
                           c(" ",
                             ifelse(g1$pTerms.pv[[1]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv, digits = 2))),
                             ifelse(g2$pTerms.pv[[1]] <= 0.05, "#", paste0("p = ",round(g2$pTerms.pv, digits = 2))),
                             ifelse(g3$pTerms.pv[[1]] <= 0.05, "#", paste0("p = ",round(g3$pTerms.pv, digits = 2))),
                             ifelse(g4$pTerms.pv[[1]] <= 0.05, "#", paste0("p = ",round(g4$pTerms.pv, digits = 2)))),
                           c("Regions",
                             paste0("F = ",round(g1$pTerms.chi.sq[[2]], digits = 2)),
                             paste0("F = ",round(g2$pTerms.chi.sq[[2]], digits = 2)),
                             paste0("F = ",round(g3$pTerms.chi.sq[[2]], digits = 2)),
                             paste0("F = ",round(g4$pTerms.chi.sq[[2]], digits = 2))),
                           c(" ",
                             ifelse(g1$pTerms.pv[[2]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv[[2]], digits = 2))),
                             ifelse(g2$pTerms.pv[[2]] <= 0.05, "#", paste0("p = ",round(g2$pTerms.pv[[2]], digits = 2))),
                             ifelse(g3$pTerms.pv[[2]] <= 0.05, "#", paste0("p = ",round(g3$pTerms.pv[[2]], digits = 2))),
                             ifelse(g4$pTerms.pv[[2]] <= 0.05, "#", paste0("p = ",round(g4$pTerms.pv[[2]], digits = 2)))),
                           c("Seasons",
                             paste0("F = ", round(g1$pTerms.chi.sq[[3]], digits = 2)),
                             paste0("F = ", round(g2$pTerms.chi.sq[[3]], digits = 2)),
                             paste0("F = ", round(g3$pTerms.chi.sq[[3]], digits = 2)),
                             paste0("F = ", round(g4$pTerms.chi.sq[[3]], digits = 2))),
                           c(" ",
                             ifelse(g1$pTerms.pv[[3]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv[[3]], digits = 2))),
                             ifelse(g1$pTerms.pv[[3]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv[[3]], digits = 2))),
                             ifelse(g1$pTerms.pv[[3]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv[[3]], digits = 2))),
                             ifelse(g1$pTerms.pv[[3]] <= 0.05, "#", paste0("p = ",round(g1$pTerms.pv[[3]], digits = 2)))),
                           c("Mean of dependent Variable",
                             round(mean(trimmed.final$fdsh), digits = 2),
                             round(mean(trimmed.final$mksh), digits = 2),
                             round(mean(trimmed.final$adsh), digits = 2),
                             round(mean(trimmed.final$chsh), digits = 2)),
                           c("------------------------------","---------------","---------------","---------------","---------------")))

