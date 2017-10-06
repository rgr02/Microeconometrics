# ------------------------------------------------------------------------------------------------------------------------------------------------------
# Dealing with Elasticities
# ------------------------------------------------------------------------------------------------------------------------------------------------------

# NONPARAMETRIC ELASITICITIES

# Kernal Bandwidth Silverman's Rule of Thumb
#kernel.bandwidth <- ((4*sd(trimmed.final$lpcexp)/(3*nrow(trimmed.final)))^5)^(1/5)


# Selecting Kernel Bandwidth lpcexp
# gridsize <- nrow(trimmed.final)
# kernel.bandwidth <- dpik(trimmed.final$lpcexp, scalest = "stdev", level = 2L, kernel = "normal", gridsize = gridsize) + 0.01

density.lpcexp <- density(trimmed.final$lpcexp)
kernel.bandwidth <- density.lpcexp$bw + 0.1

# Nonparametric Estimate fdsh
np.est.fdsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.fdsh, degree = 2, bandwidth = kernel.bandwidth)
np.deriv.fdsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.fdsh, drv = 1, degree = 2, bandwidth = kernel.bandwidth)
fdsh.elasticity <- (1 + (np.deriv.fdsh$y / np.est.fdsh$y))

# Nonparametric Estimate mksh
np.est.mksh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.mksh, degree = 2, bandwidth = kernel.bandwidth)
np.deriv.mksh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.mksh, drv = 1, degree = 2, bandwidth = kernel.bandwidth)
mksh.elasticity <- (1 + (np.deriv.mksh$y / np.est.mksh$y))

# Nonparametric Estimate adsh
np.est.adsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.adsh, degree = 2, bandwidth = kernel.bandwidth)
np.deriv.adsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.adsh, drv = 1, degree = 2, bandwidth = kernel.bandwidth)
adsh.elasticity <- (1 + (np.deriv.adsh$y / np.est.adsh$y))

# Nonparametric Estimate chsh
np.est.chsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.chsh, degree = 2, bandwidth = kernel.bandwidth)
np.deriv.chsh <- locpoly(trimmed.final$lpcexp,trimmed.final$adj.chsh, drv = 1, degree = 2, bandwidth = kernel.bandwidth)
chsh.elasticity <- (1 + (np.deriv.chsh$y / np.est.chsh$y))
summary(chsh.elasticity)

np.elast.fdsh <- ggplot(as.data.frame(np.est.fdsh)) +
  geom_point(aes(np.est.fdsh$x,fdsh.elasticity), col = "blue")+
  geom_hline(yintercept = 1) +
  xlim(4,7.15) +
  ylim(0.3,1) +
  xlab("Log Per Capita Expenditure") +
  ylab("") +
  ggtitle("Nonparametric Elasticity of Food Expenditure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

np.elast.mksh <- ggplot(as.data.frame(np.est.mksh))+
  geom_point(aes(np.est.mksh$x,mksh.elasticity), col = "green")+
  geom_hline(yintercept = 1) +
  xlim(4,7.15) +
  ylim(0,4) +
  xlab("Log Per Capita Expenditure") +
  ylab("") +
  ggtitle("Nonparametric Elasticity of Milk Expenditure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

np.elast.adsh <- ggplot(as.data.frame(np.est.adsh))+
  geom_point(aes(np.est.adsh$x,adsh.elasticity), col = "red")+
  geom_hline(yintercept = 1) +
  xlim(4,7.15) +
  ylim(-0.5,1.5) +
  xlab("Log Per Capita Expenditure") +
  ylab("") +
  ggtitle("Elasticity of Adult Good Expenditure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

np.elast.chsh <- ggplot(as.data.frame(np.est.chsh))+
  geom_point(aes(np.est.chsh$x,chsh.elasticity), col = "orange")+
  geom_hline(yintercept = 1) +
  xlim(4,7.15) +
  ylim(-3,3) +
  xlab("Log Per Capita Expenditure") +
  ylab("") +
  ggtitle("Elasticity of Child Good Expenditure") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(np.elast.fdsh, np.elast.chsh, 
          np.elast.mksh, np.elast.adsh, cols = 2)

