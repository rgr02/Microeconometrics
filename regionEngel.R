
# libraries
library(ggplot2)
source("controls.R")

r1 <- lm(punjab$adj.fdsh ~ punjab$lpcexp + punjab$lpcexpsq)
r2 <- lm(sind$adj.fdsh ~ sind$lpcexp + sind$lpcexpsq)
r3 <- lm(nwfp$adj.fdsh ~ nwfp$lpcexp + nwfp$lpcexpsq)
r4 <- lm(baluchistan$adj.fdsh ~ baluchistan$lpcexp + baluchistan$lpcexpsq)

ggplot() +
  geom_point(aes(punjab$lpcexp, r1$fitted.values), col = "orange") +
  xlim(4.0,7.2) +
  ylim(0.3, 0.7) +
  xlab("Log Per Capita Expenditure") +
  geom_point(aes(baluchistan$lpcexp, r4$fitted.values), col = "red") +
  ylab("Adjusted Budget Share: Food") +
  geom_point(aes(sind$lpcexp, r2$fitted.values), col = "green") +
  ggtitle("Region in which Household Resides") +
  geom_point(aes(nwfp$lpcexp, r3$fitted.values), col = "blue") +
  theme_minimal() +
  labs(caption = paste0("orange - Punjab", "\n", "green - Sind", "\n", "blue - NWFP","\n","red - Baluchistan")) +
  theme(plot.title = element_text(hjust = 0.5))