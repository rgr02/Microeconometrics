
# libraries
library(ggplot2)
source("controls.R")

m1 <- lm(earn1$adj.fdsh ~ earn1$lpcexp + earn1$lpcexpsq)
m2 <- lm(earn2$adj.fdsh ~ earn2$lpcexp + earn2$lpcexpsq)
m3 <- lm(earn3$adj.fdsh ~ earn3$lpcexp + earn3$lpcexpsq)

summary(m1$fitted.values)
summary(m2$fitted.values)
summary(m3$fitted.values)

ggplot() +
  geom_point(aes(earn1$lpcexp, m1$fitted.values), col = "orange", show.legend = T) +
  #xlim(4.0,7.2) +
  #ylim(0.25, 0.55) +
  xlab("Log Per Capita Expenditure") +
  ylab("Adjusted Budget Share: Food") +
  geom_point(aes(earn2$lpcexp, m2$fitted.values), col = "green") +
  ggtitle("Labor Market Status of Household") +
  geom_point(aes(earn3$lpcexp, m3$fitted.values), col = "blue") +
  theme_minimal() +
  labs(caption = paste0("orange - Single Earner HH", "\n", "green - Double Earner HH", "\n", "blue - Multiple Earner HH")) +
  theme(plot.title = element_text(hjust = 0.5))
