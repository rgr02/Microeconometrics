# Clear workspace
rm(list = ls())

# Libraries
library(dplyr)
library(foreign)

# Dataset
load("final.RData")

# Checking Correctness
str(final)
final <- as.data.frame(final)
TRUE %in% is.na(final)

# Trimming Density
dens <- density(final$fdsh, n = length(final$fdsh), from = min(final$fdsh), to = max(final$fdsh))
length(dens$x)
final$density <- dens$y

final$density <- sort(final$density)
plot(final$density)
trimmed.final <- final[99:9741,]

# Saving Trimmed Data
save(trimmed.final, file = "trimmed.final.RData", list = "trimmed.final")
write.csv(trimmed.final, file = "trimmed.final.csv")
write.dta(trimmed.final, file = "trimmed.final.dta")