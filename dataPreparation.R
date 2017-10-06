rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)

setwd("C:/Users/Rudi/_Studium/_SS2017/Microeconometrics/Replication")

# Cuts in the data which determine one observation
chunks <- c()
for (i in 1:(136374 - 14)) {
  if (i %% 14 == 0) {
    print(i)
    chunks <- c(chunks, i)
  }
}

# Building 1.st observation
skip <- 0
data <- c()
for (i in 1:14) {
  row <- read_delim(
    file =  "ba-data/ba-data.txt",
    delim = ";",
    col_names = FALSE,
    skip = skip,
    n_max = 1
  )
  data <- c(data, row)
  skip <- i
}
data <- as.data.frame(data) # Vector equals original data
final <- data
length(final)
data <- NA

# Building next observations
data <- c()
for (i in chunks) {
  skip <- i
  # Building next row vector
  for (j in 1:14) {
    row <- read_delim(
      file =  "ba-data/ba-data.txt",
      delim = ";",
      col_names = FALSE,
      skip = skip,
      n_max = 1
    )
    data <- c(data, row)
    skip <- i + j
  }
  # Appending row Vector to dataframe
  data <- as.data.frame(data) # Vector equals original data
  final <- rbind(final, data)
  data <- c()
  print(i)
}

data <- c()
for (i in 14:136374) {
  if (i %% 14 != 0) {
    row <- read_delim(
      file =  "ba-data/ba-data.txt",
      delim = ";",
      col_names = FALSE,
      skip = skip,
      n_max = 1
    )
    data <- c(data, row)
  } else{
    final <- rbind(final, as.data.frame(data))
    data <- c()
  }
  skip <- i
  print(i)
}

# Naming Variables
# 9.6.17 renaming first prpm... because of dublicate columns ??? "prpm01_1"

names(final) <- c(
  "hhcode",
  "fdsh",
  "lsize",
  "lpcexp",
  "lpcexpsq",
  "lpcinc",
  "lpcincsq",
  "mksh",
  "adsh",
  "chsh",
  "dp",
  "dn",
  "ds",
  "db",
  "dq1",
  "dq2",
  "dq3",
  "dq4",
  "dmorder",
  "dforder",
  "hhhlit",
  "hhslit",
  "prpm01",
  "prpm02",
  "prpm03",
  "prpm04",
  "prpm05",
  "prpm06",
  "prpm07",
  "prpm08",
  "wprpm09",
  "wprpm10",
  "wprpm11",
  "wprpm12",
  "wprpm13",
  "wprpm14",
  "wprpm15",
  "pmwork5",
  "pmwork6",
  "pmwork7",
  "dprpm09",
  "dprpm10",
  "dprpm11",
  "dprpm12",
  "dprpm13",
  "dprpm14",
  "dprpm15",
  "pmdept5",
  "pmdept6",
  "pmdept7",
  "prpf01",
  "prpf02",
  "prpf03",
  "prpf04",
  "prpf05",
  "prpf06",
  "prpf07",
  "prpf08",
  "wprpf09",
  "wprpf10",
  "wprpf11",
  "wprpf12",
  "wprpf13",
  "wprpf14",
  "wprpf15",
  "pfwork5",
  "pfwork6",
  "pfwork7",
  "dprpf09",
  "dprpf10",
  "dprpf11",
  "dprpf12",
  "dprpf13",
  "dprpf14",
  "dprpf15",
  "pfdept5",
  "pfdept6",
  "pfdept7",
  "prpm01_1",
  "prpm02_1",
  "prpm03_1",
  "prpm04_1",
  "prpm05_1",
  "prpm06_1",
  "prpm07_1",
  "prpm08_1",
  "prpm09",
  "prpm10",
  "prpm11",
  "prpm12",
  "prpm13",
  "prpm14",
  "prpm15",
  "m5",
  "m6",
  "m7",
  "prpf01_1",
  "prpf02_1",
  "prpf03_1",
  "prpf04_1",
  "prpf05_1",
  "prpf06_1",
  "prpf07_1",
  "prpf08_1",
  "prpf09",
  "prpf10",
  "prpf11",
  "prpf12",
  "prpf13",
  "prpf14",
  "prpf15",
  "f5",
  "f6",
  "f7"
)

dummies <- c("dp",
             "dn",
             "ds",
             "dq1",
             "dq2",
             "dq3",
             "dq4",
             "dmorder",
             "dforder",
             "hhhlit",
             "hhslit")

for(i in dummies){
  final[,i] <- as.factor(final[,i])
}
str(final)

save(final, file = "final.RData", list = "final")
write.csv(final, file = "final.csv")
