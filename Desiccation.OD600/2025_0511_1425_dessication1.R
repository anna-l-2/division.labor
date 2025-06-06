setwd("C:/Users/ajl21/Indiana University/Lennon, Jay - DoL/Anna/Dessication_assay/OD600_reads/")
data <- read.csv("2025_0511_dessication1_master.csv")
data

data.t <-t(data)
data.t
#as data comes in change 1:3 
D6 <- as.data.frame(data[1:48, 1:6])
D6
spo <- as.data.frame(data[49:96, 1:6])
spo

#Graph
plot(data)
