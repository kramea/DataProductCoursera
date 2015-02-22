library(ggplot2)
library(reshape2)

r <- read.csv('ma3t.csv')

i <- read.csv('inf.csv')

pp <- subset(r, Result == 'Purchase Probability (100%)')

pp <- pp[,-c(37:56)]

pp <- pp[,c(-11:-20)]

pp <- pp[,c(-1,-2, -10)]

colnames(pp) <- c("Region", "Area", "Risk", "Driver", "HomeCharging", "WorkCharging", 
                  "VehicleTech", 2015:2030)

pp[,7] <- as.character(pp[,7])


ii <- subset(i, Result == 'Purchase Probability (100%)')

ii <- ii[,-c(37:56)]

ii <- ii[,c(-11:-20)]

ii <- ii[,c(-1,-2, -10)]

colnames(ii) <- c("Region", "Area", "Risk", "Driver", "HomeCharging", "WorkCharging", 
                  "VehicleTech", 2015:2030)

ii[,7] <- as.character(ii[,7])