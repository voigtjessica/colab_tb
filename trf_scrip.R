

library(readxl)
library(dplyr)
setwd("C:\\Users\\mgaldino\\2018\\Achados e Pedidos\\Scripts")

tfr3 <- read_excel("Base TRF3_limpo.xlsx")
trf3 <- tfr3  %>%
  select(-X__1, -X__2)
head(trf3)
View(trf3)

save(trf3, file="trf3.RData")
