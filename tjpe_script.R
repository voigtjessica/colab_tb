

library(data.table)
library(dplyr)
library(janitor)

setwd("C:\\Users\\mgaldino\\2018\\Achados e Pedidos\\Scripts")
tjpe17 <- fread("tjpe.txt", colClasses = "character")
tjpe16 <- fread("tjpe16.txt" , colClasses = "character")
tjpe15 <- fread("tjpe15.txt" , colClasses = "character")
tjpe14 <- fread("tjpe14.txt" , colClasses = "character")
tjpe13 <- fread("tjpe13.txt" , colClasses = "character")
tjpe12 <- fread("tjpe12.txt", colClasses = "character")

tjpe <- bind_rows(tjpe17, tjpe16, tjpe15, tjpe14, tjpe13, tjpe12)
tjpe <- tjpe %>%
  clean_names()

names(tjpe)[1] <- "num_pedido"

tjpe_pedidos <- tjpe %>%
  select(num_pedido, descricao)

save(tjpe_pedidos, file="tjpe_pedidos.RData")
