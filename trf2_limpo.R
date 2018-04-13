###TRF2

library(readr)
TRF2_limpo <- read_delim("C:/Users/Renato/Desktop/Projetos/Achados e Pedidos/Bases Relatório LAI/R-Colab/TRF2_limpo.csv",
                         ";", escape_double = FALSE, col_types = cols(DATA = col_date(format = "%d/%m/%Y")),
                         trim_ws = TRUE)
View(TRF2_limpo)