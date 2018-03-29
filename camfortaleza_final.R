library(readr)
library(janitor)
library(dplyr)
library(tidyr)

pedidos_camfor2 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CamFort/pedidos_camfor2.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

camfortaleza_limpo <- pedidos_camfor2 %>%
  clean_names() %>%
  rename(pedido = pergunta,
         data_do_pedido = data_do_recebimento) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, "%d/%m/%y"),
         protocolo = NA,
         anexo = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA, 
         data_recurso_seg = NA,
         esfera = "municipal",
         poder = "legislativo",
         orgao = "camara municipal de fortaleza") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(camfortaleza_limpo , file="camfortaleza_final.Rdata")
  