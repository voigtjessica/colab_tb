library(readr)
library(janitor)
library(dplyr)
library(tidyr)

cldf2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CLDF/cldf2.xlsx", 
                    col_types = c("text", "date", "text", 
                                  "text", "text", "date", "text", "numeric"))

cldf_limpo <- cldf2 %>%
  clean_names() %>%
  rename(protocolo = numero_da_demanda,
         data_do_pedido = data,
         pedido = demanda) %>%
  mutate(anexo = NA,
         recurso_prim = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "distrital",
         poder = "legislativo",
         orgao = "camara legislativa do distrito federal") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao)  

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(cldf_limpo , file="cldf_final.Rdata")
