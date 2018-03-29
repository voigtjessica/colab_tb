library(dplyr)
library(janitor)

#pedidos
pedidos <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovAL/pedidos.xls", 
                      col_types = c("text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text"))
pedidos_limpo <- pedidos %>%
  clean_names() %>%
  rename(protocolo = processo,
         data_do_pedido = data,
         pedido = solicitacao,
         data_da_resposta = data_resposta) %>%
  mutate(anexo = NA,
         data_do_pedido = as.Date(data_do_pedido, "%d/%m/%Y"),
         data_da_resposta = as.Date(data_resposta, "%d/%m/%Y" ))

#recursos

recursos <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovAL/recursos.xls")

recursos_limpo <- recursos %>%
  clean_names() %>%
  rename(protocolo = solicitacao_origem,
         recurso_prim = solicitacao,
         data_recurso_prim = data,
         resposta_recurso_prim = resposta,
         data_resposta_recurso_prim = data_resposta) %>%
  mutate(data_recurso_prim  = as.Date(data_recurso_prim , "%d/%m/%Y"),
         data_resposta_recurso_prim = as.Date(data_resposta_recurso_prim, "%d/%m/%Y"))

#final
goval_limpo <- pedidos_limpo %>%
  left_join(recursos_limpo, by="protocolo") %>%
  mutate(resposta = gsub("x{2,}", "", resposta),
         resposta = gsub("X{2,}", "", resposta),
         resposta_recurso_prim = gsub("x{2,}", "", resposta_recurso_prim),
         resposta_recurso_prim = gsub("X{2,}", "", resposta_recurso_prim),
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "estadual",
         poder = "executivo",
         orgao = "governo estadual de alagoas") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao) 

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(goval_limpo , file="goval_final.Rdata")
