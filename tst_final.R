library(readxl)
library(dplyr) 
library(janitor)
library(stringr)

tst_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/TST/tst_limpo.xlsx", 
                        col_types = c("text", "date", "text", 
                                      "text", "date", "text", "date", "text", 
                                      "date", "text", "numeric"))

tst_final <- tst_limpo %>%
  clean_names() %>%
  rename(data_do_pedido = data_recebimento,
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         data_resposta_recurso_prim = data_resposta_2,
         resposta_recurso_prim = resposta_2) %>%
  mutate(anexo = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "federal",
         poder = "judiciário",
         orgao = "tribunal superior do trabalho") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao) 

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(tst_final , file="tst_final.Rdata")
