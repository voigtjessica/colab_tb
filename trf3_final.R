#TRF3
library(dplyr) 
library(janitor)
library(readxl)
library(tidyr)

Base_TRF3 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/TRF3/Base TRF3.xlsx", 
                        col_types = c("text", "date", "text", 
                                      "text", "text", "text"))

trf3 <- Base_TRF3 %>%
  clean_names() %>%
  mutate(conteudo = gsub("&gt;", "", conteudo),
         conteudo = gsub("n&atilde;o", "", conteudo),
         conteudo = gsub("<.*?>", "", conteudo)) %>%
  group_by(processo) %>%
  mutate(num_interacao = 1:n(),
         tipo = ifelse(num_interacao == 1, "pedido",
                     ifelse(num_interacao == 2, "resposta",
                            ifelse(num_interacao == 3, "resposta_recurso_prim",
                                   "resposta_recurso_seg")))) %>%
  spread(tipo, conteudo) %>%
  mutate(data = ifelse(num_interacao == 1, "data_do_pedido",
                       ifelse(num_interacao == 2, "data_da_resposta",
                              ifelse(num_interacao == 3, "data_resposta_recurso_prim",
                                     "data_resposta_recurso_seg")))) %>%
  spread(data, data_geracao)
  
#pedido

pedido <- trf3 %>%
  filter(num_interacao == 1) %>%
  select(processo, pedido,data_do_pedido )

#resposta
resposta <- trf3 %>%
  filter(num_interacao == 2) %>%
  select(processo, resposta,data_da_resposta )

#resposta rec prim

resp_rec_prim <- trf3 %>%
  filter(num_interacao == 3) %>%
  select(processo,resposta_recurso_prim, data_resposta_recurso_prim )

#resp rec seg

resp_rec_seg <- trf3 %>%
  filter(num_interacao == 4) %>%
  select(processo, resposta_recurso_seg,data_resposta_recurso_seg )

trf3_final <- pedido %>%
  left_join(resposta, by ="processo") %>%
  left_join(resp_rec_prim, by ="processo") %>%
  left_join(resp_rec_seg, by="processo") %>%
  arrange(processo) %>%
  rename(protocolo = processo )%>%
  mutate(anexo = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "federal",
         poder = "judiciário",
         orgao = "tribunal regional federal da terceira regiao")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(trf3_final  , file="trf3_final.Rdata")
