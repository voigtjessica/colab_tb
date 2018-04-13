library(dplyr) 
library(janitor)
library(stringr)
load("tjpe_full.Rdata") #TJPE

tjpe_final <- tjpe %>%
  filter(!is.na(num_pedido),
         tipo != "Reclamação") %>%
  select(-c(tipo, situacao, categoria, titulo)) %>%
  mutate(data_do_pedido = as.Date(data_de_cadastro, "%d/%m/%Y"),
         data_de_conclusao = as.Date(data_de_conclusao, "%d/%m/%Y"),
         ano = format(data_do_pedido, "%Y"),
         protocolo = paste(ano, num_pedido, sep=".")) %>%
  group_by(protocolo) %>%
  mutate(num_interacao = 1:n()) %>%
  ungroup() %>%
  mutate(protocolo = ifelse(num_interacao == 2 & 
                              protocolo == "2015.003", "2016.003",
                            ifelse(num_interacao == 1 & 
                                     protocolo == "2015.014", "2016.014", 
                                   ifelse(protocolo =="NA.038", "2015.038", protocolo))),
         resposta = ifelse(is.na(resposta), resposta_ao_usuario, resposta),
         resposta = ifelse(is.na(resposta), resposta_da_ouvidoria, resposta),
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA,
         esfera = "estadual",
         poder = "judiciario",
         orgao = "tribunal de justica de pernambuco") %>%
  filter(protocolo != "NA.") %>%
  rename(pedido = descricao,
         data_da_resposta = data_de_conclusao,
         anexo = respostas_com_anexo) %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao) 

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(tjpe_final , file="tjpe_final.Rdata")   
