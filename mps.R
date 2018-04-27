# LDA Ministérios públicos
library(dplyr)
library(xlsx)

load("C:/Users/jvoig/OneDrive/Documentos/colab_tb/lista_df_mps.RData") 

mppi <- lista_mps_final[[1]]
mprj <- as.data.frame(lista_mps_final[[2]])

# No caso do PI, o número do pedido é por ano, 

mppi_base <- mppi %>%
  mutate(ano = format(data_da_entrada, "%Y"),
         protocolo = paste(ano , manifestacao, sep="."),
         protocolo = gsub(" *\\(.*?\\) *", "", protocolo)) %>%
  arrange(protocolo)
View(mppi_base)

mppi_base$protocolo[7] <- "2017.3"
mppi_base$data_da_entrada[7] <- "2017-01-23"
mppi_base$protocolo[69] <- "2015.19"
mppi_base$data_da_entrada[69] <- "2015-06-22"
mppi_base$data_da_entrada <- format(mppi_base$data_da_entrada, "%Y-%m-%d")

mppi_base %>% group_by(protocolo) %>% summarise(p = n()) %>% filter(p>1) #ok

mppi_base <- mppi_base %>%
  rename(data_do_pedido = data_da_entrada,
         pedido = teor) %>%
  mutate(esfera = "estadual",
         poder = "ministerio publico",
         orgao = "ministerio publico estadual do piaui",
         assunto = NA, outros = NA, atendimento = NA,
         nao_e_pedido_de_informacao = NA, contem_dados_pessoais = NA,
         pedido_pasta_do_anexo_pedido = NA, anexo_com_extensao_pedido = NA,
         pasta_do_anexo_resposta = NA,
         anexo_com_extensao_resposta = NA,
         data_recurso_1 = NA,
         recurso_1 = NA,
         pasta_do_anexo_recurso_1 = NA,
         anexo_com_extensao_recurso_1 = NA, 
         data_resposta_recurso_1 = NA,
         resposta_recurso_1 = NA,
         pasta_do_anexo_resposta_recurso_1 = NA, 
         anexo_com_extensao_resposta_recurso_1 = NA,
         data_recurso_2 = NA,
         recurso_2 = NA, 
         pasta_do_anexo_recurso_2 = NA,
         anexo_com_extensao_recurso_2 = NA,
         data_resposta_recurso_2 = NA,
         resposta_recurso_2 = NA) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros, atendimento, 
         nao_e_pedido_de_informacao, contem_dados_pessoais, data_do_pedido, 
         pedido, pedido_pasta_do_anexo_pedido, anexo_com_extensao_pedido, 
         resposta, pasta_do_anexo_resposta, anexo_com_extensao_resposta, data_recurso_1, 
         recurso_1, pasta_do_anexo_recurso_1, anexo_com_extensao_recurso_1, data_resposta_recurso_1, 
         resposta_recurso_1, pasta_do_anexo_resposta_recurso_1, anexo_com_extensao_resposta_recurso_1, 
         data_recurso_2, recurso_2, pasta_do_anexo_recurso_2, anexo_com_extensao_recurso_2, 
         data_resposta_recurso_2,resposta_recurso_2) 

## MPRJ

mprj %>% group_by(numero_da_solicitacao) %>% summarise(p=n()) %>% filter(p>1) #ok

mprj_base <- mprj %>%
  rename(protocolo = numero_da_solicitacao,
         data_do_pedido = data_da_solicitacao,
         pedido = solicitacao) %>%
  mutate(esfera = "estadual",
         poder = "ministerio publico",
         orgao = "ministerio publico estadual do rio de janeiro", 
         assunto = NA, 
         outros = NA, 
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA, 
         contem_dados_pessoais = NA, 
         pedido_pasta_do_anexo_pedido = NA, 
         anexo_com_extensao_pedido = NA, 
         pasta_do_anexo_resposta = NA, 
         anexo_com_extensao_resposta = NA, 
         data_recurso_1 = NA, 
         recurso_1 = NA, 
         pasta_do_anexo_recurso_1 = NA, 
         anexo_com_extensao_recurso_1 = NA, 
         data_resposta_recurso_1 = NA, 
         resposta_recurso_1 = NA, 
         pasta_do_anexo_resposta_recurso_1 = NA, 
         anexo_com_extensao_resposta_recurso_1 = NA, 
         data_recurso_2 = NA, 
         recurso_2= NA , 
         pasta_do_anexo_recurso_2= NA, 
         anexo_com_extensao_recurso_2 = NA, 
         data_resposta_recurso_2 = NA,
         resposta_recurso_2 = NA) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros, 
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais, 
         data_do_pedido, pedido, pedido_pasta_do_anexo_pedido, anexo_com_extensao_pedido, 
         resposta, pasta_do_anexo_resposta, anexo_com_extensao_resposta, 
         data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, anexo_com_extensao_recurso_1, 
         data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1, 
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, 
         pasta_do_anexo_recurso_2, anexo_com_extensao_recurso_2, data_resposta_recurso_2,
         resposta_recurso_2) 

## Empilhando para o LDA

base_mps <- mprj_base %>%
  mutate(data_do_pedido = as.character(data_do_pedido)) %>%
  bind_rows(mppi_base)

setwd("C://Users//jvoig//OneDrive//Documentos//colab_tb")
save(base_mps, file= "base_mps.Rdata")
load("base_mps.Rdata")

write.xlsx(as.data.frame(base_mps), file="base_mps.xlsx", sheetName="base_mps",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
