#Prefeitura Vitória

library(readxl)
library(dplyr)
library(janitor)

pref_vit_12 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2012.xls", 
                          col_types = c("text", "text", "text", 
                                        "text", "text"))

pref_vit_2013 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2013.xls", 
                            col_types = c("text", "text", "text", 
                                          "text", "text"))

pref_vit_2014 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2014.xls", 
                            col_types = c("text", "text", "text", 
                                          "text", "text"))

pref_vit_2015 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2015.xls", 
                            col_types = c("text", "text", "text", 
                                          "text", "text"))

# Por conta das horas, nos anos de 2016 e 2017 eu sou obrigada a converter de data
# pra texto pra data novamente. Não achei um jeito rápido de resolver isso.

pref_vit_2016 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2016.xls", 
                            col_types = c("text", "text", "text", 
                                          "date", "date")) %>%
  clean_names() %>%
  mutate(data_de_abertura = as.character(data_de_abertura),
         data_de_conclusao = as.character(data_de_conclusao),
         data_de_abertura = as.Date(data_de_abertura, format="%Y-%m-%d"),
         data_de_conclusao = as.Date(data_de_conclusao, format="%Y-%m-%d")) 


pref_vit_2017 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Vitoria/Relatório Geral 2017 - abertos até 31_03_17.xls", 
                        col_types = c("text", "text", "text", 
                                      "date", "date")) %>%
  clean_names() %>%
  mutate(data_de_abertura = as.character(data_de_abertura),
         data_de_conclusao = as.character(data_de_conclusao),
         data_de_abertura = as.Date(data_de_abertura, format="%Y-%m-%d"),
         data_de_conclusao = as.Date(data_de_conclusao, format="%Y-%m-%d")) 

#todos tem as datas com character, menos o arq de 2017

base_pref_vitoria <- bind_rows(pref_vit_12, pref_vit_2013,
                               pref_vit_2014) %>%
  clean_names() %>%
  mutate(data_de_abertura = as.character(data_de_abertura),
         data_de_conclusao = as.character(data_de_conclusao),
         data_de_abertura = as.Date(data_de_abertura, format="%d/%m/%Y"),
         data_de_conclusao = as.Date(data_de_conclusao, format="%d/%m/%Y")) %>%
  bind_rows(pref_vit_2016, pref_vit_2017) %>%
  rename(protocolo = numero_do_chamado,
         pedido = descricao_do_chamado,
         resposta = ultimo_historico,
         data_do_pedido = data_de_abertura,
         data_da_resposta = data_de_conclusao) %>%
  mutate(esfera = "municipal", 
         poder = "executivo", 
         orgao = "prefeitura municipal de vitoria", 
         assunto = NA , 
         outros = NA ,
         atendimento = NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , 
         resposta_duplicada = NA , 
         pasta_do_anexo_pedido = NA ,
         anexo_com_extensao_pedido = NA , 
         pasta_do_anexo_resposta = NA ,
         anexo_com_extensao_resposta = NA , 
         data_recurso_1 = NA , 
         recurso_1 = NA , 
         pasta_do_anexo_recurso_1 = NA ,
         anexo_com_extensao_recurso_1 = NA , 
         data_resposta_recurso_1 = NA , 
         resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , 
         recurso_2 = NA , 
         pasta_do_anexo_recurso_2 = NA ,
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA , 
         resposta_recurso_2 = NA , 
         pasta_do_anexo_resposta_recurso_2 = NA , 
         anexo_com_extensao_resposta_recurso_2 = NA ,
         data_recurso_3 = NA , 
         recurso_3 = NA , 
         data_resposta_recurso_3 = NA , 
         resposta_recurso_3 = NA , 
         anexo_com_extensao_recurso_3 = NA ) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros,
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais,
         e_complementacao_de_pedido, resposta_duplicada,data_do_pedido, pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido, data_da_resposta, resposta, pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta, data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, 
         anexo_com_extensao_recurso_1, data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1,
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
         anexo_com_extensao_recurso_2, data_resposta_recurso_2, resposta_recurso_2, pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
         data_recurso_3, recurso_3, data_resposta_recurso_3, resposta_recurso_3, anexo_com_extensao_recurso_3)

save(base_pref_vitoria, file="base_pref_vitoria.Rdata")


