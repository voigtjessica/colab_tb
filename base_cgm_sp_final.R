# CGM 

library(readxl)
library(data.table)
library(dplyr)
library(janitor)
`%notin%` = function(x,y) !(x %in% y)

cgm_sp <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Controladoria Geral do Município de São Paulo - 08022017.xlsx")

base_cgm_sp <- cgm_sp %>%
  clean_names() %>%
  select(-c(titulo_do_pedido_copiar_o_mesmo_para_os_respectivos_resposta_recurso_etc,
            foi_prorrogado,
            orgao,
            status,
            municipio_nao_preencher_quando_for_nivel_estadual_e_federal,
            sigla_da_uf_nao_preecher_para_nivel_federal,
            id)) %>%
  rename(esfera = nivel_de_governo,
         poder = esfera_de_governo,
         pastanex = nome_da_pasta_que_contem_os_arquivos_anexos,
         arqanex =  nome_dos_arquivos_anexos_que_estao_na_pasta_da_id_referente_a_esta_linha_separados_por_ponto_e_virgula) %>%  
  mutate(protocolo  = as.character(protocolo),
         pastanex = as.character(pastanex),
         arqanex = as.character(arqanex),
         data_de_envio = as.Date(data_de_envio, format="%d-%m-%Y"))
         
#normalmente, eu daria um spread no tipo de interação, o problema é que cada uma das outras
#colunas vira uma nova coluna a depender do tipo de interação. Por essa razão eu prefiro 
#dividir as planilhas e usar o leftjoin pelo número de protocolo.

#Pedidos

nao_anonimizados_dup <- c(6159,6185,6207,6320,6371)

ped <- base_cgm_sp %>%
  rename(data_do_pedido = data_de_envio,
         pedido = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_pedido = pastanex,
         anexo_com_extensao_pedido = arqanex) %>%
  filter(tipo_de_interacao == "Pedido",
         pedido != "0") %>%
  distinct(protocolo, pedido, .keep_all=TRUE) %>%
  mutate(id = 1:n()) %>%
  filter(id %notin% nao_anonimizados_dup) %>%
  select(-c(tipo_de_interacao, id))
  
resp <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Resposta do Pedido") %>%
  rename(data_da_resposta = data_de_envio,
         resposta = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_resposta = pastanex,
         anexo_com_extensao_resposta = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))

rec1 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Recurso - 1ª Instância") %>%
  rename(recurso1 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         data_recurso_1 = data_de_envio,
         pasta_do_anexo_recurso_1 = pastanex,
         anexo_com_extensao_recurso_1 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))

resp_rec1 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Resposta do Recurso - 1ª Instância") %>%
  rename(data_resposta_recurso_1 = data_de_envio,
         resposta_recurso_1 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_resposta_recurso_1 = pastanex,
         anexo_com_extensao_resposta_recurso_1 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))

rec2 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Recurso - 2ª Instância") %>%
  rename(data_recurso_2 = data_de_envio,
         recurso_2 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_recurso_2 = pastanex,
         anexo_com_extensao_recurso_2 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))

resp_rec2 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Resposta do Recurso - 2ª Instância") %>%
  rename(data_resposta_recurso_2 = data_de_envio,
         resposta_recurso_2 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_resposta_recurso_2 = pastanex,
         anexo_com_extensao_resposta_recurso_2 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))
  
rec3 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Recurso - 3ª Instância") %>%
  rename(data_recurso_3 = data_de_envio,
         recurso_3 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_recurso_3 = pastanex,
         anexo_com_extensao_recurso_3 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))

resp_rec3 <- base_cgm_sp %>%
  filter(tipo_de_interacao == "Resposta do Recurso - 3ª Instância") %>%
  rename(data_resposta_recurso_3 = data_de_envio,
         resposta_recurso_3 = conteudo_do_pedido_resposta_recurso_etc_um_para_cada_id,
         pasta_do_anexo_resposta_recurso_3 = pastanex,
         anexo_com_extensao_resposta_recurso_3 = arqanex) %>%
  select(-c(tipo_de_interacao, poder, esfera, situacao))
  

base_cgm_sp_final <- ped %>%
  left_join(resp, by="protocolo") %>%
  left_join(rec1, by="protocolo") %>%
  left_join(resp_rec1, by="protocolo") %>%
  left_join(rec2, by="protocolo") %>%
  left_join(resp_rec2, by="protocolo") %>%
  left_join(rec3, by="protocolo") %>%
  left_join(resp_rec3, by="protocolo") %>%
  distinct(protocolo, pedido, .keep_all = TRUE) %>%
  mutate(data_recurso_4 = NA ,
         recurso_4 = NA ,
         pasta_do_anexo_recurso_4 = NA ,
         anexo_com_extensao_recurso_4 = NA ,
         data_resposta_recurso_4 = NA ,
         resposta_recurso_4 = NA ,
         pasta_do_anexo_resposta_recurso_4 = NA ,
         anexo_com_extensao_resposta_recurso_4 = NA,
         orgao = "controladoria-geral do municipio de sao paulo",
         assunto = NA, 
         outros = NA,
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA,           
         contem_dados_pessoais = NA, 
         e_complementacao_de_pedido = NA,
         resposta_duplicada = NA)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_cgm_sp_final, file="base_cgm_sp_final.Rdata")
