library(readr)
library(janitor)
library(dplyr)
library(tidyr)

#1
X170622_000123a <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CamDep/originais/170622-000123a.xlsx", 
                              col_types = c("numeric", "text", "text", 
                                            "text", "text", "text"))
#2
X170622_000123b <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CamDep/originais/170622-000123b.xlsx", 
                              col_types = c("text", "text", "text", 
                                            "text", "text", "text"))
#3
X170622_000123c <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CamDep/originais/170622-000123c.xlsx", 
                              col_types = c("text", "text", "text", 
                                            "text", "text", "text"))

camdep_base <- X170622_000123a %>%
  rename(ID_CAMARA = ID_camara) %>%
  rbind(X170622_000123b) %>%
  rbind(X170622_000123c) %>%
  clean_names() %>%
  filter(tipo_de_interacao != "Resposta Automática" ) %>%
  mutate(texto = gsub("<.*?>", "", texto),
         texto = gsub("&nbsp;", "", texto),
         prot_resp1 = lag(protocolo),
         prot_tot = ifelse(is.na(protocolo), prot_resp1, protocolo), #não sei fazer isso automaticamente
         prot_tot = ifelse(is.na(prot_tot), lag(prot_tot), prot_tot),
         prot_tot = ifelse(is.na(prot_tot), lag(prot_tot), prot_tot),
         prot_tot = ifelse(is.na(prot_tot), lag(prot_tot), prot_tot),
         prot_tot = ifelse(is.na(prot_tot), lag(prot_tot), prot_tot),
         prot_tot = ifelse(is.na(prot_tot), lag(prot_tot), prot_tot)) %>%
  select(id_camara, prot_tot, texto, data_de_criacao_da_demanda,  data_da_interacao,
         tipo_de_interacao) %>%
  rename(protocolo = prot_tot)

demandas <- camdep_base %>%
  filter(tipo_de_interacao == "Demandante") %>%
  rename(pedido = texto,
         data_do_pedido = data_de_criacao_da_demanda) %>%
  select(protocolo, data_do_pedido, pedido) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, "%d/%m/%Y"))

respostas_recursos <- camdep_base %>%
  filter(tipo_de_interacao == "Resposta") %>%
  mutate(data_da_interacao = as.Date(data_da_interacao, "%d/%m/%Y")) %>%
  group_by(protocolo) %>%
  mutate(num_interacao = 1:n()) %>%
  mutate(tipo = ifelse(num_interacao == 1, "resposta",
                       ifelse(num_interacao == 2, "recurso_prim",
                              ifelse(num_interacao == 3, "resposta_recurso_prim",
                                     "recurso_seg")))) %>%
  spread(tipo, texto) %>%
  mutate(data = ifelse(num_interacao == 1, "data_da_resposta",
                       ifelse(num_interacao == 2, "data_recurso_prim",
                              ifelse(num_interacao == 3, "data_resposta_recurso_prim",
                                     "data_recurso_seg")))) %>%
  spread(data, data_da_interacao) %>%
  select(protocolo, num_interacao, data_da_resposta, resposta, recurso_prim,
         data_recurso_prim, resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg, data_recurso_seg) 
  
#respostas

respostas <- respostas_recursos %>%
  select(protocolo, data_da_resposta, resposta) %>%
  mutate(anexo = NA) %>%
  filter(!is.na(resposta))

#recurso prim

recprim <- respostas_recursos %>%
  select(protocolo, recurso_prim, data_recurso_prim) %>%
  filter(!is.na(recurso_prim))

#resp recurso seg

resprecprim <- respostas_recursos %>%
  select(protocolo, resposta_recurso_prim, data_resposta_recurso_prim) %>%
  filter(!is.na(resposta_recurso_prim))

#recurso seg

recpseg <- respostas_recursos %>%
  select(protocolo, recurso_seg, data_recurso_seg) %>%
  filter(!is.na(recurso_seg))

#final

camdep_final <- demandas %>%
  left_join(respostas, by="protocolo") %>%
  left_join(recprim, by="protocolo") %>%
  left_join(resprecprim, by="protocolo") %>%
  left_join(recpseg, by="protocolo") %>%
  mutate(esfera = "federal",
         poder = "legislativo",
         orgao = "camara dos deputados")
  
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(camdep_final, file="camdep_final.Rdata")
