#Gov MG

library(dplyr)
library(data.table)
library(readxl)
library(janitor)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\GovMG")

pedidos_2012 <- fread("relatorio_pedidos_2012_limpo.csv")

pedidos_2013 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovMG/relatorio_pedidos_2013.xlsx", 
                                     col_types = c("text", "date", "text", 
                                                   "text", "date", "date", "date", "text", 
                                                   "date", "text", "date", "text", "text"))
pedidos_2014 <- fread("relatorios_pedidos_2014_limpo.csv")

pedidos_2015_1<- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovMG/relatorio_pedidos_2015- até 13-10.xlsx", 
                            col_types = c("text", "date", "text", 
                                          "text", "text", "text", "text", "date", 
                                          "date", "date", "text", "numeric", 
                                          "text", "date", "text", "text", "date", 
                                          "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "text", "text", 
                                          "text", "date", "text", "text", "text", 
                                          "text"))


## Sistema novo 2015
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\GovMG\\2015_out_dez")

anexo_pedidos_2015 <- fread("anexo_pedidos_2015.csv")
anexos_recursos_2015 <- fread("anexos_recursos_2015.csv")
anexos_respostas_pedidos <- fread("anexos_respostas_pedidos.csv")
pedidos_2015_2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovMG/2015_out_dez/pedidos_2015.xlsx")
recursos_2015 <- fread("recursos_2015.csv") #outubro a dezembro


## Sistema novo 2016
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\GovMG\\pedidos_1o_sem_2016")

anexo_pedidos_2016 <- fread("anexo_pedidos_2016.csv")
anexo_recurso_2016  <- fread("anexo_recurso.csv")
anexo_resposta_pedido_2016 <- fread("anexo_resposta_pedido.csv")
anexo_resposta_recurso_2016 <- fread("anexo_resposta_recurso.csv")
pedidos_2016 <- fread("pedidos_2016.csv")
recurso_2016 <- fread("recurso.csv")

# 2015: pedidos, recursos e anexos:

#a. deixando só o que interessa:

base_pedidos_2015_1 <- pedidos_2015_1 %>%   #não tem o recurso
  clean_names() %>%
  select(-c(categoria_demanda, subcategoria_demanda, encaminhamento, data_envio_ao_orgao, prazo_legal,
            prazo_estendido, controle_reclamacao, informou_reclamacao, encerrado, codigo_resposta, observacao,
            tempo_resposta_1,tempo_resposta_2, validacao_tempo_1, validacao_tempo_2, 
            prazo_de_20_dias_ultrapassado , 
            data_de_hoje, situacao_da_demanda_em_relacao_ao_prazo_de_20_dias, 
            situacao_da_demanda_em_relacao_ao_prazo_de_30_dias, 
            situacao_p_recurso_e_reclamacao, alerta_de_10_dias, prazo_de_30_dias_ultrapassado, 
            maior_tempo_de_resposta, orgao_destino)) %>%
  rename(data_do_pedido = data_recebimento,
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         data_resposta_recurso_1 = data_resposta_2,
         resposta_recurso_1 = resposta_2 ) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format = "%d/%m/%y"))


base_pedidos_2015_2 <- pedidos_2015_2  %>% 
  clean_names() %>%
  rename(protocolo = numero_protocolo,
         data_do_pedido = data_de_abertura,
         pedido = desc_do_pedido,
         data_da_resposta = data_registro_resposta,
         resposta = texto_da_resposta) %>%
  select(protocolo, data_do_pedido, pedido, data_da_resposta, resposta) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"))

names(anexo_pedidos_2015) <- c("anexo_com_extensao_pedido",
                               "protocolo",
                               "link_anexo_pedido")

names(anexos_recursos_2015) <- c("anexo_com_extensao_recurso_1",
                                 "protocolo",
                                 "link_anexo_recurso")
anexos_recursos_2015$protocolo <-  as.character(anexos_recursos_2015$protocolo)


names(anexos_respostas_pedidos) <- c("anexo_com_extensao_resposta",
                                     "protocolo",
                                     "link_anexo_resposta")
anexos_recursos_2015$protocolo <- as.character(anexos_recursos_2015$protocolo)

recursos_2015_prim <- recursos_2015 %>%
  clean_names() %>%
  filter(instancia_do_recurso == "Primeira Instância") %>%
  rename(protocolo = numero_do_protocolo,
         data_recurso_1 = data_abertura,
         recurso_1 = texto_recurso,
         resposta_recurso_1 = texto_resposta_recurso,
         data_resposta_recurso_1 = data_registro_resposta) %>%
  select(protocolo, data_recurso_1, recurso_1, resposta_recurso_1, data_resposta_recurso_1)

recursos_2015_seg <- recursos_2015 %>%
  clean_names() %>%
  filter(instancia_do_recurso == "Segunda Instância") %>%
  rename(protocolo = numero_do_protocolo,
         data_recurso_2 = data_abertura,
         recurso_2 = texto_recurso,
         resposta_recurso_2 = texto_resposta_recurso,
         data_resposta_recurso_2 = data_registro_resposta) %>%
  select(protocolo, data_recurso_2, recurso_2, resposta_recurso_2, data_resposta_recurso_2)


#b. juntando tudo de 2015

pedidos_2015 <- base_pedidos_2015_2 %>%
  left_join(recursos_2015_prim, by = "protocolo") %>% 
  left_join(recursos_2015_seg, by="protocolo") %>%
  left_join(anexo_pedidos_2015, by="protocolo") %>% 
  left_join(anexos_recursos_2015, by="protocolo") %>%
  left_join(anexos_respostas_pedidos, by="protocolo") %>% 
  bind_rows(base_pedidos_2015_2)

rm(pedidos_2015_1)
rm(pedidos_2015_2)

#Ufa! 2016:

pedidos_2016 <- pedidos_2016 %>%
clean_names() %>%
  rename(protocolo = numero_protocolo,
         data_do_pedido = data_de_abertura,
         pedido = desc_do_pedido,
         data_da_resposta = data_registro_resposta,
         resposta = texto_da_resposta) %>%
  select(protocolo, data_do_pedido, pedido, data_da_resposta, resposta) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"))

names(anexo_pedidos_2016) <- c("anexo_com_extensao_pedido",
                               "protocolo",
                               "link_anexo_pedido")

anexo_recurso_2016 <- anexo_recurso_2016 %>%
  clean_names() %>%
  rename(protocolo = numero_do_protocolo,
         anexo_com_extensao_recurso_1 = nome_do_arquivo,
         link_anexo_recurso = link_link_para_download) %>%
  select(protocolo, anexo_com_extensao_recurso_1, link_anexo_recurso)


names(anexo_resposta_pedido_2016) <- c("anexo_com_extensao_resposta",
                                     "protocolo",
                                     "link_anexo_resposta")


names(anexo_resposta_recurso_2016) <- c("anexo_com_extensao_resposta_recurso_1",
                                        "protocolo",
                                        "link_anexo_resposta_recurso_1")

recursos_2016_prim <- recurso_2016 %>%
  clean_names() %>%
  filter(instancia_do_recurso == "Primeira Instância") %>%
  rename(protocolo = numero_do_protocolo,
         data_recurso_1 = data_abertura,
         recurso_1 = texto_recurso,
         resposta_recurso_1 = texto_resposta_recurso,
         data_resposta_recurso_1 = data_registro_resposta) %>%
  select(protocolo, data_recurso_1, recurso_1, resposta_recurso_1, data_resposta_recurso_1) %>%
  mutate(data_recurso_1 = as.Date(data_recurso_1, format="%d/%m/%Y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format="%d/%m/%Y"))


recursos_2016_seg <- recurso_2016 %>%
  clean_names() %>%
  filter(instancia_do_recurso == "Segunda Instância") %>%
  rename(protocolo = numero_do_protocolo,
         data_recurso_2 = data_abertura,
         recurso_2 = texto_recurso,
         resposta_recurso_2 = texto_resposta_recurso,
         data_resposta_recurso_2 = data_registro_resposta) %>%
  select(protocolo, data_recurso_2, recurso_2, resposta_recurso_2, data_resposta_recurso_2) %>%
  mutate(data_recurso_2 = as.Date(data_recurso_2, format="%d/%m/%Y"),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2, format="%d/%m/%Y"))


pedidos_2016_final <- pedidos_2016 %>%
  left_join(anexo_pedidos_2016, by="protocolo") %>%
  left_join(anexo_recurso_2016, by="protocolo") %>%
  left_join(anexo_resposta_pedido_2016, by="protocolo") %>%
  left_join(anexo_resposta_recurso_2016, by="protocolo") %>%
  left_join(recursos_2016_prim, by="protocolo") %>%
  left_join(recursos_2016_seg, by="protocolo")

# rm(base_pedidos_2015_2) 
# rm(anexo_pedidos_2015)
# rm(anexos_recursos_2015)
# rm(anexos_respostas_pedidos)
# rm(recursos_2015_prim)
# rm(recursos_2015_seg)
# 
# rm(anexo_pedidos_2016)
# rm(anexo_recurso_2016)
# rm(anexo_resposta_pedido_2016)
# rm(anexo_resposta_recurso_2016)
# rm(recursos_2016_prim)
# rm(recursos_2016_seg)

pedidos_2016 <- pedidos_2016_final

# rm(pedidos_2016_final)

#Phew! 2012:

base_pedidos_2012 <- pedidos_2012 %>%
  clean_names() %>%
  rename(data_do_pedido = data_recebimento,
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         data_resposta_recurso_1 = data_resposta_2,
         resposta_recurso_1 = resposta_2 ) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format = "%d/%m/%y")) %>%
  select(-c(orgao_destino, prazo_20_dias, encerrado, observacao))

#2013

base_pedidos_2013 <- pedidos_2013 %>%
  clean_names() %>%
  rename(data_do_pedido = data_recebimento,
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         data_resposta_recurso_1 = data_resposta_2,
         resposta_recurso_1 = resposta_2 ) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format = "%d/%m/%y")) %>%
  select(-c(orgao_destino, prazo_20_dias, encerrado, observacao, prazo_extendido,
            data_envio_ao_orgao))

base_pedidos_2014 <- pedidos_2014 %>%
  clean_names() %>%
  rename(data_do_pedido = data_recebimento,
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         data_resposta_recurso_1 = data_resposta_2,
         resposta_recurso_1 = resposta_2 ) %>%
  select(protocolo, data_do_pedido, pedido, data_da_resposta, resposta,data_resposta_recurso_1,
         resposta_recurso_1) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format = "%d/%m/%y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d/%m/%y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format = "%d/%m/%y"))


p2012_2014 <- base_pedidos_2012 %>%
  rbind(base_pedidos_2013) %>%
  rbind(base_pedidos_2014) %>%
  mutate( data_recurso_1 = ifelse(is.na(data_resposta_recurso_1), NA, "não disponível"),
          recurso_1 = ifelse(is.na(data_resposta_recurso_1), NA, "não disponível"),
          data_recurso_2 = NA , recurso_2 = NA, resposta_recurso_2 = NA , 
          data_resposta_recurso_2 = NA , anexo_com_extensao_pedido = NA, 
          link_anexo_pedido = NA, anexo_com_extensao_recurso_1 = NA, 
          link_anexo_recurso = NA, anexo_com_extensao_resposta = NA, 
          link_anexo_resposta =  NA, anexo_com_extensao_resposta_recurso_1 = NA, 
          link_anexo_resposta_recurso_1 = NA)  

base_gov_mg <- pedidos_2015 %>%
  mutate(anexo_com_extensao_resposta_recurso_1 = NA,
         link_anexo_resposta_recurso_1 = NA) %>%
  rbind(pedidos_2016) %>%
  rbind(p2012_2014) %>%
  mutate(pedido_0_1 = ifelse(is.na(link_anexo_pedido), pedido, 
                          paste(pedido, link_anexo_pedido, sep=" Link do anexo do pedido em: ")),
         recurso_1_1 = ifelse(is.na(link_anexo_recurso), recurso_1, 
                              paste(recurso_1, link_anexo_recurso, sep=" Link do anexo do recurso em: ")),
         resposta_0_1 = ifelse(is.na(link_anexo_resposta), resposta, 
                               paste(resposta, link_anexo_resposta, sep=" Link do anexo da resposta em: ")),
         resposta_recurso_1_1 = ifelse(is.na(link_anexo_resposta_recurso_1), recurso_1,
                                       paste(recurso_1, link_anexo_resposta_recurso_1, sep=" Link do anexo da resposta do recurso em: "))) %>%
  select(-c(pedido, recurso_1, resposta, resposta_recurso_1, link_anexo_pedido, 
            link_anexo_recurso, link_anexo_resposta, link_anexo_resposta_recurso_1)) %>%
  rename(pedido = pedido_0_1,
         recurso_1 = recurso_1_1,
         resposta = resposta_0_1,
         resposta_recurso_1 = resposta_recurso_1_1) %>%
  mutate(esfera = "estadual", 
         poder = "executivo", 
         orgao = "governo do estado de minas gerais", 
         assunto = NA, 
         outros = NA,
         atendimento  = NA, 
         nao_e_pedido_de_informacao  = NA, 
         contem_dados_pessoais  = NA,
         e_complementacao_de_pedido  = NA, 
         resposta_duplicada = NA, 
         pasta_do_anexo_pedido = NA,
         pasta_do_anexo_resposta = NA,
         pasta_do_anexo_recurso_1 = NA,
         pasta_do_anexo_resposta_recurso_1 = NA,
         pasta_do_anexo_recurso_2 = NA,
         anexo_com_extensao_recurso_2 = NA, 
         pasta_do_anexo_resposta_recurso_2 = NA, 
         anexo_com_extensao_resposta_recurso_2 = NA,
         data_recurso_3 = NA, 
         recurso_3 = NA, 
         data_resposta_recurso_3 = NA, 
         resposta_recurso_3 = NA, 
         anexo_com_extensao_recurso_3 = NA)


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_gov_mg, file="base_gov_mg.Rdata")
