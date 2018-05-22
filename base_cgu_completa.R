library(dplyr)
library(data.table)

setwd("C:\\Users\\jvoig\\Documents\\Achados_e_pedidos\\20180129_Arquivos_csv_2017")

pedidos_cgu_2017_com_recurso <- fread("pedidos_cgu_2017_com_recurso.csv", encoding = "UTF-8", na.strings="")
pedidos_cgu_2017_sem_recurso <- fread("pedidos_cgu_2017_sem_recurso.csv", encoding = "UTF-8", na.strings="")
recursos_cgu <-fread("recursos_cgu.csv", encoding = "UTF-8", na.strings="")

glimpse(pedidos_cgu_2017_sem_recurso)
glimpse(pedidos_cgu_2017_com_recurso)
glimpse(recursos_cgu)

#Vou começar dos recursos e subir para os pedidos:

rec1 <- recursos_cgu %>%
  filter(interac == "Recurso - 1ª Instância") %>%
  rename(data_recurso_1 = data,
         recurso_1 = conteudo,
         pasta_do_anexo_recurso_1 = pastanx,
         anexo_com_extensao_recurso_1 = nomeanex,
         destino = orgao) %>%
  select(protocolo, destino, recurso_1, data_recurso_1, pasta_do_anexo_recurso_1, 
         anexo_com_extensao_recurso_1)

resp1 <- recursos_cgu %>%
  filter(interac == "Resposta do Recurso - 1ª Instância") %>%
  rename(data_resposta_recurso_1 = data,
         resposta_recurso_1 = conteudo,
         pasta_do_anexo_resposta_recurso_1 = pastanx,
         anexo_com_extensao_resposta_recurso_1 = nomeanex,
         destino = orgao) %>%
  select(protocolo, destino, data_resposta_recurso_1, resposta_recurso_1, 
         pasta_do_anexo_resposta_recurso_1,
         anexo_com_extensao_resposta_recurso_1)

rec2 <- recursos_cgu %>%
  filter(interac == "Recurso - 2ª Instância") %>%
  rename(data_recurso_2 = data,
         recurso_2 = conteudo,
         pasta_do_anexo_recurso_2 = pastanx,
         anexo_com_extensao_recurso_2 = nomeanex,
         destino = orgao) %>%
  select(protocolo, destino, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
         anexo_com_extensao_recurso_2)
  
resp2 <- recursos_cgu %>%
  filter(interac == "Resposta do Recurso - 2ª Instância") %>%
  rename(data_resposta_recurso_2 = data, 
         resposta_recurso_2 = conteudo, 
         pasta_do_anexo_resposta_recurso_2 = pastanx, 
         anexo_com_extensao_resposta_recurso_2 = nomeanex,
         destino = orgao) %>%
  select(protocolo, destino, data_resposta_recurso_2, resposta_recurso_2, 
         pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2)

rec3 <- recursos_cgu %>%
  filter(interac == "Recurso - 3ª Instância") %>%
  rename(data_recurso_3 = data, 
         recurso_3 = conteudo, 
         pasta_do_anexo_resposta_recurso_3 = pastanx, 
         anexo_com_extensao_resposta_recurso_3 = nomeanex,
         destino = orgao) %>%
  select(protocolo, destino, data_recurso_3 , recurso_3,pasta_do_anexo_resposta_recurso_3, 
         anexo_com_extensao_resposta_recurso_3)
         
resp3 <- recursos_cgu %>%
  filter(interac == "Resposta do Recurso - 3ª Instância") %>%
  rename(data_resposta_recurso_3 = data, 
         resposta_recurso_3 = conteudo, 
         anexo_com_extensao_recurso_3 = nomeanex,
         pasta_do_anexo_resposta_recurso_3 = pastanx,
         destino = orgao) %>%
  select(protocolo,destino, data_resposta_recurso_3 , resposta_recurso_3,
         anexo_com_extensao_recurso_3, pasta_do_anexo_resposta_recurso_3 )
  
rec4 <- recursos_cgu %>%
  filter(interac == "Recurso - 4ª Instância") %>%
  rename(data_recurso_4 = data, 
         recurso_4 = conteudo, 
         pasta_do_anexo_resposta_recurso_4 = pastanx, 
         anexo_com_extensao_resposta_recurso_4 = nomeanex) %>%
  select(protocolo, data_recurso_4 , recurso_4,pasta_do_anexo_resposta_recurso_4, 
         anexo_com_extensao_resposta_recurso_4)

resp4 <- recursos_cgu %>%
  filter(interac == "Resposta do Recurso - 4ª Instância") %>%
  rename(data_resposta_recurso_4 = data, 
         resposta_recurso_4 = conteudo, 
         anexo_com_extensao_recurso_4 = nomeanex,
         pasta_do_anexo_resposta_recurso_4 = pastanx) %>%
  select(protocolo, data_resposta_recurso_4 , resposta_recurso_4,
         anexo_com_extensao_recurso_4, pasta_do_anexo_resposta_recurso_4 )

recursos_base <- rec1 %>%
  left_join(resp1) %>%
  left_join(rec2) %>%
  left_join(resp2) %>%
  left_join(rec3) %>%
  left_join(resp3) %>%
  left_join(rec4) %>%
  left_join(resp4)
##################################################################

ped_com_rec_resp <- pedidos_cgu_2017_com_recurso %>%
  filter(interac == "Resposta do Pedido") %>%
  rename(data_da_resposta = data,
         resposta = conteudo,
         pasta_do_anexo_resposta = pastanx,
         anexo_com_extensao_resposta = nomeanex) %>%
  select(protocolo, data_da_resposta, resposta, 
         pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta) %>%
  distinct(protocolo, resposta, .keep_all = TRUE)

ped_com_rec <- pedidos_cgu_2017_com_recurso %>%
  filter(interac == "Pedido") %>%
  rename(data_do_pedido = data,
         pedido = conteudo,
         pasta_do_anexo_pedido = pastanx,
         anexo_com_extensao_pedido = nomeanex) %>%
  select(protocolo, data_do_pedido, 
         pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido) %>%
  distinct(protocolo, pedido, .keep_all = TRUE) %>%              #por alguma razão tinham protocolos duplicados
  left_join(ped_com_rec_resp)


ped_sem_resp <- pedidos_cgu_2017_sem_recurso %>%
  filter(interac == "Resposta do Pedido") %>%
  rename(data_da_resposta = data,
         resposta = conteudo,
         pasta_do_anexo_resposta = pastanx,
         anexo_com_extensao_resposta = nomeanex) %>%
  select(protocolo, data_da_resposta, resposta, 
         pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta) %>%
  distinct(protocolo, resposta, .keep_all = TRUE)

ped_sem <- pedidos_cgu_2017_sem_recurso %>%
  filter(interac == "Pedido") %>%
  rename(data_do_pedido = data,
         pedido = conteudo,
         pasta_do_anexo_pedido = pastanx,
         anexo_com_extensao_pedido = nomeanex) %>%
  select(protocolo, data_do_pedido, 
         pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido) %>%
  distinct(protocolo, pedido, .keep_all = TRUE)

base_cgu_completa <- ped_sem %>%
  left_join(ped_sem_resp) %>%
  bind_rows(ped_com_rec) %>%
  left_join(recursos_base) %>%
  mutate(esfera = "federal", 
         poder = "executivo", 
         orgao = "controladoria-geral da uniao",  
         assunto = NA , outros = NA ,
         atendimento = NA , nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , resposta_duplicada = NA )

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_cgu_completa, file="base_cgu_completa.Rdata")
