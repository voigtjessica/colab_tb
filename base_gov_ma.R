#gov Maranhão

library(data.table)
library(dplyr)
library(tidyr)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\zzGovMA")
pedidos <- fread("Planilha_Pedidos_Tratada_1.csv")

glimpse(pedidos)

names(pedidos) <- c("id_maranhao",
                    "secretaria",
                    "pedido",
                    "status",
                    "protocolo",
                    "data_do_pedido",
                    "data_da_resposta",
                    "classificacao",
                    "resposta")

recursos <- fread("Planilha_Recurso_Tratada2.csv")

names(recursos) <- c("id_maranhao",
                     "protocolo",
                     "justificativa_cidadao",
                     "secretaria",
                     "tipo_recurso",
                     "v6",
                     "recurso",
                     "status_governo",
                     "data_recurso",
                     "data_resposta_recurso",
                     "v11",
                     "resposta_recurso")

recursos1 <- recursos %>%
  filter(tipo_recurso == "1. Primeira Instância") %>%
  rename(recurso_1 = recurso,
         resposta_recurso_1 = resposta_recurso,
         data_recurso_1 = data_recurso,
         data_resposta_recurso_1 = data_resposta_recurso) %>%
  select(protocolo, recurso_1, resposta_recurso_1, data_recurso_1, data_resposta_recurso_1, id_maranhao )
    
recursos2 <- recursos %>%
  filter(tipo_recurso == "2. Segunda Instância") %>%
  rename(recurso_2 = recurso,
         resposta_recurso_2 = resposta_recurso,
         data_recurso_2 = data_recurso,
         data_resposta_recurso_2 = data_resposta_recurso) %>%
  select(protocolo, recurso_2, resposta_recurso_2, data_recurso_2, data_resposta_recurso_2, id_maranhao )    

recursos3 <- recursos %>%
  filter(tipo_recurso == "3. CMRI - Comissão Mista de Reavaliação de Informações") %>%
  rename(recurso_3 = recurso,
         resposta_recurso_3 = resposta_recurso,
         data_recurso_3 = data_recurso,
         data_resposta_recurso_3 = data_resposta_recurso) %>%
  select(protocolo, recurso_3, resposta_recurso_3, data_recurso_3, data_resposta_recurso_3, id_maranhao )  

recursos_final <- recursos1 %>%
  left_join(recursos2, by="protocolo") %>%
  left_join(recursos3, by="protocolo") %>%
  rename(id_recurso_ma1 = id_maranhao.x,
         id_recurso_ma2 = id_maranhao.y,
         id_recurso_ma3 = id_maranhao)


base_gov_ma <- pedidos %>%
  left_join(recursos_final, by="protocolo") %>%
  mutate(responsavel = NA, 
         esfera = NA, 
         poder = NA, 
         orgao = NA, 
         assunto = NA, 
         outros = NA,
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA, 
         contem_dados_pessoais = NA,
         e_complementacao_de_pedido = NA,
         resposta_duplicada = NA, 
         pasta_do_anexo_pedido = NA,
         anexo_com_extensao_pedido = NA,
         pasta_do_anexo_resposta = NA , 
         anexo_com_extensao_resposta  = NA,
         pasta_do_anexo_recurso_1 = NA ,
         anexo_com_extensao_recurso_1 = NA,
         pasta_do_anexo_resposta_recurso_1 = NA,
         anexo_com_extensao_resposta_recurso_1 = NA ,
         pasta_do_anexo_recurso_2 = NA,
         anexo_com_extensao_recurso_2 = NA,
         anexo_com_extensao_recurso_3 = NA,
         pasta_do_anexo_recurso_2 = NA,
         pasta_do_anexo_resposta_recurso_2 = NA,
         anexo_com_extensao_resposta_recurso_2 = NA) %>%
  select(responsavel, esfera, poder, orgao, protocolo, assunto, outros,
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais,
         e_complementacao_de_pedido, resposta_duplicada,data_do_pedido, pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido, data_da_resposta, resposta, pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta, data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, 
         anexo_com_extensao_recurso_1, data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1,
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
         anexo_com_extensao_recurso_2, data_resposta_recurso_2, resposta_recurso_2, pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
         data_recurso_3, recurso_3, data_resposta_recurso_3, resposta_recurso_3, anexo_com_extensao_recurso_3, id_maranhao, id_recurso_ma1 ,id_recurso_ma2, id_recurso_ma3)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_gov_ma, file="base_gov_ma.Rdata")
