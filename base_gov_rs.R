#Base RS
#Vou usar a base que o Manoel já tinha adaptado.

library(dplyr)
library(janitor)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\GovRS")

load("pedidos_rs.Rdata")

glimpse(lista_pedidos_final) #melhor que head

base_gov_rs <- lista_pedidos_final %>%
  mutate(data_pedido = gsub("DATA INÍCIO PREVISTO: ", "", data_pedido),
         data_resposta = gsub("DATA FIM REAL: ", "", data_resposta),
         pedido = gsub("DESCRIÇÃO: ", "", pedido),
         data_pedido = as.Date(data_pedido, format="%d/%m/%Y"),
         data_resposta = as.Date(data_resposta, format="%d/%m/%Y")) %>%
  rename(data_da_resposta = data_resposta,
         data_do_pedido = data_pedido,
         protocolo = num_pedido) %>%
  mutate(esfera = "estadual", poder = "executivo", 
         orgao="governo estadual do rio grande do sul", 
         assunto = NA , outros = NA ,
         atendimento = NA , nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , resposta_duplicada = NA , 
         pasta_do_anexo_pedido = NA ,
         anexo_com_extensao_pedido = NA , pasta_do_anexo_resposta = NA , 
         anexo_com_extensao_resposta = NA , data_recurso_1 = NA , 
         recurso_1 = NA , pasta_do_anexo_recurso_1 = NA ,
         anexo_com_extensao_recurso_1 = NA , 
         data_resposta_recurso_1 = NA , resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , recurso_2 = NA , pasta_do_anexo_recurso_2 = NA ,
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA , resposta_recurso_2 = NA , 
         pasta_do_anexo_resposta_recurso_2 = NA , 
         anexo_com_extensao_resposta_recurso_2 = NA ,
         data_recurso_3 = NA , recurso_3 = NA , 
         data_resposta_recurso_3 = NA , 
         resposta_recurso_3 = NA , anexo_com_extensao_recurso_3 = NA ) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros,
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais,
         e_complementacao_de_pedido, resposta_duplicada,data_do_pedido, pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido, data_da_resposta, resposta, pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta, data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, 
         anexo_com_extensao_recurso_1, data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1,
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
         anexo_com_extensao_recurso_2, data_resposta_recurso_2, resposta_recurso_2, pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
         data_recurso_3, recurso_3, data_resposta_recurso_3, resposta_recurso_3, anexo_com_extensao_recurso_3)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_gov_rs, file="base_gov_rs.Rdata")
