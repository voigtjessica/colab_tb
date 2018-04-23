#base_rio_branco

library(readxl)
library(dplyr)
library(janitor)

rio_branco_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Rio Branco/rio_branco_limpo.xlsx", 
                               col_types = c("numeric", "numeric", "numeric", 
                                             "text", "text", "text", "text", "date", 
                                             "date", "text", "date", "text", "text", 
                                             "text", "text", "text", "text"))

base_pref_rio_branco <- rio_branco_limpo %>%
  clean_names() %>%
  rename(protocolo = numero_do_pedido,
         pedido = informacao_pedido) %>%
  mutate(esfera = "municipal", 
         poder = "executivo", 
         orgao = "prefeitura municipal de rio branco", 
         assunto = NA , 
         outros = NA ,
         atendimento = NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
       e_complementacao_de_pedido, resposta_duplicada,pedido, pasta_do_anexo_pedido,
       anexo_com_extensao_pedido, resposta, pasta_do_anexo_resposta, 
       anexo_com_extensao_resposta, data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, 
       anexo_com_extensao_recurso_1, data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1,
       anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
       anexo_com_extensao_recurso_2, data_resposta_recurso_2, resposta_recurso_2, pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
       data_recurso_3, recurso_3, data_resposta_recurso_3, resposta_recurso_3, anexo_com_extensao_recurso_3)