#base_pref_porto_velho

library(readxl)
library(janitor)
base_prefeitura_porto_velho_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref P Velho/base_prefeitura_porto_velho_limpo.xlsx", 
                                                col_types = c("text", "text", "text", 
                                                              "text", "text", "text", "text", "numeric", 
                                                              "text", "text"))

base_pref_porto_velho <- base_prefeitura_porto_velho_limpo %>%
  clean_names() %>%
  mutate(data_da_pergunta = as.Date(data_da_pergunta, format = "%d.%m.%Y"),
         data_da_resposta = as.Date(data_da_resposta, format = "%d.%m.%Y"),
         recurso_1ª_instancia = ifelse(recurso_1ª_instancia == "Não houve", NA, recurso_1ª_instancia),
         recurso_2ª_instancia = ifelse(recurso_2ª_instancia == "Não  houve" |
                                         recurso_2ª_instancia == "Não houve", NA, recurso_2ª_instancia))%>%
  rename(protocolo = numero_do_protocolo, 
         data_do_pedido = data_da_pergunta,
         recurso_1 = recurso_1ª_instancia,
         pedido = perguntas,
         resposta = respostas,
         resposta_recurso_1 = resposta,
         recurso_2 = recurso_2ª_instancia,
         resposta_recurso_2 = resposta_1,
         anexo_com_extensao_resposta = anexos) %>%
  mutate(esfera = "municipal", 
         poder = "executivo", 
         orgao = "prefeitura municipal de porto velho", 
         assunto = NA , 
         outros = NA , 
         atendimento = NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , 
         resposta_duplicada = NA , pasta_do_anexo_pedido = NA ,
         anexo_com_extensao_pedido = NA , pasta_do_anexo_resposta = NA ,
         data_recurso_1 = NA , pasta_do_anexo_recurso_1 = NA , 
         anexo_com_extensao_recurso_1 = NA , data_resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA , 
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , pasta_do_anexo_recurso_2 = NA ,
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA , 
         pasta_do_anexo_resposta_recurso_2 = NA , 
         anexo_com_extensao_resposta_recurso_2 = NA ,
         data_recurso_3 = NA , 
         recurso_3 = NA , 
         data_resposta_recurso_3 = NA , 
         resposta_recurso_3 = NA , 
         anexo_com_extensao_recurso_3 = NA )

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_pref_porto_velho, file="base_pref_porto_velho.Rdata")
