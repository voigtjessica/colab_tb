# Prefeitura de João Pessoa
library(readODS)
library(janitor)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\Pref J Pessoa")

pedidos_pref_jp <- read_ods("demandas_sic.ods", na = "NULL")

base_pref_jp <- pedidos_pref_jp %>%
  clean_names() %>%
  rename(data_do_pedido = criacao,
         data_da_resposta = fechamento,
         pedido = mensagem,
         anexo_com_extensao_resposta = anexo) %>%
  mutate(protocolo = as.character(numero),
         esfera = "municipal", 
         poder = "executivo", 
         orgao = "prefeitura municipal de joao pessoa", 
         assunto = NA , outros = NA ,
         atendimento = NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , 
         resposta_duplicada = NA , 
         pasta_do_anexo_pedido = NA ,
         anexo_com_extensao_pedido = NA , 
         pasta_do_anexo_resposta  = NA ,
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
         data_recurso_3 = NA , recurso_3 = NA , 
         data_resposta_recurso_3 = NA , 
         resposta_recurso_3 = NA , 
         anexo_com_extensao_recurso_3 = NA )

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_pref_jp, file="base_pref_jp.Rdata")
