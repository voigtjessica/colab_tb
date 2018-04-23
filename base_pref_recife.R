#Recife maravilindos

library(readr)
pedidostransparencia2012 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2012.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE)

pedidostransparencia2013 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2013.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE)

pedidostransparencia2014 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2014.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE)

pedidostransparencia2015 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2015.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE) 
  

pedidostransparencia2016 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2016.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE)

pedidostransparencia2017 <- read_delim("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Pref Recife/pedidostransparencia2017.csv", 
                                       ";", escape_double = FALSE, col_types = cols(ano = col_character()), 
                                       trim_ws = TRUE)


base_pref_recife <-  bind_rows(pedidostransparencia2015) %>%
  bind_rows(pedidostransparencia2016) %>%
  bind_rows(pedidostransparencia2017) %>%
  select(-c(X36, X37, X38)) %>%
  bind_rows(pedidostransparencia2012) %>%
  bind_rows(pedidostransparencia2013) %>%
  bind_rows(pedidostransparencia2014) %>%
  rename(protocolo = numero,
         data_do_pedido = data_pedido,
         pedido = descricao,
         anexo_com_extensao_pedido = arquivo_pedido,
         data_da_resposta = data_resposta,
         anexo_com_extensao_resposta = arquivos_resposta_pedido,
         anexo_com_extensao_recurso_1 = arquivo_1o_recurso,
         recurso_1 = descricao_1o_recurso,
         resposta_recurso_1 = resposta_1o_recurso,
         data_resposta_recurso_1 = data_resp_1o_recurso,
         anexo_com_extensao_resposta_recurso_1 = arquivos_resposta_1o_recurso,
         anexo_com_extensao_recurso_2 = arquivo_2o_recurso,
         recurso_2 = descricao_2o_recurso,
         resposta_recurso_2 = resposta_2o_recurso,
         data_resposta_recurso_2 = data_resp_2o_recurso,
         anexo_com_extensao_resposta_recurso_2 = arquivos_resposta_2o_recurso) %>%
  mutate(esfera = "municipal", 
         poder = "executivo", 
         orgao = "prefeitura municipal do recife",  
         assunto = NA , outros = NA ,
         atendimento = NA , nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , 
         resposta_duplicada = NA , pasta_do_anexo_pedido = NA ,
         pasta_do_anexo_resposta = NA ,
         data_recurso_1 = NA , 
         pasta_do_anexo_recurso_1 = NA ,
         pasta_do_anexo_resposta_recurso_1 = NA ,
         data_recurso_2 = NA ,  pasta_do_anexo_recurso_2 = NA ,
         pasta_do_anexo_resposta_recurso_2 = NA ,
         data_recurso_3 = NA , recurso_3 = NA , 
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

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_pref_recife, file="base_pref_recife.Rdata")
