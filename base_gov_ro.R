#Base Gov Ro

library(readxl)
library(dplyr)
library(janitor)
library(xlsx)


setwd("C://Users//jvoig//OneDrive//Documentos//Colab//COLAB//GovRO")

PerguntasRespostas <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovRO/PerguntasRespostas.xlsx")


#OBS: encoding do arquivo de recurso está ruim no original!
RecursosResposta <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovRO/RecursosResposta.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text"))
 
# dates <- c("May 27 1984", "July 7 2005")
# betterDates <- as.Date(dates,
#                        format = "%B %d %Y")
# https://www.stat.berkeley.edu/~s133/dates.html
#Feb 17 2016 7:18PM


# corrigindo as datas
# os pedidos estão duplicados, vou corrigir isso tb

pedido <- PerguntasRespostas %>%
  clean_names() %>%
  mutate( data_registro = as.character(substr(data_registro, 0, 11)),
          data_resposta = as.character(substr(data_resposta, 0, 11)),
          data_registro = gsub("Jan", "01", data_registro),
          data_registro = gsub("Feb", "02", data_registro),
          data_registro = gsub("Mar", "03", data_registro),
          data_registro = gsub("Apr", "04", data_registro),
          data_registro = gsub("May", "05", data_registro),
          data_registro = gsub("Jun", "06", data_registro),
          data_registro = gsub("Jul", "07", data_registro),
          data_registro = gsub("Aug", "08", data_registro),
          data_registro = gsub("Sep", "09", data_registro),
          data_registro = gsub("Oct", "10", data_registro),
          data_registro = gsub("Nov", "11", data_registro),
          data_registro = gsub("Dec", "12", data_registro),
          data_resposta = gsub("Jan", "01", data_resposta),
          data_resposta = gsub("Feb", "02", data_resposta),
          data_resposta = gsub("Mar", "03", data_resposta),
          data_resposta = gsub("Apr", "04", data_resposta),
          data_resposta = gsub("May", "05", data_resposta),
          data_resposta = gsub("Jun", "06", data_resposta),
          data_resposta = gsub("Jul", "07", data_resposta),
          data_resposta = gsub("Aug", "08", data_resposta),
          data_resposta = gsub("Sep", "09", data_resposta),
          data_resposta = gsub("Oct", "10", data_resposta),
          data_resposta = gsub("Nov", "11", data_resposta),
          data_resposta = gsub("Dec", "12", data_resposta),
          data_do_pedido = as.Date(data_registro, format = "%m %d %Y "), 
          data_da_resposta = as.Date(data_resposta, format = "%m %d %Y "), 
          id_pedido = as.character(id_pedido),
          resposta = ifelse(is.na(anexo), resposta, paste(resposta, anexo, 
                                                           sep=" Acesse o anexo da resoposta em: "))) %>%
  rename(pedido = desc_pedido) %>%
  select(id_pedido, num_protocolo, data_do_pedido, pedido,data_da_resposta, resposta) %>%
  distinct(id_pedido, .keep_all = TRUE) #323
         
#Funcionou!!!!! To emocionada.

base_gov_ro <- pedido %>%
  left_join(recurso, by = "id_pedido")

x <- pedido %>%
  group_by(pedido) %>%
  summarise(p = n()) %>%
  filter(p>1)


recurso <- RecursosResposta %>%
  mutate(data_recurso_1 = as.Date(DataRegistro, format = "%B %d %Y"),
         data_resposta_recurso_1 = as.Date(DataRegistroResposta, format = "%B %d %Y"),
         IdPedido = as.character(IdPedido),
         recurso_1 = txtJustificativa,
         recurso_1 = gsub("Ã³", "ó", recurso_1),
         recurso_1 = gsub("Ã§", "ç", recurso_1),
         recurso_1 = gsub("Ã§", "ç", recurso_1),
         recurso_1 = gsub("Ãƒ", "ã", recurso_1),
         recurso_1 = gsub("Ã¢", "â", recurso_1),
         recurso_1 = gsub("Ãº", "ú", recurso_1),
         recurso_1 = gsub("â€œ", " ", recurso_1),
         recurso_1 = gsub("Ãµ", "õ", recurso_1),
         recurso_1 = gsub("Ã", "í", recurso_1),
         resposta_recurso_1 = RespostaRecurso,
         resposta_recurso_1 = gsub("Ã³", "ó", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ã§", "ç", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ã§", "ç", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ãƒ", "ã", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ã¢", "â", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ãº", "ú", resposta_recurso_1),
         resposta_recurso_1 = gsub("â€œ", " ", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ãµ", "õ", resposta_recurso_1),
         resposta_recurso_1 = gsub("Ã", "í", resposta_recurso_1),
         resposta_recurso_1 = ifelse(is.na(Anexo),RespostaRecurso, 
                                     paste(RespostaRecurso, Anexo, 
                                           sep=" Acesse o anexo da resposta do recurso em: "))) %>%
          clean_names()
  
  
  
  
  
  
  select(id_pedido, id_recurso, data_recurso_1,recurso_1, data_resposta_recurso_1, resposta_recurso_1)

#Parei porque eu não consigo resolver.
  