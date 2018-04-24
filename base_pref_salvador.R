# Prefeitura de Salvador

library(readxl)    
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

months_english <- function() list(JAN = "01", FEB = "02", MAR = "03")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\Pref Salvador")
pedidos_salvador <- read_excel_allsheets("pedidos_limpo.xlsx")

p2015 <- pedidos_salvador[[1]]
p2016 <- pedidos_salvador[[2]]
p2017 <- pedidos_salvador[[3]]

meses = c("mes, num", 
          "JAN, 01",
          "FEV, 02", 
          "MAR, 03",
          "ABR, 04",
          "MAI, 05",
          "JUL, 06",
          "JUN, 07",
          "AGO, 08",
          "SET, 09",
          "OUT, 10",
          "NOV, 11",
          "DEZ, 12")

meses  <- read.csv(text = meses , strip.white = TRUE, colClasses = c("character", "character"))

base_pref_salvador <- bind_rows(p2015, p2016, p2017) %>%
  clean_names() %>%
  left_join(meses) %>%
  mutate(dia_ficticio = "15", #criei um dia fictício, já que a prefeitura só forneceu mês e ano.
         data_do_pedido = paste(ano, num, dia_ficticio, sep="-"),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d")) %>%
  rename(pedido = descricao_da_manifestacao) %>%
  mutate(esfera = "municipal", 
         poder="executivo", 
         orgao="prefeitura municipal de Salvador", 
         assunto = NA , 
         outros = NA ,
         atendimento = NA , nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , resposta_duplicada = NA ,
         pasta_do_anexo_pedido = NA , 
         anexo_com_extensao_pedido = NA , data_da_resposta = NA , pasta_do_anexo_resposta = NA ,
         anexo_com_extensao_resposta = NA , data_recurso_1 = NA , recurso_1 = NA , 
         pasta_do_anexo_recurso_1 = NA ,
         anexo_com_extensao_recurso_1 = NA , data_resposta_recurso_1 = NA , 
         resposta_recurso_1 = NA , pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , data_recurso_2 = NA , 
         recurso_2 = NA , pasta_do_anexo_recurso_2 = NA ,
         anexo_com_extensao_recurso_2 = NA , data_resposta_recurso_2 = NA , 
         resposta_recurso_2 = NA , pasta_do_anexo_resposta_recurso_2 = NA , 
         anexo_com_extensao_resposta_recurso_2 = NA ,
         data_recurso_3 = NA , recurso_3 = NA , 
         data_resposta_recurso_3 = NA , resposta_recurso_3 = NA , 
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
save(base_pref_salvador, file="base_pref_salvador.Rdata")         
