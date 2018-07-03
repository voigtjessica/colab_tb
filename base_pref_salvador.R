library(dplyr)
library(janitor)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\zzPref Salvador\\test2")

#Planilha de anexos:
anexos_salv <- read_excel("pedidos_e_anexos.xlsx") %>%
  clean_names() %>%
  select(protocolo, anexos)

#Planilha de pedidos:
pedidos_salvador_list <- read_excel_allsheets("pedidos.xlsx")

salv_2015 <- as.data.frame(pedidos_salvador_list[[1]]) %>%
  clean_names() %>%
  select(-c(x71))

salv_2016 <- as.data.frame(pedidos_salvador_list[[2]]) %>%
  clean_names() %>%
  select(-c(x132))

salv_2017 <- as.data.frame(pedidos_salvador_list[[3]]) %>%
  clean_names() %>%
  select(-c(x41))

teste_igualdade_nomes_var_df <- function(base1, base2) {
  
  x <- names(base1)
  y <- names(base2)
  n <- length(x)
  k <- length(y)
  
  teste_nome_igual_x <- numeric()
  teste_nome_igual_y <- numeric()
  
  for ( i in 1:n) {
    teste_nome_igual_x[i] <- x[i] %in% y
  }
  
  for ( i in 1:k) {
    teste_nome_igual_y[i] <- y[i] %in% x
  }
  resp_x <- paste(x[!as.logical(teste_nome_igual_x)], collapse = ", ")
  resp_y <- paste(y[!as.logical(teste_nome_igual_y)], collapse = ", ")
  
  print(paste("as variáveis em x que não estão em y, são:", resp_x,
              ". E as variáveris de y que não estão em x, são:", resp_y,
              sep=" "))
  
}

teste_igualdade_nomes_var_df(salv_2015, salv_2016)
teste_igualdade_nomes_var_df(salv_2015, salv_2017)

# Empilhando

base_pref_salvador <- salv_2015 %>%
  bind_rows(salv_2016, salv_2017)

#Acertando datas:

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

base_pref_salvador <- base_pref_salvador %>%
  left_join(meses, by="mes") %>%
  mutate(dia_ficticio = "15", #criei um dia fictício, já que a prefeitura só forneceu mês e ano.
         data_do_pedido = paste(ano, num, dia_ficticio, sep="-"),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d")) %>%
  left_join(anexos_salv, by="protocolo") %>%
  rename(pedido = descricao_da_manifestacao,
         anexo_com_extensao_resposta = anexos) %>%
  mutate(esfera = "municipal", 
         poder="executivo", 
         orgao="prefeitura municipal de Salvador", 
         assunto = NA , 
         outros = NA ,
         atendimento = NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         e_complementacao_de_pedido = NA , 
         resposta_duplicada = NA ,
         pasta_do_anexo_pedido = NA , 
         anexo_com_extensao_pedido = NA , 
         data_da_resposta = NA , 
         pasta_do_anexo_resposta = NA ,
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
         data_resposta_recurso_3 = NA , resposta_recurso_3 = NA , 
         anexo_com_extensao_recurso_3 = NA,
         situacao = NA, 
         pasta_do_anexo_recurso_3 = NA , 
         pasta_do_anexo_resposta_recurso_3 = NA , 
         anexo_com_extensao_resposta_recurso_3 = NA , 
         data_recurso_4 = NA , recurso_4 = NA , 
         pasta_do_anexo_recurso_4 = NA , 
         anexo_com_extensao_recurso_4 = NA , 
         data_resposta_recurso_4 = NA , 
         resposta_recurso_4 = NA , 
         pasta_do_anexo_resposta_recurso_4 = NA , 
         anexo_com_extensao_resposta_recurso_4 = NA ) %>%
  select(esfera, poder, orgao, protocolo, situacao, assunto, outros,
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais,
         e_complementacao_de_pedido, resposta_duplicada,data_do_pedido, pedido, pasta_do_anexo_pedido,
         anexo_com_extensao_pedido, data_da_resposta, resposta, pasta_do_anexo_resposta, 
         anexo_com_extensao_resposta, data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, 
         anexo_com_extensao_recurso_1, data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1,
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, pasta_do_anexo_recurso_2,
         anexo_com_extensao_recurso_2, data_resposta_recurso_2, resposta_recurso_2, pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
         data_recurso_3, recurso_3, data_resposta_recurso_3, resposta_recurso_3, anexo_com_extensao_recurso_3,
         pasta_do_anexo_recurso_3, pasta_do_anexo_resposta_recurso_3, 
         anexo_com_extensao_resposta_recurso_3, data_recurso_4, recurso_4, 
         pasta_do_anexo_recurso_4, anexo_com_extensao_recurso_4, 
         data_resposta_recurso_4, resposta_recurso_4, 
         pasta_do_anexo_resposta_recurso_4, 
         anexo_com_extensao_resposta_recurso_4)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_pref_salvador, file="base_pref_salvador.Rdata")         
