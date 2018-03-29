library(readr)
library(janitor)

camsalvador <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Câmara de Salvador/camsalvador.xlsx", 
                          col_types = c("numeric", "date", "text", 
                                        "date", "text", "text"))

#Corrigindo as datas, vou ter que importar a nova planilha como character nas datas

camsalvador2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/Câmara de Salvador/camsalvador.xlsx")

id_contato <- c(13, 35)

data_contato <- camsalvador2 %>%
  clean_names() %>%
  select(caso, data_contato) %>%
  filter(caso %in% id_contato) %>%
  mutate(data_contato = ifelse(caso == 13, "10/03/2012", "30/05/2014"),
         data_contato = as.Date(data_contato, "%d/%m/%Y"))

id_resposta <- c(19,20,8,4,1,21,2,3,17,13,16,15,10,22)

data_resposta <- camsalvador2 %>%
  clean_names() %>%
  select(caso, data_resposta) %>%
  filter(caso %in% id_resposta) %>%
  mutate(data_resposta = gsub("\\s*\\([^\\)]+\\)", "", data_resposta),
         data_resposta= ifelse(caso == 13, "10/09/2012", data_resposta),
         data_resposta = as.Date(data_resposta, "%d/%m/%Y"))

##Não consegui inserir as duas datas corrigidas na mesma variável. Consertar isso depois

camsalvador_limpo <- camsalvador %>%
  clean_names() %>%
  rename(protocolo = caso,
         pedido = solicitacao,
         data_do_pedido = data_contato,
         data_da_resposta = data_resposta,
         anexo = anexos) %>%
  mutate(recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         esfera = "municipal",
         poder = "legislativo",
         orgao = "camara municipal de salvador")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(camsalvador_limpo, file="camsalvador_final.Rdata")
