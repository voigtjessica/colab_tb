library(readxl)
library(janitor)
library(dplyr)

####!!! ESSE ARQUIVO POSSUI PROBLEMAS: OS PROTOCOLOS DOS RECURSOS NÃO BATIAM COM OS PROTOCOLOS DOS PEDIDOS.
# POR ISSO AQUI SÃO CONSIDERADOS TODOS OS PEDIDOS, E ASSUME-SE QUE NENHUM POSSUI RECURSO.

#pedidos e recursos 2015

LAI00035_Base_STJ_ <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/STJ/LAI00035 (Base STJ).xlsx", 
                                 col_types = c("numeric", "text", "date", 
                                               "text", "text", "text", "text", "text", 
                                               "date"))

#2012:
X2012 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/STJ/limpos/2012.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "numeric", "date", "text", 
                                  "text", "text"))

#2013:
X2013 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/STJ/limpos/2013.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "text", "date", "text", "date", 
                                  "text"))

#2014:
X2014 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/STJ/limpos/2014.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "text", "date", "text", "date", 
                                  "text"))

#2015:
X2015 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/STJ/limpos/2015.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "text", "date", "text", "date", 
                                  "text"))

X2012 <- X2012 %>%
  clean_names() %>%
  rename(resposta_da_ouvidoria = resposta)

X2013 <- X2013 %>%
  clean_names() 

X2014 <- X2014 %>%
  clean_names()

X2015 <- X2015 %>%
  clean_names()

X2016 <- LAI00035_Base_STJ_ %>%
  clean_names() %>%
  filter(criado_em > "2015-12-31") %>%
  rename(codigo = number,
         data_de_cadastro = criado_em)

basestj <- X2012 %>%
  rbind(X2013) %>%
  rbind(X2014) %>%
  rbind(X2015) %>%
  rbind(X2016) %>% #unique(basestj$tipo)
  filter(tipo == "Informação" | tipo == "Pedido de Informações", # unique(basestj$situacao
         situacao != "REPETIDA", situacao != "CANCELADA") %>%
  rename(protocolo = codigo,
         data_do_pedido = data_de_cadastro, 
         pedido = descricao, 
         resposta = resposta_da_ouvidoria,
         data_da_resposta = data_de_conclusao) %>%
  mutate(anexo = NA,
         recurso_prim = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "federal",
         poder = "judiciario",
         orgao = "supremo tribunal de justica") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao)  


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(basestj , file="basestj_limpa.Rdata")
