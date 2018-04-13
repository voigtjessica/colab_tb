library(readxl)
library(dplyr)

tjsp_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/TJSP/tjsp_limpo.xls", 
                         col_types = c("text", "text", "text", 
                                       "numeric", "text")) %>%
  select(-c(X__1)) %>%
  clean_names() %>%
  rename(pedido = detalhamento_do_assunto,
         data_do_pedido = entrada) %>%
  mutate(data_da_resposta = NA,
         anexo = NA,
         recurso_prim = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "estadual",
         poder = "jusiciario",
         orgao = "tribunal de justica de sao paulo")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(tjsp_limpo , file="tjsp_limpo.Rdata")
