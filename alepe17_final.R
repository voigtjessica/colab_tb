library(readr)
library(janitor)

alepe17 <- read_csv("C:/Users/jvoig/OneDrive/Documentos/text_mining/R-Colab/Preciso originais/Base ALEPE.csv")
#não tem protocolo
#não tem recursos

alepe17_limpo <- alepe17 %>% 
  clean_names() %>% 
  mutate(ID = 1:n()) %>%
  filter(ID!=7) %>%
  mutate(protocolo = NA, 
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         anexo = NA) %>%
  select(protocolo,
         solicitacao, 
         data_de_solicitacao, 
         resposta_oficial, 
         data_de_conclusao,
         recurso_prim,
         data_recurso_prim,
         resposta_recurso_prim,
         data_resposta_recurso_prim) %>%
  rename(pedido = solicitacao, data_do_pedido = data_de_solicitacao,
         resposta = resposta_oficial, data_da_resposta = data_de_conclusao) %>%
  mutate(esfera = "estadual",
         poder = "legislativo",
         orgao = "assembleia legislativa de pernambuco")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(alepe17_limpo, file="alepe17_final.Rdata")
