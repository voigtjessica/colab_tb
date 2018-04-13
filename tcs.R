#TCs

library(dplyr)
library(janitor)
library(googledrive)

#Primeiro, eu vou juntar todas as tabelas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("listya_df_tribunais_de_contas.Rdata")
names(lista)

#tce-go
tce_go <- as.data.frame(lista[[1]])
tce_go %>% group_by(id) %>% summarise(p =n()) %>% filter(p>1) #sem duplicatas

# Observações :
# 1. não entendi o que é a coluna [8] recurso

tce_go_base <- tce_go %>%
  rename(protocolo = id,
         data_do_pedido  = data_solicitacao,
         pedido = pedido_descricao,
         data_da_resposta = data_resposta,
         anexo_com_extensao_resposta = anexo_da_resposta,
         recurso_1 = desc_recurso_a,
         resposta_recurso_1 = resposta_recurso,
         anexo_com_extensao_recurso_1 = anexo_do_recurso) %>%
  mutate(esfera = "estadual" , 
         poder = "tribunal de contas" ,
         orgao  = "tribunal de contas estadual de goias" ,  
         assunto = NA ,
         outros = NA , 
         atendimento  = NA,
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         pedido_pasta_do_anexo_pedido  = NA , 
         anexo_com_extensao_pedido = NA ,
         pasta_do_anexo_resposta = NA , 
         data_recurso_1 = NA ,
         pasta_do_anexo_recurso_1 = NA ,
         data_resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , 
         recurso_2 = NA ,
         pasta_do_anexo_recurso_2 = NA , 
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , pasta_do_anexo_resposta_recurso_1 , 
         anexo_com_extensao_resposta_recurso_1 , data_recurso_2 , recurso_2 , 
         pasta_do_anexo_recurso_2 , anexo_com_extensao_recurso_2 , data_resposta_recurso_2,
         resposta_recurso_2) 


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tce_go_base, file="tce_go_base.Rdata")
rm(tce_go)


# TCE - PI

tce_pi <- as.data.frame(lista[[2]])
tce_pi %>% group_by(protocolo) %>% summarise(p =n()) %>% filter(p>1)


tce_rr <- as.data.frame(lista[[3]])
tce_pi <- as.data.frame(lista[[4]])
tcm_ce <- as.data.frame(lista[[5]])
tcm_rj <- as.data.frame(lista[[6]])
tcm_sp <- as.data.frame(lista[[7]])
tcu <- as.data.frame(lista[[8]])
