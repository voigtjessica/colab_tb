#nao_e_pedido :

# script que vai juntar todos os pedidos de acesso à informação que não são pedidos.
# ir nas planilhas revisadas e empilhadas, filtrar desses o que não é 
# pedido de info e deixar tudo junto pra renata olhar

library(googlesheets)
library(dplyr)
library(janitor)

#Autenticação:
gs_ls() 

#TCs
tcs_final_sheet <- gs_title("tcs_final_05062018.xlsx")
tcs_final <- gs_read(tcs_final_sheet)

tcs_final  <- tcs_final  %>%
  clean_names() %>%
  select(-c(data_recurso_1, data_resposta_recurso_1))

#MP
mp_final_sheet <- gs_title("mp_final_18062018")
mp_final <- gs_read(mp_final_sheet)

mp_final <- mp_final %>%
  clean_names() %>%
  mutate(base_de_dados = as.character(base_de_dados))  %>%
  select(-c(data_recurso_1, data_resposta_recurso_1, data_do_pedido,data_recurso_2,
            data_resposta_recurso_2))

#Legislativo
leg_final_sheet <- gs_title("leg_final_18062018")
leg_final <- gs_read(leg_final_sheet)

leg_final <- leg_final %>%
  clean_names() %>%
  mutate(nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         revisado = as.character(revisado),
         base_de_dados = as.character(base_de_dados)) 

#Judiciario
jud_final_sheet <- gs_title("jud_final_11062018")
jud_final <- gs_read(jud_final_sheet)

jud_final <- jud_final %>%
  clean_names() %>%
  mutate(base_de_dados = as.character(base_de_dados),
         base_de_dados = as.character(base_de_dados),
         revisado = as.character(revisado))   %>%
  select(-c( data_resposta_recurso_1, data_do_pedido,
            data_resposta_recurso_2))

#Executivo Municipal
exec_municipal_sheet <- gs_title("exec_municipal_final_250618")
exec_municipal <- gs_read(exec_municipal_sheet)

exec_municipal <- exec_municipal %>%
  clean_names()   %>%
  select(-c(data_recurso_1, data_resposta_recurso_1, data_do_pedido,data_recurso_2,
            data_resposta_recurso_2)) %>%
  mutate(nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido))

#Executivo estadual
exec_estadual_sheet <- gs_title("exec_estadual_final_01072018.xlsx")
exec_estadual <- gs_read(exec_estadual_sheet)

exec_estadual <- exec_estadual %>%
  clean_names()   %>%
  select(-c(data_recurso_1, data_resposta_recurso_1, data_do_pedido,data_recurso_2,
            data_resposta_recurso_2))

#CGU
#ainda está em revisão, adicionar ao final.
cgu_sheet <- gs_title("cgu_revisão em andamento.xlsx")
cgu <- gs_read(cgu_sheet)

cgu <- cgu %>%
  clean_names() %>%
  select(-c(data_recurso_1, data_resposta_recurso_1, data_do_pedido,data_recurso_2,
            data_resposta_recurso_2)) %>%
  mutate(protocolo = as.character(protocolo),
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido))

#Juntando e pegando o que não é pedido:

nao_e_pedido <- tcs_final %>%
  bind_rows(mp_final) %>%
  mutate(nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         revisado = as.character(revisado),
         base_de_dados = as.character(base_de_dados)) %>%
  bind_rows(leg_final, jud_final) %>%
  mutate(e_complementacao_de_pedido = as.character(e_complementacao_de_pedido)) %>%
  bind_rows(exec_municipal, exec_estadual, cgu) %>%
  filter(nao_e_pedido_de_informacao != 1)

