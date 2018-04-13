library(dplyr) 
library(janitor)
library(stringr)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("trf3_final.Rdata") #trf3      modelo
load("cnj_final.Rdata")  #CNJ       okok
load("basestj_limpa.Rdata") #STJ    okok
load("trf2_final.Rdata") #trf2      okok
load("tst_final.Rdata") #tst        okok
load("tjpe_final.Rdata") #TJPE      okok
load("tjsp_limpo.Rdata") #TJSP      okok


#deixando tudo igual
{ 
cnj_limpo <- cnj_limpo %>%
  mutate(resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA) %>%
    select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
           recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
           recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao)  

basestj <- basestj %>%
  mutate(resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA,
         data_da_resposta = as.character(data_da_resposta),
         data_do_pedido = as.character(data_do_pedido),
         orgao = "superior tribunal de justica") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao) 

trf2_final <- trf2_final %>%
  mutate(resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA,
         data_da_resposta = as.character(data_da_resposta),
         data_do_pedido = as.character(data_do_pedido)) %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao)  

tst_final <- tst_final %>%
  mutate(resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA,
         data_da_resposta = as.character(data_da_resposta),
         data_do_pedido = as.character(data_do_pedido)) %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao)  

tjsp_limpo <- tjsp_limpo %>%
  mutate(resposta_recurso_seg = NA,
         data_resposta_recurso_seg = NA) %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao) 

tjpe_final <- tjpe_final %>%
  mutate(data_da_resposta = as.character(data_da_resposta),
         data_do_pedido = as.character(data_do_pedido))
}

base_judiciario <- trf3_final %>%
  bind_rows(cnj_limpo) %>%
  mutate(data_da_resposta = as.character(data_da_resposta),
         data_do_pedido = as.character(data_do_pedido)) %>%
  bind_rows(basestj) %>% 
  bind_rows(trf2_final) %>%
  bind_rows(tst_final) %>%
  bind_rows(tjpe_final) %>%
  bind_rows(tjsp_limpo) %>%  #não consegui converter para data sem perder dados.
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,resposta_recurso_seg,data_resposta_recurso_seg,esfera,poder,orgao) %>%  
  mutate(nao_e_pedido_de_info = NA,
         contem_dados_pessoais = NA,
         nome_da_pasta_anexo = NA,
         nome_do_anexo_mais_extensao = NA)


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(base_judiciario , file="base_judiciario.Rdata")

base_judiciario_lda_v2 <- base_judiciario$pedido

save(base_judiciario_lda_v2 , file="base_judiciario_lda_v2.Rdata")
################
# Ajeitando o template:
library(janitor)

nomes_novos <- c("Responsável, Esfera, Poder,Órgão,Protocolo,Assunto,Outros,Atendimento,Não é pedido de informação,Contém dados pessoais,Pedido	Pasta do anexo (pedido),Anexo com extensão (pedido),Resposta,Pasta do anexo (resposta),Anexo com extensão (resposta),Recurso 1,Pasta do anexo (recurso 1),Anexo com extensão (recurso 1),Resposta Recurso 1,Pasta do anexo (resposta recurso 1),Anexo com extensão (resposta recurso 1),Recurso 2,Pasta do anexo (recurso 2),Anexo com extensão (recurso 2)")

nomes_novos_df <- read.csv(text = nomes_novos , strip.white = TRUE)

nomes_novos_df <- nomes_novos_df %>% 
  clean_names()

load("base_judiciario.Rdata")

base_judiciario_v2 <- base_judiciario %>%
  rename(nao_e_pedido_de_informacao = nao_e_pedido_de_info, 
       pedido_pasta_do_anexo_pedido = nome_da_pasta_anexo,
       pasta_do_anexo_resposta = nome_do_anexo_mais_extensao, 
       recurso_1 = recurso_prim, 
       recurso_2 = recurso_seg,  
       resposta_recurso_1 = resposta_recurso_prim, 
       resposta_recurso_2 =  resposta_recurso_seg, 
       anexo_com_extensao_pedido = anexo,
       data_recurso_1 = data_recurso_prim,
       data_resposta_recurso_1 = data_resposta_recurso_prim,
       data_recurso_2 = data_recurso_seg,
       data_resposta_recurso_2 = data_resposta_recurso_seg) %>%
  mutate(pasta_do_anexo_recurso_1 = NA,
       pasta_do_anexo_resposta_recurso_1 = NA,
       pasta_do_anexo_recurso_2 = NA,
       anexo_com_extensao_recurso_1 = NA,
       anexo_com_extensao_recurso_2 = NA,
       anexo_com_extensao_resposta = NA,
       anexo_com_extensao_resposta_recurso_1 = NA,
       assunto = NA,
       atendimento = NA,
       outros = NA) %>%
  select(esfera,
       poder,
       orgao,
       protocolo,
       assunto,
       outros,
       atendimento,
       nao_e_pedido_de_informacao,
       contem_dados_pessoais,
       data_do_pedido,
       pedido,
       pedido_pasta_do_anexo_pedido,
       anexo_com_extensao_pedido,
       data_da_resposta,
       resposta,
       pasta_do_anexo_resposta,
       anexo_com_extensao_resposta,
       data_recurso_1,
       recurso_1,
       pasta_do_anexo_recurso_1,
       anexo_com_extensao_recurso_1,
       data_resposta_recurso_1,
       resposta_recurso_1,
       pasta_do_anexo_resposta_recurso_1,
       anexo_com_extensao_resposta_recurso_1,
       data_recurso_2,
       recurso_2,
       pasta_do_anexo_recurso_2,
       anexo_com_extensao_recurso_2,
       data_resposta_recurso_2,
       resposta_recurso_2)


#eu perdi o script que eu criava os arquivos de cada um, por isso vou só dar um join aqui

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("ana_judiciario.Rdata")
load("jose_judiciario.Rdata")
load("lizandra_judiciario.Rdata")
load("lucas_judiciario.Rdata")

prot_ana <- ana_jud_final %>%
  select(protocolo, responsavel)

prot_jose <- jose_jud_final %>%
  select(protocolo, responsavel)

prot_liz <- liz_jud_final %>%
  select(protocolo, responsavel)

prot_lucas <- lucas_jud_final %>%
  select(protocolo, responsavel)

base_judiciario_v3 <- base_judiciario_v2 %>%
  left_join(prot_ana) %>%
  left_join(prot_jose) %>%
  left_join(prot_liz) %>%
  left_join(prot_lucas) 

#####VER QUE DUPLICATAS SÃO ESSAS

  

#A ana está com muitos pedidos a mais, vou passar alguns para o José
