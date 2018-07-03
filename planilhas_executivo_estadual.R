# Governos Estaduais:

library(purrr)
library(xlsx)
library(googledrive)

#base_gov_estaduais
load("base_gov_estaduais.Rdata")

amostra_test <- list(1, 2, 3 ,4,5)

#Função:
gerar_planilha_1 <- function(base, amostra) {
  stopifnot(require(purrr))
  
  minha_amostra <- base %>%
    split(.$orgao) %>%
    map2(amostra, sample_n)
  
  reultado <- bind_rows(minha_amostra)
  
}

# teste_amostra <- gerar_planilha_1(base_gov_estaduais, amostra_test)

amostra <- list(525, 525, 525 , 525, 47) #Todas + Hugo
banco_estagiarios <- gerar_planilha_1(base_gov_estaduais, amostra)

assignment1 <- c(rep("Ana", 125), rep("José", 125), rep("Lizandra", 125), rep("Lucas", 125), rep("Hugo", 25))
assignment2 <- c(rep("Ana", 10), rep("José", 10), rep("Lizandra", 10), rep("Lucas", 10), rep("Hugo", 7))

aux <- banco_estagiarios %>% 
  filter(orgao!="governo estadual do rio grande do sul") %>%
  group_by(orgao) %>% 
  mutate(responsavel = assignment1)
  
aux2 <- banco_estagiarios %>% 
  filter(orgao == "governo estadual do rio grande do sul") %>%
  group_by(orgao) %>% 
  mutate(responsavel = assignment2)

banco_estagiarios_final <- aux %>%
  bind_rows(aux2) %>%
  split(.$responsavel)


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
respon <- c("Ana", "Jose", "Lizandra", "Lucas", "Hugo")

# Salvando como Excel:
for (i in seq_along(respon)) { 
  write.xlsx(as.data.frame(banco_estagiarios_final[[i]]),
          file = paste0(respon[i], "_base_executivo_estadual.xlsx"), 
           sheetName=paste0(respon[i], "_base_executivo_estadual"),
            col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
} 

# Mandando pro drive:
for (i in seq_along(respon)){
  teste_gdrive_df_sheet <- drive_upload(paste0(respon[i], "_base_executivo_estadual.xlsx"),
    path="~/TB/achados e pedidos/Bases_classificação/COLAB",
    name = paste0(respon[i], "_base_executivo_estadual"),
    type = "spreadsheet")
}

# M-A-R-A-V-I-L-H-O-S-O <3 

# Consertando um erro com o gov do estado do maranhão:
# Eu peguei o protocolo errado da planilha original, tenho que dar um join com o protocolo novo. 
# Como eu acredito que a nova ordem que vai criar não vai ser a mesma de antes, vou usar as 
# planilhas prontas.

library(googlesheets)
library(googledrive)
library(xlsx)
library(dplyr)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("base_gov_ma.Rdata")
load("base_gov_mg.Rdata")

base_gov_mg1 <- base_gov_mg %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA,
         anexo_com_extensao_resposta_recurso_3 = NA,
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA, 
         responsavel = NA, 
         obs_id_ma = NA, 
         id_maranhao = NA, 
         id_recurso_ma1 = NA, 
         id_recurso_ma2 = NA, 
         id_recurso_ma3 = NA)

ids_ma <- base_gov_ma %>%
  select(protocolo, id_maranhao, id_recurso_ma1, id_recurso_ma2, id_recurso_ma3)

# Ana


ana_base_executivo_estadual_v2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/colab_tb/Ana_base_executivo_estadual.xlsx")

ana_base_executivo_estadual_ma <- ana_base_executivo_estadual_v2 %>%
  filter(orgao == "governo de estado do maranhao") %>%
  mutate(obs_id_ma =  protocolo,
         data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_recurso_1 = as.Date(data_recurso_1, format="%d/%m/%Y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format="%d/%m/%Y")) %>%
  left_join(ids_ma, by="protocolo")

ana_base_executivo_estadual_mg_ma <- ana_base_executivo_estadual_v2  %>%
  filter(orgao == "governo do estado de minas gerais") %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y")) %>%
  select(protocolo) %>%
  filter(!is.na(protocolo)) %>%
  left_join(base_gov_mg1) %>%
  bind_rows(ana_base_executivo_estadual_ma) %>%
  mutate(responsavel == "ana")

write.xlsx(as.data.frame(ana_base_executivo_estadual_mg_ma), 
           file="ana_base_executivo_estadual_mg_ma.xlsx", 
           sheetName="ana_base_executivo_estadual_mg_ma",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

ana_base_executivo_estadual_mg_ma_sheet <- drive_upload(
  "ana_base_executivo_estadual_mg_ma.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB/Ana Alini Lins",
  name = "ana_base_executivo_estadual_mg_ma",
  type = "spreadsheet")

# Lucas

lucas_base_executivo_estadual_v2  <- read_excel("C:/Users/jvoig/OneDrive/Documentos/colab_tb/Lucas_base_executivo_estadual.xlsx")

lucas_base_executivo_estadual_ma <- lucas_base_executivo_estadual_v2 %>%
  filter(orgao == "governo de estado do maranhao") %>%
  mutate(obs_id_ma =  protocolo,
         data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta= as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_recurso_1  = as.Date(data_recurso_1,format="%d/%m/%Y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format="%d/%m/%Y"))  %>%
  left_join(ids_ma, by="protocolo")

lucas_base_executivo_estadual_mg_ma <- lucas_base_executivo_estadual_v2 %>%
  filter(orgao == "governo do estado de minas gerais") %>%
  select(protocolo) %>%
  filter(!is.na(protocolo)) %>%
  left_join(base_gov_mg1) %>%
  bind_rows(lucas_base_executivo_estadual_ma) %>%
  mutate(responsavel == "lucas")

write.xlsx(as.data.frame(lucas_base_executivo_estadual_mg_ma), 
           file="lucas_base_executivo_estadual_mg_ma.xlsx", 
           sheetName="lucas_base_executivo_estadual_mg_ma",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

lucas_base_executivo_estadual_mg_ma_sheet <- drive_upload(
  "lucas_base_executivo_estadual_mg_ma.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB/Lucas Alves",
  name = "lucas_base_executivo_estadual_mg_ma",
  type = "spreadsheet")


# Liz
library(googlesheets)

url_liz <- "https://docs.google.com/spreadsheets/d/1LUkckvPx5FNBs0hiRwFje47_60Wm7Bs2fd9m1F-NJz8/edit?usp=sharing"
liz_sheet <- gs_title("Lizandra_base_executivo_estadual1")
liz_sheet_df <- liz_sheet %>%
  gs_read()


liz_base_executivo_estadual_v2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/colab_tb/Lizandra_base_executivo_estadual.xlsx")

#ver o que ela já fez

liz_feitos_ma <- liz_sheet_df %>%
  filter(orgao == "governo de estado do maranhao",
         !is.na(assunto)) %>%
  left_join(ids_ma, by="protocolo")

protocolos_feitos_ma <- liz_feitos_ma$protocolo

`%notin%` = function(x,y) !(x %in% y)

teste_igualdade_nomes_var_df(liz_ma, liz_feitos_ma )

liz_ma <- liz_base_executivo_estadual_v2 %>%
  filter(orgao == "governo de estado do maranhao",
         protocolo %notin% protocolos_feitos_ma ) %>%
  left_join(ids_ma, by="protocolo") %>%
  bind_rows(liz_feitos_ma) %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_recurso_1 = as.Date(data_recurso_1, format="%d/%m/%Y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, fomart="%d/%m/%Y"))
  
base_mg2 <- base_gov_mg %>%
  select(-c(assunto,
            outros,
            atendimento,
            nao_e_pedido_de_informacao,
            contem_dados_pessoais,
            e_complementacao_de_pedido,
            resposta_duplicada))

liz_feitos_mg <- liz_sheet_df %>%
  filter(orgao == "governo do estado de minas gerais",
         !is.na(assunto)) %>%
  select(protocolo,
         assunto,
         outros,
         'Não Atendido',
         nao_e_pedido_de_informacao,
         contem_dados_pessoais,
         e_complementacao_de_pedido,
         resposta_duplicada) %>%
  left_join(base_mg2) 

protocolo_mg_liz <- liz_feitos_mg$protocolo

liz_base_executivo_estadual_mg_ma <- liz_base_executivo_estadual_v2  %>%
  filter(orgao == "governo do estado de minas gerais",
         protocolo %notin% protocolo_mg_liz) %>%
  filter(!is.na(protocolo)) %>%
  select(protocolo) %>%
  left_join(base_gov_mg1, by="protocolo") %>%
  bind_rows(liz_feitos_mg) %>%
  bind_rows(liz_ma)

write.xlsx(as.data.frame(liz_base_executivo_estadual_mg_ma), 
           file="liz_base_executivo_estadual_mg_ma.xlsx", 
           sheetName="liz_base_executivo_estadual_mg_ma",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

#read.csv(liz_base_executivo_estadual_mg_ma, file="liz_base_executivo_estadual_mg_ma.csv")

liz_base_executivo_estadual_mg_ma_sheet <- drive_upload(
  "liz_base_executivo_estadual_mg_ma.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB/Lizandra Aguiar Pinto de Oliveira",
  name = "liz_base_executivo_estadual_mg_ma",
  type = "spreadsheet")

# José deu tudo errado,
# o original dele tá com menos pedidos.
# vou gerar primeiro maranhão e mg para depois separar mais pedidos do resto

prot_outros <- lucas_base_executivo_estadual_mg_ma %>%
  bind_rows(liz_base_executivo_estadual_mg_ma, 
            ana_base_executivo_estadual_mg_ma) %>%
  select(protocolo, orgao)

base_ret_ma_mg <- base_gov_estaduais %>%
  filter(orgao == "governo do estado de minas gerais"|
           orgao == "governo de estado do maranhao") %>%
  anti_join(prot_outros, by=c("protocolo", "orgao"))

y <- c(125, 125)

jose_base_executivo_estadual_v2 <- gerar_planilha_1(base_ret_ma_mg, y)

jose_base_executivo_estadual_ma <- jose_base_executivo_estadual_v2 %>%
  filter(orgao == "governo de estado do maranhao") %>%
  mutate(obs_id_ma =  protocolo,
         data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta= as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_recurso_1  = as.Date(data_recurso_1,format="%d/%m/%Y"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format="%d/%m/%Y")) %>%
  left_join(ids_ma, by="protocolo")

jose_base_executivo_estadual_mg_ma <- jose_base_executivo_estadual_v2  %>%
  filter(orgao == "governo do estado de minas gerais") %>%
  select(protocolo) %>%
  mutate(protocolo = ifelse(protocolo == "", NA, protocolo)) %>%
  filter(!is.na(protocolo)) %>%
  left_join(base_gov_mg1) %>%
  bind_rows(jose_base_executivo_estadual_ma) %>%
  mutate(responsavel = "jose")

write.xlsx(as.data.frame(jose_base_executivo_estadual_mg_ma), 
           file="jose_base_executivo_estadual_mg_ma.xlsx", 
           sheetName="jose_base_executivo_estadual_mg_ma",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

jose_base_executivo_estadual_mg_ma_sheet <- drive_upload(
  "jose_base_executivo_estadual_mg_ma.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB/José Vitor da Silva",
  name = "jose_base_executivo_estadual_mg_ma",
  type = "spreadsheet")

#agora o restante dos pedidos que não entrou:

original_jose <- read_excel("C:/Users/jvoig/OneDrive/Documentos/colab_tb/Jose_base_executivo_estadual.xlsx")

original_jose %>%
  group_by(orgao) %>%
  summarise(p=n())

#preciso de 100 de cada orgao menos mg e ma

prot_usados <- original_jose %>%
  bind_rows(ana_base_executivo_estadual_v2 , 
            lucas_base_executivo_estadual_v2, 
            liz_base_executivo_estadual_v2) %>%
  select(protocolo, orgao)

comp_jose_estadual <- base_gov_estaduais %>%
  filter(orgao != "governo do estado de minas gerais",
         orgao != "governo de estado do maranhao") %>%
  anti_join(prot_usados, by=c("protocolo", "orgao"))
  
jose_rs <- comp_jose_estadual %>%
  filter(orgao == "governo estadual do rio grande do sul") %>%
  mutate(responsavel = "José")

assigment3 <- c(rep("José", 100))

jose_rn_al_rs <- comp_jose_estadual %>%
  filter(orgao != "governo estadual do rio grande do sul") %>%
  gerar_planilha_1(100) %>%
  bind_rows(jose_rs)
  
write.xlsx(as.data.frame(jose_rn_al_rs), 
           file="jose_rn_al_rs.xlsx", 
           sheetName="jose_rn_al_rs",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

jose_rn_al_rs_sheet <- drive_upload(
  "jose_rn_al_rs.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB/José Vitor da Silva",
  name = "jose_rn_al_rs",
  type = "spreadsheet")
