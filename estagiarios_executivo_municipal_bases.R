# Bases dos estagiários Prefeituras + cgm:

library(data.table)
library(dplyr)
library(xlsx)
library(googledrive)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load(file="base_prefeituras.Rdata")
load("base_cgm_sp_final.Rdata")

# Prefeitura	SP	São Paulo	  500  
# Prefeitura	PE	Recife    	500  
# Prefeitura	PB	João Pessoa	500  
# Prefeitura	ES	Vitória	    500  
# Prefeitura	BA	Salvador	  244  
# Prefeitura	AC	Rio Branco	235  
# Prefeitura	RO	Porto Velho	170  

#Hugo 229 da amostra de cima
45 + 45 + 45 + 45 + 16 + 16 + 17


(455 + 455 + 455 + 455 + 228 + 219 + 153) /4
# 605

#                             ANA  JOSE  LIZ   LUCAS
# Prefeitura	SP	São Paulo	  113+ 114 + 114 + 114
# Prefeitura	PE	Recife    	114+ 113 + 114 + 114
# Prefeitura	PB	João Pessoa	114+ 114 + 113 + 114
# Prefeitura	ES	Vitória     114+ 114 + 114 + 113
# Prefeitura	BA	Salvador	  57 + 57 + 57 + 57
# Prefeitura	AC	Rio Branco	55 + 55 + 54 + 55  
# Prefeitura	RO	Porto Velho	39 + 38 + 38 + 38
153 /4 #38.25

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

teste_igualdade_nomes_var_df(base_cgm_sp_final, base_prefeituras)

base_cgm_sp_final <- base_cgm_sp_final%>%
  rename(recurso_1 = recurso1)

base_prefeituras <- base_prefeituras %>%
  mutate(situacao = NA)

X <- split(base_prefeituras, base_prefeituras$orgao)
Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 1:52]) 

#Assign the dataframes in the list Y to individual objects
pref_jp <- Y[[1]] %>%
  mutate(situacao  = as.character(situacao ),
         data_do_pedido = substr(data_do_pedido, 0 , 10 ),
         data_do_pedido = gsub('([0-9]+) .*', '\\1', data_do_pedido),
         data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_do_pedido = as.character(gsub("00", "20", data_do_pedido)),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = substr(data_da_resposta, 0 , 10 ),
         data_da_resposta = gsub('([0-9]+) .*', '\\1', data_da_resposta),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_da_resposta = as.character(gsub("00", "20", data_da_resposta)),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1),
         data_recurso_2 = as.Date(data_recurso_2),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2),
         data_recurso_3 = as.Date(data_recurso_3),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3),
         data_recurso_4 = as.Date(data_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4))

pref_porto_velho <- Y[[2]]%>%
  mutate(situacao  = as.character(situacao ),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = gsub("0207", "2017", data_da_resposta),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_recurso_1 = as.Date(data_recurso_1),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1),
         data_recurso_2 = as.Date(data_recurso_2),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2),
         data_recurso_3 = as.Date(data_recurso_3),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3),
         data_recurso_4 = as.Date(data_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4))

pref_rio_branco <- Y[[3]] %>%
  mutate(situacao  = as.character(situacao),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_recurso_1 = as.Date(data_recurso_1),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1),
         data_recurso_2 = as.Date(data_recurso_2),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2),
         data_recurso_3 = as.Date(data_recurso_3),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3),
         data_recurso_4 = as.Date(data_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4))

# Salvador 
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load(file="base_pref_salvador.Rdata")
pref_salvador <- base_pref_salvador

#Vitória
pref_vitoria <- Y[[5]] %>%
  mutate(situacao = as.character(situacao),
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_recurso_1 = as.Date(data_recurso_1),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1),
         data_recurso_2 = as.Date(data_recurso_2),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2),
         data_recurso_3 = as.Date(data_recurso_3),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3),
         data_recurso_4 = as.Date(data_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4))

pref_recife <- Y[[6]] %>%
  mutate(situacao  = as.character(situacao),
         data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"),
         data_recurso_1 = as.Date(data_recurso_1),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1),
         data_recurso_2 = as.Date(data_recurso_2),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2),
         data_recurso_3 = as.Date(data_recurso_3),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3),
         data_recurso_4 = as.Date(data_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4))


base_cgm_sp_final <- base_cgm_sp_final %>%
  mutate(data_recurso_4 = as.Date(data_recurso_4),
         recurso_4 = as.character(recurso_4),
         pasta_do_anexo_recurso_4 = as.character(pasta_do_anexo_recurso_4),
         anexo_com_extensao_recurso_4 = as.character(anexo_com_extensao_recurso_4),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4),
         resposta_recurso_4 = as.character(resposta_recurso_4),
         pasta_do_anexo_resposta_recurso_4 = as.character(pasta_do_anexo_resposta_recurso_4),
         anexo_com_extensao_resposta_recurso_4 = as.character(anexo_com_extensao_resposta_recurso_4),
         assunto = as.character(assunto),
         outros = as.character(outros),
         atendimento = as.character(atendimento),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido),
         resposta_duplicada = as.character(resposta_duplicada))

set.seed(12)
hugo <- sample_n(base_cgm_sp_final, 45) %>%
  bind_rows(sample_n(pref_recife, 45),
            sample_n(pref_jp, 45),
            sample_n(pref_vitoria, 45), 
            sample_n(pref_salvador, 16),
            sample_n(pref_rio_branco, 16),
            sample_n(pref_porto_velho, 17))

set.seed(11)
ana_sp <- base_cgm_sp_final %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(113)

set.seed(11)
ana_recife <- pref_recife %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(114)

set.seed(11)
ana_jp <- pref_jp %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(114)

set.seed(11)
ana_vit <- pref_vitoria %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(114)

set.seed(11)
ana_salvador <- pref_salvador %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(57)

set.seed(11)
ana_rio_branco <- pref_rio_branco %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(55)

set.seed(11)
ana_porto_velho <- pref_porto_velho %>%
  anti_join(hugo, by="protocolo") %>%
  sample_n(39)

ana_base_pref <- ana_sp %>%
  bind_rows(ana_recife,
            ana_jp,
            ana_vit,
            ana_salvador,
            ana_rio_branco,
            ana_porto_velho) %>%
  mutate(responsavel = "Ana")

## 

set.seed(45)
jose_sp <- base_cgm_sp_final %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(45)
jose_recife <- pref_recife %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(113)

set.seed(45)
jose_jp <- pref_jp %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(11)
jose_vit <- pref_vitoria %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(45)
jose_salvador <- pref_salvador %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(57)


set.seed(45)
jose_rio_branco <- pref_rio_branco %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(55)

set.seed(45)
jose_porto_velho <- pref_porto_velho %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  sample_n(39)

jose_base_pref <- jose_sp %>%
  bind_rows(jose_recife,
            jose_jp,
            jose_vit,
            jose_salvador,
            jose_rio_branco,
            jose_porto_velho) %>%
  mutate(responsavel = "José")

## LIZ

set.seed(66)
liz_sp <- base_cgm_sp_final %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(66)
liz_recife <- pref_recife %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(66)
liz_jp <- pref_jp %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(113)

set.seed(66)
liz_vit <- pref_vitoria %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(66)
liz_salvador <- pref_salvador %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(57)

set.seed(66)
liz_rio_branco <- pref_rio_branco %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(54)

set.seed(45)
liz_porto_velho <- pref_porto_velho %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  sample_n(39)

liz_base_pref <- liz_sp %>%
  bind_rows(liz_recife,
            liz_jp,
            liz_vit,
            liz_rio_branco,
            liz_porto_velho,
            liz_salvador) %>%
  mutate(responsavel = "Lizandra")

## Lucas

set.seed(99)
lucas_sp <- base_cgm_sp_final %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(99)
lucas_recife <- pref_recife %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(99)
lucas_jp <- pref_jp %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") %>%
  sample_n(114)

set.seed(99)
lucas_vit <- pref_vitoria %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") %>%
  sample_n(113)

set.seed(66)
lucas_rio_branco <- pref_rio_branco %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") %>%
  sample_n(55)

set.seed(45)
lucas_porto_velho <- pref_porto_velho %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") 

set.seed(66)
lucas_salvador <- pref_salvador %>%
  anti_join(hugo, by="protocolo") %>%
  anti_join(ana_base_pref, by="protocolo") %>%
  anti_join(jose_base_pref, by="protocolo") %>%
  anti_join(liz_base_pref, by="protocolo") 

lucas_base_pref <- lucas_sp %>%
  bind_rows(lucas_recife,
            lucas_jp,
            lucas_vit,
            lucas_rio_branco,
            lucas_porto_velho,
            lucas_salvador) %>%
  mutate(responsavel = "Lucas")


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
write.xlsx(as.data.frame(hugo), file="hugo2.xlsx", sheetName="hugo2",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
write.xlsx(as.data.frame(ana_base_pref), file="ana_base_pref.xlsx", 
           sheetName="ana_base_pref", col.names=TRUE, 
           row.names=FALSE, append=FALSE, showNA=FALSE)
write.xlsx(as.data.frame(jose_base_pref), file="jose_base_pref.xlsx", 
           sheetName="jose_base_pref", col.names=TRUE, 
           row.names=FALSE, append=FALSE, showNA=FALSE)
write.xlsx(as.data.frame(lucas_base_pref), file="lucas_base_pref.xlsx", 
           sheetName="lucas_base_pref", col.names=TRUE, 
           row.names=FALSE, append=FALSE, showNA=FALSE)
write.xlsx(as.data.frame(liz_base_pref), file="liz_base_pref.xlsx", 
           sheetName="liz_base_pref", col.names=TRUE, 
           row.names=FALSE, append=FALSE, showNA=FALSE)

#upload Gdrive

teste_gdrive_df_sheet <- drive_upload(
  "hugo2.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB",
  name = "hugo2",
  type = "spreadsheet")

ana_base_pref_sheet <- drive_upload(
  "ana_base_pref.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB",
  name = "ana_base_pref",
  type = "spreadsheet")

jose_base_pref_sheet <- drive_upload(
  "jose_base_pref.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB",
  name = "jose_base_pref",
  type = "spreadsheet")

lucas_base_pref_sheet <- drive_upload(
  "lucas_base_pref.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB",
  name = "lucas_base_pref",
  type = "spreadsheet")

liz_base_pref_sheet <- drive_upload(
  "liz_base_pref.xlsx",
  path="~/TB/achados e pedidos/Bases_classificação/COLAB",
  name = "liz_base_pref_pref",
  type = "spreadsheet")

## Juntando o que o Hugo já fez com o Hugo 2:
#Interrompi aqui porque o hugo parou de classificar. 
# library(googlesheets)
# 
# url_contatos <- "https://docs.google.com/spreadsheets/d/1fEy1SdUzGc83UDklQLWaeFP_JKyk5CJ5E8HE_Bc820I/edit?usp=sharing"
# 
# #Autenticação:
# gs_ls() 
# 
# #Importando:
# hugo_sheet <- gs_title("hugo_pref")
# 
# #Atribuindo o df a um objeto:
# hugo1 <- hugo_sheet %>%
#   gs_read() %>%
#   select(protocolo, assunto)
# 
# hugo3 <- hugo_sheet %>%
#   gs_read() %>%
#   mutate(nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))
# 
# hugo2 <- hugo  %>%
#   select(-c(assunto)) %>%
#   anti_join(hugo1, by="protocolo") %>%
#   bind_rows(hugo3)
#    
# 
# teste_igualdade_nomes_var_df(hugo, hugo1)
# 
# hugo2 %>%
#   group_by(assunto) %>%
#   summarise(p=n())
# 
# hugo %>%
#   group_by(assunto) %>%
#   summarise(p=n())
# (hugo1$assunto1)
