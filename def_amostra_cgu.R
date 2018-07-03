#LDA da CGU - iniciado.
# Manoel vai criar um loop nesse script para que rode um LDA para cada orgao.

setwd("C:\\Users\\mgaldino\\2018\\Achados e Pedidos\\LDa Executivo")
load("base_cgu_completa.Rdata")

library(ldatuning)
library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
#devtools::install_github("sfirke/janitor")
library(janitor)
# devtools::install_github("mgaldino/tbaep")
library(tbaep)
library(googlesheets)

## Preparando...

base_cgu_completa <- base_cgu_completa %>%
  mutate(orgao1 = ifelse(grepl("Ag[êe]ncia|CVM", orgao), "agencia", orgao),
         orgao1 = ifelse(grepl("Banco|Caixa|BB", orgao), "bancos", orgao1),
         orgao1 = ifelse(grepl("Pesquisas", orgao), "Institutos de pesquisas", orgao1),
         orgao1 = ifelse(grepl("Universidade", orgao), "universidades", orgao1),
         orgao1 = ifelse(grepl("Museu", orgao), "museus", orgao1),
         orgao1 = ifelse(grepl("Empresa|BR[ÁA]S|Companhia|FURNAS|S.A.|Ltda", orgao),
                         "estatal", orgao1),
         orgao1 = ifelse(grepl("Instituto Federal|Centro Federal|CP II", orgao), "escolas técnicas e CPII", orgao1),
         orgao1 = ifelse(grepl("Hospital|Maternidade", orgao), "hospitais", orgao1),
         orgao1 = ifelse(grepl("Instituto", orgao), "institutos", orgao1),
         orgao1 = ifelse(grepl("Centro de Tecnolo", orgao), "centros de tecnologia", orgao1),
         orgao1 = ifelse(grepl("Superintendência", orgao), "superintendencias", orgao1))

base_cgu_completa <- base_cgu_completa %>%
  group_by(orgao1) %>%
  mutate(total= n()) %>%
  ungroup() %>%
  mutate(orgao1 = ifelse(grepl("Laboratório", orgao), "laboratorios", orgao1),
         orgao1 = ifelse(grepl("Ministério", orgao) & total < 300, "Ministérios menores", orgao1),
         orgao1 = ifelse(grepl("Secretaria de", orgao) & total < 300, "Secretarias menores", orgao1),
         orgao1 = ifelse(grepl("Fundação", orgao) & total < 300, "Fundações menores", orgao1)) %>%
  group_by(orgao1) %>%
  mutate(total= n()) %>%
  ungroup() %>%
  mutate(orgao1 = ifelse(total < 150, "outros", orgao1)) # %>%
  #select(-c(orgao, total))

# remove pedidos com informações em anexo
base_cgu_completa <- base_cgu_completa %>%
  filter(!grepl("Questionário em anexo.", pedido)) %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                      transliterations = c("Latin-ASCII")))

base_cgu_completa <- base_cgu_completa %>%
         mutate(pedido = gsub("(^sou| sou) [a-z]*", " ", pedido))

View(base_cgu_completa)
base_cgu_completa_v2 <- base_cgu_completa

save(base_cgu_completa_v2, file="base_cgu_completa_v2.RData")

setwd("C:\\Users\\mgaldino\\2018\\Achados e Pedidos\\LDa Executivo")
load("base_cgu_completa_v2.Rdata")

res <- base_cgu_completa_v2 %>%
  group_by(orgao1) %>%
  summarise(total = n(),
            amostra_simples = round(total*0.0387551, 0),
            amostra_ampliada = round(total*0.05382653, 0))

View(res)


