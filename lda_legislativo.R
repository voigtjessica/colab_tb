#legislativo

library(dplyr)

#Montando a base:

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
load("alepe17_final.Rdata")
load("camara_curitiba_final.Rdata")
load("camdep_final.Rdata")
load("camfortaleza_final.Rdata")
load("camsalvador_final.Rdata")
load("cldf_final.Rdata")

alepe17_limpo <- alepe17_limpo %>%
  mutate(anexo = NA)

camsalvador_limpo <- camsalvador_limpo %>%
  mutate(recurso_seg = NA,
         data_recurso_seg = NA) 

base_legislativo <- alepe17_limpo %>%
  rbind(camara_curitiba_final) %>%
  mutate(recurso_seg = NA,
         data_recurso_seg = NA) %>%
  rbind(camdep_final) %>%
  rbind(camfortaleza_limpo) %>%
  rbind(camsalvador_limpo) %>%
  rbind(cldf_limpo)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(base_legislativo, file="base_legislativo.Rdata")

#iniciando a produção dos documentos:

library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
#devtools::install_github("sfirke/janitor")
library(janitor)
# devtools::install_github("mgaldino/tbaep")
library(tbaep)
library(dplyr)
