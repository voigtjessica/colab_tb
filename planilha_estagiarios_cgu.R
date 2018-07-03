# Dividindo a CGU

library(purrr)
library(xlsx)
library(googledrive)
library(dplyr)
library(googlesheets)
library(janitor)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("base_cgu_completa.Rdata")
glimpse(base_cgu_completa)

#Baixando a relação dos órgãos e amostras:
url <- "https://docs.google.com/spreadsheets/d/1ZiaTG94tt_F-DqjM9GiAWx5vNYlDkcUYktnJV_DUC2A/edit?usp=sharing"
gs_ls() 
amostras_cgu_sheet <- gs_title("cgu_dist")
amostras_cgu <- amostras_cgu_sheet %>%
  gs_read() %>%
  clean_names() %>%
  arrange(orgaos) %>%
  select(orgaos, amostra)

glimpse(amostras_cgu)
glimpse(base_cgu_completa)

orgaos_cgu <- sort(unique(amostras_cgu$orgaos))

base_cgu_completa %>%
  filter(destino %in% orgaos_cgu) %>%
  group_by(destino) %>%
  count()

# Nomes estão iguais.

base_cgu_orgaos <- base_cgu_completa %>%
  filter(destino %in% orgaos_cgu) %>%
  arrange(destino)

# Criando o tamanho da amostra
# Como está em ordem alfabética já está na ordem certa.

amostra_real <- as.numeric(amostras_cgu$amostra)
x <- c(1,2,3)

#Função:
gerar_planilha <- function(base, amostra) {
  stopifnot(require(purrr))
  
  minha_amostra <- base %>%
    split(.$destino) %>%
    map2(amostra, sample_n) %>%
    print()
  
  reultado <- bind_rows(minha_amostra)
  
}

base_cgu_estagiarios <- gerar_planilha(base_cgu_orgaos, amostra_real)

x <- base_cgu_orgaos %>%
  group_by(destino) %>%
  summarise(pedidos = n()) %>%
  left_join(amostras_cgu, by=c("destino" = "orgaos"))


base_cgu_completa %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         ano = format(data_do_pedido, "%Y")) %>%
  group_by(ano) %>%
  summarise(pedidos = n())
                      
  