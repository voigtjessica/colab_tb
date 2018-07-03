library(dplyr)
library(readxl)
library(janitor)
library(data.table)

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


#recolocando as datas no legislativo.

#importando a planilha sem data:
leg2 <- read_excel("C:/Users/jvoig/OneDrive/Documentos/colab_tb/recolocando_data/leg2.xlsx")

#buscando os Rdatas com as datas:
unique(leg2$orgao)
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
list.files(path = ".")

# Bases join com pedidos: 
# Alepe  

load("alepe17_final.Rdata") 

dt_alepe <- alepe17_limpo %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"), data_do_pedido,
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"), 
         data_recurso_seg = as.Date(NA),
         data_recurso_prim = as.Date(data_recurso_prim),
         data_resposta_recurso_prim = as.Date(data_resposta_recurso_prim)) %>%
  select(pedido, data_do_pedido, data_da_resposta, data_recurso_prim , data_resposta_recurso_prim )

glimpse(dt_alepe)
rm(alepe17_limpo)

# camara municipal de fortaleza   
load("camfortaleza_final.Rdata")

dt_fortaleza <- camfortaleza_limpo %>%
  mutate(data_recurso_prim = as.Date(data_recurso_prim),
         data_resposta_recurso_prim = as.Date(data_resposta_recurso_prim),
         data_recurso_seg = as.Date(data_recurso_seg),
         protocolo = as.Date(protocolo)) %>%
  select(pedido, data_do_pedido, data_da_resposta, data_recurso_prim,
         data_resposta_recurso_prim, data_recurso_seg)

glimpse(dt_fortaleza)
rm(camfortaleza_limpo)


# Bases com Join no protocolo
# camara municipal de curitiba
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\zzCâmCurtib\\limpos")

dt_curitiba_2013 <- fread("camcuritiba_pedido_2013_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%m/%d/%Y"),
         data_de_resposta = as.Date(data_de_resposta, format="%d/%m/%Y")) %>%
  rename(data_da_resposta = data_de_resposta)

dt_curitiba_2014 <- fread("camcuritiba_pedido_2014_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))

dt_curitiba_2015 <- fread("camcuritiba_pedido_2015_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))

dt_curitiba_2016 <- fread("camcuritiba_pedido_2016_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))
  
dt_curitiba_2017 <- fread("camcuritiba_pedido_2017_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%d/%m/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))


teste_igualdade_nomes_var_df(dt_curitiba_2013, dt_curitiba_2014)
teste_igualdade_nomes_var_df(dt_curitiba_2014, dt_curitiba_2015)
teste_igualdade_nomes_var_df(dt_curitiba_2015, dt_curitiba_2016)
teste_igualdade_nomes_var_df(dt_curitiba_2016, dt_curitiba_2017)

pedidos_curitiba <- dt_curitiba_2013 %>%
  bind_rows(dt_curitiba_2014, dt_curitiba_2015, dt_curitiba_2016,
            dt_curitiba_2017) %>%
  rename(protocolo = codigo)

glimpse(pedidos_curitiba) #foi!

dt_rec_curitiba_2014 <- fread("camcuritiba_recurso_2014_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_recurso = as.Date(data_do_recurso, format="%m/%d/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))

dt_rec_curitiba_2015 <- fread("camcuritiba_recurso_2015_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_recurso = as.Date(data_do_recurso, format="%m/%d/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))

dt_rec_curitiba_2016 <- fread("camcuritiba_recurso_2016_limpo.txt", na.strings = "") %>%
  clean_names() %>%
  mutate(data_do_recurso = as.Date(data_do_recurso, format="%m/%d/%Y"),
         data_da_resposta = as.Date(data_da_resposta, format="%d/%m/%Y"))

teste_igualdade_nomes_var_df(dt_rec_curitiba_2014, dt_rec_curitiba_2015)
teste_igualdade_nomes_var_df(dt_rec_curitiba_2015, dt_rec_curitiba_2016)

recursos <- dt_rec_curitiba_2014 %>%
  bind_rows(dt_rec_curitiba_2015,dt_rec_curitiba_2016 ) %>%
  rename(protocolo = codigo,
         recurso_prim = recurso,
         data_recurso_prim = data_do_recurso,
         resposta_recurso_prim = resposta,
         data_resposta_recurso_prim = data_da_resposta)

dt_curitiba_final <- pedidos_curitiba %>%
  left_join(recursos, by="protocolo") %>%
  select(protocolo, data_do_pedido, data_da_resposta, data_recurso_prim,data_resposta_recurso_prim )

rm(dt_curitiba_2013)
rm(dt_curitiba_2014)
rm(dt_curitiba_2015)
rm(dt_curitiba_2016)
rm(dt_curitiba_2017)
rm(dt_rec_curitiba_2014)
rm(dt_rec_curitiba_2015)
rm(dt_rec_curitiba_2016)
rm(pedidos_curitiba)
rm(recursos)

# camara dos deputados
load("camdep_final.Rdata")
dt_camdep <- camdep_final %>%
  select(protocolo, data_do_pedido, data_da_resposta, data_recurso_prim , data_resposta_recurso_prim,
         data_recurso_seg )
rm(camdep_final)

# camara municipal de salvador
load("camsalvador_final.Rdata")
dt_salvador <- camsalvador_limpo %>%
  select(protocolo, data_do_pedido, data_da_resposta, data_recurso_prim , 
         data_resposta_recurso_prim ) %>%
  mutate(data_recurso_seg = NA,
         data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_recurso_prim = as.Date(data_recurso_prim),
         data_resposta_recurso_prim = as.Date(data_resposta_recurso_prim),
         data_recurso_seg = as.Date(data_recurso_seg),
         protocolo = as.character(protocolo))

glimpse(dt_salvador)
rm(camsalvador_limpo)


# camara legislativa do distrito federal
load("cldf_final.Rdata")

dt_cldf <- cldf_limpo %>%
  mutate(data_do_pedido = as.Date(data_do_pedido, format="%Y-%m-%d"),
         data_da_resposta = as.Date(data_da_resposta, format="%Y-%m-%d"),
         data_recurso_prim = as.Date(data_recurso_prim),
         data_resposta_recurso_prim = as.Date(data_resposta_recurso_prim),
         data_recurso_seg = as.Date(data_recurso_seg)) %>%
  select(protocolo, data_do_pedido, data_da_resposta,data_recurso_prim,
         data_resposta_recurso_prim, data_recurso_seg)
glimpse(dt_cldf)
rm(cldf_limpo)

#"assembleia legislativa de pernambuco"  
load("alepe17_final.Rdata")

# preparando as tabelas pro join

join_pedidos <- dt_alepe %>%
  bind_rows(dt_fortaleza)
  
join_protocolos <- dt_camdep %>%
  bind_rows(dt_cldf,
            dt_curitiba_final,
            dt_salvador)

legis_pedidos <- leg2 %>%
  filter(orgao == "assembleia legislativa de pernambuco"|
         orgao == "camara municipal de fortaleza") %>%
  left_join(join_pedidos)

legis_final <- leg2 %>%
  filter(orgao != "assembleia legislativa de pernambuco",
           orgao != "camara municipal de fortaleza") %>%
  left_join(join_protocolos) %>%
  bind_rows(legis_pedidos)
  
legis_final %>%
  filter(is.na(data_do_pedido))%>%
  group_by(orgao) %>%
  summarise(pedidos = n())

glimpse(legis_final)
