#Câmara dos vereadores Curitiba

#2013 pedido
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\CâmCurtib\\limpos")

camcuritiba_pedido_2013_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_pedido_2013_limpo.xlsx",
                                            col_types = c("text", "text", "date", 
                                                          "text", "date"))

camcuritiba_pedido_2013 <- camcuritiba_pedido_2013_limpo %>%
  clean_names() %>%
  filter(!is.na(codigo),
         pedido != "EM DUPLICIDADE",
         pedido != "TESTE DO SISTEMA")

#não há recursos

#2014 pedido  
camcuritiba_pedido_2014_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_pedido_2014_limpo.xlsx",
                                            col_types = c("text", "text", "date", 
                                                          "text", "date"))

camcuritiba_pedido_2014 <- camcuritiba_pedido_2014_limpo %>%
  clean_names() %>%
  filter(pedido != "TESTE DO SISTEMA")
  
#2014 recursos - só em primeira instância

camcuritiba_recurso_2014_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_recurso_2014_limpo.xlsx",
                                             col_types = c("text", "text", "date", 
                                                           "text", "date"))

camcuritiba_recurso_2014 <- camcuritiba_recurso_2014_limpo  %>%
  clean_names() %>%
  rename(recurso_prim = recurso,
         data_recurso_prim = data_do_recurso,
         resposta_recurso_prim = resposta,
         data_resposta_recurso_prim = data_da_resposta)

camcuritiba2014 <- camcuritiba_pedido_2014 %>%
  full_join(camcuritiba_recurso_2014)

#em 2014, pedido e recurso possuem números diferentes

#2015 - pedidos

url_camcurped2015 <- "https://docs.google.com/spreadsheets/d/14laocTd61-QcS_49BSYCnul-gnelVUlO9EczXCEtbWo/edit?usp=sharing"
gs_ls() 
camcurped2015_sheet <- gs_title("camcuritiba_pedido_2015_limpo4")
camcuritiba_pedido_2015_limpo <- camcurped2015_sheet %>%
  gs_read()  
  
camcuritiba_pedido_2015 <- camcuritiba_pedido_2015_limpo %>%
  clean_names() %>%
  filter(pedido != "TESTE DO SISTEMA")

# 2015 recursos

camcuritiba_recurso_2015_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_recurso_2015_limpo.xlsx",
                                             col_types = c("text", "text", "date", 
                                                           "text", "date"))

camcuritiba_recurso_2015 <- camcuritiba_recurso_2015_limpo %>%
  clean_names() %>%
  rename(recurso_prim = recurso,
         data_recurso_prim = data_do_recurso,
         resposta_recurso_prim = resposta,
         data_resposta_recurso_prim = data_da_resposta)

camcuritiba2015 <- camcuritiba_pedido_2015 %>%
  full_join(camcuritiba_recurso_2015)

#em 2015, pedido e recurso possuem números diferentes

#pedidos 2016

camcuritiba_pedido_2016_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_pedido_2016_limpo.xlsx",
                                            col_types = c("text", "text", "date", 
                                                          "text", "date"))

camcuritiba_pedido_2016 <- camcuritiba_pedido_2016_limpo %>%
  clean_names() %>%
  filter(pedido != "SIC INCOMPLETO",
         pedido != "TESTE DO SISTEMA")

#recursos 2016

camcuritiba_recurso_2016_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_recurso_2016_limpo.xlsx",
                                             col_types = c("text", "text", "date", 
                                                           "text", "date"))

camcuritiba_recurso_2016 <- camcuritiba_recurso_2016_limpo %>%
  clean_names() %>%
  rename(recurso_prim = recurso,
         data_recurso_prim = data_do_recurso,
         resposta_recurso_prim = resposta,
         data_resposta_recurso_prim = data_da_resposta)
  
camcuritiba2016 <- camcuritiba_pedido_2016 %>%
  full_join(camcuritiba_recurso_2016)

#Pedidos 2017

camcuritiba_pedido_2017_limpo <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CâmCurtib/limpos/camcuritiba_pedido_2017_limpo.xlsx", 
                                            col_types = c("text", "text", "date", 
                                                          "text", "date"))

camcuritiba_pedido_2017 <- camcuritiba_pedido_2017_limpo %>%
  clean_names() %>%
  filter(pedido != "SIC INCOMPLETO",
         pedido != "TESTE DO SISTEMA")


#Template:

names(camcuritiba_pedido_2013) <- names(camcuritiba_pedido_2017)

camara_curitiba_final <- camcuritiba_pedido_2013 %>%
  rbind(camcuritiba_pedido_2017) %>%
  mutate(recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA) %>%
  rbind(camcuritiba2014) %>%
  rbind(camcuritiba2015) %>%
  rbind(camcuritiba2016) %>%
  rename(protocolo = codigo) %>%
  mutate(esfera = "municipal",
         poder = "legislativo",
         orgao = "camara municipal de curitiba",
         anexo = NA)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(camara_curitiba_final, file="camara_curitiba_final.Rdata")
