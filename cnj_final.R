library(dplyr)
library(janitor)
#CNJ

#2012

LAI_levantamento_per_e_resp_2012_FINAL_ <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/LAI levantamento per e resp 2012 (FINAL).xlsx",
                                                      col_types = c("numeric", "date", "text", 
                                                                    "text", "date")) %>%
  clean_names()


#2013
LAI_levantamento_per_e_resp_2013_FINAL_ <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/LAI levantamento per e resp 2013 (FINAL).xlsx", 
                                                      col_types = c("numeric", "date", "text", 
                                                                    "text", "date"))%>%
  clean_names() %>%
  rename(texto_relato = texto_do_relato)

#2014

LAI_levantamento_per_e_resp_2014_FINAL_ <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/LAI levantamento per e resp 2014 (FINAL).xlsx", 
                                                      col_types = c("text", "text", "text", 
                                                                    "date", "date"))%>%
  clean_names()

#2015 - cabeçalho é diferente

Levantamento_LAI_Respostas_e_Perguntas_2015_excel <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/Levantamento LAI Respostas e Perguntas 2015 excel.xlsx", 
                                                                col_types = c("text", "text", "text")) %>%
  clean_names() %>%
  mutate(data_relato = NA,
         data_resposta = NA) %>%
  rename(texto_relato = pergunta)

#2016

Levantamento_LAI_Respostas_e_Perguntas_2016_REV_RP_Excel_pdf <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/Levantamento LAI Respostas e Perguntas 2016 REV RP (Excel) pdf.xlsx", 
                                                                           col_types = c("text", "text", "text")) %>%
  clean_names() %>%
  mutate(data_relato = NA,
         data_resposta = NA) %>%
  rename(texto_relato = pergunta)

#2017

Levantamento_perguntas_e_respostas_LAI_2017_rev_RP_final_excel_docx <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/CNJ/Levantamento perguntas e respostas LAI 2017 rev RP final excel.docx.xlsx", 
                                                                                  col_types = c("text", "text", "text", 
                                                                                                "date", "date")) %>%
  clean_names() %>%
  rename(data_relato = data_de_entrada,
         data_resposta = data_de_resposta,
         texto_relato = pergunta)

#CNJ Base:

cnj_limpo <- LAI_levantamento_per_e_resp_2012_FINAL_  %>%
  rbind(LAI_levantamento_per_e_resp_2013_FINAL_) %>%
  rbind(LAI_levantamento_per_e_resp_2014_FINAL_) %>%
  rbind(Levantamento_LAI_Respostas_e_Perguntas_2015_excel) %>%
  rbind(Levantamento_LAI_Respostas_e_Perguntas_2016_REV_RP_Excel_pdf) %>%
  rbind(Levantamento_perguntas_e_respostas_LAI_2017_rev_RP_final_excel_docx) %>%
  mutate(texto_relato = gsub("-{2,}", "", texto_relato), #2 OU + TRAÇOS
         texto_relato = gsub("X{2,}", "", texto_relato), #DOIS OU MAIS X
         texto_relato = gsub("\\[\\]", "", texto_relato)) %>% # [] 
  rename(protocolo = cod_relato,
         data_do_pedido = data_relato,
         pedido = texto_relato,
         data_da_resposta = data_resposta) %>%
  mutate(anexo = NA,
         recurso_prim = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "federal",
         poder = "judiciario",
         orgao = "conselho nacional de justica")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(cnj_limpo , file="cnj_final.Rdata")

