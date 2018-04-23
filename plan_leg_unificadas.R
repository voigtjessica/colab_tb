library(googlesheets)
library(dplyr)
library(janitor)
library(xlsx)

url_alini <- "https://drive.google.com/open?id=1DMqgGj9exXReQBDW6cYaKPxNsb-LKM_0C_EUIXwggCY"
gs_ls() 
ana_sheet <- gs_title("ana_legislativo")
ana_legis <- ana_sheet %>%
  gs_read() %>%
  clean_names()

url_jose <- "https://docs.google.com/spreadsheets/d/10IisL1u-dGsev1G8B0CPxG9GdhRfGJE0VU5KvhKHhmg/edit?usp=sharing"
gs_ls() 
jose_sheet <- gs_title("jose_legislativo")
jose_legis <- jose_sheet %>%
  gs_read()%>%
  clean_names()

url_liz <- "https://docs.google.com/spreadsheets/d/1_khfedY9nMtRieavxXhERx0LmPTuIYoyWR_nvHEE7GE/edit?usp=sharing"
gs_ls() 
liz_sheet <- gs_title("lizandra_legislativo")
liz_legis <- liz_sheet %>%
  gs_read()%>%
  clean_names()

url_lucas <-"https://docs.google.com/spreadsheets/d/1p8wrmEqwI_NiS84DTflaK4Ws_GkJjU8QSP1MDpiHYVo/edit?usp=sharing"
gs_ls() 
lucas_sheet <- gs_title("lucas_legislativo")
lucas_legis <- lucas_sheet %>%
  gs_read()%>%
  clean_names()


# #N/A

legis_classificado <- ana_legis %>%
  mutate(nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao)) %>%
  bind_rows(jose_legis) %>%
  bind_rows(liz_legis) %>%
  bind_rows(lucas_legis)

legis_classificado[] <- lapply(legis_classificado, gsub, pattern='#N/A', replacement=NA)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(legis_classificado, file="legis_classificado.Rdata")
write.xlsx(as.data.frame(legis_classificado), file="legis_classificado.xlsx", sheetName="legis_classificado", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

#por alguma razão, o write.xlsx me retornava um erro:
# 
# Error in .jcall(cell, "V", "setCellValue", value) : 
#   method setCellValue with signature ([Ljava/lang/String;)V not found

#Pelo o que eu pesquisei, esse erro foi provocado porque o meu objeto não era lido como
# um data frame, por isso eu inseri o comando as.data.frame() no write.xlsx()
