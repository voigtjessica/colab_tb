
library(dplyr) 
library(janitor)
library(stringr)

abril <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/TRF2/abril.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "text", "text"))

marco <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/TRF2/marco.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "text", "text"))

#eu mantive a data da resposta como o character porque o ano aqui tem dois formatos e eu não tenho tempo de arrumar isso agora
trf2_final <- abril %>%
  rbind(marco) %>%
  clean_names() %>%
  mutate(resposta_coicid_data = as.character(resposta_coicid_data),
         data_da_resposta = word(resposta_coicid_data,-1), #normalmente a data é a ultima palavra
         data_da_resposta = gsub(".*\\.","",data_da_resposta))  %>% #removendo tudo antes do ponto, jã que algumas datas vieram com a frase anterior junto
  rename(pedido = assunto,
         data_do_pedido = data,
         resposta = resposta_coicid_data) %>%
  mutate(anexo = NA,
         recurso_prim = NA,
         recurso_prim = NA,
         data_recurso_prim = NA,
         resposta_recurso_prim = NA,
         data_resposta_recurso_prim = NA,
         recurso_seg = NA,
         data_recurso_seg = NA,
         esfera = "federal",
         poder = "judiciário",
         orgao = "tribunal regional federal da segunda regiao") %>%
  select(protocolo,data_do_pedido,pedido,data_da_resposta,resposta,anexo,
         recurso_prim,data_recurso_prim,resposta_recurso_prim,data_resposta_recurso_prim,
         recurso_seg,data_recurso_seg,esfera,poder,orgao)  

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(trf2_final , file="trf2_final.Rdata")
