library(dplyr)
library(googledrive)
library(xlsx)

#divisão pedidos tcs

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("tce_pi_vdd_base.Rdata")
load("tce_go_base.Rdata")
load("tce_rr_base.Rdata")
load("tcm_ce_base.Rdata")
load("tcm_rj_base.Rdata")
load("tcm_sp_base.Rdata")
load("tcu_base.Rdata")

# Ana
# TCM - RJ		125 ok
# TCM - SP		122 ok
# Tribunal de Contas da União		125
# Tribunal de Contas	RR	32 - inteira
# TOTAL		404

set.seed(10)

ana_tcm_rj_prot <-data.frame(sample(tcm_rj_base$protocolo, 125))  %>%
  mutate(responsavel = "Ana")
colnames(ana_tcm_rj_prot)=c("protocolo", "responsavel")

ana_tcm_rj_prot$protocolo <- as.character(ana_tcm_rj_prot$protocolo)
tcm_rj_base$protocolo <- as.character(tcm_rj_base$protocolo)

ana_tcm_rj <- tcm_rj_base %>%
  left_join(ana_tcm_rj_prot) %>%
  filter(!is.na(responsavel)) %>%
  select(-c(data_do_pedido, data_da_resposta))

tcm_rj_base2 <- tcm_rj_base %>%
  left_join(ana_tcm_rj) %>%
  filter(is.na(responsavel))

#

ana_tcm_sp_prot <-data.frame(sample(tcm_sp_base$protocolo, 125))  %>%
  mutate(responsavel = "Ana")
colnames(ana_tcm_sp_prot)=c("protocolo", "responsavel")

ana_tcm_sp_prot$protocolo <- as.character(ana_tcm_sp_prot$protocolo)
tcm_sp_base$protocolo <- as.character(tcm_sp_base$protocolo)

ana_tcm_sp <- tcm_sp_base %>%
  left_join(ana_tcm_sp_prot) %>%
  filter(!is.na(responsavel)) %>%
  select(-c(data_do_pedido, data_da_resposta))

tcm_sp_base2 <- tcm_sp_base %>%
  left_join(ana_tcm_sp_prot) %>%
  filter(is.na(responsavel))

#

ana_tcu_prot <-data.frame(sample(tcu_base$protocolo, 125))  %>%
  mutate(responsavel = "Ana")
colnames(ana_tcu_prot)=c("protocolo", "responsavel")

ana_tcu_prot$protocolo <- as.character(ana_tcu_prot$protocolo)
tcu_base$protocolo <- as.character(tcu_base$protocolo)

ana_tcu <- tcu_base %>%
  left_join(ana_tcu_prot) %>%
  filter(!is.na(responsavel)) %>%
  select(-c(data_do_pedido, data_da_resposta))

tcu_base2 <-tcu_base %>%
  left_join(ana_tcu_prot) %>%
  filter(is.na(responsavel))

#

ana_tce_rr <- tce_rr_base %>%
  mutate(responsavel = "Ana",
         protocolo = as.character(protocolo)) %>%
  select(-c(data_do_pedido, data_da_resposta))

#

ana_tcs <- ana_tcm_rj %>%
  bind_rows(ana_tcm_sp) %>%
  bind_rows(ana_tcu) %>%
  bind_rows(ana_tce_rr) 


####### ATENÇÃO
## retirei as colunas data_do_pedido, data_da_resposta pq as datas não estavam padronizadas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
drive_find(n_max=10)  
write.xlsx(ana_tcs, file="ana_tcs.xlsx", sheetName="ana_tcs", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

ana_tcs_df_sheet <- drive_upload(
  "ana_tcs.xlsx",
  path="~/TB/2018/Achados e Pedidos/COLAB/Ana Alini Lins/",
  name = "ana_tcs",
  type = "spreadsheet")


#

# Órgão		José
# TCM - RJ	125 ok
# TCM - SP	122
# Tribunal de Contas	CE		50
# Tribunal de Contas	RR	32	
# Tribunal de Contas da União		113
# TOTAL		410

# TCM - RJ	125

set.seed(22)

jose_tcm_rj_prot <-data.frame(sample(tcm_rj_base2$protocolo, 125))  %>%
  mutate(responsavel = "José")
colnames(jose_tcm_rj_prot)=c("protocolo", "responsavel")

jose_tcm_rj_prot$protocolo <- as.character(jose_tcm_rj_prot$protocolo)
tcm_rj_base2$protocolo <- as.character(tcm_rj_base2$protocolo)

jose_tcm_rj <- tcm_rj_base2 %>%
  select(-c(data_do_pedido, data_da_resposta, responsavel)) %>%
  left_join(jose_tcm_rj_prot) %>%
  filter(!is.na(responsavel))
  
tcm_rj_base3 <- tcm_rj_base2 %>%
  select(-c(data_do_pedido, data_da_resposta, responsavel)) %>%
  left_join(jose_tcm_rj_prot) %>%
  filter(is.na(responsavel))

# TCM - SP

jose_tcm_sp_prot <-data.frame(sample(tcm_sp_base2$protocolo, 122))  %>%
  mutate(responsavel = "José")
colnames(jose_tcm_sp_prot)=c("protocolo", "responsavel")

jose_tcm_sp_prot$protocolo <- as.character(jose_tcm_sp_prot$protocolo)
tcm_sp_base2$protocolo <- as.character(tcm_sp_base2$protocolo)

jose_tcm_sp <- tcm_sp_base2 %>%
  select(-c(data_do_pedido, data_da_resposta, responsavel)) %>%
  left_join(jose_tcm_sp_prot) %>%
  filter(!is.na(responsavel)) 

tcm_sp_base3 <- tcm_sp_base2 %>%
  select(-c(data_do_pedido, data_da_resposta, responsavel)) %>%
  left_join(jose_tcm_sp_prot) %>%
  filter(is.na(responsavel))

# Tribunal de Contas	CE 50

jose_tce_ce <- tcm_ce_base %>%
  mutate(responsavel = "José",
         protocolo = as.character(protocolo)) %>%
  select(-c(data_do_pedido, data_da_resposta))

# Tribunal de Contas	RR	32	

jose_tce_rr <- tce_rr_base %>%
  mutate(responsavel = "José") %>%
  select(-c(data_do_pedido, data_da_resposta))

# Tribunal de Contas da União		113

jose_tcu_prot <-data.frame(sample(tcu_base2$protocolo, 113))  %>%
  mutate(responsavel = "José")
colnames(jose_tcu_prot)=c("protocolo", "responsavel")

jose_tcu_prot$protocolo <- as.character(jose_tcu_prot$protocolo)
tcu_base2$protocolo <- as.character(tcu_base2$protocolo)

jose_tcu <- tcu_base2 %>%
  select(-c(data_do_pedido, data_da_resposta, responsavel)) %>%
  left_join(jose_tcu_prot) %>%
  filter(!is.na(responsavel)) 

tcu_base3 <- tcu_base2 %>%
  select(-c(responsavel)) %>%
  left_join(jose_tcu_prot) %>%
  filter(is.na(responsavel))

#Final

jose_tcs <- jose_tcm_rj %>%
  bind_rows(jose_tcm_sp) %>%
  bind_rows(jose_tce_ce) %>%
  bind_rows(jose_tce_rr) %>%
  bind_rows(jose_tcu)

####### ATENÇÃO
## retirei as colunas data_do_pedido, data_da_resposta pq as datas não estavam padronizadas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")

write.xlsx(jose_tcs, file="jose_tcs.xlsx", sheetName="jose_tcs", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

jose_tcs_df_sheet <- drive_upload(
  "jose_tcs.xlsx",
  path="~/TB/2018/Achados e Pedidos/COLAB/José Vitor da Silva/",
  name = "jose_tcs",
  type = "spreadsheet")


#

# Órgão		Lucas
# TCM - RJ		125 ok
# TCM - SP		122 ok
# Tribunal de Contas da União		99
# Tribunal de Contas	PI	64
# TOTAL		410

# TCM RJ 125

set.seed(77)

lucas_tcm_rj_prot <-data.frame(sample(tcm_rj_base3$protocolo, 125))  %>%
  mutate(responsavel = "Lucas")
colnames(lucas_tcm_rj_prot)=c("protocolo", "responsavel")

lucas_tcm_rj_prot$protocolo <- as.character(lucas_tcm_rj_prot$protocolo)
tcm_rj_base3$protocolo <- as.character(tcm_rj_base3$protocolo)

lucas_tcm_rj <- tcm_rj_base3 %>%
  select(-c(responsavel)) %>%
  left_join(lucas_tcm_rj_prot) %>%
  filter(!is.na(responsavel))

tcm_rj_base4 <- tcm_rj_base3 %>%
  select(-c(responsavel)) %>%
  left_join(lucas_tcm_rj_prot) %>%
  filter(is.na(responsavel))

# TCM - SP		122 

lucas_tcm_sp_prot <-data.frame(sample(tcm_sp_base3$protocolo, 122))  %>%
  mutate(responsavel = "Lucas")
colnames(lucas_tcm_sp_prot)=c("protocolo", "responsavel")

lucas_tcm_sp_prot$protocolo <- as.character(lucas_tcm_sp_prot$protocolo)
tcm_sp_base3$protocolo <- as.character(tcm_sp_base3$protocolo)

lucas_tcm_sp <- tcm_sp_base3 %>%
  select(-c(responsavel)) %>%
  left_join(lucas_tcm_sp_prot) %>%
  filter(!is.na(responsavel)) 

tcm_sp_base4 <- tcm_sp_base3 %>%
  select(-c(responsavel)) %>%
  left_join(lucas_tcm_sp_prot) %>%
  filter(is.na(responsavel))

# Tribunal de Contas da União		99

lucas_tcu_prot <-data.frame(sample(tcu_base3$protocolo, 99))  %>%
  mutate(responsavel = "Lucas")
colnames(lucas_tcu_prot)=c("protocolo", "responsavel")

lucas_tcu_prot$protocolo <- as.character(lucas_tcu_prot$protocolo)
tcu_base3$protocolo <- as.character(tcu_base3$protocolo)

lucas_tcu <- tcu_base3 %>%
  select(-c(responsavel, data_do_pedido, data_da_resposta)) %>%
  left_join(lucas_tcu_prot) %>%
  filter(!is.na(responsavel)) 

tcu_base4 <- tcu_base3 %>%
  select(-c(responsavel)) %>%
  left_join(lucas_tcu_prot) %>%
  filter(is.na(responsavel))

# Tribunal de Contas	PI	64

lucas_tce_pi <- tce_pi_vdd_base %>%
  mutate(responsavel = "Lucas") %>%
  select(-c(data_do_pedido, data_da_resposta))
  

#Final

lucas_tcs <- lucas_tcm_rj %>%
  bind_rows(lucas_tcm_sp) %>%
  bind_rows(lucas_tcu) %>%
  bind_rows(lucas_tce_pi) %>%
  select(-c(data_do_pedido, data_da_resposta))

####### ATENÇÃO
## retirei as colunas data_do_pedido, data_da_resposta pq as datas não estavam padronizadas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")

write.xlsx(lucas_tcs, file="lucas_tcs.xlsx", sheetName="lucas_tcs", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

lucas_tcs_df_sheet <- drive_upload(
  "lucas_tcs.xlsx",
  path="~/TB/2018/Achados e Pedidos/COLAB/Lucas Alves/",
  name = "lucas_tcs",
  type = "spreadsheet")


# Órgão		Lizandra
# TCM - RJ		125 ok
# TCM - SP		122
# Tribunal de Contas da União		163
# TOTAL		410

set.seed(312)

liz_tcm_rj_prot <-data.frame(sample(tcm_rj_base4$protocolo, 125))  %>%
  mutate(responsavel = "Lizandra")
colnames(liz_tcm_rj_prot)=c("protocolo", "responsavel")

liz_tcm_rj_prot$protocolo <- as.character(liz_tcm_rj_prot$protocolo)
tcm_rj_base4$protocolo <- as.character(tcm_rj_base4$protocolo)

liz_tcm_rj <- tcm_rj_base4 %>%
  select(-c(responsavel)) %>%
  left_join(liz_tcm_rj_prot) %>%
  filter(!is.na(responsavel))

# TCM - SP

lizandra_tcm_sp <- tcm_sp_base4 %>%
  mutate(responsavel = "Lizandra") 

# Tribunal de Contas da União		163

liz_tcu_prot <-data.frame(sample(tcu_base4$protocolo, 163))  %>%
  mutate(responsavel = "Lizandra")
colnames(liz_tcu_prot)=c("protocolo", "responsavel")

liz_tcu_prot$protocolo <- as.character(liz_tcu_prot$protocolo)
tcu_base4$protocolo <- as.character(tcu_base4$protocolo)

liz_tcu <- tcu_base4 %>%
  select(-c(responsavel, data_do_pedido, data_da_resposta)) %>%
  left_join(liz_tcu_prot) %>%
  filter(!is.na(responsavel)) 

#Final

liz_tcs <- liz_tcm_rj %>%
  bind_rows(lizandra_tcm_sp) %>%
  bind_rows(liz_tcu) %>%
  select(-c(data_do_pedido, data_da_resposta))

####### ATENÇÃO
## retirei as colunas data_do_pedido, data_da_resposta pq as datas não estavam padronizadas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")

write.xlsx(liz_tcs, file="liz_tcs.xlsx", sheetName="liz_tcs", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

liz_tcs_df_sheet <- drive_upload(
  "liz_tcs.xlsx",
  path="~/TB/2018/Achados e Pedidos/COLAB/Lizandra Aguiar Pinto de Oliveira/",
  name = "liz_tcs",
  type = "spreadsheet")

