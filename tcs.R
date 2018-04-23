#TCs

library(dplyr)
library(janitor)
library(googledrive)

#Primeiro, eu vou juntar todas as tabelas.

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("listya_df_tribunais_de_contas.Rdata")
names(lista)

#tce-go
tce_go <- as.data.frame(lista[[1]])
tce_go %>% group_by(id) %>% summarise(p =n()) %>% filter(p>1) #sem duplicatas

# Observações :
# 1. não entendi o que é a coluna [8] recurso

tce_go_base <- tce_go %>%
  rename(protocolo = id,
         data_do_pedido  = data_solicitacao,
         pedido = pedido_descricao,
         data_da_resposta = data_resposta,
         anexo_com_extensao_resposta = anexo_da_resposta,
         recurso_1 = desc_recurso_a,
         resposta_recurso_1 = resposta_recurso,
         anexo_com_extensao_recurso_1 = anexo_do_recurso) %>%
  mutate(esfera = "estadual" , 
         poder = "tribunal de contas" ,
         orgao  = "tribunal de contas estadual de goias" ,  
         assunto = NA ,
         outros = NA , 
         atendimento  = NA,
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         pedido_pasta_do_anexo_pedido  = NA , 
         anexo_com_extensao_pedido = NA ,
         pasta_do_anexo_resposta = NA , 
         data_recurso_1 = NA ,
         pasta_do_anexo_recurso_1 = NA ,
         data_resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , 
         recurso_2 = NA ,
         pasta_do_anexo_recurso_2 = NA , 
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , pasta_do_anexo_resposta_recurso_1 , 
         anexo_com_extensao_resposta_recurso_1 , data_recurso_2 , recurso_2 , 
         pasta_do_anexo_recurso_2 , anexo_com_extensao_recurso_2 , data_resposta_recurso_2,
         resposta_recurso_2) 

tce_go_base$protocolo <- as.character(tce_go_base$protocolo)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tce_go_base, file="tce_go_base.Rdata")
rm(tce_go)

#TCE RR

tce_rr <- as.data.frame(lista[[3]])

#nem todos os protocolos estão preenchidos, nesse caso vou substituir pelas datas

tce_rr_base <- tce_rr %>%
  mutate(protocolo = ifelse(is.na(protocolo), data_recebimento, protocolo)) %>%
  group_by(protocolo) %>%
  mutate(num_interacao = 1:n()) %>%
  ungroup () %>%
  mutate(protocolo1 = as.character(protocolo),
         protocolo1 = ifelse(num_interacao > 1, paste(protocolo, 
                                                     num_interacao, sep="."), protocolo)) %>% 
  select(-c(protocolo)) %>%
  rename(protocolo = protocolo1,
         data_do_pedido = data_recebimento, 
         pedido = descricao_demanda,
         data_da_resposta = data_resposta_1,
         resposta = resposta_1,
         anexo_com_extensao_resposta = observacao) %>%
  mutate(esfera = "estadual", 
         poder = "tribunal de contas" , 
         orgao = "tribunal de contas estadual de roraima" ,
         assunto = NA , 
         outros = NA , 
         atendimento= NA , 
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA , 
         pedido_pasta_do_anexo_pedido = NA , 
         anexo_com_extensao_pedido = NA ,  
         pasta_do_anexo_resposta = NA ,
         data_recurso_1 = NA , 
         recurso_1 = NA , 
         pasta_do_anexo_recurso_1 = NA , 
         anexo_com_extensao_recurso_1 = NA , 
         data_resposta_recurso_1 = NA , 
         resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA , 
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , 
         recurso_2 = NA , 
         pasta_do_anexo_recurso_2 = NA , 
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , atendimento , 
         nao_e_pedido_de_informacao , contem_dados_pessoais , data_do_pedido ,
         pedido , pedido_pasta_do_anexo_pedido , anexo_com_extensao_pedido , 
         data_da_resposta , resposta , pasta_do_anexo_resposta , 
         anexo_com_extensao_resposta , data_recurso_1 , recurso_1 , 
         pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , 
         pasta_do_anexo_resposta_recurso_1 , anexo_com_extensao_resposta_recurso_1 , 
         data_recurso_2 , recurso_2 , pasta_do_anexo_recurso_2 , 
         anexo_com_extensao_recurso_2 , data_resposta_recurso_2,resposta_recurso_2) 

# tce_rr_base %>% group_by(protocolo) %>% summarise(p =n()) %>% filter(p>1)  
# x <- tce_rr_base %>%
#   filter(protocolo =="1422230400" | protocolo =="1423612800" | protocolo == "1464566400")
# 3 protocolos iguais, mas todos são pedidos diferentes feitos nos mesmos dias, 
# ajeitei acima. 

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tce_rr_base, file="tce_rr_base.Rdata")
rm(tce_rr)

# TCM- CE
tcm_ce <- as.data.frame(lista[[5]])

tcm_ce_base <- tcm_ce %>%
  clean_names() %>%
  rename(protocolo = nº,
         data_do_pedido = data,
         pedido = solicitacao,
         anexo_com_extensao_resposta = anexos) %>%
  mutate(esfera = "municipal" , 
         poder = "tribunal de contas" , 
         orgao = "tribunal de contas dos municipios do ceara" , 
         assunto = NA , 
         outros = NA  ,
         atendimento = NA  , 
         nao_e_pedido_de_informacao = NA  , 
         contem_dados_pessoais = NA  ,
         pedido_pasta_do_anexo_pedido  = NA  , 
         anexo_com_extensao_pedido = NA  , 
         data_da_resposta = NA  , 
         pasta_do_anexo_resposta = NA  ,
         data_recurso_1 = NA  , 
         recurso_1 = NA  , 
         pasta_do_anexo_recurso_1 = NA  ,
         anexo_com_extensao_recurso_1 = NA  , 
         data_resposta_recurso_1 = NA  ,
         resposta_recurso_1 = NA  , 
         pasta_do_anexo_resposta_recurso_1 = NA  , 
         anexo_com_extensao_resposta_recurso_1 = NA  , 
         data_recurso_2 = NA  ,
         recurso_2 = NA  , 
         pasta_do_anexo_recurso_2 = NA  , 
         anexo_com_extensao_recurso_2 = NA  ,
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA ) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , atendimento , 
         nao_e_pedido_de_informacao , contem_dados_pessoais , data_do_pedido , pedido , 
         pedido_pasta_do_anexo_pedido , anexo_com_extensao_pedido , data_da_resposta , 
         resposta , pasta_do_anexo_resposta , anexo_com_extensao_resposta , 
         data_recurso_1 , recurso_1 , pasta_do_anexo_recurso_1 , 
         anexo_com_extensao_recurso_1 , data_resposta_recurso_1 , resposta_recurso_1 , 
         pasta_do_anexo_resposta_recurso_1 , anexo_com_extensao_resposta_recurso_1 , 
         data_recurso_2 , recurso_2 , pasta_do_anexo_recurso_2 , 
         anexo_com_extensao_recurso_2 , data_resposta_recurso_2,resposta_recurso_2) 

    
#tcm_ce_base %>% group_by(manifestacao_email) %>% summarise(p =n()) %>% filter(p>1) 
#não tem dois iguais

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tcm_ce_base, file="tcm_ce_base.Rdata")
rm(tcm_ce)

# TCM RJ
tcm_rj <- as.data.frame(lista[[6]])

# As linhas que possuem "chamado pai" são recursos. Vou retirar da base e dar pedido com left_join

tcm_rj_recursos <- tcm_rj %>%
  mutate(chamado_pai = ifelse(is.na(chamado_pai), 0, chamado_pai)) %>%
  filter(chamado_pai != 0) %>%
  select(-c(chamado)) %>%
  rename(chamado = chamado_pai,
         data_recurso_1 = abertura ,
         data_resposta_recurso_1 = fechamento,
         recurso_1 = descricao ,
         resposta_recurso_1  = resposta) %>%
  mutate(id=1:n()) %>%
  filter(id != 1) %>%
  select(-c(categoria, macro_funcao, origem, status, sexo, idade, id))

tcm_rj_base <- tcm_rj %>%
  mutate(id = 1:n()) %>%
  filter(id != 965 , id != 1107 ) %>%
  select(-c(id, chamado_pai)) %>%
  left_join(tcm_rj_recursos, by="chamado") %>%
  rename(protocolo = chamado,
         data_do_pedido = abertura,
         data_da_resposta = fechamento,
         pedido  = descricao) %>%
  mutate(esfera = "municipal" , 
         poder = "tribunal de contas", 
         orgao = "tribunal de contas municipal do Rio de Janeiro",  
         assunto = NA , 
         outros = NA  , 
         atendimento = NA  ,
         nao_e_pedido_de_informacao = NA  , 
         contem_dados_pessoais = NA  , 
         pedido_pasta_do_anexo_pedido = NA  ,
         anexo_com_extensao_pedido = NA  ,
         pasta_do_anexo_resposta = NA  , 
         anexo_com_extensao_resposta = NA  ,
         pasta_do_anexo_recurso_1 = NA  , 
         anexo_com_extensao_recurso_1 = NA  , 
         pasta_do_anexo_resposta_recurso_1 = NA  , 
         anexo_com_extensao_resposta_recurso_1 = NA  , 
         data_recurso_2 = NA  , 
         recurso_2 = NA  , 
         pasta_do_anexo_recurso_2 = NA  , 
         anexo_com_extensao_recurso_2 = NA  , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA)  %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , 
         pasta_do_anexo_resposta_recurso_1 , anexo_com_extensao_resposta_recurso_1 , 
         data_recurso_2 , recurso_2 , pasta_do_anexo_recurso_2 , 
         anexo_com_extensao_recurso_2 , data_resposta_recurso_2,resposta_recurso_2) 

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tcm_rj_base, file="tcm_rj_base.Rdata")
rm(tcm_rj)
rm(tcm_rj_recursos)

#
tcm_sp <- as.data.frame(lista[[7]])

#base do TCM não conseguiu importar datas
#não sei o que quer dizer a aba controle

tcm_sp %>% group_by(atendimento) %>% summarise(p=n()) %>% filter(p>1)
#não tem duplicatas, e nem recursos

tcm_sp_base <- tcm_sp %>%
  rename(protocolo = atendimento,
         data_do_pedido = data_entrada,
         data_da_resposta = data_saida,
         pedido = pergunta) %>%
  mutate(esfera = "municipal" , 
         poder = "tribunal de contas", 
         orgao = "tribunal de contas do municipio de sao paulo" , 
         assunto = NA , 
         outros = NA  ,
         atendimento = NA  , 
         nao_e_pedido_de_informacao = NA  , 
         contem_dados_pessoais = NA  , 
         pedido_pasta_do_anexo_pedido = NA  , 
         anexo_com_extensao_pedido  = NA , 
         pasta_do_anexo_resposta = NA  , 
         anexo_com_extensao_resposta = NA  , 
         data_recurso_1 = NA  , 
         recurso_1 = NA  , 
         pasta_do_anexo_recurso_1 = NA  , 
         anexo_com_extensao_recurso_1 = NA  , 
         data_resposta_recurso_1 = NA  , 
         resposta_recurso_1 = NA  , 
         pasta_do_anexo_resposta_recurso_1 = NA  , 
         anexo_com_extensao_resposta_recurso_1 = NA  , 
         data_recurso_2 = NA  , 
         recurso_2 = NA  , 
         pasta_do_anexo_recurso_2 = NA  , 
         anexo_com_extensao_recurso_2 = NA  , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA ) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , 
         pasta_do_anexo_resposta_recurso_1 , anexo_com_extensao_resposta_recurso_1 , 
         data_recurso_2 , recurso_2 , pasta_do_anexo_recurso_2 , 
         anexo_com_extensao_recurso_2 , data_resposta_recurso_2,resposta_recurso_2)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tcm_sp_base, file="tcm_sp_base.Rdata")
rm(tcm_sp)

#TCU 

tcu <- as.data.frame(lista[[8]]) 
#base do TCU não conseguiu importar datas

tcu %>% group_by(manifestacao) %>% summarise(p=n()) %>% filter(p>1)

#não tem duplicatas e aparentemente nem recursos

tcu_base <- tcu %>%
  rename(protocolo = manifestacao,
         data_do_pedido = data_entrada,
         data_da_resposta = data_resposta,
         pedido = teor) %>%
    mutate(esfera = "federal" , 
           poder = "tribunal de contas" , 
           orgao = "tribunal de contas da uniao" , 
           assunto = NA , 
           outros = NA  , 
           atendimento = NA  , 
           nao_e_pedido_de_informacao = NA  , 
           contem_dados_pessoais = NA  , 
           pedido_pasta_do_anexo_pedido = NA  , 
           anexo_com_extensao_pedido = NA  ,
           pasta_do_anexo_resposta = NA  , 
           anexo_com_extensao_resposta = NA  ,
           data_recurso_1 = NA  , 
           recurso_1 = NA  , 
           pasta_do_anexo_recurso_1 = NA  , 
           anexo_com_extensao_recurso_1 = NA  , 
           data_resposta_recurso_1 = NA  , 
           resposta_recurso_1 = NA  , 
           pasta_do_anexo_resposta_recurso_1 = NA  , 
           anexo_com_extensao_resposta_recurso_1 = NA  , 
           data_recurso_2 = NA  , 
           recurso_2 = NA  , 
           pasta_do_anexo_recurso_2 = NA  , 
           anexo_com_extensao_recurso_2 = NA  , 
           data_resposta_recurso_2 = NA ,
           resposta_recurso_2 = NA ) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , 
         pasta_do_anexo_resposta_recurso_1 , anexo_com_extensao_resposta_recurso_1 , 
         data_recurso_2 , recurso_2 , pasta_do_anexo_recurso_2 , 
         anexo_com_extensao_recurso_2 , data_resposta_recurso_2,resposta_recurso_2)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tcu_base, file="tcu_base.Rdata")
rm(tcu)    
    

## TCE - PI, não entendi a diferença desta para a outra base
tce_pi_vdd <- as.data.frame(lista[[4]])

tce_pi_vdd_base <- tce_pi_vdd %>%
  mutate(protocolo = ifelse(protocolo == "Não Protocolado", data_de_entrada, protocolo),
         protocolo = ifelse(protocolo == "1483401600" & grepl("Olá, bom dia.", teor),
                            "1483401600.1", ifelse(protocolo == "1483401600" & !grepl("Olá, bom dia.", teor), 
                                                   "1483401600.2", protocolo))) %>%
  rename(data_do_pedido  = data_de_entrada,
         pedido = teor,
         data_da_resposta = data_de_resposta) %>%
  mutate(esfera = "estadual" , 
         poder = "tribunal de contas" ,
         orgao  = "tribunal de contas estadual de goias" ,  
         assunto = NA ,
         outros = NA , 
         atendimento  = NA,
         nao_e_pedido_de_informacao = NA , 
         contem_dados_pessoais = NA ,
         pedido_pasta_do_anexo_pedido  = NA , 
         anexo_com_extensao_pedido = NA ,
         pasta_do_anexo_resposta = NA , 
         data_recurso_1 = NA ,
         pasta_do_anexo_recurso_1 = NA ,
         data_resposta_recurso_1 = NA , 
         pasta_do_anexo_resposta_recurso_1 = NA ,
         anexo_com_extensao_resposta_recurso_1 = NA , 
         data_recurso_2 = NA , 
         recurso_2 = NA ,
         pasta_do_anexo_recurso_2 = NA , 
         anexo_com_extensao_recurso_2 = NA , 
         data_resposta_recurso_2 = NA ,
         resposta_recurso_2 = NA,
         anexo_com_extensao_resposta = NA,
         recurso_1 = NA,
         resposta_recurso_1 = NA,
         anexo_com_extensao_recurso_1 = NA) %>%
  select(esfera , poder , orgao , protocolo , assunto , outros , 
         atendimento , nao_e_pedido_de_informacao , contem_dados_pessoais , 
         data_do_pedido , pedido , pedido_pasta_do_anexo_pedido , 
         anexo_com_extensao_pedido , data_da_resposta , resposta , 
         pasta_do_anexo_resposta , anexo_com_extensao_resposta , data_recurso_1 , 
         recurso_1 , pasta_do_anexo_recurso_1 , anexo_com_extensao_recurso_1 , 
         data_resposta_recurso_1 , resposta_recurso_1 , pasta_do_anexo_resposta_recurso_1 , 
         anexo_com_extensao_resposta_recurso_1 , data_recurso_2 , recurso_2 , 
         pasta_do_anexo_recurso_2 , anexo_com_extensao_recurso_2 , data_resposta_recurso_2,
         resposta_recurso_2) 

#tce_pi_vdd_base %>% group_by(protocolo) %>% summarise(p=n()) %>% filter(p>1)
#Essa versão parece ter menos recursos que a outra, confirmar com o hugo qual usar.


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tce_pi_vdd_base, file="tce_pi_vdd_base.Rdata")
rm(tce_pi_vdd) 
