
library(xlsx) 
#template para estagiários do legislativo:

# Câmara dos Deputados                     500    #divide por todos
# Câmara de Vereadores    PR    Curitiba   500    #divide por todos
# CNJ         500   #dividir por todos
# STJ         500   #dividir por todos





#cada planilha Leg + jud = 751 pedidos

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
load("alepe17_final.Rdata")              #alepe
load("camara_curitiba_final.Rdata")      #camara municipal de curitiba
load("camdep_final.Rdata")               #camara dos deputados
load("camfortaleza_final.Rdata")         #camara municipal de fortaleza
load("camsalvador_final.Rdata")          #camara municipal de salvador
load("cldf_final.Rdata")                 #Camara municipal do DF
load("cnj_final.Rdata")  #CNJ
load("basestj_limpa.Rdata") #STJ  
load("tjpe_pedidos.Rdata") #TJ-PE   
load("trf3_pedidos.Rdata") #TRF3 
load("tjsp.Rdata") #tjsp
load("trf2.Rdata") # TRF2 

# Ana

set.seed(10)

ana_dep <- data.frame(sample(camdep_final$protocolo, 125)) %>%            #      125 ok
  mutate(responsavel = "Ana")
colnames(ana_dep)=c("protocolo", "responsavel")

camdep2 <- camdep_final %>%
  left_join(ana_dep) %>%
  filter(is.na(responsavel))

ana_camcuritiba <- data.frame(sample(camara_curitiba_final$protocolo, 125)) %>% #      125 ok
  mutate(responsavel = "Ana")
colnames(ana_camcuritiba)=c("protocolo", "responsavel")

camara_curitiba_final2 <- camara_curitiba_final %>%
  left_join(ana_camcuritiba) %>%
  filter(is.na(responsavel))

ana_camfor <- data.frame(sample(camfortaleza_limpo$protocolo, 171 )) %>%  # CâmFortaleza   171 ok
  mutate(responsavel = "Ana")

ana_camsalvador <- data.frame(sample(camsalvador_limpo$protocolo, 87)) %>% # CâmSalvador    87
  mutate(responsavel = "Ana")
colnames(ana_camsalvador)=c("protocolo", "responsavel")
ana_camsalvador$protocolo <- as.character(ana_camsalvador$protocolo)


ana_cnj <- data.frame(sample(cnj_limpo$protocolo, 125))  %>%             # CNJ* 125
  mutate(responsavel = "Ana")
colnames(ana_cnj)=c("protocolo", "responsavel")

ana_stj <- data.frame(sample(basestj$protocolo, 137)) %>%                      # STJ* 137
  mutate(responsavel = "Ana")
colnames(ana_stj)=c("protocolo", "responsavel")

# José

set.seed(8)
jose_dep <- data.frame(sample(camdep2$protocolo, 125)) %>%
  mutate(responsavel = "José")
colnames(jose_dep)=c("protocolo", "responsavel")

camdep3 <- camdep2 %>%
  select(-c(responsavel)) %>%
  left_join(jose_dep, by="protocolo") %>%
  filter(is.na(responsavel))

jose_camcuritiba <- data.frame(sample(camara_curitiba_final2$protocolo, 125))  %>% # 125
  mutate(responsavel = "José")
colnames(jose_camcuritiba)=c("protocolo", "responsavel")

camara_curitiba_final3 <- camara_curitiba_final2 %>%
  select(-c(responsavel)) %>%
  left_join(jose_camcuritiba, by="protocolo") %>%
  filter(is.na(responsavel))

                                                                             # TJ-PE  179
                                                                             # CNJ*   121
                                                                             # STJ*   119
                                                                             # TJMT    36 *nao se sabe
# Lizandra

set.seed(12)
liz_dep <- data.frame(sample(camdep3$protocolo, 125)) %>%
  mutate(responsavel = "Lizandra")
colnames(liz_dep)=c("protocolo", "responsavel")
  
camdep4 <- camdep3 %>%
  select(-c(responsavel)) %>%
  left_join(liz_dep, by="protocolo") %>%
  filter(is.na(responsavel))

liz_camcuritiba <- data.frame(sample(camara_curitiba_final3$protocolo, 125)) %>% # 125
  mutate(responsavel = "Lizandra")
colnames(liz_camcuritiba)=c("protocolo", "responsavel")

camara_curitiba_final4 <- camara_curitiba_final3 %>%
  select(-c(responsavel)) %>%
  left_join(liz_camcuritiba, by="protocolo") %>%
  filter(is.na(responsavel))

liz_cldf <- cldf_limpo %>% # CLDF  160
  mutate(responsavel = "Lizandra") %>%
  select(protocolo, responsavel)

liz_alepe <- alepe17_limpo %>% # ALPE  8
  mutate(responsavel = "Lizandra",
         protocolo = as.character(protocolo)) %>%
  select(protocolo, responsavel)

                                                                            # CNJ*  121
                                                                            # STJ*  119
                                                                            # TST   22
                                                                            # TJSP  31
                                                                            
                                                                            
# Lucas

set.seed(445)
lucas_dep <- data.frame(sample(camdep4$protocolo, 125)) %>%
  mutate(responsavel = "Lucas")
colnames(lucas_dep)=c("protocolo", "responsavel")
  
lucas_camcuritiba <- data.frame(sample(camara_curitiba_final4$protocolo, 125)) %>% #125
  mutate(responsavel = "Lucas")
colnames(lucas_camcuritiba)=c("protocolo", "responsavel")
                                                                              # CNJ*  121
                                                                              # STJ*  119
                                                                              # TRF3  123
                                                                              # TRF2  107

#juntando tudo do legislativo 
{
  base_legislativo1 <- ana_dep %>%
    rbind(ana_camcuritiba) %>%
    rbind(ana_camsalvador) %>%
    rbind(jose_dep) %>%
    rbind(jose_camcuritiba) %>%
    rbind(liz_dep) %>%
    rbind(liz_camcuritiba) %>%
    rbind(liz_cldf) %>%
    rbind(lucas_dep) %>%
    rbind(lucas_camcuritiba) %>%
    full_join(base_legislativo, by = "protocolo") %>%
    mutate(responsavel = ifelse(orgao == "assembleia legislativa de pernambuco",
                                "Lizandra", ifelse(orgao == "camara municipal de fortaleza",
                                                   "Ana",responsavel)))
  exemplo_leg <-  base_legislativo1 %>%
    filter(is.na(responsavel))

  exemplo_leg1 <- data.frame(sample(exemplo_leg$protocolo, 100))
  colnames(exemplo_leg1) <- c("protocolo")
  
  exemplo_leg2 <- exemplo_leg1 %>%
  left_join(base_legislativo1)
  
  ana_leg_final <- base_legislativo1 %>%
    filter(responsavel == "Ana")
  
  lizandra_leg_final <- base_legislativo1 %>%
    filter(responsavel == "Lizandra")
  jose_leg_final <- base_legislativo1 %>%
    filter(responsavel == "José")
  lucas_leg_final <- base_legislativo1 %>%
    filter(responsavel == "Lucas")
}

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(exemplo_leg2 , file="exemplos_treinamento.Rdata")

save(ana_leg_final , file="ana_legislativo.Rdata")
write.table(ana_leg_final , file="ana_legislativo.csv", 
            sep=";", row.names=F, na="", quote = F)
write.xlsx(ana_leg_final, file = "ana_legislativo.xlsx",
           sheetName = "ana_legislativo", row.names = FALSE)

save(lizandra_leg_final , file="lizandra_legislativo.Rdata")
write.table(lizandra_leg_final , file="lizandra_legislativo.csv", 
            sep=";", row.names=F, na="", quote = F)
write.xlsx(lizandra_leg_final, file = "lizandra_legislativo.xlsx",
           sheetName = "lizandra_legislativo", row.names = FALSE)

save(jose_leg_final , file="jose_legislativo.Rdata")
write.table(jose_leg_final , file="jose_legislativo.csv", 
            sep=";", row.names=F, na="", quote = F)
write.xlsx(jose_leg_final, file = "jose_legislativo.xlsx",
           sheetName = "jose_legislativo", row.names = FALSE)

save(lucas_leg_final , file="lucas_legislativo.Rdata")
write.table(lucas_leg_final , file="lucas_legislativo.csv", 
            sep=";", row.names=F, na="", quote = F)
write.xlsx(lucas_leg_final, file = "lucas_legislativo.xlsx",
           sheetName = "lucas_legislativo", row.names = FALSE)
