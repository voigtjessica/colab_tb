# Poder executivo
# "base_gov_ro.R" - a base de rondônia não foi utilizada porque ela está com problema na origem        

#Os poderes executivos serão tratados da seguinte forma:
# LDA1 - CGM
# LDA2 - Prefeituras
# LDA3 - Governos estaduais
# Governo federal dividido nos órgãos que serão classificados um por um. (Manoel vai fazer isso)


# Nesse script eu deixo as bases iguais e salvo os Rdatas.

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

library(dplyr)
library(data.table)
library(janitor)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")

list.files(path = "C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb", 
          full.names = FALSE, recursive = TRUE)

load("base_cgm_sp_final.Rdata")
load("base_cgu_completa.Rdata")            

# CGM - SP
 
all.equal(base_cgm_sp_final,base_cgu_completa, check.names = TRUE, tolerance = FALSE,
          scale = FALSE, check.attributes = FALSE)

#resolvendo header diferente:

base_cgm_sp_final <- base_cgm_sp_final %>%
  rename(recurso_1 = recurso1) %>%
  select(-c(situacao))

base_cgu_completa <- base_cgu_completa %>%
  mutate(pasta_do_anexo_recurso_4 = NA,
         pasta_do_anexo_recurso_3 = NA)

#deixando tudo como character:

ix <- 1:51 
base_cgu_completa[ix] <- lapply(base_cgu_completa[ix], as.character) 
base_cgm_sp_final[ix] <- lapply(base_cgm_sp_final[ix], as.character)

#Base da CGU vai ser a minha orientação daqui pra frente.
#Gov Maranhão

load("base_gov_ma.Rdata")

# vendo se tudo está igual:
teste_igualdade_nomes_var_df(base_cgu_completa, base_gov_ma)

#resolvendo isso e outras coisas que eu já vi que estavam erradas:

base_gov_ma <- base_gov_ma %>%
  select(-c(responsavel)) %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA , 
         anexo_com_extensao_resposta_recurso_3 = NA , 
         data_recurso_4 = NA , 
         recurso_4 = NA , 
         pasta_do_anexo_resposta_recurso_4 = NA , 
         anexo_com_extensao_resposta_recurso_4 = NA , 
         data_resposta_recurso_4 = NA , 
         resposta_recurso_4 = NA , 
         anexo_com_extensao_recurso_4 = NA , 
         pasta_do_anexo_recurso_4 = NA , 
         pasta_do_anexo_recurso_3 = NA,
         esfera = "estadual",
         poder = "executivo",
         orgao = "governo de estado do maranhao")

#colocando tudo como character
base_gov_ma[ix] <- lapply(base_gov_ma[ix], as.character)
glimpse(base_gov_ma)

# Gov MG:
load("base_gov_mg.Rdata")                  

glimpse(base_gov_mg)
teste_igualdade_nomes_var_df(base_cgu_completa, base_gov_mg)

base_gov_mg <- base_gov_mg %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA )

base_gov_mg[ix] <- lapply(base_gov_mg[ix], as.character)

#Gov RN

load("base_gov_rn.Rdata")  
glimpse(base_gov_rn)
teste_igualdade_nomes_var_df(base_cgu_completa, base_gov_rn)

base_gov_rn <- base_gov_rn %>%
  select(-c(StatusPedido)) %>%
  rename(data_do_pedido = DataCadastro,
         data_da_resposta = DataFinalizacao,
         pedido = Descricao,
         resposta = Resposta) %>%
  mutate(pasta_do_anexo_pedido = NA, 
         anexo_com_extensao_pedido = NA, 
         pasta_do_anexo_resposta = NA, 
         anexo_com_extensao_resposta = NA, 
         recurso_1 = NA, 
         data_recurso_1 = NA, 
         pasta_do_anexo_recurso_1 = NA, 
         anexo_com_extensao_recurso_1 = NA, 
         data_resposta_recurso_1 = NA, 
         resposta_recurso_1 = NA, 
         pasta_do_anexo_resposta_recurso_1 = NA, 
         anexo_com_extensao_resposta_recurso_1 = NA, 
         data_recurso_2 = NA, 
         recurso_2 = NA, 
         pasta_do_anexo_recurso_2 = NA, 
         anexo_com_extensao_recurso_2 = NA, 
         data_resposta_recurso_2 = NA, 
         resposta_recurso_2 = NA, 
         pasta_do_anexo_resposta_recurso_2 = NA, 
         anexo_com_extensao_resposta_recurso_2 = NA, 
         data_recurso_3 = NA, 
         recurso_3 = NA, 
         pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_resposta_recurso_3 = NA, 
         resposta_recurso_3 = NA, 
         anexo_com_extensao_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         esfera = "estadual",
         poder = "executivo", 
         orgao = "governo do estado do rio grande do norte", 
         assunto = NA, 
         outros = NA, 
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA,
         contem_dados_pessoais = NA, 
         e_complementacao_de_pedido = NA, 
         resposta_duplicada = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA)

base_gov_rn[ix] <- lapply(base_gov_rn[ix], as.character)

# Gov rs
          
load("base_gov_rs.Rdata")   
glimpse(base_gov_rs)
teste_igualdade_nomes_var_df(base_cgu_completa, base_gov_rs)

base_gov_rs <- base_gov_rs %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, pasta_do_anexo_recurso_3 = NA)

base_gov_rs[ix] <- lapply(base_gov_rs[ix], as.character)

# Pref João Pessoa

load("base_pref_jp.Rdata")
glimpse(base_pref_jp)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_jp)

base_pref_jp <- base_pref_jp %>%
  select(-c(numero, status)) %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA
         , data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA)

base_pref_jp[ix] <- lapply(base_pref_jp[ix], as.character)

# Prefeitura de porto velho

load("base_pref_porto_velho.Rdata")
glimpse(base_pref_porto_velho)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_porto_velho)

base_pref_porto_velho <- base_pref_porto_velho %>%
  mutate( pasta_do_anexo_resposta_recurso_3 = NA, 
          anexo_com_extensao_resposta_recurso_3 = NA, 
          data_recurso_4 = NA, 
          recurso_4 = NA, 
          pasta_do_anexo_resposta_recurso_4 = NA, 
          anexo_com_extensao_resposta_recurso_4 = NA, 
          data_resposta_recurso_4 = NA, 
          resposta_recurso_4 = NA, 
          anexo_com_extensao_recurso_4 = NA, 
          pasta_do_anexo_recurso_4 = NA, pasta_do_anexo_recurso_3 = NA)

base_pref_porto_velho[ix] <- lapply(base_pref_porto_velho[ix], as.character)

# Prefeitura de Recife

load("base_pref_recife.Rdata")
glimpse(base_pref_recife)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_recife)

base_pref_recife <- base_pref_recife %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA)

base_pref_recife[ix] <- lapply(base_pref_recife[ix], as.character)

# Prefeitura de Rio Branco

load("base_pref_rio_branco.Rdata")
glimpse(base_pref_rio_branco)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_rio_branco)

base_pref_rio_branco <- base_pref_rio_branco %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, 
         pasta_do_anexo_recurso_3 = NA)

base_pref_rio_branco[ix] <- lapply(base_pref_rio_branco[ix], as.character)

#Salvadô

load("base_pref_salvador.Rdata")
glimpse(base_pref_salvador)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_salvador)

base_pref_salvador <- base_pref_salvador %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, pasta_do_anexo_recurso_3 = NA)

base_pref_salvador[ix] <- lapply(base_pref_salvador[ix], as.character)

#Prefeitura Vitória

load("base_pref_vitoria.Rdata")
glimpse(base_pref_vitoria)
teste_igualdade_nomes_var_df(base_cgu_completa, base_pref_vitoria)

base_pref_vitoria <- base_pref_vitoria %>%
  mutate(pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         pasta_do_anexo_recurso_4 = NA, pasta_do_anexo_recurso_3 = NA )

base_pref_vitoria[ix] <- lapply(base_pref_vitoria[ix], as.character)

#Gov Alagoas

load("goval_final.Rdata")
glimpse(goval_final)
teste_igualdade_nomes_var_df(base_cgu_completa, goval_final)

goval_final <- goval_final %>%
  rename(recurso_1 = recurso_prim,
         data_recurso_1 = data_recurso_prim,
         resposta_recurso_1 = resposta_recurso_prim,
         data_resposta_recurso_1 = data_resposta_recurso_prim,
         recurso_2 = recurso_seg,
         data_recurso_2 = data_recurso_seg ) %>%
  mutate(pasta_do_anexo_pedido = NA, 
         anexo_com_extensao_pedido = NA, 
         pasta_do_anexo_resposta = NA, 
         anexo_com_extensao_resposta = NA, 
         pasta_do_anexo_recurso_1 = NA, 
         anexo_com_extensao_recurso_1 = NA, 
         pasta_do_anexo_resposta_recurso_1 = NA, 
         anexo_com_extensao_resposta_recurso_1 = NA, 
         pasta_do_anexo_recurso_2 = NA, 
         anexo_com_extensao_recurso_2 = NA, 
         data_resposta_recurso_2 = NA, 
         resposta_recurso_2 = NA, 
         pasta_do_anexo_resposta_recurso_2 = NA, 
         anexo_com_extensao_resposta_recurso_2 = NA, 
         data_recurso_3 = NA, 
         recurso_3 = NA, 
         pasta_do_anexo_resposta_recurso_3 = NA, 
         anexo_com_extensao_resposta_recurso_3 = NA, 
         data_resposta_recurso_3 = NA, 
         resposta_recurso_3 = NA, 
         anexo_com_extensao_recurso_3 = NA, 
         data_recurso_4 = NA, 
         recurso_4 = NA, 
         pasta_do_anexo_resposta_recurso_4 = NA, 
         anexo_com_extensao_resposta_recurso_4 = NA, 
         data_resposta_recurso_4 = NA, 
         resposta_recurso_4 = NA, 
         anexo_com_extensao_recurso_4 = NA, 
         assunto = NA, 
         outros = NA, 
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA, 
         contem_dados_pessoais = NA, 
         e_complementacao_de_pedido = NA, 
         resposta_duplicada = NA, 
         pasta_do_anexo_recurso_4 = NA, pasta_do_anexo_recurso_3 = NA) %>%
  select(-c(anexo))

goval_final[ix] <- lapply(goval_final[ix], as.character)
 
## Juntando todas essas bases.

base_executivo <- bind_rows(base_cgm_sp_final, base_cgu_completa, base_gov_ma, base_gov_mg, base_gov_rn, base_gov_rs, 
                        base_pref_jp, base_pref_porto_velho, base_pref_recife, base_pref_rio_branco, base_pref_salvador,
                        base_pref_vitoria, goval_final)

base_prefeituras <- bind_rows(base_pref_jp, base_pref_porto_velho, 
                              base_pref_recife, base_pref_rio_branco, 
                              base_pref_salvador,
                              base_pref_vitoria)

base_gov_estaduais <- bind_rows(base_gov_ma, base_gov_mg, base_gov_rn, base_gov_rs, goval_final)

save(base_executivo, file="base_executivo.Rdata")
save(base_prefeituras, file="base_prefeituras.Rdata")
save(base_gov_estaduais, file="base_gov_estaduais.Rdata")


## Agora começamos o LDA |_|

load("base_executivo.Rdata")

library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
#devtools::install_github("sfirke/janitor")
library(janitor)
# devtools::install_github("mgaldino/tbaep")
library(tbaep)
library(googlesheets)

base_executivo_lda <- base_executivo %>%
  select(pedido) %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                         transliterations = c("Latin-ASCII")))

url_laistopwords <- "https://docs.google.com/spreadsheets/d/11BtQ1Lfsnmm5DMw-ejBcI39NJWXgNp7NkGRV7Kci8Hc/edit?usp=sharing" 
gs_ls() 
laistopwords_sheet <- gs_title("stopwords_executivo")
laistopwords <- laistopwords_sheet %>%
  gs_read()
colnames(laistopwords) = c("V1")

pt_stop_words <- read.csv(url("http://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt"),
                          encoding = "UTF-8", header = FALSE)
pt_stop_words2 <- data.frame(iconv(pt_stop_words$V1, from="UTF-8",to="ASCII//TRANSLIT")) #estou fazendo uma stopwords sem acento
colnames(pt_stop_words2) = c("V1") #deixando os dfs com o mesmo nome
pt_stopwordsfinal <- pt_stop_words %>%
  rbind(pt_stop_words2) %>%
  distinct(V1, .keep_all = TRUE) %>%
  mutate(V1 = as.character(V1)) %>%
  arrange(V1)

my_stopwords <- unique(c(stopwords("portuguese"), pt_stopwordsfinal$V1, laistopwords$V1))

duplicated(my_stopwords)

# transforma DF em vetor 
set.seed(13)
tamanho <- 10000 # 55000
x <-  data.frame(sample(base_executivo_lda$pedido, tamanho))

names(x) <- c("pedido")
base_executivo_lda1 <- x$pedido

ped1 <- Corpus(VectorSource(base_executivo_lda1))
ped <- Corpus(VectorSource(base_executivo_lda1)) # tenho que transformar o dataframe em corpus
ped
inspect(ped[15:18])
inspect(ped1[15:18])

ped <- tm_map(ped, content_transformer(tolower))
ped <- tm_map(ped, removeNumbers)
ped <- tm_map(ped, removePunctuation) #tira os pontos
f <- content_transformer(function(x, pattern) gsub("¿", "", x))
ped <- tm_map(ped, f)

ped <- tm_map(ped, removeWords, my_stopwords) # demora um pouco

ped <- tm_map(ped , stripWhitespace) #extrawhitespace
ped <- tm_map(ped, stemDocument, language = "portuguese")


inspect(ped[15:18])
dtm.control <- list(wordLengths = c(3,Inf),
                    weighting = weightTf)

dtm <- DocumentTermMatrix(ped, control = dtm.control)
dim(dtm)
#inspect(dtm[1:20,1:20])
freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras
dtm1 <- dtm[-index, ] # remove # palavras que não ocorrem em nenhum documento.
findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x


##########################################
# LDA v1

set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 20 # número de tópicos. Estou chutando

## função pra extrair termos
SpecificTerms <- function(lda.model,n=1) {
  p <- posterior(lda.model)$terms
  n <- min(n,ncol(p))
  cumulativep <- colSums(p)
  specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms <- t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}

set.seed(2)
system.time(lda1 <- LDA(dtm1, k))

# t termos mais prováveis por tópico
t <- 10
View(terms(lda1, t))

# t termos com prob acima de minimo
minimo <- .015
terms(lda2, t, threshold=minimo)  

# tópicos mais prováveis por documentos
topics(lda1)
inspect(ped1[311])

