# LDA Ministérios públicos
library(dplyr)

load("C:/Users/jvoig/OneDrive/Documentos/colab_tb/lista_df_mps.RData") 

mppi <- lista_mps_final[[1]]
mprj <- as.data.frame(lista_mps_final[[2]])

# No caso do PI, o número do pedido é por ano, 

mppi_base <- mppi %>%
  mutate(ano = format(data_da_entrada, "%Y"),
         protocolo = paste(ano , manifestacao, sep="."),
         protocolo = gsub(" *\\(.*?\\) *", "", protocolo)) %>%
  arrange(protocolo)
View(mppi_base)

mppi_base$protocolo[7] <- "2017.3"
mppi_base$data_da_entrada[7] <- "2017-01-23"
mppi_base$protocolo[69] <- "2015.19"
mppi_base$data_da_entrada[69] <- "2015-06-22"
mppi_base$data_da_entrada <- format(mppi_base$data_da_entrada, "%Y-%m-%d")

mppi_base %>% group_by(protocolo) %>% summarise(p = n()) %>% filter(p>1) #ok

mppi_base <- mppi_base %>%
  rename(data_do_pedido = data_da_entrada,
         pedido = teor) %>%
  mutate(esfera = "estadual",
         poder = "ministerio publico",
         orgao = "ministerio publico estadual do piaui",
         assunto = NA, outros = NA, atendimento = NA,
         nao_e_pedido_de_informacao = NA, contem_dados_pessoais = NA,
         pedido_pasta_do_anexo_pedido = NA, anexo_com_extensao_pedido = NA,
         pasta_do_anexo_resposta = NA,
         anexo_com_extensao_resposta = NA,
         data_recurso_1 = NA,
         recurso_1 = NA,
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
         resposta_recurso_2 = NA) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros, atendimento, 
         nao_e_pedido_de_informacao, contem_dados_pessoais, data_do_pedido, 
         pedido, pedido_pasta_do_anexo_pedido, anexo_com_extensao_pedido, 
         resposta, pasta_do_anexo_resposta, anexo_com_extensao_resposta, data_recurso_1, 
         recurso_1, pasta_do_anexo_recurso_1, anexo_com_extensao_recurso_1, data_resposta_recurso_1, 
         resposta_recurso_1, pasta_do_anexo_resposta_recurso_1, anexo_com_extensao_resposta_recurso_1, 
         data_recurso_2, recurso_2, pasta_do_anexo_recurso_2, anexo_com_extensao_recurso_2, 
         data_resposta_recurso_2,resposta_recurso_2) 

## MPRJ

mprj %>% group_by(numero_da_solicitacao) %>% summarise(p=n()) %>% filter(p>1) #ok

mprj_base <- mprj %>%
  rename(protocolo = numero_da_solicitacao,
         data_do_pedido = data_da_solicitacao,
         pedido = solicitacao) %>%
  mutate(esfera = "estadual",
         poder = "ministerio publico",
         orgao = "ministerio publico estadual do rio de janeiro", 
         assunto = NA, 
         outros = NA, 
         atendimento = NA, 
         nao_e_pedido_de_informacao = NA, 
         contem_dados_pessoais = NA, 
         pedido_pasta_do_anexo_pedido = NA, 
         anexo_com_extensao_pedido = NA, 
         pasta_do_anexo_resposta = NA, 
         anexo_com_extensao_resposta = NA, 
         data_recurso_1 = NA, 
         recurso_1 = NA, 
         pasta_do_anexo_recurso_1 = NA, 
         anexo_com_extensao_recurso_1 = NA, 
         data_resposta_recurso_1 = NA, 
         resposta_recurso_1 = NA, 
         pasta_do_anexo_resposta_recurso_1 = NA, 
         anexo_com_extensao_resposta_recurso_1 = NA, 
         data_recurso_2 = NA, 
         recurso_2= NA , 
         pasta_do_anexo_recurso_2= NA, 
         anexo_com_extensao_recurso_2 = NA, 
         data_resposta_recurso_2 = NA,
         resposta_recurso_2 = NA) %>%
  select(esfera, poder, orgao, protocolo, assunto, outros, 
         atendimento, nao_e_pedido_de_informacao, contem_dados_pessoais, 
         data_do_pedido, pedido, pedido_pasta_do_anexo_pedido, anexo_com_extensao_pedido, 
         resposta, pasta_do_anexo_resposta, anexo_com_extensao_resposta, 
         data_recurso_1, recurso_1, pasta_do_anexo_recurso_1, anexo_com_extensao_recurso_1, 
         data_resposta_recurso_1, resposta_recurso_1, pasta_do_anexo_resposta_recurso_1, 
         anexo_com_extensao_resposta_recurso_1, data_recurso_2, recurso_2, 
         pasta_do_anexo_recurso_2, anexo_com_extensao_recurso_2, data_resposta_recurso_2,
         resposta_recurso_2) 

## Empilhando para o LDA

base_mps <- mprj_base %>%
  mutate(data_do_pedido = as.character(data_do_pedido)) %>%
  bind_rows(mppi_base)

setwd("C://Users//jvoig//OneDrive//Documentos//colab_tb")
save(base_mps, file= "base_mps.Rdata")
load("base_mps.Rdata")

## LDA

library(tm)
library(SnowballC)
library(seqinr)
library(RTextTools)
library(topicmodels)
library(data.table)
library(devtools)
#devtools::install_github("sfirke/janitor")
library(janitor)
# devtools::install_github("mgaldino/tbaep")
library(tbaep)
library(dplyr)
library(googlesheets)

base_mps_lda <- base_mps %>%
  select(pedido) %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                         transliterations = c("Latin-ASCII")))

##stopwords:
url_mpstopwords <- "https://docs.google.com/spreadsheets/d/1SvmI4ESGmO8ScpDBEr89RAeQebiCcv3QPYExmqSu9vQ/edit?usp=sharing" 
gs_ls() 
mpsstopwords_sheet <- gs_title("stopwords_mps")
mpsstopwords <- mpsstopwords_sheet %>%
  gs_read()
colnames(mpsstopwords) = c("V1")

pt_stop_words <- read.csv(url("http://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt"),
                          encoding = "UTF-8", header = FALSE)
pt_stop_words2 <- data.frame(iconv(pt_stop_words$V1, from="UTF-8",to="ASCII//TRANSLIT")) #estou fazendo uma stopwords sem acento
colnames(pt_stop_words2) = c("V1") #deixando os dfs com o mesmo nome

pt_stopwordsfinal <- pt_stop_words %>%
  rbind(pt_stop_words2) %>%
  distinct(V1, .keep_all = TRUE) %>%
  mutate(V1 = as.character(V1)) %>%
  arrange(V1)

my_stopwords <- unique(c(stopwords("portuguese"), pt_stopwordsfinal$V1, mpsstopwords$V1))
duplicated(my_stopwords) #não há duplicatas

# removendo stopwords manualmente
my_stopwords1 <- iconv(my_stopwords, from = "utf-8", to="ASCII//TRANSLIT")
head(my_stopwords1, 20)

base_mps_lda <- base_mps_lda %>%
  mutate(pedido = iconv(pedido, from = "utf-8", to="ASCII//TRANSLIT"),
         pedido = removeWords(tolower(pedido), tolower(my_stopwords1)))

#Removendo acentos
# transforma DF em vetor 
base_mps_lda1 <- base_mps_lda$pedido #com o Dataframesource poderia não ter transformado o df em vetor

ped1 <- Corpus(VectorSource(base_mps_lda1))
ped <- Corpus(VectorSource(base_mps_lda1)) # tenho que transformar o dataframe em corpus
ped
inspect(ped[15:18])

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
inspect(dtm[1:20,1:20])
freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras
dtm1 <- dtm[-index, ]
dim(dtm1)
findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x

##
set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 10 # número de tópicos. Estou chutando

SpecificTerms <- function(lda.model,n=1) {
  p <- posterior(lda.model)$terms
  n <- min(n,ncol(p))
  cumulativep <- colSums(p)
  specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
  
  topterms <- t(apply(specificp, 1, function(x)
    (colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
  
  topterms
}

set.seed(22)
lda2 <- LDA(dtm1, k)

# t termos mais prováveis por tópico
t <- 10
View(terms(lda2, t))

# t termos com prob acima de minimo
minimo <- .02
terms(lda2, t, threshold=minimo)  

# tópicos mais prováveis por documentos
topics(lda2)
inspect(ped1[101])

