library(XML)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
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
library(googledrive)

#Text-mining TCs
#colocando todas as planilhas juntas

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("tce_pi_vdd_base.Rdata")
load("tce_go_base.Rdata")
load("tce_rr_base.Rdata")
load("tcm_ce_base.Rdata")
load("tcm_rj_base.Rdata")
load("tcm_sp_base.Rdata")
load("tcu_base.Rdata")

tce_pi_ped <- tce_pi_vdd_base %>%
  select(pedido)
tce_go_ped <- tce_go_base %>%
  select(pedido)
tce_rr_pedido <- tce_rr_base %>%
  select(pedido)
tcm_ce_pedido <- tcm_ce_base %>%
  select(pedido)
tcm_rj_pedi <- tcm_rj_base %>%
  select(pedido)
tcm_sp_ped <- tcm_sp_base %>%
  select(pedido)
tcu_ped <- tcu_base %>%
  select(pedido)

tcs_base_lda <- tce_pi_ped %>%
  bind_rows(tce_go_ped) %>% 
  bind_rows(tce_rr_pedido) %>%
  bind_rows(tcm_ce_pedido) %>%
  bind_rows(tcm_rj_pedi) %>%
  bind_rows(tcm_sp_ped) %>%
  bind_rows(tcu_ped)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(tcs_base_lda, file="tcs_base_lda.Rdata")

rm(tce_pi_vdd_base)
rm(tce_go_base)
rm(tce_rr_base)
rm(tcm_ce_base)
rm(tcm_rj_base)
rm(tcm_sp_base)
rm(tcu_base)
rm(tce_go_ped)
rm(tce_rr_pedido)  
rm(tcm_ce_pedido)
rm(tcm_rj_pedi)
rm(tcm_sp_ped)
rm(tcu_ped)
rm(tce_pi_ped)

## Fazendo o Text-mining

#removendo acentos
tcs_base_lda <-  tcs_base_lda %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                                        transliterations = c("Latin-ASCII")))

##stopwords:
url_tcsstopwords <- "https://docs.google.com/spreadsheets/d/1PS2PCP_p2h8Z5HisyasHBY0DxKDDl1DYAZucp4NSBfU/edit?usp=sharing" 
gs_ls() 
stopwords_tcs_sheet <- gs_title("stopwords_tcs")
stopwords_tcs <- stopwords_tcs_sheet %>%
  gs_read()
colnames(stopwords_tcs) = c("V1")

pt_stop_words <- read.csv(url("http://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt"),
                          encoding = "UTF-8", header = FALSE)

pt_stop_words2 <- data.frame(iconv(pt_stop_words$V1, from="UTF-8",to="ASCII//TRANSLIT")) #estou fazendo uma stopwords sem acento

colnames(pt_stop_words2) = c("V1") #deixando os dfs com o mesmo nome

pt_stopwordsfinal <- pt_stop_words %>%
  rbind(pt_stop_words2) %>%
  distinct(V1, .keep_all = TRUE) %>%
  mutate(V1 = as.character(V1)) %>%
  arrange(V1)

my_stopwords <- unique(c(stopwords("portuguese"), pt_stopwordsfinal$V1, stopwords_tcs$V1))
duplicated(my_stopwords) #não há duplicatas




# transforma DF em vetor 
tcs_base_lda1 <- tcs_base_lda$pedido 

# Um corpus para acompanhar o pedido depois
ped1 <- Corpus(VectorSource(tcs_base_lda1))

# E um para trabalhar
ped <- Corpus(VectorSource(tcs_base_lda1)) 
ped                                        #8948 documentos
inspect(ped[15:18])

ped <- tm_map(ped, removeWords, my_stopwords) #primeira vez
ped <- tm_map(ped, content_transformer(tolower))
ped <- tm_map(ped, removeNumbers)
ped <- tm_map(ped, removePunctuation) 
f <- content_transformer(function(x, pattern) gsub("¿", "", x))
ped <- tm_map(ped, f)

ped <- tm_map(ped, removeWords, my_stopwords) # segunda vez

ped <- tm_map(ped , stripWhitespace)
ped <- tm_map(ped, stemDocument, language = "portuguese")

##########################
inspect(ped[22:25])
dtm.control <- list(wordLengths = c(3,Inf),
                    weighting = weightTf)

dtm <- DocumentTermMatrix(ped, control = dtm.control)
dim(dtm)
inspect(dtm[1:20,1:20])

## cada doc é uma linha
# cada palavra uma coluna
# número de colunas e igual número de palavras únicas em todos os docs

freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras

dtm1 <- dtm[-index, ] # remove # palavras que não ocorrem em nenhum documento.
findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x

###
# LDA

set.seed(12)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) 

k <- 10 # número de tópicos menor que o do topic likelihood

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
system.time(lda2 <- LDA(dtm1, k))

# t termos mais prováveis por tópico
t <- 10
View(terms(lda1, t))
View(terms(lda2, t))
# t termos com prob acima de minimo
minimo <- .015
terms(lda1, t, threshold=minimo) 

minimo <- .015
terms(lda2, t, threshold=minimo) 

# tópicos mais prováveis por documentos
topics(lda1)
inspect(ped1[619])

topics(lda2)
inspect(ped1[957])


#Fizemos os dois LDas (1 e 2) e usamos todos os tópicos do LDA2 (menos 2, 6,8 e 9, que eram repetidos)
# + o tópico 7 do LDA 1