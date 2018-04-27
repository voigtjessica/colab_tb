#LDA da CGU - iniciado.
# Manoel vai criar um loop nesse script para que rode um LDA para cada orgao.

load("base_cgu_completa.Rdata")

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

base_executivo_lda <- base_cgu_completa %>%
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
tamanho <- 10000 # esse foi o limite que o LDA me deixou rodar
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

