load(file="base_prefeituras.Rdata") #base_prefeituras


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
library(wordcloud)

base_prefeituras_lda <- base_prefeituras %>%
  select(pedido) %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                         transliterations = c("Latin-ASCII")))

base_prefeituras_lda %>%
  slice(1:5)
#funcionou 


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
#duplicated(my_stopwords)

# transforma DF em vetor

#set.seed(13)
#tamanho <- 10000 # 55000
#x <-  data.frame(sample(base_executivo_lda$pedido, tamanho))
#names(x) <- c("pedido")


base_executivo_lda1 <- base_prefeituras_lda$pedido

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
# findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x

# LDA v1 - Prefeituras

set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 10 # topic likelihood

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
minimo <- .010
terms(lda1, t, threshold=minimo)  

# tópicos mais prováveis por documentos
topics(lda1)
inspect(ped1[553])

### CGM

load("base_cgm_sp_final.Rdata")

base_cgm_sp_lda <- base_cgm_sp_final %>%
  select(pedido) %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                         transliterations = c("Latin-ASCII")))

base_cgm_sp_lda %>%
  slice(1:5)

#stopwords já está lá em cima
base_cgm_sp_lda1 <- base_cgm_sp_lda$pedido

ped_cgm1 <- Corpus(VectorSource(base_cgm_sp_lda1))
ped_cgm <- Corpus(VectorSource(base_cgm_sp_lda1)) # tenho que transformar o dataframe em corpus
ped_cgm
inspect(ped_cgm[15:18])

ped_cgm <- tm_map(ped_cgm, content_transformer(tolower))
ped_cgm <- tm_map(ped_cgm, removeNumbers)
ped_cgm <- tm_map(ped_cgm, removePunctuation) #tira os pontos
ped_cgm <- tm_map(ped_cgm, f)

ped_cgm <- tm_map(ped_cgm, removeWords, my_stopwords) # demora um pouco

ped_cgm <- tm_map(ped_cgm , stripWhitespace) #extrawhitespace
ped_cgm <- tm_map(ped_cgm, stemDocument, language = "portuguese")


inspect(ped_cgm[15:18])
dtm.control <- list(wordLengths = c(3,Inf),
                    weighting = weightTf)

dtm_2 <- DocumentTermMatrix(ped_cgm, control = dtm.control)
dim(dtm_2)
#inspect(dtm[1:20,1:20])
freq_words <- rowSums(as.matrix(dtm_2)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras
dtm_21 <- dtm_2[-index, ] # remove # palavras que não ocorrem em nenhum documento.
findFreqTerms(dtm_21, 5) #encontrando termos que ocorreram ao menos 5x

# LDA 3 CGM

set.seed(44)
trainpoints <- sample(1:nrow(dtm_21), 1*nrow(dtm_21),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 40 # topic likelihood

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
system.time(lda3 <- LDA(dtm_21, k))

# t termos mais prováveis por tópico
t <- 10
View(terms(lda3, t))

# t termos com prob acima de minimo
minimo <- .015
terms(lda3, t, threshold=minimo)  

# tópicos mais prováveis por documentos
y <- as.data.frame(topics(lda3))
sort(desc(table(y)))
colnames(y) <- c("topicos")

y %>%
  filter(topicos == 7) %>%
  sample_n(5)

topics(lda3)
inspect(ped_cgm1[157])

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(dtm_21, file="CGM_DTM.Rdata")
save(dtm1, file="prefeituras_dtm.Rdata")

# http://henrygarner.com/r/lda/wordclouds/2017/11/10/lda-and-wordclouds-in-r.html

# topic <- 1
# df <- data.frame(term = lda3@terms, p = exp(lda3@beta[topic,]))
# head(df[order(-df$p),])
# 
# wordcloud(words = df$terms,
#           freq = df$p,
#           max.words = 10,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors=brewer.pal(8, "Dark2"))
