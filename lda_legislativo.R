#legislativo

library(dplyr)

#Montando a base:

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
load("alepe17_final.Rdata")
load("camara_curitiba_final.Rdata")
load("camdep_final.Rdata")
load("camfortaleza_final.Rdata")
load("camsalvador_final.Rdata")
load("cldf_final.Rdata")

alepe17_limpo <- alepe17_limpo %>%
  mutate(anexo = NA)

camsalvador_limpo <- camsalvador_limpo %>%
  mutate(recurso_seg = NA,
         data_recurso_seg = NA) 

base_legislativo <- alepe17_limpo %>%
  rbind(camara_curitiba_final) %>%
  mutate(recurso_seg = NA,
         data_recurso_seg = NA) %>%
  rbind(camdep_final) %>%
  rbind(camfortaleza_limpo) %>%
  rbind(camsalvador_limpo) %>%
  rbind(cldf_limpo)

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\Colab\\COLAB\\templates")
save(base_legislativo, file="base_legislativo.Rdata")

setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
load("base_legislativo.Rdata")
#iniciando a produção dos documentos:

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

# remove acentos, lowecase etc.

base_legislativo_lda <- base_legislativo %>%
  mutate(pedido = snakecase::to_any_case(pedido, case = "none",
                                                    transliterations = c("Latin-ASCII")))


##stopwords:
url_laistopwords <- "https://docs.google.com/spreadsheets/d/1s2FwjhzjSIKNR3oE0FjuL8McIihBL-Z9z8BVlH3JeG0/edit?usp=sharing" 
gs_ls() 
laistopwords_sheet <- gs_title("stopwords_lai")
laistopwords <- laistopwords_sheet %>%
  gs_read()
colnames(laistopwords) = c("V1")

#laistopwords

pt_stop_words <- read.csv(url("http://raw.githubusercontent.com/stopwords-iso/stopwords-pt/master/stopwords-pt.txt"),
                          encoding = "UTF-8", header = FALSE)

# If you know the encoding of the string, or if its encoding is the 
# current locale encoding, then you can use the iconv function to convert 
# the string to ASCII. Something like: 
#   
#   iconv(accented.string, to="ASCII//TRANSLIT") 

pt_stop_words2 <- data.frame(iconv(pt_stop_words$V1, from="UTF-8",to="ASCII//TRANSLIT")) #estou fazendo uma stopwords sem acento

colnames(pt_stop_words2) = c("V1") #deixando os dfs com o mesmo nome


pt_stopwordsfinal <- pt_stop_words %>%
  rbind(pt_stop_words2) %>%
  distinct(V1, .keep_all = TRUE) %>%
  mutate(V1 = as.character(V1)) %>%
  arrange(V1)

my_stopwords <- unique(c(stopwords("portuguese"), pt_stopwordsfinal$V1, laistopwords$V1))


duplicated(my_stopwords) #não há duplicatas

#Removendo acentos

base_legislativo_lda <- base_legislativo_lda %>%
  select(pedido) 

# transforma DF em vetor 
base_legislativo_lda1 <- base_legislativo_lda$pedido #com o Dataframesource poderia não ter transformado o df em vetor

ped1 <- Corpus(VectorSource(base_legislativo_lda1))
ped <- Corpus(VectorSource(base_legislativo_lda1)) # tenho que transformar o dataframe em corpus
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

#no caso do tm_map(), usar content_transformer(function), 
#para usar funções do R para dfs , pode ser usado com o tolower ou o gsub

inspect(ped[15:18])
dtm.control <- list(wordLengths = c(3,Inf),
                    weighting = weightTf)

dtm <- DocumentTermMatrix(ped, control = dtm.control)
dim(dtm)
inspect(dtm[1:20,1:20])
freq_words <- rowSums(as.matrix(dtm)) # Quantas palavras cada documento (linha) tem
index <- which(freq_words==0) # índice de documentos em que não há palavras
dtm1 <- dtm[-index, ] # remove # palavras que não ocorrem em nenhum documento.
findFreqTerms(dtm1, 5) #encontrando termos que ocorreram ao menos 5x


###########################################################
# LDA v2

set.seed(51)
trainpoints <- sample(1:nrow(dtm1), 1*nrow(dtm1),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8

k <- 10 # número de tópicos. Estou chutando

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
lda2 <- LDA(dtm1, k)

# t termos mais prováveis por tópico
t <- 10
View(terms(lda2, t))

# t termos com prob acima de minimo
minimo <- .015
terms(lda2, t, threshold=minimo)  

# tópicos mais prováveis por documentos
topics(lda2)
inspect(ped1[101])
