library(readxl)

setwd("C:\\Users\\mgaldino\\2018\\Achados e Pedidos\\Scripts\\tribunais_de_contas")

arquivos <- list.files(recursive=T)

lista <- list()

for ( i in 1:length(arquivos)) {
  lista[[i]] <- read_excel(arquivos[i]) %>%
    clean_names() 
}


pastas <- list.files()
vec <- list()
for (i in 1:length(pastas)) {
  vec[[i]] <- which(grepl(pastas[i], arquivos)==T)
}

lista_aux <- list()
vec2 <- vec[which(lengths(vec)>1)]

for (i in 1:4) {
  lista_aux[[i]] <- lista[[vec2[[1]][i]]]
}
lista_df <- bind_rows(lista_aux)

for (i in 1:5) {
  lista_aux[[i]] <- lista[[vec2[[2]][i]]]
}

lista_df1 <- bind_rows(lista_aux)

lista <- lista[-(7:15)]
lista[[7]] <-  lista_df
lista[[8]] <-  lista_df1
length(lista)

arquivos1 <- gsub("/", "-", arquivos[1:6])
arquivos1 <- gsub(".xls[x]*", "", arquivos1)
nome_files <- c(arquivos1, "TCM-SP", "TCU")

names(lista) <- nome_files

save(lista, file="listya_df_tribunais_de_contas.RData")

