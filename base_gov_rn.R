library(readxl)
base_gov_rn <- read_excel("C:/Users/jvoig/OneDrive/Documentos/Colab/COLAB/GovRN/Base Governo RN (anonimizada).xls", 
                                           col_types = c("text", "text", "text", 
                                                         "text", "text", "text"))


setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\colab_tb")
save(base_gov_rn, file="base_gov_rn.Rdata")
