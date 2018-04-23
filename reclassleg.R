##RECLASSIFICAÇÃO

setwd("C:\\Users\\Renato\\Desktop\\Projetos\\Achados e Pedidos\\Colab\\Reclassificação\\Legislativo")

library(data.table)
library(tidyr)
library(dplyr)
library(stringr)


##unificando assuntos

leg <- legis_classificado %>%
  mutate(assunto = gsub("10- outros", "10-outros", assunto),
         assunto = gsub("1- consultorias", "1-consultorias", assunto),
         assunto = gsub("3- contatos", "3-contatos", assunto),
         assunto = gsub("2- verbas representantes", "2-verbas", assunto),
         assunto = gsub("4- emendas", "4-emendas", assunto),
         assunto = gsub("5- concursos", "5-concursos", assunto),
         assunto = gsub("6- acervo", "6-acervo", assunto),
         assunto = gsub("7- tramitação", "7-tramitação", assunto),
         assunto = gsub("8-outros", "10-outros", assunto),
         assunto = gsub("8- licitações", "8-licitações", assunto),
         assunto = gsub("9- servidores", "9-servidores", assunto),
         assunto = gsub("7- tramitaç", "7-tramitação", assunto))

##lista outros em 10-outros

outros_outros <- leg %>%
  filter(assunto == "10-outros")

unique(outros_outros$outros)

##criando values

#midia

a = c("TV Fortaleza","Tv Fortaleza", "Programação TV Fortaleza", "Programação TV Fortaleza",
      "Programação TV Camara", "Funcionamento Tv e Radio da Camara", "Tv camara",
      "Midia", "Programação de TV (acervo)", "Programação de TV", "mídia", "mídias",
      "programa TV", "programação(TV)")


#legislação/esclarecimento/tramitação

b = c("legislação", "ESCLARECIMENTOS", "PEC(TRAMITAÇÃO)", "LEGISLAÇÃO", "Esclarecimentos",
      "esclareciento", "esclarecimetos e legislação", "esclarecimentos (Estrutura organizacional)",
      "esclarecimentos (lei)", "esclarecimentos", "esclarecimetos (legislação)", "esclarecimento",
      "Legislação", "Esclarecimento de Lei", "Esclarecimento", "Esclarecimento de lei", 
      "MP ou legislação (emendas)", "PEC(tramitação)", "consulta,legislação(tramitação)",
      "ordem do dia(tramitação)", "legislação(tramitação)", "legislação, base de dados", 
      "legislação,MPV", "legislação;servidores", "\"tramitação\"", "Interpretação de lei",
      "Informação especifica da lei", "Informação sobre legislação", "erro no site, legislação (acervo)",
      "Forma de expressar opinião sobre projeto de lei", "Informação sobre pessoa homenageada",
      "Informações sobre direitos","Parabenizar vereador por projeto aprovado", "Atraso de do votação")

#base de dados

c = c("banco de dados", "Base de dados", "base de dados", "base de dados; pesquisa" ,
      "base de dados(acervo)", "Banco de dados", "Banco de Dados", "Bando de dados")

#logradouro

d = c("endereço", "logradouros", "logradouros(pavimentação)", "informações (rua)",
      "Logradouro", "logradouro", "logradouro(tramitação)", "Informação sobre pessoa que dá nome a rua")


#estrutura organizacional

e = c("estrutura organizacional", "estrutura organizacional - cursos",
      "contatos(estruturas organizacional)", "Estrutura Organizacional",
      "estrutura organizacional;contato", "estrutura organizacional; legislação;esclarecimento",
      "Estrutura organizacional da camara", "contatos;estrutura organizacional",
      "Estrutura organizacional", "regimento interno")

#problemas e dificuldades site

f = c("INSTRUÇÕES - SITE", "instruções-site", "Instruções site", "Informações sobre o prorio portal",
      "problemas tecnicos", "problemas técnicos")

#funcionamento

g = c("visitas", "visitas", "Visita a camara", "Funcionamento da casa", "Informação sobre o programa",
      "Informações sobre a biblioteca", "Duvida sobre o funcionamento da camara")

#agenda

h = c("programação", "agenda (eventos)", "agenda", "Programação", "Programação da casa")

#cursos, palestras e campanhas

i = c("Cursos", "Campanha", "campanha de conscientização", "curso(acervo)", "curso",  
      "Certificado de participação")

#pesquisa e acervo

j = c("pesquisas (acervo)", "pesquisas", "Pesquisa", "Resposta de questionario",
      "Relação quantitativa")

#orçamento

k = c("orçamento", "Orçamento", "Prestação de contas", "Orçamento orgão")

#vagas

l = c("vagas(estágio;concurso)", "Informaçoes osbre estágio", "Informação sobre envio de curriculos")


#declarações

m = c("LAI como RH", "DECLARAÇÃO", "Declaração", "declaração", "Requerimento de documentação")


#servidores

n = c("Falta do servidor", "Informação de servidor", "Informação do servidor")

#solic serv publico

o = c("Requerimento de serviço publico", "Solicitação de serviço publico", 
      "Serviço publico", "Solicitação de serviço Publico", "Requerimento de abertura de processo administrativo",
      "Solicitação de fiscalização")

#outros

p = c("(CONTATOS DE NAO DEPUTADOS)", "COMPRAR LIVROS", "DISCURSO/FALA", "?", 
      "informações representantes", NA, "LAI para conseguir informação do solicitante",
      "repasse", "Participação", "Reclamação", "Autorização de publicação de comentário",
      "Pedido", "Participação em projeto", "Requerimento", "discussão", "Documentação",
      "CPI", "Fiscalização do sistema", "Informação sobre documentação", "Informação sobre monumento",
      "Autorização", "Solicitação de contato", "Convenios da camara", "Participação em pregão",
      "Biografia", "Participaçao em CPI", "Programação do minha casa minha vida",
      "Resultado programa minha casa minha vida", "Sugestão", "Inscrição em Projeto")


##unificando outros em 10-outros


leg2 <- leg %>%
  mutate(outros2 = case_when(assunto == "10-outros" & outros %in% a ~ "midia",
                             assunto == "10-outros" & outros %in% b ~ "legislação",
                             assunto == "10-outros" & outros %in% c ~ "base de dados",
                             assunto == "10-outros" & outros %in% d ~ "logradouro",
                             assunto == "10-outros" & outros %in% e ~ "estrutura org",
                             assunto == "10-outros" & outros %in% f ~ "problemas site",
                             assunto == "10-outros" & outros %in% g ~ "funcionamento orgao",
                             assunto == "10-outros" & outros %in% h ~ "agenda",
                             assunto == "10-outros" & outros %in% i ~ "cursos, campanhas, palestras",
                             assunto == "10-outros" & outros %in% j ~ "pesquisa e acervo",
                             assunto == "10-outros" & outros %in% k ~ "orcamento",
                             assunto == "10-outros" & outros %in% l ~ "vagas",
                             assunto == "10-outros" & outros %in% m ~ "declaracoes e docs",
                             assunto == "10-outros" & outros %in% n ~ "servidores",
                             assunto == "10-outros" & outros %in% o ~ "req serv publ",
                             assunto == "10-outros" & outros %in% p ~ "outros"))


leg2_valid <- leg2 %>%
  filter(assunto=="10-outros") %>%
  group_by(outros2) %>%
  summarise(count=n())


##validação

valid <- leg %>%
  filter(outros=="Documentação")

#limpando para exportar em CSV

leg4 <- leg2 %>%
  mutate_all(funs(str_replace_all(., "\r", " "))) %>%
  mutate_all(funs(str_replace_all(., "\n", " ")))


write.table(leg4, "leg.csv", sep = ";", row.names = F, fileEncoding="UTF-8", na="")


##lucas

legluc <- leg2 %>%
  filter(assunto=="5-concursos" | assunto=="6-acervo")

##236 linhas

#lizandra

legliz <- leg2 %>%
  filter(assunto=="1-consultorias" | assunto=="2-verbas" | assunto=="4-emendas" | 
           assunto == "8-licitações" | assunto == "9-servidores")

###198 linhas

##ana

legana <- leg2 %>%
  filter(assunto=="7-tramitação" & orgao == "camara dos deputados")

##176 linhas


##jose

legjose <- leg2 %>%
  filter(assunto=="3-contatos" | assunto=="7-tramitação" & orgao != "camara dos deputados")

##235 linhas

write.table(legluc, "legluc.csv", sep = ";", row.names = F, fileEncoding="UTF-8", na="")

