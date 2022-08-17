#----- Instalando as bibliotecas necessárias ------#

#install.packages('plotly')
#install.packages('hrbrthemes')
#install.packages('ggplot')
#install.packages('tidyverse')
#install.packages("dplyr")

#------ Carregando as bibliotecas ------#
library(hrbrthemes) # Temas para os gráficos
library(ggplot2) #Criação de gráficos 
library(dplyr) #Manipulação dos dados


#------ Removendo variáveis presentes e manipulando as casas decimais ------#
rm(list = ls())
options(scipen = 999)

#------ Lendo o database ------#
#Sugiro extrair a pasta 'analisexp' diretamente em C:
data <- read.csv("C:/analisexp/amazondt.csv", sep=";")
View(data)

#Hipótese: Em média, ocorrem mais incêndios no verão nos Estados do nordeste.

#Para uma análise mais completa, primeiramente, vamos agrupar por ano e criar um gráfico

pl_a<- data %>% group_by(year) #Agrupando por ano
view(pl_a)

g1 <- ggplot(data = summarise(.data = pl_a, ocor = sum(number)),
            aes(x = year, y = ocor))+
            geom_line() + geom_point() + theme_bw()+
            theme(text = element_text(size = 12))+
            labs(title = "Ocorrências ao longo dos anos", x = "Anos", y = "N° Ocorrências")
g1 #N° de ocorrencia ao longo dos anos

#Agora, agrupando por Estados

pl_e <- data %>% group_by(state) #Agrupando por Estado
view(pl_e)

g2 <- ggplot(data = pl_e, aes(x = number, y = state))+
  geom_col() + theme_bw() +
  theme(text = element_text(size = 12))+
  labs(title = "Ocorrências ao longo dos anos por Estado", x = "N° Ocorrências", y = "")
g2 #N° de ocorrencia por Estados

g3 <- ggplot(data = pl_e, aes(x = number, y = state))+
  geom_col() + theme_bw() + facet_wrap(~year)+
  labs(title = "Ocorrências or Estado ao longo dos anos", x = "N° Ocorrências", y = "")+
  theme(text = element_text(size = 12))
g3 #N° de ocorrencia por Estados ao longo dos anos


#Calculando a média de casos casos em cada Estado

a <- summarise(.data = pl_e, avg.caso = mean(number, trim = 0.4),med.caso = median(number)) %>% arrange(desc(avg.caso))
View(a)

g6 <- ggplot(data = a)+ geom_point(aes(x = state, y = med.caso), size = 2)+
      geom_line(aes(x = state, y = med.caso, group = 1), size = 1)+theme_bw()+
      labs(title = "Média de casos dos Estados", x = "", y = "Média de casos")
g6 #Média de casos dos estados. Podemos ver que Maranhão, Mato grosso e Pará concentram os casos, 
#o que prova que parte da tese é falsa. Dando sequência:


#Descobrindo em que meses mais ocorrem queimadas
mes <- data %>% group_by(month) %>%  summarise(casos02 = mean(number, trim = 0.4),
      med = median(number)) %>% arrange(desc(casos02)) %>% filter(casos02 > 35) %>% select(month) %>% pull()
mes 

#Exibindo graficamente
g4 <- ggplot(data = data, aes(x = number, y = state))+
      geom_col() + facet_wrap(~month) + theme_bw()+
      theme(text = element_text(size = 12))
g4


#Rankeando o mês com a maior soma de casos
mes <- as.data.frame(data %>% group_by(month) %>% summarise(topcas = sum(number)))   
View(mes)

#Exibindo graficamente
g5 <- ggplot(data = mes)+
      geom_col(aes(x = month, y = topcas))
g5

#Como podemos ver, a hipótese apresentada, de que ocorriam mais queimadas no verão não é verdadeira,
#visto que como apresentado graficamente, os meses em que mais ocorrem queimadas são os meses de Juho
#à Janeiro, ou seja, inverno e Primavera.

#Portanto, podemos concluir que no Brasil, no período entre 1998 e 2017, a maioria dos incêndios ocorreram na
#região norte e nordeste, e ocorreram majoritariamente durante o inverno e a primavera


