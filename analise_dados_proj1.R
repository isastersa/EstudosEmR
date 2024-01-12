#CARREGAR PACOTES
library(dplyr)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
#ABRIR DIRETÓRIO SESSION>SET WORKING DIRECTORY>CHOOSE
setwd("C:/Users/User/Desktop/EstudosR/udemy/projeto1/Projeto1/data")
#ABRIR ARQUIVO
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",")
glimpse(covid_sp_tratado)

#ALTERANDO CLASSES E NOMES
covid_sp_tratado$data <- as.Date(covid_sp_tratado$data, format = '%Y-%m-%d')
covid_sp_tratado$idoso <- as.numeric(covid_sp_tratado$idoso...)
covid_sp_tratado <- select(covid_sp_tratado, -c(18))

covid_sp_tratado$casos_pc <- as.numeric(covid_sp_tratado$casos_pc)
covid_sp_tratado$casos_mm7d <- as.numeric(covid_sp_tratado$casos_mm7d)
covid_sp_tratado$obitos_pc <- as.numeric(covid_sp_tratado$obitos_pc)
covid_sp_tratado$obitos_mm7d <- as.numeric(covid_sp_tratado$obitos_mm7d)
covid_sp_tratado$letalidade <- as.numeric(covid_sp_tratado$letalidade)

covid_sp_tratado <- rename(covid_sp_tratado, idoso_porcentagem = idoso)


#FILTRO POR LINHAA
covid_campinas <- covid_sp_tratado %>% filter(municipio=="Campinas")
#a tabela apresenta um erro na area de campinas
covid_campinas["area"] <- covid_campinas$area/100
covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area

covid_guarulhos <- covid_sp_tratado[which(covid_sp_tratado$municipio=="Guarulhos"),]
#a tabela apresenta um erro na area de guarulhos
covid_guarulhos["area"] <- covid_guarulhos$area/100
covid_guarulhos["dens_demografica"] <- covid_guarulhos$pop/covid_guarulhos$area


#ANALISES ESTATISTICAS


#Medidas de tendencias centrais
#MÉDIA
mean(covid_campinas$obitos_novos)
mean(covid_campinas$casos_novos)
summarise_at(covid_campinas,vars(obitos_novos, casos_novos), mean)

summarise_at(covid_guarulhos,vars(obitos_novos, casos_novos), mean)

#FAZENDO GRÁFICO DA MÉDIA MÓVEL
plot(covid_campinas$data,covid_campinas$casos_mm7d, 
     title("MÉDIA MÓVEL CASOS"), col= "red")
plot(covid_campinas$data,covid_campinas$obitos_mm7d, 
     title("MÉDIA MÓVEL OBITOS"), col= "purple")

#MEDIANA
median(covid_campinas$obitos_novos)
median(covid_campinas$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos, casos_novos), median)

#MODA
#criando a função moda
moda <- function(x){
  valor_unico <- unique(x) #vai rastrear quais os valores unicos naquela coluna
  valor_unico[which.max(tabulate(match(x,valor_unico)))]
}

moda(covid_campinas$obitos_novos)
moda(covid_campinas$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos, casos_novos), moda)

#MODA IGNORANDO O ZERO
covid_guarulhos %>%
  filter(obitos_novos>0 & casos_novos>0) %>%
  summarise_at(vars(obitos_novos, casos_novos), moda)

#apenas os casos no mes de julho
covid_julho_campinas <- covid_campinas %>% filter(mes==7)
summarise_at(covid_julho_campinas,vars(obitos_novos, casos_novos), moda)
summarise_at(covid_julho_campinas,vars(obitos_novos, casos_novos), mean)

#histograma
hist(covid_julho_campinas$obitos_novos, col = "blue")
hist(covid_julho_campinas$casos_novos, col = "red")



#MEDIDAS DE POSIÇÃO
#minimo
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos, casos_novos), min)

#maximo
max(covid_campinas$obitos_novos)
max(covid_campinas$casos_novos)

summarise_at(covid_guarulhos,vars(obitos_novos, casos_novos), max)

#amplitude total
range(covid_campinas$obitos_novos)
range(covid_campinas$casos_novos)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), range)

#Quartis
quantile(covid_campinas$obitos_novos)
quantile(covid_campinas$casos_novos)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), quantile)

#Amplitude Interquartil
IQR(covid_campinas$obitos_novos)
IQR(covid_campinas$casos_novos)
summarise_at(covid_guarulhos, vars(obitos_novos, casos_novos), IQR)

#SUMMARY indica max, min quartis, media e mediana
summary(covid_campinas$casos_novos)
summary(covid_campinas$obitos_novos)
summary(covid_guarulhos$casos_novos)
summary(covid_guarulhos$obitos_novos)

#BOXPLOT
summary(covid_julho_campinas$casos_novos)
boxplot(covid_julho_campinas$casos_novos)

summary(covid_julho_campinas$obitos_novos)
boxplot(covid_julho_campinas$obitos_novos)

summary(covid_guarulhos$casos_novos)
boxplot(covid_guarulhos$casos_novos)

summary(covid_guarulhos$obitos_novos)
boxplot(covid_guarulhos$obitos_novos)

#TRATANDO OS OUTLIERS

#Identificando os outliers
covid_guarulhos%>%identify_outliers(casos_novos)

#cria uma variavel outliers e retira (out) as estatisticas
#do boxplot que são da variavel $casos_novos
outliers <-  c(boxplot.stats(covid_guarulhos$casos_novos)$out)

#cria variavel onde é retirado as linhas -c onde
#os casos_novos apareceram em outliers (excluir todos os outliers)
covid_guarulhos_sem_outliers <- covid_guarulhos[-c(which
                              (covid_guarulhos$casos_novos %in% outliers)),]
boxplot(covid_guarulhos_sem_outliers$casos_novos)


#Excluindo alguns outliers. (devem ser excluindos usando alguma referencia)
covid_campinas%>%identify_outliers(casos_novos)
covid_campinas_sem_outliers <- covid_campinas %>%
                              filter(data!="2020-06-19")
boxplot(covid_campinas_sem_outliers$casos_novos)



#MEDIDAS DE DISPERSÃO
#Variancia
var(covid_guarulhos$casos_novos)
var(covid_guarulhos$obitos_novos)
var(covid_campinas$casos_novos)
var(covid_campinas$obitos_novos)
var(covid_julho_campinas$casos_novos)
var(covid_julho_campinas$obitos_novos)

#Desvio Padrao
sd(covid_guarulhos$casos_novos)
sd(covid_guarulhos$obitos_novos)
sd(covid_campinas$casos_novos)
sd(covid_campinas$obitos_novos)
sd(covid_julho_campinas$casos_novos)
sd(covid_julho_campinas$obitos_novos)

#TESTES DE NORMALIDADE
#Gráficos: Histograma e QQplot
#Númericos: Shapiro-Wilk (limite de 5000 amostras), Anderson-Darling
#         Kolmogorov-Smirnov, Cramer-Von Mises

#Nível de significância de 0,05 (5%) ou nível de confiança de 95%
#Quando o parâmentro p> 0,05 (distribuição normal).

if(!require(nortest)) install.packages("nortest")
library(nortest)

hist(covid_campinas$casos_novos, probability = T, col = "blue")
lines(density(covid_campinas$casos_novos), col = "red")
#o gráfico acima mostra que é uma distribuição assimétrica á direita

qqnorm(covid_campinas$casos_novos)
qqline(covid_campinas$casos_novos)
#como o gráfico está distante da linha, não é distribuição normal


#resultado de p deve ser p>0,05. no teste p< 2.2e-16, ou seja, não normal
shapiro.test(covid_campinas$casos_novos)
ad.test(covid_campinas$casos_novos)
lillie.test(covid_campinas$casos_novos)
cvm.test(covid_campinas$casos_novos)

#CORRELAÇÃO LINEAR
#method: "pearson" para dados paramétricos (normalidade e homocedasticidade-homogeneos)
#       "spearman" (volume grande de dados não paramétricos)
#       "kendall" (volume pequeno de dados não paramétricos)

plot(covid_campinas$casos, covid_campinas$obitos) #gráfico para ver se há correção
#o gráfico lembrou a ideia de uma função linear positiva. logo, há correlação positiva

#nesse caso a distribuição não é normal (não é paramétrico), e são em grande volume
#logo, usaremos o metodo spearman

cor(covid_campinas$casos, covid_campinas$obitos, method = "spearman")
#valor: 0.9996747. Ou seja, correlação muito forte

#MODELO DE REGRESSÃO
regressao <- lm(formula = obitos ~ casos, data = covid_campinas)
regressao$coefficients #Intercept 51.670 casos 0.0337 -> obitos = 51,67+0,0337*casos
summary(regressao) #aparece min, max, quartis, mediana

#Qual é a precisão do modelo? 
#Pelo coeficiente de determinação ajustado (aparece do summary) : 0,9832

###############################################################################
#Gráfico de linha com ajuste de reta COM GGPLOT2
if(!require(ggplot2)) install.packages("ggplo2")
library(ggplot2)
if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)
library(tidyverse)

ggplot(data = covid_campinas, mapping = aes(x = casos, y = obitos))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme_dark()

###### GRÁFICO E MATRIZ DE CORRELAÇÃO
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

matriz_corr <- cor(covid_campinas[5:13], method = "spearman")

corrplot(matriz_corr, method = "color")

corrplot(matriz_corr, method = "color",
         type = "full", order = "original",
         addCoef.col = "black", #adiciona o coeficiente à matriz
         tl.col = "black", tl.srt = 45) #cor e rotação do nome das variaveis

#GRAFICOS LINEARES POR CIDADE
covid_cidades <- covid_sp_tratado %>% 
                filter(municipio %in% c("Campinas", "Guarulhos", "Sorocaba"))

ggplot(covid_cidades, aes(x=casos, y=obitos, color = municipio))+
  geom_line()+
  labs(title = "Evolução dos óbitos em função dos casos de COVID",
       x="Casos", y="óbitos", color="Municípios")+
  theme_classic()
