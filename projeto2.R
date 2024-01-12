#######################################################
###    EXPLORAÇÃO E ANÁLISE DE DADOS - PROJETO 2    ###
#######################################################

#Carregar pacotes
library(dplyr)
library(rstatix)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(plotly)) install.packages("plotly")
library(plotly)

#BURCAR DIRETORIO (PASTA COM ARQUIVOS)
setwd("C:/Users/User/Desktop/EstudosR/udemy/projeto1/Projeto1/data")

#ABRIR ARQUIVO
srag_sp <- read.csv('SRAG_2020.csv', sep = ";")

#TRATAMENTO INICIAL 
#excluindo colunas
srag_sp_mod <- select(srag_sp, -c(51:133))
srag_sp_mod <- select(srag_sp_mod, -c(5:8))
srag_sp_mod <- select(srag_sp_mod, -c(6,8))

#reconhecendo e modificando classe variaveis
glimpse(srag_sp_mod)

srag_sp_mod$DT_NOTIFIC <- as.Date(srag_sp_mod$DT_NOTIFIC, format = '%m/%d/%Y')

#renomeando colunas
srag_sp_mod <- rename(srag_sp_mod, sexo = CS_SEXO, idade = NU_IDADE_N,
                      raca = CS_RACA, zona = CS_ZONA, data_notif = DT_NOTIFIC)

#Verificando valores missing
sapply(srag_sp_mod, function(x) sum(is.na(x)))
sapply(srag_sp_mod, function(x) sum(is.nan(x)))


############################################################
library(graphics)
#GRÁFICO DE BARRAS

table(srag_sp_mod$sexo) #ver quais sexos e quantos
srag_sp_mod <- srag_sp_mod %>% filter(sexo!= "I") #exclui as linhas que apareciam I 

grafico_barras_sexo = table(srag_sp_mod$sexo)
barplot(grafico_barras_sexo, col = "green", main = "Quantidade por sexo")

ggplot(srag_sp_mod, aes(x=sexo))+
  geom_bar(fill='red')+ #preenchido com vermelho
  labs(title = "Quantidade por sexo", subtitle = "SRAG",
       x = "Sexo", y = "Contagem") #labs é abreviação de labels. Definir rótulos

#GRÁFICO POR RAÇA

#onde esta NA atribuir 9 que é ignorando de acordo com o documento de legenda
srag_sp_mod$raca[which(is.na(srag_sp_mod$raca))] <- 9 

srag_sp_mod$raca[srag_sp_mod$raca == 1] <- "Branca"
srag_sp_mod$raca[srag_sp_mod$raca == 2] <- "Preta"
srag_sp_mod$raca[srag_sp_mod$raca == 3] <- "Amarela"
srag_sp_mod$raca[srag_sp_mod$raca == 4] <- "Parda"
srag_sp_mod$raca[srag_sp_mod$raca == 5] <- "Indígena"
srag_sp_mod$raca[srag_sp_mod$raca == 9] <- "Ignorado"


table(srag_sp_mod$raca)
grafico_barras_raca = table(srag_sp_mod$raca)
barplot(grafico_barras_raca, col = "purple", main = "Quantidade por sexo")

#Contagem da raça e gráfico
srag_sp_mod %>%
  group_by(raca) %>%
  summarise(
    contagem = n()
  ) %>% #com essa classificacao feita criar o grafico abaixo
  ggplot(aes(x=raca, y = contagem, label = contagem))+
    geom_bar(fill = 'purple', stat = 'identity')+ #color dá o contorno e alpha a transparencia
    geom_label(size=3, alpha = 0.5)+ #adicionar o valor exato no gráfico
    labs(title = "Quantidade por raça", subtitle = "SRAG",
         x = "Raça", y = "Contagem")+
    theme(plot.title = element_text(hjust = 0.5), #para centralizar  o titulo
          plot.subtitle = element_text(hjust = 0.5)) #para centralizar  o subtitulo
  
#GRÁFICO POR RAÇA, SEXO E REGIÃO

srag_sp_mod$zona[which(is.na(srag_sp_mod$zona))] <- 9 

srag_sp_mod$zona[srag_sp_mod$zona == 1] <- "Urbana"
srag_sp_mod$zona[srag_sp_mod$zona == 2] <- "Rural"
srag_sp_mod$zona[srag_sp_mod$zona == 3] <- "Periurbana"
srag_sp_mod$zona[srag_sp_mod$zona == 9] <- "Ignorado"

ggplot(srag_sp_mod, aes(x= raca, y = sexo, fill = factor(zona)))+
  geom_col(position = "dodge")+ #position dodge os graficos ficam lado a lado ao inves de sobreposto
  labs(title = "Zona por sexo e raça",
       x = "Raça", y = "Sexo", fill = "Zona")

#Grupos Múltiplos.
#Agregando por sexo e raça
srag_sp_mod %>%
  group_by(sexo, raca) %>%
  summarise(
    contagem = n()
  ) %>%
  ggplot(aes(x=raca, y=contagem, fill = sexo, label = contagem)) +
  geom_bar(stat = "identity")+
  geom_label(size=3, alpha = 0.5)

#GRAFICO DE BARRAS NA HORIZONTAL
ggplot(srag_sp_mod, aes(x=raca, y=sexo, fill=zona))+
  geom_col(position = "dodge")+
  labs(title = "Zona por sexo e raça",
       x = "Raça", y = "Sexo", fill = "Zona")+
  coord_flip() #inverte para horizontal

#GRAFICO DE BARRAS EMPILHADO 

grafico_empilhado <- aggregate(idade~sexo + zona,
                               data = srag_sp_mod, FUN = mean)
ggplot(grafico_empilhado, aes(x=zona, y=idade, fill = factor(sexo)))+
  geom_col(position = "stack")

#GRAFICO COM PLOTLY - graficos interativos
srag_sp_mod %>%
  plot_ly(x = ~ raca) %>% #Eixo x relacionado com a raca
  layout(xaxis = list(title= "Raça"), #titulo do eixo x
         yaxis = list(title="Quantidade")) #titulo do eixo y

######################################
#BOXPLOT

#arrumar idade
srag_sp_mod$idade[srag_sp_mod$TP_IDADE == 1] <- 0
srag_sp_mod$idade[srag_sp_mod$TP_IDADE == 2] <- 0

summary(srag_sp_mod$idade)
boxplot(srag_sp_mod$idade)
#tratamento dos outliers
srag_sp_mod %>% identify_outliers(idade)
outliers <- c(boxplot.stats(srag_sp_mod$idade)$out)
srag_sem_outliers <-srag_sp_mod[-c(which(srag_sp_mod$idade %in% outliers)),]

summary(srag_sem_outliers$idade)
boxplot(srag_sem_outliers$idade)

#BOXPLOT COM GGPLOT2
srag_sp_mod %>% filter(!is.na(idade)) %>%
  ggplot(aes(x = " ", y = idade)) + #não tem eixo x no boxplot
  geom_boxplot(width = .3, outlier.colour = "purple") #widht é a largura do boxplot

srag_sem_outliers %>% filter(!is.na(idade)) %>%
  ggplot(aes(x = " ", y = idade)) + #não tem eixo x no boxplot
  geom_boxplot(width = .3, outlier.colour = "red") #widht é a largura do boxplot

#BOXPLOT COM PLOTLY - PLOTYLY É ITERATIVO
plot_ly(srag_sp_mod, y = srag_sp_mod$idade,
        type = "box") %>%
        layout(title = "BOXPLOT POR IDADE",
        yaxis = list(title = "Idade"))

plot_ly(srag_sem_outliers, y = srag_sem_outliers$idade,
        type = "box") %>%
       layout(title = "BOXPLOT POR IDADE",
         yaxis = list(title = "Idade"))

###BOXPLOT COLETIVO (Serve tambem para outros tipos de graficos)

#Função par permite configurar a area de plotagem
#a mfrow(a1, a2) permite que seja criado gráfico em a1 linhas e a2 colunas
par(mfrow=c(1,2))
boxplot(srag_sem_outliers$idade, ylab = "idade sem outliers")
boxplot(srag_sp_mod$idade, ylab = "idade com outliers")

par(mfrow=c(1,2))
boxplot(idade ~ sexo,srag_sem_outliers, ylab = "Idade", xlab = "Sexo")
boxplot(idade ~ raca,srag_sem_outliers, ylab = "Idade", xlab = "Raça")

par(mfrow=c(2,2))
boxplot(idade ~ sexo,srag_sem_outliers, ylab = "Idade", xlab = "Sexo")
boxplot(idade ~ raca,srag_sem_outliers, ylab = "Idade", xlab = "Raça")
boxplot(idade ~ zona,srag_sem_outliers, ylab = "Idade", xlab = "Zona")
boxplot(srag_sem_outliers$idade, ylab = "idade sem outliers")

#COM GGPLOT
ggplot(srag_sem_outliers, aes(x=factor(sexo), y= idade))+ #o eixo x é dividido pelo fatores de sexo
  geom_boxplot(fill = "dodgerblue")+
  labs(y="Idade", x="Sexo", 
       title = "Distribuição das idades por sexo")+
  theme(plot.title = element_text(hjust = 0.5)) #titulo centralizado

#COM PLOTLY
plot_ly(srag_sem_outliers, y=srag_sem_outliers$idade, 
        color = srag_sem_outliers$sexo, type = "box") %>%
        layout(title = "BOXPLOT POR IDADE",
               xaxis = list(title = "Sexo"),
               yaxis = list(title = "Idade"))

########################################################
#ANÁLISE ESTATÍTICA
par(mfrow=c(1,1))
#Histograma
hist(srag_sem_outliers$idade, col = "blue",
     main = "Distribuição por idades",
     xlab = "Idades", ylab = "Frequência")

hist(srag_sem_outliers$idade, probability = T, #probability = T é para o grafico mostrar em porcentagem 
     col = "pink") 
lines(density(srag_sem_outliers$idade))

summary(srag_sem_outliers$idade)

#Criando a função moda
moda <- function(m){
  valor_unico <- unique(m)
  valor_unico[which.max(tabulate(match(m,valor_unico)))]
}
moda(srag_sem_outliers$idade)

#GRÁFICO DE DISTRIBUIÇÃO NORMAL
qqnorm(srag_sem_outliers$idade, col = "blue")
qqline(srag_sem_outliers$idade)

#Testes de normalidade
library(nortest)
ad.test(srag_sem_outliers$idade) #p-value < 2.2e-16. LOGO não é normal

#COM O GGPLOT2
ggplot(srag_sem_outliers, aes(x=idade))+
  geom_histogram(fill = "red", bins = 25)+#bins:largura
  geom_freqpoly()+
  labs(title = "Histograma da idade",
       x = "Idade", y = "Frequencia")+
  theme(plot.title = element_text(hjust = 0.5))

#COM PLOTLY
plot_ly(x=srag_sem_outliers$idade, type = "histogram") %>%
  layout(title = "Histograma por idade",
         xaxis = list(title = "Idade"), yaxis = list(title = "Frequencia"))

#############################
#Gráfico de dispersão 
plot(srag_sem_outliers$data_notif, srag_sem_outliers$idade,
     title("Casos de SRAG por mês e por idade"), col = "purple",
     xlab = "Meses", ylab = "Idade")

scatter.smooth(srag_sem_outliers$data_notif, srag_sem_outliers$idade)

#Com GGPLOT
ggplot(srag_sem_outliers, aes(x = data_notif, y = idade)) +
  geom_point()+
  labs(title = "Relação data de notificação e idade",
       x = "Data de notificação", y = "Idade")

#COM GGPLOT (4VARIAVEIS)
srag_sout_campinas <- srag_sem_outliers %>%
  filter(ID_MN_RESI=="CAMPINAS")

ggplot(srag_sout_campinas, aes(x=data_notif, y = idade,
                               color = raca, shape = sexo))+
  geom_point()+
  labs(title = "Relação entre data de notificação, idade,raça e sexo da cidade de Campinas",
       x = "Data de Notificação", y = "Idade",
       color = "Raça", shape = "Sexo") + 
  theme(plot.title = element_text(hjust = .5))

#COM O PLOTLY
plot_ly(x=srag_sout_campinas$data_notif, y=srag_sout_campinas$idade,
      type = 'scatter', mode = 'markers', color = srag_sout_campinas$sexo)

#GRÁFICO DE BOLHAS
#COM GGPLOT
ggplot(srag_sout_campinas, aes(x=data_notif, y = zona, size = idade))+
  geom_point()+
  labs(title = "Relação entre data e região por idade",
       x = "Data de Notificação", y = "Zona", size = "Idade")

#COM PLOTLY
plot_ly(x=srag_sout_campinas$data_notif,
        y=srag_sout_campinas$zona,
        type = 'scatter', mode='markers',
        size=srag_sout_campinas$idade)

#GRÁFICO DE SETORES
table(srag_sout_campinas$sexo)
pie(table(srag_sout_campinas$sexo), col = c("red", "blue"), radius = 1)

srag_sout_campinas$sexo <- as.factor(srag_sout_campinas$sexo)

contagem <- table(srag_sout_campinas$sexo)
nomes <- levels(srag_sout_campinas$sexo)
porcentagem <- round(100*contagem/sum(contagem),2)
rotulo <- paste(nomes, " (", porcentagem, "%", ")", sep = " ")

pie(table(srag_sout_campinas$sexo), labels = rotulo, main = "SRAG por Sexo - Campinas",
    col = c("red", "blue"), radius = 1)

#COM GGPLOT
library(scales)
ggplot(srag_sout_campinas, aes(x = " ", fill = sexo))+
  geom_bar(width = 1)+
  coord_polar("y")+
  theme(plot.background = element_rect(fill = "gray", colour = "red")) #elementos de fundo do grafico

dados_pizza <- data.frame(
  grupo = c("Masculino", "Feminino"), 
  valores = c(1311,1041), 
  soma = sum(table(srag_sout_campinas$sexo)))

ggplot(dados_pizza, aes(x=" ", y=valores, fill = grupo))+
  geom_col(width = 1, color = "white")+
  geom_text(aes(label = percent(valores/soma, accuracy = 0.1)),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "Reds")+
  coord_polar(theta = "y")+
  theme_void()+
  labs(title = "SRAG por Sexo", fill = "Legenda")

#COM PLOTLY
plot_ly(srag_sout_campinas, labels = ~sexo, type = 'pie')
plot_ly(srag_sout_campinas, labels = ~raca, type = 'pie')
plot_ly(srag_sout_campinas, labels = ~zona, type = 'pie')


#########
#Frequencias
if(!require(sampling)) install.packages("sampling")
if(!require(TeachingSampling)) install.packages("TeachingSampling")
library(sampling)
library(TeachingSampling)

#Tabela de Frequencias Absolutas
freq_abs <- table(srag_sem_outliers$idade)
View(freq_abs)

#Tabela de Frequencias Relativas
freq_rel <- prop.table(freq_abs)
View(freq_rel)

#Porcentagem da Frequencia Relativa
porc_freq_Rel <- 100*prop.table(freq_rel) 
View(porc_freq_Rel)

#Criar uma linha com o total
freq_abs <- c(freq_abs, sum(freq_abs))
names(freq_abs)[112] <- "Total"
View(freq_abs)

#Juntando as frequencias com seus totais
freq_rel <- c(freq_rel, sum(freq_rel))
porc_freq_Rel <- c(porc_freq_Rel, sum(porc_freq_Rel))

#Tabela final com os valores
tabela_freq <- cbind(freq_abs,
                     freq_rel = round(freq_rel, digits = 5),
                     porc_freq_Rel = round(porc_freq_Rel, digits = 2))
View(tabela_freq)

#Construindo CLASSES de Frequencias
intervalo_classes <- seq(0,120,10) #DO 0 A 120, DE 10 EM 10
View(intervalo_classes)
tabela_classes <- table(cut(srag_sem_outliers$idade, breaks = intervalo_classes,
                            right = FALSE)) #o numero da direita nao entra no intervalo
View(tabela_classes)

#GRÁFICOS DE FREQUENCIA

#Histograma
hist(srag_sem_outliers$idade, col = "red")

df1 <- as.data.frame(tabela_classes)

df1 %>% 
  plot_ly(x = ~Var1, y = ~Freq) %>%
  layout(xaxis = list(title = "Intervalo de idades"),
         yaxis = list(title = "Quantidade"))

#Polígono de frequência
plot(tabela_classes, type = 'o')

#Gráfico de Ogiva

#Frequencia Acumulada
freq_rel_classes <- prop.table(table(cut(srag_sem_outliers$idade,
                                         breaks = c(intervalo_classes))))
View(freq_rel_classes)

freq_acum_classes <- cumsum(tabela_classes)[seq_along(intervalo_classes)]
View(freq_acum_classes)

#Gráfico
plot(intervalo_classes, freq_acum_classes, type = 'o')

#Gráfico de OGIVA NO GGPLOT
df <- as.data.frame(freq_acum_classes)

ggplot(df, aes(x = intervalo_classes, y = freq_acum_classes))+
  geom_line()+
  geom_point()+
  labs(title = "Gráfico OGIVA: Frequência Acumulado por classes de idade",
       x = "Idade",
       y = "Frequência Acumulada de SRAG")
