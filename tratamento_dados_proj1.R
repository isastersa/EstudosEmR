if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
#ABRIR ARQUIVO
setwd("C:/Users/User/Desktop/EstudosR/udemy/projeto1/Projeto1/data")
covid_sp <- read.csv2('dados_covid_sp.csv', sep = ";")

View(covid_sp)

#Renomeando variaveis (Colunas)
 covid_sp_alterado <- rename(covid_sp, municipio = nome_munic)
 
 covid_sp_alterado <- rename(covid_sp_alterado, data = datahora, 
                             rotulo_mapa = map_leg, codigo_mapa = map_leg_s)
#EXCLUIR COLUNAS
 covid_sp_alterado$cod_ra <- NULL
covid_sp_alterado <- select(covid_sp_alterado, -c(21))
covid_sp_alterado <- subset(covid_sp_alterado,select = -c(codigo_ibge, cod_drs))
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15))
covid_sp_alterado <- select(covid_sp_alterado, -c(17:19))

#EXCUIR LINHAS
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))
covid_sp_alterado <- slice(covid_sp_alterado, -c(239661:239666))
covid_sp_alterado <- covid_sp_alterado %>% filter(municipio != "Ignorado")

#TRATAMENTO DE MISSINGS
#quais colunas aparecem NA?
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
#quais colunas aparecem NAN?
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

#SUBSTITUIR VALORES MISSING
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)

covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
covid_sp_alterado2 <- replace(x=covid_sp_alterado, list = is.na(covid_sp_alterado),
                              values = 54)
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$semana_epidem == 54] <- 2021
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-01'&
                                covid_sp_alterado2$data <= '2021-01-07'] <- 54
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-08'&
                                covid_sp_alterado2$data <= '2021-01-14'] <- 55
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-15'&
                                   covid_sp_alterado2$data <= '2021-01-21'] <- 56
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-22'&
                                   covid_sp_alterado2$data <= '2021-01-28'] <- 57
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-29'&
                                   covid_sp_alterado2$data <= '2021-02-04'] <- 58
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-05'&
                                   covid_sp_alterado2$data <= '2021-02-11'] <- 59
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-12'&
                                   covid_sp_alterado2$data <= '2021-02-18'] <- 60
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-19'&
                                   covid_sp_alterado2$data <= '2021-02-25'] <- 61
covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-02-26'&
                                   covid_sp_alterado2$data <= '2021-03-01'] <- 62

#VERIFICACAO DA TIPAGEM DOS ATRIBUTOS
str(covid_sp_alterado2)
#Transformação da tipagem de atributos
covid_sp_alterado2$semana_epidem <- as.integer(covid_sp_alterado2$semana_epidem)
str(covid_sp_alterado2)
covid_sp_alterado2$data <- as.Date(covid_sp_alterado2$data, format = '%Y-%m-%d')
glimpse(covid_sp_alterado2)

#Criando nova coluna com calculo
covid_sp_alterado2["idoso(%)"] <- 100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop

#Exportação de arquivos
write.table(covid_sp_alterado2, file="covid_sp_tratado.csv", sep=",")
