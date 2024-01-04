if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
#ABRIR ARQUIVO
setwd("C:/Users/User/Desktop/EstudosR/udemy/projeto1/Projeto1/data")
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")

View(covid_sp)
