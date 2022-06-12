rm(list=(ls()))
install.packages(c("BETS", "tseries", "urca", "TSA","tmtest",
                   "normtest", "FinTS", "fpp2"))
library(readxl)
library(ggthemes)
library(fpp2)
library(tidyverse)
library(gridExtra)
library(TSstudio)
library(BETS)
library(tseries)
library(urca)
library(TSA)
library(tmtest)
library(normtest)
library(FinTS) 



Fertilizantes <- read_excel("~/Google Drive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Analise de Série Temporais/Trabalho:Prova/Fertilizantes.xlsx")             


# 1) Um breve comentário inicial relacionado à análise exploratória dos dados, incluindo a visualização, 
# identificação de padrões, decomposição e o entendimento do padrão da série.  

summary(Fertilizantes)
class(Fertilizantes)
length(Fertilizantes)
head(Fertilizantes)
tail(Fertilizantes)

hist(Fertilizantes$data,Fertilizantes$fertilizantes, breaks = 5)

###Transformação em Série de Tempo

ts.fert <- ts(Fertilizantes$fertilizantes, frequency = 12, start = c(1998, 1))
class(ts.fert)
ts.fert
length(ts.fert)

hist(ts.fert)

autoplot(ts.fert,lwd = 1.5) 
boxplot(ts.fert)

## Decomposição




##############################################################################################################################
# 2)Considerar os seguintes subconjuntos de dados: 

#Intervalo de janeiro/2007 até dezembro/ 2018 para modelagem da série temporal para treinamento (traino). 
#intervalo de janeiro/2019 até outubro/2021 será base para testar o modelo (teste). 
#Notar o comportamento da série no período de março/20, a grande parada por conta dos casos de covid. 

janelatreino <- window(ts.fert, frequency = 12, start= c(2007, 1), end = c(2018,12))
janelatreino
autoplot(janelatreino, col = 'brown3', lwd = 1.5)+
  xlab('Mês')+
  ylab('Ton/ Fertilizantes')+
  theme_minimal()+
  ggtitle('Demanda Fertilizantes - Jan/2007 - Dez/2018')

janelateste <- window(ts.fert, frequency = 12, start = c(2019,1), end = c(2021,10))
janelateste
autoplot(janelateste, col = 'brown3', lwd = 1.5)+
  xlab('Mês')+
  ylab('Ton/ Fertilizantes')+
  theme_minimal()+
  ggtitle('Demanda Fertilizantes - Jan/2019 - Out/2021')

##############################################################################################################################
# 3)Selecionar os modelos de estudo: 
  
# Holt-Winters Aditivo  - o modelo Holt - Winters captura o efeito sazonal o que nao acontecia no modelo SES e de Holt 
# Holt_Winters Multiplicativo 

#3.1) Comportamento Sazonal da demanda

seasonplot(ts.fert, 12, col=rainbow(12), year.labels=TRUE, main="Demanda Fertilizantes")

# Com o TSstudio

ts_plot(ts.fert, 
        title = "Demanda de Fertilizantes",
        Ytitle = "Tonelagem")

#Sazonalidade

ts_seasonal(ts.fert, type = "all") 
# type all, normal,  cycle, box

# Alternativamente
boxplot(ts.fert ~ cycle(ts.fert),
        col="orange",
        xlab="Mês", 
        ylab="Taxa %",
        main ="Boxplot Demanda de Fertilizantes")

ggseasonplot(ts.fert)
ggsubseriesplot(ts.fert)

# Decomposição Melhorada ( usar no pdf)
library(gridExtra)

d1 <- ts.fert%>%
  decompose(type= "additive")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Aditiva")

d2 <- ts.fert%>%
  decompose(type= "multiplicative")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Multiplicativa")

grid.arrange(d1,d2,ncol = 2)

# 3.2 Holt-Winter Multiplicativo

fit.HW_addit <- hw(ts.fert,seasonal="additive", h = 36, level = 0)

# notar o seguinte:

# o modelo ver com $fitted
fit.HW_addit$fitted

# A previsão o próprio comando da linha já especificou o comprimento h
fit.HW_addit

autoplot(ts.fert, series =  "Dados") +
  autolayer(fit.HW_addit$fitted, series= "HW_Aditivo",  showgap = FALSE)+
  autolayer(fit.HW_addit, series="Previsão HW Aditivo h =12")+
  ggtitle("Demanda Fertilizantes - HW Aditivo")

# 3.3 Holt-Winter - Multiplicativo 


fit.HW_mult <- hw(ts.fert,seasonal="multiplicative", h=36, level = 0)

autoplot(ts.fert) +
  autolayer(fit.HW_mult$fitted, series= "HW_Multiplicativo")+
  autolayer(fit.HW_mult, series="Previsão HW Multiplicativo h =12")+
  ggtitle("Demanda de Fertilizantes- HW Multiplicativo")

#https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/plot.forecast

autoplot(ts.fert, series = "Dados") +
  autolayer(fit.HW_addit, series="HW aditivo", showgap = FALSE) +
  autolayer(fit.HW_mult, series="HW multiplicativo", showgap = FALSE) +
  xlab("Mês") +
  ylab("TTon/ Fertilizante)") +
  ggtitle("Demanda Fertilizantes- Holt-Winters ") +
  guides(colour=guide_legend(title="Forecast"))

# Calcula - se a acurácia para comparar os método adit e multiplicativo
accuracy(fit.HW_addit)
accuracy(fit.HW_mult)

# O menor Erro é o aditivo

# 3.4) Modelo SARIMA(p,d,q)(P,D,Q)[s]. 



##############################################################################################################################
# 4) Plotar os correlogramas ACF e PACF e verificar a estacionariedade da série temporal.  
# O pacote TStudio tem a função ts_cor() que é bem interessante. Vale a pena dar um visitada no sítio deste pacote indiano. 

# verificando as autocorrelações seriais

acf  <- ggAcf(ts.fert, lag.max = 60)
pacf <- ggPacf(ts.fert,lag.max= 60)

grid.arrange(acf, pacf, nrow =2)

ts_cor(ts.fert)

# Se o ACf declinar geometricamente
# e o PACF p-valores signifattivos
# então o modelo se caractarizaria por AR(p)

# Por? isso, temos 03 questões:
# 1. a série tem tend?ncia, 
# 2. a série tem uum comportamento aditivo
# 3. a série tem sazonalidade

# 5) Efetuar os testes de raíz-unitária: Augmented Dickey_Fuller, 
# Kwiatkowski–Phillips–Schmidt–Shin (KPSS) e Phillip-Perron. 

# Efetuando os testes de estacionariedade

library(tseries)

############################
# p-valor baixo rejeita Ho.#  Regra de Ouro
############################

# TESTE ADF

# Ho. A série não é estacionaria
# H1: A serie é estacionaria

adf.test(ts.fert)

# Pelo KPSS

# Ho. A série é estacionaria
# H1: A serie não é estacionaria

kpss.test(ts.fert)

# PHILLP-PERRON
# Ho. A série não é estacionaria
# H1: A serie é estacionaria

pp.test(ts.fert)

# CONCLUSÃO DO TESTE DE ESTACIONARIEDADE: 

# O TESTE KPSS ESTÁ DIVERGENTE DO ADF E DO PP

# Eefetuando a Diferenciação *****GRUPO VERIFICAR O NUMERO DE LAGS??????? 

# A função  diff efetua a primeira diferença no lag 1. 

a1 <- autoplot(diff(ts.fert, lag = 1, differences = 1))+
  ylab("diff(ts.fert)")+
  ggtitle("Demanda de Fertilizantes ")

a2 <- ggAcf(diff(ts.fert, lag = 1, differences = 1),
            lag.max = 36)+
  ggtitle("Demanda de Fertilizantes")

grid.arrange(a1, a2, nrow =2)

ts_cor(diff(ts.fert, lag = 1, differences = 1))

##############################################################################################################################
# 6) Modelagem do SARIMA(p,d,q)(P,D,Q)[s]. 




###############################################################################################################################
# 7) Escolha do melhor modelos pelo critério de aceitação de Akaike.



###############################################################################################################################
# 8) Com base no melhor modelo, efetuar a previsão com base no subconjunto de treinamento (traino) para o período até dezembro-2022. 
# Comparar com as demandas com a base teste. 


