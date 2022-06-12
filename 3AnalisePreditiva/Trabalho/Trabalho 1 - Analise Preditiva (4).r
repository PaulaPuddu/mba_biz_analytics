#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# TRABALHO INDIVIDUAL
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(dplyr)
library(rstatix)
library(car)
library(stringr)
library(MLmetrics)

data_wood <- readxl::read_excel(path = "/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/An�lise Preditiva/regm_woodard.xls")
data_wood <- data_wood %>% na.omit()
data_wood <- data_wood[,-1]
data_wood <- REGM_woodart[,-1]

colnames(data_wood)[1] <- 'ano_construcao'
colnames(data_wood)[2] <- 'area_util'
colnames(data_wood)[3] <- 'total_andares'
colnames(data_wood)[4] <- 'area_total'
colnames(data_wood)[5] <- 'total_banheiros'
colnames(data_wood)[6] <- 'total_lareiras'
colnames(data_wood)[7] <- 'cep'
colnames(data_wood)[8] <- 'valor_terreno'
colnames(data_wood)[9] <- 'valor_total'


#CONVES�O PARA M2
data_wood$area_util <- round(data_wood$area_util * as.double(0.092903),2)
data_wood$area_total <- round(data_wood$area_total * as.double(4046.86))

#CONFORNE ENUNCIADO.  
data_wood$cep <- substr(data_wood$cep,3,str_length(data_wood$cep)-1)

#TRATAMENTO DE DADOS
#data_wood$total_andares <- round(data_wood$total_andares)
#data_wood$total_banheiros <- round(data_wood$total_banheiros)

summary(data_wood)
str(data_wood)


#EXCLUS�O POR OUTLIERS
#EXCLUS�O DE OUTLIERS DE VALOR DO IM�VEL ABAIXO DE 60 MIL E ACIMA DE 300 MIL.
data_wood <- data_wood %>% subset(valor_total >= 60000 & valor_total <= 300000) 
#EXCLUS�O DE OUTLIERS DE ANO DE CONSTRU��O DO IM�VEL ABAIXO DE 1950.
data_wood <- data_wood %>% subset(ano_construcao>1950)
#EXCLUS�O DE OUTLIERS DE �REA �TIL ABAIXO DE 250M2
data_wood <- data_wood %>% subset(area_util <= 250) 
#EXCLUS�O DE OUTLIERS DE �REA TOTAL ACIMA DE 2000 M2
#N�O FORAM EXCLU�DOS OBSERVA��ES(3) QUE POSSUEM VALOR 0 NA VARI�VEL �REA TOTAL
dw2 <- data_wood %>% subset(area_total >= 2000)
`%notin%` <- Negate(`%in%`)
data_wood <- filter(.data = data_wood, area_total %notin% dw2$area_total)
#EXCLUS�O DE OUTLIERS DE VALOR DE TERRENO ACIMA DE 100000.
dw2 <- data_wood %>% subset(valor_terreno >= 100000)
`%notin%` <- Negate(`%in%`)
data_wood <- filter(.data = data_wood, valor_terreno %notin% dw2$valor_terreno)
#EXCLUS�O DE OUTLIERS DEVIDO A ERROS PERCENTUAIS ACMA DE 20% PRA MAIS O MENOS.
data_wood = data_wood %>% subset(valor_total!=92749 & valor_total!=74734 & valor_total!=89871 & valor_total!=163800)
rm(dw2)



##########AN�LISE UNIVARIDA DAS VARI�VEIS

par(mfrow=c(1,2))
par(las=1)
#par(mar=c(8,0,0,0))
hist(data_wood$valor_total,
     main = 'Distribui�ao dos Im�veis por Valor.',
     cex.main = 0.8,
     col=hcl.colors(1, palette = "Peach"),
     ylab = 'Total de Im�veis',
     xlab = 'Valor dos Im�veis (U$)',
     xaxt = 'n',
     ylim=c(0,12))
grid(10,col="gray")
aty <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
boxplot(data_wood$valor_total,
        main = 'An�lise Univariada. Valor dos Im�veis.',
        cex.main = 0.8,
        col=hcl.colors(1, palette = "Peach"), yaxt = 'n')
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
rm(aty)

par(mfrow=c(1,2))
par(las=2)
hist(data_wood$ano_construcao,
     main = 'Distribui�ao dos Im�veis por Ano de Constru��o.',
     col=hcl.colors(1, palette = "Peach"),
     cex.main = 0.8,
     breaks = 10,
     ylab = 'Total de Im�veis',
     xlab = 'Ano de Constru��o',
     xaxt = 'n',
     ylim=c(0,15))
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=atx, labels=format(atx, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
grid(col="gray")
par(las=2)
boxplot(data_wood$ano_construcao,
        main = 'An�lise Univariada. Ano de Constru��o dos Im�veis',
        cex.main = 0.8,
        ylim = c(1950,2000),
        col=hcl.colors(1, palette = "Peach"))

par(las=2)
par(mfrow=c(1,2))
hist(data_wood$area_util,
     main = 'Distribui�ao dos Im�veis por �rea �til.',
     col=hcl.colors(1, palette = "Peach"),
     cex.main = 0.8,
     ylab = 'Total de Im�veis',
     xlab = '�rea �til em m2.',
     xaxt = 'n',
     ylim=c(0,12));
grid(6,col="gray")
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at = atx, labels = format(atx, scientific=FALSE), hadj = 0.9, cex.axis = 0.8, las = 2)
boxplot(data_wood$area_util,
        main = 'An�lise Univariada. �rea �til dos Im�veis',
        cex.main = 0.8,
        ylim = c(50,250),
        ylab = '�rea do Im�vel(m2).',
        col=hcl.colors(1, palette = "Peach"))

#DISTRIBUI��O DE IM�VEIS POR STORY HOME
vrPorcentagem1 <- round((nrow(subset(data_wood, total_andares == 1)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem15 <- round((nrow(subset(data_wood, total_andares == 1.5)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem175 <- round((nrow(subset(data_wood, total_andares == 1.75)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_wood, total_andares == 2)) / nrow(data_wood))*100, digits = 2)
par(las=1)
par(mfrow=c(1,1))
barplot(table(data_wood$total_andares),
        main = 'Distribui��o de Im�veis por Story Home.',
        ylab = 'Total de Im�veis', 
        xlab = 'Story Home', 
        ylim = c(0,35),
        col = hcl.colors(4, palette = "Blues 3"),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem15,'%',sep = ''),str_c('',vrPorcentagem175,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = '')))
legend("topright", legend = c('1.0 Story','1.5 Story','1.75 Story','2.0 Story'), fill = hcl.colors(5, palette = "Blues 3"))
grid(8,col="gray")
rm(vrPorcentagem1, vrPorcentagem15,vrPorcentagem175,vrPorcentagem2, atx)

#DISTRIBUI��O DE IM�VEIS POR �REA TOTAL
par(las=1)
par(mfrow=c(1,2))
hist(data_wood$area_total,
     main = 'Distribui�ao dos Im�veis por �rea Total',
     cex.main = '0.7',
     col=hcl.colors(1, palette = "Peach"),
     ylab = 'Total de Im�veis',
     xlab = '�rea Total em m2.',
     xaxt = 'n',
     ylim=c(0,12))
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at = atx, labels = format(atx, scientific=FALSE), hadj = 0.9, cex.axis = 0.8, las = 1)
grid(col="gray")
boxplot(data_wood$area_total,
        cex.main = '0.7',
        ylim=c(0,2000),
        main = 'An�lise Univariada. �rea Total dos Im�veis(m2)',
        col=hcl.colors(1, palette = "Peach"))
rm(dw2,atx,`%notin%`)

#DISTRIBUI��O DE IM�VEIS POR TOTAL DE BANHEIROS
par(mfrow=c(1,1))
vrPorcentagem1 <- round((nrow(subset(data_wood, total_banheiros == 1)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem15 <- round((nrow(subset(data_wood, total_banheiros == 1.5)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_wood, total_banheiros == 2)) / nrow(data_wood))*100, digits = 2)
vrPorcentagem25 <- round((nrow(subset(data_wood, total_banheiros == 2.5)) / nrow(data_wood))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_wood, total_banheiros == 3)) / nrow(data_wood))*100, digits = 2)
par(las=1)
barplot(table(data_wood$total_banheiros),
        main = 'Distribui��o de Im�veis por Total de Banheiros',
        ylab = 'Total de Im�veis', 
        xlab = 'Total de Banheiros', 
        ylim = c(0,20),
        col = hcl.colors(5, palette = "Emrld"),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem15,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = ''),str_c('',vrPorcentagem25,'%',sep = ''),str_c('',vrPorcentagem3,'%',sep = '')))
legend("topright", legend = c('1 banheiro.','1,5 banheiros','2 banheiros','2,5 banheiros','3 banheiros'), fill = hcl.colors(5, palette = "Emrld"))
grid(10,col="gray")
rm(vrPorcentagem1, vrPorcentagem15, vrPorcentagem2,vrPorcentagem25,vrPorcentagem3)

#DISTRIBUI��O DE IM�VEIS POR TOTAL DE LAREIRAS
vrPorcentagem1 <- round((nrow(subset(data_wood, total_lareiras == 0)) / nrow(data_wood))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_wood, total_lareiras == 1)) / nrow(data_wood))*100, digits = 2)
par(las=1)
barplot(table(data_wood$total_lareiras),
        main = 'Distribui��o de Im�veis por Total de Lareiras',
        ylab = 'Total de Im�veis', 
        xlab = 'Percentual de Lareiras', 
        ylim = c(0,45),
        col = hcl.colors(3, palette = "Purple-Orange"),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = '')))
legend("topright", legend = c('N�o possui','1 Lareira'), fill = hcl.colors(3, palette = "Purple-Orange"))
grid(8,col="gray")
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUI��O DE IM�VEIS POR CEP
par(las=1)
par(mfrow=c(1,1))
barplot(table(data_wood$cep),
        main = 'Distribui��o de Im�veis por CEP',
        cex.main = 0.9,
        ylab = 'Total de Im�veis', 
        xlab = 'CEP do Im�vel', 
        ylim = c(0,16),
        col = hcl.colors(8, palette = "Dynamic"))
grid(19,col="gray")

par(mfrow=c(1,2))
par(las=1)
hist(data_wood$valor_terreno,
     main = 'Distribui�ao dos Im�veis por Valor do Terreno.',
     cex.main = 0.8,
     col=hcl.colors(1, palette = "Peach"),
     ylab = 'Total de Im�veis',
     xlab = 'Valor do Terreno ($)',
     xaxt = 'n',
     ylim=c(0,15))
grid(10,col="gray")
aty <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
boxplot(data_wood$valor_terreno,
        main = 'An�lise Univariada. Valor do Terreno dos Im�veis.',
        cex.main = 0.8,
        col=hcl.colors(1, palette = "Peach"), yaxt = 'n')
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)

##########FIM AN�LISE UNIVARIDA DAS VARI�VEIS

##########IN�CIO DAS AN�LISES BIVARIDAS

#AN�LISE BIVARIADA - VALOR X ANO
par(mfrow = c(1,1))
plot(data_wood$valor_total ~ data_wood$ano_construcao,
     main = 'An�lise Bivariada. Valor do Im�vel x Ano de Constru��o.',
     xlab = 'Ano de Constru��o',
     ylab = 'Valor do Im�vel (U$)',
     xlim = c(1950,2000), 
     ylim = c(50000,250000), 
     xaxt = 'n', 
     yaxt = 'n',
     lwd = 1,
     pch=19, 
     col="red")
abline(lm(data_wood$valor_total ~ data_wood$ano_construcao))
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at = atx, labels = format(atx, scientific=FALSE), hadj = 0.9, cex.axis = 0.8, las = 1)
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at = aty, labels = format(aty, scientific=FALSE), hadj = 0.9, cex.axis = 0.8, las = 2)
grid(10,col="gray")
R=cor(data_wood$valor_total,data_wood$ano_construcao) #correla��o
round(R,2)
rm(atx,aty)
##CORRELA��O DE 0.37

#AN�LISE BIVARIADA - VALOR X �REA �TIL
par(mfrow=c(1,1))
par(las=1)
par(mar=c(8,4,2,2))
plot(data_wood$valor_total  ~ data_wood$area_util,
     main = 'An�lise Bivariada. Valor do Im�vel x �rea �til.',
     xlab = '�rea �til em m2.',
     ylab = 'Valor do Im�vel (U$)',
     ylim=c(50000,250000), 
     lwd = 1,
     pch=19, 
     col="red",
     xaxt='n',
     yaxt='n')
abline(lm(data_wood$valor_total ~ data_wood$area_util))
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=atx, labels=format(atx, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
mtext(side=2, text="y", line=4.5)
grid(10,col="gray")
R=cor(data_wood$valor_total,data_wood$area_util) #correla��o
round(R,2)
rm(R)
##CORRELA��O DE .77. FORT�SSIMA!!


##########AN�LISE BIVARIADA - VALOR X STORY HOME
par(las = 1)
boxplot(data_wood$valor_total ~ data_wood$total_andares,
        main = 'An�lise Bivariada. Valor do Im�vel x Story Home.',
        yaxt = 'n',
        ylab = 'Valor do Im�vel (U$)',
        xlab = 'Story Home',
        col = hcl.colors(5, palette = "Tropic"))
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at = aty, labels = format(aty, scientific = FALSE), hadj = 0.9, cex.axis = 0.8, las = 2)
grid(12,col = "gray")

##########FIM AN�LISE BIVARIADA - VALOR X TOTAL DE ANDARES

##########AN�LISE BIVARIADA - VALOR X �REA TOTAL DO IM�VEL

#AN�LISE BIVARIADA - VALOR X �REA TOTAL
par(mfrow=c(1,1))
par(las=2)
plot(data_wood$valor_total  ~ data_wood$area_total,
     main = 'An�lise Bivariada. Valor do Im�vel x �rea Total',
     ylab = 'Valor do Im�vel (U$)',
     ylim=c(50000,250000), 
     lwd = 1,
     xaxt = 'n',
     yaxt = 'n',
     pch=20, 
     col="red",
     xlab = '�rea Total em m2')
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=atx, labels=format(atx, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
abline(lm(data_wood$valor_total ~ data_wood$area_total))
grid()
R=cor(data_wood$valor_total,data_wood$area_total) #correla��o
round(R,2)
#CORELA��O .12 BAIXA.

#AN�LISE BIVARIADA - VALOR X TOTAL DE BANHEIROS

#AN�LISE BIVARIADA - VALOR X �REA TOTAL
par(mfrow=c(1,1))
par(las=2)
plot(data_wood$valor_total  ~ data_wood$total_banheiros,
     main = 'An�lise Bivariada. Valor do Im�vel x Total de Banheiros',
     ylab = 'Valor do Im�vel (U$)',
     ylim=c(0,250000), 
     lwd = 1,
     xaxt = 'n',
     yaxt = 'n',
     xlab = 'Total de Banheiros',
     pch=19, 
     col="red")
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=atx, labels=format(atx, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
abline(lm(data_wood$valor_total ~ data_wood$total_banheiros))
grid()
R=cor(data_wood$valor_total,data_wood$total_banheiros) #correla��o
round(R,2)

##########AN�LISE BIVARIADA - VALOR X TOTAL DE LAREIRAS

par(las = 1)
boxplot(data_wood$valor_total ~ data_wood$total_lareiras,
        main = 'An�lise Bivariada. Valor do Im�vel x Total de Lareiras.',
        yaxt = 'n',
        ylab = '',
        xlab = 'Total de Lareiras',
        col = hcl.colors(3, palette = "ag_GrnYl"))
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at = aty, labels = format(aty, scientific = FALSE), hadj = 0.9, cex.axis = 0.8, las = 2)
grid(10,col = "gray")

##########AN�LISE BIVARIADA - VALOR X CEP
par(las = 1)
boxplot(data_wood$valor_total ~ data_wood$cep,
        main = 'An�lise Bivariada. Valor do Im�vel x CEP.',
        yaxt = 'n',
        ylab = '',
        xlab = 'CEP',
        col = hcl.colors(3, palette = "ag_GrnYl"))
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at = aty, labels = format(aty, scientific = FALSE), hadj = 0.9, cex.axis = 0.8, las = 2)
grid(10,col = "gray")


#AN�LISE BIVARIADA - VALOR DO IM�VEL X VALOR DO TERRENO
par(las=2)
par(mar=c(8,4,2,2))
plot(data_wood$valor_total ~ data_wood$valor_terreno,
     main = 'An�lise Bivariada. Valor do Im�vel x Valor do Terreno.',
     xaxt='n', 
     yaxt='n',
     ylab = 'Valor do Im�vel (U$)',
     xlab = 'Valor do Terreno (U$)',
     pch=19, 
     col="red")
abline(lm(data_wood$valor_total ~ data_wood$valor_terreno))
atx <- seq(par("xaxp")[1], par("xaxp")[2], (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3])
axis(1, at=atx, labels=format(atx, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=1)
grid()
R=cor(data_wood$valor_total,data_wood$valor_terreno) #correla��o
round(R,2)
#CORELA��O .89. FORTE.


#PREVENDO VALOR DO IM�VEL
r1=lm(data=data_wood, valor_total ~ .)  
summary(r1)
#r2 = 0,9588

#CRIT�RIO DE AKAICE PARA SELEC�O DE VARI�VEIS
r2 <- step(r1, trace =  FALSE)
summary(r2)

#FORMATA AS AS ESTIMATIVAS DO MODELO EM VALOR REAL(U$). PADR�O � NOTA��O CIENT�FICA.
options(scipen=999)

summary(r2)
r3=lm(data=data_wood, valor_total ~ ano_construcao + area_util + total_andares + total_lareiras + valor_terreno) 
summary(r3)

r4 = predict(r3, newdata = data_wood);r4


influenceIndexPlot(r3)

influencePlot(r3)

data_wood2 = cbind(data_wood, valor_previsto = fitted(r3), residual = resid(r3))

#ANALISANDO O PODER PREDITIVO DOS ERROS PERCENTUAIS
data_wood2 <- data_wood2 %>% mutate(ep=data_wood2$residual/data_wood2$valor_total*100)
plot(data_wood2$ep,
     main = 'Erros Percentuais',
     pch = 19,
     col = 2,
     yaxt = 'n')
aty <- seq(par("yaxp")[1], par("yaxp")[2], (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE), hadj=0.9, cex.axis=0.8, las=2)
abline(h = 0, col = 4)
grid(col=3)
rm(aty)

vvif <- vif(r3)
vvif
#N�O H� VALORES ACIMA DE 5. N�O H� MULTICOLINARIEDADE

#MAPE: m�dia dos erros percentuais absolutos
MAPE(y_pred = data_wood2$valor_previsto, y_true = data_wood2$valor_total)*100

#RMSE: raiz da m�dias dos quadrados dos res�duos
RMSE(y_pred = data_wood2$valor_previsto, y_true = data_wood2$valor_total)
 















