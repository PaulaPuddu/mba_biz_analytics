qq3=BSB_T10_AP_P1_Q3
summary(qq3)
str(qq3)

# ANALISE UNIVARIADA 
# Variavel Alvo
boxplot(qq3$EXPN, col=18);grid()

# Variavel previsoras 
table(qq3$INCM, useNA = "ifany")
boxplot(qq3$INCM, col=18);grid()
which(qq3$INCM>19000)  # 6, 8, 49

table(qq3$HIGH, useNA = "ifany")
boxplot(qq3$HIGH, col=18);grid()

table(qq3$OVER, useNA = "ifany")
boxplot(qq3$OVER, col=18);grid()
which(qq3$OVER>16) #29
which(qq3$OVER<5) #49

table(qq3$PHYD, useNA = "ifany")
boxplot(qq3$PHYD, col=18);grid()
which(qq3$PHYD>350)

#ANALISE BIVARIADA 
plot(qq3$INCM, qq3$EXPN, pch=19, col=4)
abline(lm(qq3$EXPN ~ qq3$INCM))
round(cor(qq3$EXPN, qq3$INCM),3)
# Correlaçao 0.265

plot(qq3$HIGH, qq3$EXPN, pch=19, col=4)
abline(lm(qq3$EXPN ~ qq3$HIGH))
round(cor(qq3$EXPN, qq3$HIGH),3)
# Correlação -0.318

plot(qq3$OVER, qq3$EXPN, pch=19, col=4)
abline(lm(qq3$EXPN ~ qq3$OVER))
round(cor(qq3$EXPN, qq3$OVER),3)
# Correlação 0.477

plot(qq3$PHYD, qq3$EXPN, pch=19, col=4)
abline(lm(qq3$EXPN ~ qq3$PHYD))
round(cor(qq3$EXPN, qq3$PHYD),3)
# Correlação 0.436

############################
# REGRESSAO LINEAR MULTIPLA
############################
names(qq3)
rm=lm(data=qq3, EXPN~.)  
summary(rm)
# Coeficiente de HIGH -12974
###############################
# ERRO PERCENTUAL - RESIDUO
##############################
qq3$y=fitted(rm) ## y a variavel que eu quero prever
qq3$res = residuals(rm)  ## é o erro = a diferença entre o real e o estimado. Avaliaçao se o modelo é bom ou ruim.
##############################
#SELEÇÃO DE VARIÁVEL 
##############################
### Vamos selecionar variaveis com base no critério de Akaike (AIC)
rm=step(rm, trace=F)
summary(rm)


library(car)
influenceIndexPlot(rm, vars = c("Cook", "Sudentized", "hat"))
influencePlot(rm)

