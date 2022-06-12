#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# INFERENCIA ESTATISTICA
# TRABALHO INDIVIDUAL
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#ALUNOS: MARCOS SOARES, ANA PAULA, F�BIO MONTEIRO, LUCAS SENA E DAVI
rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(MASS)
library(dplyr)
library(rstatix)
library(stringr)
library(ggplot2)
library(reshape2)



#--------------------------------------------------------------------------------#
# Fator de risco para baixo peso ao nascer
# DATASET: low_birth_weight.csv
#--------------------------------------------------------------------------------#

# Contexto: O nascimento de beb�s com baixo � preocupante devido ao fato de que 
# as taxas de mortalidade infantil e defeitos de nascen�a s�o mais altos nesses
# casos. O comportamento de uma mulher gr�vida, como dieta, fumo e h�bitos pre-
# natais, pode alterar suas chances do parto ser prematuro e consequentemente
# alterar o peso do beb�. Os dados remetem a um estudo realizado em 1986 (adap-
# tado de Hosmer e Lomeshow 2000) no qual foram coletados dados de 189 mulheres
# (das quais 59 tiveram beb�s de baixo peso) de um hospital nos EUA.
# O objetivo do estudo foi identificar os fatores de risco associados ao parto
# de um beb� de baixo peso. 

# Foco nas vari�veis catg�ricas

# VARIAVEL ALVO
# LOW - Low birth weight ( 0=No (birth weight >= 2500 g), 
#                          1=Yes (birth weight < 2500 g) )
# VARIAVEIS EXPLICATIVAS
# RACE - Race of mother (1=White, 2=Black, 3=Other)
# SMOKE - Smoking status during pregnancy (0=No, 1=Yes)
# PTL - History of premature labor (0=None, 1=One, etc.) *** trate como categ
# HT - History of hypertension (0=No, 1=Yes)
# UI - Presence of uterine irritability (0=No, 1=Yes)
# FTV - Number of physician visits during the first trimester
# BWT - The actual birth weight (in grams) ****** deu origem ao alvo- n�o usar
# AGE - Age of mother (in years)
# LWT - Weight of mother at the last menstrual period (in pounds)

## 1) Leitura do Arquivo
path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Infer�ncia Estat�stica/Trabalho1/'
data_nascidos <- read.csv(paste(path_arquivos,'low_birth_weight.csv',sep = ''), sep = ';', header = TRUE, stringsAsFactors = TRUE)

data_nascidos <- low_birth_weight
## 2) Verifica�ao do data frame

summary(data_nascidos)
str(data_nascidos)

## 3) TRATAMENTO DAS VARI�VEIS
#CONVERS�O DE LIBRAS EM KG
data_nascidos$LWT <- round(as.double(data_nascidos$LWT / as.double(2.2046)), digits = 2)
data_nascidos <- data_nascidos %>% filter(!is.na(data_nascidos$PTL))
#REMO��O DA VARI�VEL BWT, N�O NECESS�RIA AO ESTUDO.
data_nascidos <- data_nascidos[, -10]

## 4) TRATAMENTO DOS DADOS
data_nascidos$LOW <- if_else(data_nascidos$LOW == 0, 'Peso Normal','Abaixo do Peso')
data_nascidos$SMOKE <- if_else(data_nascidos$SMOKE == 0, 'N�o Fumante','Fumante')


## 5) FATORIZANDO....
data_nascidos$RACE = factor(data_nascidos$RACE, levels = c(1,2,3), labels = c('White', 'Black','Other'))
data_nascidos$SMOKE <- factor(data_nascidos$SMOKE)
data_nascidos$PTL = factor(data_nascidos$PTL, levels = c(0,1,2), labels = c('Sem Hist�rico', 'Uma Incid�nca','Duas ou mais incid�ncias'))
data_nascidos$UI = factor(data_nascidos$UI, levels = c(0,1), labels = c('N�o', 'Sim'))
data_nascidos$FTV <- factor(data_nascidos$FTV)
data_nascidos$HT = factor(data_nascidos$HT, levels = c(0,1), labels = c('N�o', 'Sim'))

summary(data_nascidos)

##########FIM TRATAMENTO DOS DADOS


## 6) VIS�O GERAL DAS INFORMA��ES - DISTRIBUI�AO DAS VARIAVEIS CATEG�RICAS 

## 6.1) AN�LISE SOBRE A COR DA PELA DAS M�ES



d1 <- group_by(.data = data_nascidos, RACE) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$RACE))

rm(d1)

#51 % DAS M�ES DO ESTUDO POSSUEM A COR DA PELE BRANCA.
#14 % DAS M�ES DO ESTUDO POSSUEM A COR DA PELE PRETA
#35 % DAS M�ES DO ESTUDO POSSUEM A COR DA PELE NEM BRANCA E NEM PRETA.

vrPorcentagemW <- round((nrow(subset(data_nascidos, RACE == 'White')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemB <- round((nrow(subset(data_nascidos, RACE == 'Black')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemO <- round((nrow(subset(data_nascidos, RACE == 'Other')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$RACE),
        main = 'Distribui��o de M�es por Cor da Pele.',
        ylab = 'Total de M�es', 
        ylim = c(0,100),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemW,'%',sep = ''),str_c('',vrPorcentagemB,'%',sep = ''),str_c('',vrPorcentagemO,'%',sep = '')))
legend("topright", legend = c('M�es Brancas','M�es Negras', 'Outras'), fill = hcl.colors(3, palette = "Peach"))

rm(vrPorcentagemW,vrPorcentagemB,vrPorcentagemO,d1)

########## FIM AN�LISE SOBRE A COR DA PELA DAS M�ES

## 6.2) AN�LISE SOBRE O CONSUMO DE TABACO DAS M�ES

d1 <- group_by(.data = data_nascidos, SMOKE) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$SMOKE))
#39.15 % DAS M�ES S�O FUMANTES

par(mar=c(6,5,5,5))
vrPorcentagemNF <- round((nrow(subset(data_nascidos, SMOKE == 'Fumante')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_nascidos, SMOKE == 'N�o Fumante')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$SMOKE),
        main = 'Distribui��o de M�es por Consumo de Tabaco.',
        ylab = 'Total de M�es', 
        ylim = c(0,150),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemNF,'%',sep = ''),str_c('',vrPorcentagemF,'%',sep = '')))
legend("topright", legend = c('Fumantes','N�o Fumantes'), fill = hcl.colors(2, palette = "Peach"))

rm(d1,dd2,vrPorcentagemF, vrPorcentagemNF)

#INTERPRETAR BOXPLOT QUARTILS E OUTLIERS...

##########FIM AN�LISE SOBRE O CONSUMO DE TABACO PELAS M�ES

## 6.3 ANALISE SOBRE O TOTAL PARTO PREMATURO 

d1 <- group_by(.data = data_nascidos, PTL) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$PTL))

#Sem Hist�rico 84%
#Uma Incid�ncia 13%
#Mais Incid�ncias 3%

par(mar=c(6,5,5,5))
vrPorcentagemSH <- round((nrow(subset(data_nascidos, PTL == 'Sem Hist�rico')) /nrow(data_nascidos))*100,digits = 2)
vrPorcentagemMI <- round((nrow(subset(data_nascidos, PTL == 'Duas ou mais incid�ncias')) /nrow(data_nascidos))*100,digits = 2)
vrPorcentagemUI <- round((nrow(subset(data_nascidos, PTL == 'Uma Incid�nca')) /nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$PTL),
        main = 'Hist�rico de Parto Prematuro.',
        ylab = 'Total de M�es', 
        ylim = c(0,200),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemSH,'%',sep = ''),str_c('',vrPorcentagemUI,'%',sep = ''), str_c('',vrPorcentagemMI,'%',sep = '')))
legend("topright", legend = c('Sem Hist�rico','Duas ou Mais Incid�ncias','Uma Incid�ncia'), fill = hcl.colors(3, palette = "Peach"))

rm(d1,vrPorcentagemSH, vrPorcentagemMI,vrPorcentagemUI)
##########FIM AN�LISE SOBRE PARTO PR� MATURO

## 6.4) AN�LISE SOBRE HISTORICO DE HIPERTENS�O ??? Erro no argumento

d1 <- group_by(.data = data_nascidos, HT) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$HT))

#94 % DAS M�ES NAO TEM HIPERTENS�O
#06 % DAS M�ES TEM HIPERTENS�O

vrPorcentagemN <- round((nrow(subset(data_nascidos, HT == 'N�o')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_nascidos, HT == 'Sim')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$HT),
        main = 'M�es com Hist�rico de Hipertens�o',
        ylab = 'Total de M�es', 
        ylim = c(0,200),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemN, '%', sep = ''),str_c('',vrPorcentagemS, '%', sep = '')))
legend("topright", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "Peach"))

rm(vrPorcentagemS,vrPorcentagemN,d1)

####################FIM DA ANALISE DE HIST�RICO DE HIPERTENS�O

## 6.5 PRESEN�A DE IRRITABILIDADE UTERINA ???? N�O SAI A PORCENTAGEM
d1 <- group_by(.data = data_nascidos, UI) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$UI))

#85 % DAS M�ES TEM HIST�RICO DE IRRITABILIDADE UTERINA
#15 % DAS M�ES TEM HIST�RICO DE IRRITABILIDADE UTERINA

vrPorcentagemN <- round((nrow(subset(data_nascidos, UI == 'N�o')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_nascidos, UI == 'Sim')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$UI),
        main = 'M�es com hist�rico de Irritabilidade Uterina',
        ylab = 'Total de M�es', 
        ylim = c(0,200),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemN, '%', sep = ''),str_c('',vrPorcentagemS, '%', sep = '')))
legend("topright", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "Peach"))

rm(vrPorcentagemN,vrPorcentagemS,d1)

####################FIM DA ANALISE DE HIST�RICO DE M�ES COM IRRITABILIDADE UTERINA        

## 6.6) AN�LISE SOBRE O TOTAL DE REC�M NACIDOS ABAIXO DO PESO

d1 <- group_by(.data = data_nascidos, LOW) %>% summarise(count=n())
d1

#31% DOS NASCIDOS FORAM ABAIXO DO PESO

par(mar=c(6,5,5,5))
vrPorcentagemAP <- round((nrow(subset(data_nascidos, LOW == 'Abaixo do Peso')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemPN <- round((nrow(subset(data_nascidos, LOW == 'Peso Normal')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$LOW),
        main = 'Distribui��o de Nascidos Abaixo do Peso',
        ylab = 'Peso em KG',
        ylim = c(0,130),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemAP,'%',sep = ''),str_c('',vrPorcentagemPN,'%',sep = '')))
legend("topleft", legend = c('Abaixo do Peso', 'Peso Normal'), fill = hcl.colors(2, palette = "Peach"))

rm(d1, vrPorcentagemAP, vrPorcentagemPN)

##########FIM AN�LISE SOBRE O TOTAL DE NASCIDOS POR PESO AO NASCER

## 6.8)AN�LISE SOBRE A IDADE DAS M�ES
mean(data_nascidos$AGE)
#M�DIA DE IDADE DAS M�ES � DE 23 ANOS.

par(mar = c(3,4,7,5))
boxplot(data_nascidos$AGE,
        col = 'pink',
        main = 'An�lise Univariada. Idade das M�es.', 
        cex.main = 0.9,
        ylab = 'Idade',
        border = 'gray20')

par(mar = c(5,4,7,5))
hist(data_nascidos$AGE,
     main = 'Distribui��o Das M�es por Idade.',
     xlab = 'Faixa et�ria em anos.',
     ylab = 'Total de M�es',
     ylim = c(0,100),
     xlim = c(14,45),
     col = hcl.colors(10, palette = "Red-Blue"))

#####################FIM VIS�O GERAL DAS INFORMA��ES#####################################

###########TESTES DE HIP�TESES 

##1.ESTUDO SOBRE A COR DA PELE DAS M�ES
#Diagrama

#Ho - A cor da pele das m�es n�o tem rela��o com o baixo peso ao nascer.
#Ha - A cor da pele das m�es tem rela��o com o baixo peso ao nascer.

d1 <- group_by(.data = data_nascidos, RACE, LOW) %>% summarise(count=n(),                                                             media=row_number())
d1

par(las = 1)
par(mar = c(6,5,5,5))
barplot(table(data_nascidos$RACE,data_nascidos$LOW),
        main = 'An�lise Bivariada. Nascimentos Abaixo do Peso x Cor da Pele da M�e.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(3), border = 'gray20')
legend("topleft", legend = c('Branca','Negra','Outros'), fill = cm.colors(3))


chisq.test(table(data_nascidos$RACE, data_nascidos$LOW))

## OU
dj <- table(data_nascidos$RACE, data_nascidos$LOW)
chisq.test(dj)


#P-VALUE > 5%
#CONCLUS�O
# Como o p-valor= 0.0905 % � maior que 5% n�o podemos rejeitar a hip�tese Nula, 
#portanto, conclu�mos que  N�o existe depend�ncia entre a cor da pela das m�es (Ra�a) 
#e o baixo peso ao nascer ao n�vel de signific�ncia de 5%. 

########## FIM AN�LISE SOBRE A COR DA PELE DAS M�ES

##2.ESTUDO SOBRE CONSUMO DE TABACO

# Diagrama

# H0: N�o existe depend�ncia entre o consumo de tabaco e o nascimento de beb�s abaixo do peso.
# Ha: Existe depend�ncia entre o consumo de tabaco e o nascimento de beb�s abaixo do peso.


par(mar = c(4,4,4,2))
barplot(table(data_nascidos$SMOKE,data_nascidos$LOW),
        main = 'An�lise Bivariada. Nascimentos Abaixo do Peso x Consumo de Tabaco.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('M�es Fumantes','M�es n�o Fumantes'), fill = cm.colors(2))


dj <- table(data_nascidos$SMOKE, data_nascidos$LOW)
chisq.test(dj)

#CONCLUS�O
#Como o p-valor=0.03958 e menor que 5% rejeitamos a hip�tese nula. Portanto,
#conclu�mos que existe depend�ncia entre o consumo de tabaco e o nascimento de beb�s abaixo do peso, ao n�vel de 
#signific�ncia de 5%. 

######FIM ESTUDO SE O TABACO INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####


##3.ESTUDO SOBRE HISTORICO DE PARTO PREMATURO

#Diagrama 
# H0: N�o existe depend�ncia entre m�es que tem hist�rico de parto prematuro e o nascimento de beb�s abaixo do peso.
# Ha: Existe depend�ncia entre m�es que tem hist�rico de parto prematuro e o nascimento de beb�s abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$PTL,data_nascidos$LOW),
        main = 'An�lise Bivariada. Nascimentos Abaixo do Peso x Historico de Parto Prematuro.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(3), border = 'gray20')
legend("topleft", legend = c('Sem Hist�rico','Uma Incid�ncia', 'Duas ou mais incid�ncias'), fill = cm.colors(3))

dj <- table(data_nascidos$PTL, data_nascidos$LOW)
chisq.test (dj, simulate.p.value = TRUE)


#CONCLUS�O
#Como o p-valor=0.00049 e � menor que 5% rejeitamos a hip�tese nula. Portanto,
#conclu�mos que existe depend�ncia entre ma�s que tem hist�rico de parto prematuro
#e o nascimento de beb�s abaixo do peso, ao n�vel de signific�ncia de 5%. 

######FIM ESTUDO SE O TABACO INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####

##4.ESTUDO SOBRE HISTORICO DE HIPERTENS�O

#Diagrama 
# H0: N�o existe depend�ncia entre ter hist�rico de hipertens�o e o nascimento de beb�s abaixo do peso.
# Ha: Existe depend�ncia entre  ter hist�rico hipertens�o e o nascimento de beb�s abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$HT,data_nascidos$LOW),
        main = 'An�lise Bivariada. Nascimentos Abaixo do Peso x Historico de Hipertens�o.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('N�o','Sim'), fill = cm.colors(2))

dj <- table(data_nascidos$HT, data_nascidos$LOW)
chisq.test (dj)


#CONCLUS�O
#Como o p-valor=0.076 e � maior que 5%  nao podemos rejeitamos a hip�tese nula. Portanto,
#conclu�mos que existe n�o depend�ncia entre m�es que tem hist�rico de hipertens�o e o nascimento de beb�s abaixo do peso, ao n�vel de 
#signific�ncia de 5%. 

######FIM ESTUDO SE  HIST�RICO DE HIPERTENS�O  INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####

##5.ESTUDO SOBRE PRESEN�A DE IRRITABILIDADE UTERINA 

#Diagrama 
# Ha: N�o existe depend�ncia entre o fato das m�es terem irritabilidade uterina e o nascimento de beb�s abaixo do peso.
# Ha: Existe depend�ncia entre o fato das m�es terem irritabilidade uterina e o nascimento de beb�s abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$UI,data_nascidos$LOW),
        main = 'An�lise Bivariada. Nascimentos Abaixo do Peso x Irritabilidade Uterina.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('N�o','Sim'), fill = cm.colors(2))

dj <- table(data_nascidos$UI, data_nascidos$LOW)
chisq.test (dj)


#CONCLUS�O
#Como o p-valor=0.035 � menor que 5% rejeitamos a hip�tese nula em favor de Ha. 
#Portanto conclu�mos que existe depend�ncia entre o hist�rico de irritabilidade uterina
#e o nascimento de beb�s abaixo do peso, ao n�vel de signific�ncia de 5%. 

######FIM ESTUDO SE A PRESEN�A DE IRRITABILIDADE UTERINA INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####


#--------------------------------------------------------------------------------#
# IMC vs G�nero em pacientes com ataque card�aco
# DATASET: heart_attack.csv
#--------------------------------------------------------------------------------#

# Contexto: Um estudo foi conduzido com pacientes de uma certa regi�o metropoli-
# tana que haviam sofrido ataque cardi�co. Nesse exemplo estamos interessados em
# determinar se h� uma rela��o entre �ndice de Massa Corporal e G�nero. Ind�vi-
# duos que apresentaram no hospital ataque card�aco foram selecionados aleatoria-
# mente para participar do estudo.


data_heart_attack=read.csv("/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Infer�ncia Estat�stica/Trabalho1/heart_attack.csv",sep=",",header=TRUE)
data_heart_attack <- data_heart_attack %>% mutate(categoria=ifelse(data_heart_attack$bmi<24.9,'Normal',
                                                                   ifelse(data_heart_attack$bmi<29.9,'Sobrepeso',
                                                                          ifelse(data_heart_attack$bmi<39.9,'Obesidade','Morbidade'))))
data_heart_attack <- heart_attack
d1 <- group_by(.data = data_heart_attack, gender, categoria) %>% summarise(count=n(),
                                                                media_bmi=mean(bmi),
                                                                var=var(bmi),
                                                                sd=sd(bmi))

d1


d1 <- group_by(.data = data_heart_attack, gender) %>% summarise(count=n(),
                                                                   media_bmi=mean(bmi),
                                                                   var=var(bmi),
                                                                  sd=sd(bmi))

d1

par(mar=c(6,5,5,5))
vrPorcentagemF <- round((nrow(subset(data_heart_attack, gender == '0')) / nrow(data_heart_attack))*100,digits = 2)
vrPorcentagemM <- round((nrow(subset(data_heart_attack, gender == '1')) / nrow(data_heart_attack))*100,digits = 2)

barplot(table(data_heart_attack$gender),
        main = 'Distribui��o por Gen�ro.',
        ylab = 'Total de pacientes', 
        ylim = c(0,400),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemM,'%',sep = '')))
legend("topright", legend = c('Feminino','Masculino'), fill = hcl.colors(2, palette = "Peach"))


boxplot(data_heart_attack$bmi ~ data_heart_attack$gender, 
       main = 'Analise Bivariada IMC X Genero',
        col= hcl.colors(5, palette = 'PuBu'),
       xlab = 'Genero',
       ylab = 'IMC')
legend("topright", legend = c('Feminino','Masculino'), fill = hcl.colors(2, palette = "PuBu"))
#Diagrama

#H0 - A m�dia de IMC de mulheres que sofreram infarto � igual ou inferior � m�dia de IMC de homens na mesma condi��o.
#Ha - A m�dia de IMC de mulheres que sofreram infarto � superior � m�dia de IMC de homens na mesma condi��o.

var.test(bmi ~ gender, data = data_heart_attack,  alternative='two.sided')
#P-VALUE < 5%. VARIABILIDADES S�O DIFERENTES

t.test(bmi ~ gender, data = data_heart_attack, alternative='greater', var.equal=FALSE)
#P-VALUE < 5%. HIP�TESE NULA REJEITADA EM FAVOR DE HA.

#CONCLUS�O
#COM O P-VALUE ABAIXO DE 5% REJEITAMOS A HIP�TESE NULA EM FAVOR DE HA. 
#DESTA FORMA CONCLU�TMOS QUE A M�DIA DE IMC DE MULHERES QUE SOFRERAM INFARTO � SUPERIOR
#A M�DIA DE IMC DE HOMENS NA MESMA CONDIC�O, APONTANDO UMA RELA��O ENTRE IMC E G�NERO.


#--------------------------------------------------------------------------------#
# Ingest�o de �lcool vs Dire��o
# DATASET: beers.csv
#--------------------------------------------------------------------------------#

# Contexto: A ingest�o de bebidas alc�olicas � uma das causas principais de aci-
# dentes com ve�culos. Um amostra de 20 motoristas foi escolhida e seu tempo de
# rea��o em uma pista de obst�culos foi medido antes e depois da ingest�o de 
# duas cervejas. O objetivo do estudo foi o de verificar se os motoristas ficam
# prejudicados ap�s beber duas cervejas


path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Infer�ncia Estat�stica/Trabalho1/'
data_beers <- read.csv(paste(path_arquivos,'beers.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_beers <- beers
data_antes <- data_beers[,1, drop = F]
colnames(data_antes)[1] <- 'Tempo.De.Rea��o'
data_antes$alcool='Antes'

data_depois <- data_beers[,2, drop = F]
colnames(data_depois)[1] <- 'Tempo.De.Rea��o'
data_depois$alcool='Depois'

d3 <- merge(data_antes, data_depois, all = T)

boxplot(Tempo.De.Rea��o ~ alcool, data = d3,
        main = 'An�lise Bivariada: Consulmo de �lcool x Tempo de rea��o', 
        cex.main = 1.5,
        xlab = 'Consumo de �lcool',
        ylab = 'Tempo de rea��o (em segundos).', 
        cex.axis = 1.2,
        cex.lab = 1.2,
        ylim = c(2,8),
        col = hcl.colors(3, palette = 'Cold'),
        border = 'gray20')

# Diagrama

# H0: media do tempo de rea��o dos motoristas ap�s o consumo de �lcool � igual ou inferior do que 
# a m�dia do tempo de rea��o antes do consumo de �lcool.

# Ha: media do tempo de rea��o dos motoristas ap�s o consumo de �lcool � maior do que 
# a m�dia do tempo de rea��o antes do consumo de �lcool.


t.test(data_beers$Before , data_beers$After, alternative = 'two.sided', paired = TRUE)

#CONSLUS�O
#Os dados n�o mostram evid�ncia o suficiente para concluir que o consumo de �lcool
#torna o tempo de rea��o dos motoristas maior do que ante do consumo de �lcool.

#--------------------------------------------------------------------------------#
# Ganho de peso em ratos
# DATASET: ratfeed.txt
#--------------------------------------------------------------------------------#

# Contexto: Um estudo foi conduzido com ratos para avaliar o impacto da dieta no
# ganho de peso nesses animais. Para isso os ratos foram induzidos a diferentes 
# dietas com diferentes quantidades e avaliado o ganho de peso nesses animais.
# No banco de dados temos.

# Amount: 1- high, 2- low
# Type: 1- beef, 2- pork, 3- cereal

path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Infer�ncia Estat�stica/Trabalho1/'
data_ratfeed <- read.table(paste(path_arquivos,'ratfeed.txt',sep = ''), sep = '', header = TRUE, stringsAsFactors = TRUE)
data_ratfeed <- ratfeed
data_ratfeed$Type = ifelse(data_ratfeed$Type==1,'Beef',ifelse(data_ratfeed$Type==2,'Pork','Cereal'))
data_ratfeed$Amount = ifelse(data_ratfeed$Amount==1,'High','Low')


colnames(data_ratfeed)[1] = 'Ganho.De.Peso'
colnames(data_ratfeed)[2] = 'Quantidade'
colnames(data_ratfeed)[3] = 'Tipo.De.Alimento'

#AN�LISE BIVARIADA GANHO DE PESO X QUANTIDADE
par(mar=c(9,5,2,3))
par(las=2)
boxplot(Ganho.De.Peso ~ Quantidade, 
        data = data_ratfeed,
        main = 'Quantidade de alimentos',
        ylim = c(40,150),
        ylab = '',xlab = '',
        col = hcl.colors(2,palette = "Terrain 2"))

d1 <- group_by(.data = data_ratfeed, Quantidade %>% summarise(count=n(),
                                                              var=var(Ganho.De.Peso)))
d1

#Diagrama

# H0 - A m�dia de ganho de peso de ratos que receberam muita quantidade de alimentos 
#� igual ou inferior a m�dia de ganho de peso de ratos que receberam pouca quantidade.

# Ha - A m�dia de ganho de peso de ratos que receberam muita quantidade de alimentos 
#� superior a m�dia de ganho de peso de ratos que receberam pouca quantidade.


var.test(Ganho.De.Peso ~ Quantidade, data = data_ratfeed,  alternative =  'two.sided')
#P-VALUE = 0.978. VARI�NCIAS N�O S�O DIFERENTES.

t.test(Ganho.De.Peso ~ Quantidade, data = data_ratfeed, alternative = 'greater', var.equal = TRUE)
#P-VALUE <<<<<<<<< 5%.

#CONCLUS�O
#COMO P-VALUE � MENOR QUE 5% PODEMOS REJEITAR A HIP�TESE NULA EM FAVOR DE HA.
#DESTA FORMA CONCLU�MOS COM 95% DE CONFIAN�A QUE A QUANTIDADE DE ALIMENTO DADA
#AOS RATOS TEM RELA�AO COM O GANHO DE PESO.


#AN�LISE BIVARIADA GANHO DE PESO X TIPO
par(mar=c(9,5,2,3))
par(las=2)
boxplot(Ganho.De.Peso ~ Tipo.De.Alimento, 
        data = data_ratfeed,
        ylim = c(40,150),
        ylab = '',xlab = '',
        col = hcl.colors(3,palette = "Terrain 2"))

d1 <- group_by(.data = data_ratfeed, Tipo.De.Alimento %>% summarise(count=n(),
                                                              var=var(Ganho.De.Peso)))
d1

#Diagrama

# H0 - As m�dias de ganho de peso n�o se diferem com rela��o ao tipo de alimento consumido pelos ratos.
# Ha - Ao menos uma das m�dias de ganho de peso se difere com rela��o aO tipo de alimento consumido pelos ratos.


waov <-  t.test(Ganho.De.Peso ~ Tipo.De.Alimento), data = data_ratfeed)
waov

#CONCLUS�O
# Considerando as vari�ncias diferentes os resultados mostram que ao menos uma 
# das m�dias de ganho de peso se difere com rela��o ao tipo de alimento consumido 
# pelos ratos.
# Desta forma conclu�mos que o tipo de alimento dado aos ratos tem rela��o com o ganho de peso.

#AN�LISE BIVARIADA GANHO DE PESO X QUANTIDADE + TIPO DE ALIMENTO
par(mar=c(9,5,2,3))
par(las=2)
boxplot(Ganho.De.Peso ~ (Quantidade + Tipo.De.Alimento), 
        data = data_ratfeed,
        ylim = c(40,150),
        ylab = '',xlab = '',
        col = hcl.colors(6,palette = "Terrain 2"))

d1 <- group_by(.data = data_ratfeed, Quantidade, Tipo.De.Alimento) %>% summarise(count=n(),
                                                                                 var=var(Ganho.De.Peso))
d1
#Diagrama

# H0 - As m�dias de ganho de peso n�o se diferem com rela��o a quantidade e o tipo de alimento consumido pelos ratos.
# Ha - Ao menos uma das m�dias de ganho de peso se difere com rela��o a quantidade e o tipo de alimento consumido pelos ratos.

waov <-  welch_anova_test(Ganho.De.Peso ~ (Quantidade + Tipo.De.Alimento), data = data_ratfeed)
waov


#CONCLUS�O
# Os resultados mostram que ao menos uma das m�dias de ganho de peso se difere 
# com rela��o a quantidade e tipo de alimento consumido pelos ratos.
# Desta forma conclu�mos que a quantidade e o tipo de alimento 
# dado aos ratos tem rela��o com o ganho de peso.


#--------------------------------------------------------------------------------#
# Gordura corporal em pacientes do sexo masculino
# Adaptado de: 
# bodyfat_men.csv
#--------------------------------------------------------------------------------#

# Objetivos: entender se existem rela��es lineares entre medi��es de circunfe-
# r�ncia de v�rias partes do corpo de homens (al�m de outras informa��es) com o 
# % de gordura corporal desses indiv�duos

# Age (years)
# Weight (lbs)
# Height (inches)
# Abdomen -- Forearm (circunference in cm)
# bodayfat (%)

# Al�m dos testes individuais, construa tamb�m uma regress�o linear com todas
# as vari�vei seguindo a metodologia vista em aula (atente � multicolinearidade
# e signific�ncia estat�stica das vari�veis)


path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Infer�ncia Estat�stica/Trabalho1/'
data_bodyfat <- read.csv(paste(path_arquivos,'bodyfat_men.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_bodyfat <- bodyfat_men
summary(data_bodyfat)

colnames(data_bodyfat)[1] = 'Idade'
colnames(data_bodyfat)[2] = 'Peso'
colnames(data_bodyfat)[3] = 'Altura'
colnames(data_bodyfat)[4] = 'Abd�men'
colnames(data_bodyfat)[5] = 'Quadril'
colnames(data_bodyfat)[6] = 'Pesco�o'
colnames(data_bodyfat)[7] = 'Coxa'
colnames(data_bodyfat)[8] = 'Punho'
colnames(data_bodyfat)[9] = 'B�ceps'
colnames(data_bodyfat)[10] = 'Antibra�o'
colnames(data_bodyfat)[11] = 'Gordura.Corporal'

#CONVERS�O DE DADOS
data_bodyfat$Peso = round(as.double(data_bodyfat$Peso * 0.453592),2)
data_bodyfat$Altura = round(as.double(data_bodyfat$Altura * 2.54),2)

#EXCLUS�O DE OUTLIERS
data_bodyfat <- data_bodyfat %>% subset(Altura > 75.00)

#r1 <- lm(Gordura.Corporal ~ (Idade  + Altura + Pesco�o + Punho + B�ceps + Antibra�o), data = data_bodyfat)
r1 <- lm(Gordura.Corporal ~ ., data = data_bodyfat)
summary(r1)


par(las=1)
plot(data_bodyfat$Gordura.Corporal, data_bodyfat$Idade + data_bodyfat$Altura,
     main = '', 
     xlab = 'Idade', 
     ylab = '% Gordura Corporal', 
     pch = 21, type = 'b',
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Idade, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

library(car)
par(mar=c(0,0,10,0))
scatterplot(Gordura.Corporal ~ (Peso + Alturad), 
            data=data_bodyfat, legend = NULL,
            main="")

vif(r1)
#FORAM OBSERVADAS MULTICOLINARIEDADES DO ALVO COM RELA��O AS VARI�VEIS:
#PESO, ABD�MEN, QUADRIL E COXA.


#IDADE
r1 <- lm(Gordura.Corporal ~ Idade, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Idade, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Idade', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Idade, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)


#PESO
r1 <- lm(Gordura.Corporal ~ Peso, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Peso, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Peso (Kg)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Peso, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#ALTURA
r1 <- lm(Gordura.Corporal ~ Altura, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Altura, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Altura (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Altura, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#ABD�MEN
r1 <- lm(Gordura.Corporal ~ Abd�men, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Abd�men, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Abd�men (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Abd�men, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#QUADRIL
r1 <- lm(Gordura.Corporal ~ Quadril, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Quadril, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Quadril (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Quadril, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#PESCO�O
r1 <- lm(Gordura.Corporal ~ Pesco�o, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Pesco�o, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Pesco�o (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Pesco�o, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#COXA
r1 <- lm(Gordura.Corporal ~ Coxa, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Coxa, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Coxa (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Coxa, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#PUNHO
r1 <- lm(Gordura.Corporal ~ Punho, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Punho, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Punho (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Punho, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#B�CEPS
r1 <- lm(Gordura.Corporal ~ B�ceps, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$B�ceps, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'B�ceps (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ B�ceps, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#ABNTIBRA�O
r1 <- lm(Gordura.Corporal ~ Antibra�o, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Antibra�o, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Antibra�o (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Antibra�o, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)



#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#





# Create data
set.seed(112)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")

data_nascidos

dd1 <- table(data_nascidos$RACE)

dd1 <- rbind(data_nascidos$RACE)

dd2 <- as.data.frame(dd1)

abc <- melt(data = data_nascidos, id.vars = 'RACE','LOW')


barplot( data = abc, variable,value)

# Get the stacked barplot
barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        space=0.04, 
        font.axis=2, 
        xlab="group")

