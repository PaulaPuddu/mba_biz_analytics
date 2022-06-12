#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# INFERENCIA ESTATISTICA
# TRABALHO INDIVIDUAL
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#ALUNOS: MARCOS SOARES, ANA PAULA, FÁBIO MONTEIRO, LUCAS SENA E DAVI
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

# Contexto: O nascimento de bebês com baixo é preocupante devido ao fato de que 
# as taxas de mortalidade infantil e defeitos de nascença são mais altos nesses
# casos. O comportamento de uma mulher grávida, como dieta, fumo e hábitos pre-
# natais, pode alterar suas chances do parto ser prematuro e consequentemente
# alterar o peso do bebê. Os dados remetem a um estudo realizado em 1986 (adap-
# tado de Hosmer e Lomeshow 2000) no qual foram coletados dados de 189 mulheres
# (das quais 59 tiveram bebês de baixo peso) de um hospital nos EUA.
# O objetivo do estudo foi identificar os fatores de risco associados ao parto
# de um bebê de baixo peso. 

# Foco nas variáveis catgóricas

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
# BWT - The actual birth weight (in grams) ****** deu origem ao alvo- não usar
# AGE - Age of mother (in years)
# LWT - Weight of mother at the last menstrual period (in pounds)

## 1) Leitura do Arquivo
path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Inferência Estatística/Trabalho1/'
data_nascidos <- read.csv(paste(path_arquivos,'low_birth_weight.csv',sep = ''), sep = ';', header = TRUE, stringsAsFactors = TRUE)

data_nascidos <- low_birth_weight
## 2) Verificaçao do data frame

summary(data_nascidos)
str(data_nascidos)

## 3) TRATAMENTO DAS VARIÁVEIS
#CONVERSÃO DE LIBRAS EM KG
data_nascidos$LWT <- round(as.double(data_nascidos$LWT / as.double(2.2046)), digits = 2)
data_nascidos <- data_nascidos %>% filter(!is.na(data_nascidos$PTL))
#REMOÇÃO DA VARIÁVEL BWT, NÃO NECESSÁRIA AO ESTUDO.
data_nascidos <- data_nascidos[, -10]

## 4) TRATAMENTO DOS DADOS
data_nascidos$LOW <- if_else(data_nascidos$LOW == 0, 'Peso Normal','Abaixo do Peso')
data_nascidos$SMOKE <- if_else(data_nascidos$SMOKE == 0, 'Não Fumante','Fumante')


## 5) FATORIZANDO....
data_nascidos$RACE = factor(data_nascidos$RACE, levels = c(1,2,3), labels = c('White', 'Black','Other'))
data_nascidos$SMOKE <- factor(data_nascidos$SMOKE)
data_nascidos$PTL = factor(data_nascidos$PTL, levels = c(0,1,2), labels = c('Sem Histórico', 'Uma Incidênca','Duas ou mais incidências'))
data_nascidos$UI = factor(data_nascidos$UI, levels = c(0,1), labels = c('Não', 'Sim'))
data_nascidos$FTV <- factor(data_nascidos$FTV)
data_nascidos$HT = factor(data_nascidos$HT, levels = c(0,1), labels = c('Não', 'Sim'))

summary(data_nascidos)

##########FIM TRATAMENTO DOS DADOS


## 6) VISÃO GERAL DAS INFORMAÇÕES - DISTRIBUIÇAO DAS VARIAVEIS CATEGÓRICAS 

## 6.1) ANÁLISE SOBRE A COR DA PELA DAS MÃES



d1 <- group_by(.data = data_nascidos, RACE) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$RACE))

rm(d1)

#51 % DAS MÃES DO ESTUDO POSSUEM A COR DA PELE BRANCA.
#14 % DAS MÃES DO ESTUDO POSSUEM A COR DA PELE PRETA
#35 % DAS MÃES DO ESTUDO POSSUEM A COR DA PELE NEM BRANCA E NEM PRETA.

vrPorcentagemW <- round((nrow(subset(data_nascidos, RACE == 'White')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemB <- round((nrow(subset(data_nascidos, RACE == 'Black')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemO <- round((nrow(subset(data_nascidos, RACE == 'Other')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$RACE),
        main = 'Distribuição de Mães por Cor da Pele.',
        ylab = 'Total de Mães', 
        ylim = c(0,100),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemW,'%',sep = ''),str_c('',vrPorcentagemB,'%',sep = ''),str_c('',vrPorcentagemO,'%',sep = '')))
legend("topright", legend = c('Mães Brancas','Mães Negras', 'Outras'), fill = hcl.colors(3, palette = "Peach"))

rm(vrPorcentagemW,vrPorcentagemB,vrPorcentagemO,d1)

########## FIM ANÁLISE SOBRE A COR DA PELA DAS MÃES

## 6.2) ANÁLISE SOBRE O CONSUMO DE TABACO DAS MÃES

d1 <- group_by(.data = data_nascidos, SMOKE) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$SMOKE))
#39.15 % DAS MÃES SÃO FUMANTES

par(mar=c(6,5,5,5))
vrPorcentagemNF <- round((nrow(subset(data_nascidos, SMOKE == 'Fumante')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_nascidos, SMOKE == 'Não Fumante')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$SMOKE),
        main = 'Distribuição de Mães por Consumo de Tabaco.',
        ylab = 'Total de Mães', 
        ylim = c(0,150),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemNF,'%',sep = ''),str_c('',vrPorcentagemF,'%',sep = '')))
legend("topright", legend = c('Fumantes','Não Fumantes'), fill = hcl.colors(2, palette = "Peach"))

rm(d1,dd2,vrPorcentagemF, vrPorcentagemNF)

#INTERPRETAR BOXPLOT QUARTILS E OUTLIERS...

##########FIM ANÁLISE SOBRE O CONSUMO DE TABACO PELAS MÃES

## 6.3 ANALISE SOBRE O TOTAL PARTO PREMATURO 

d1 <- group_by(.data = data_nascidos, PTL) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$PTL))

#Sem Histórico 84%
#Uma Incidência 13%
#Mais Incidências 3%

par(mar=c(6,5,5,5))
vrPorcentagemSH <- round((nrow(subset(data_nascidos, PTL == 'Sem Histórico')) /nrow(data_nascidos))*100,digits = 2)
vrPorcentagemMI <- round((nrow(subset(data_nascidos, PTL == 'Duas ou mais incidências')) /nrow(data_nascidos))*100,digits = 2)
vrPorcentagemUI <- round((nrow(subset(data_nascidos, PTL == 'Uma Incidênca')) /nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$PTL),
        main = 'Histórico de Parto Prematuro.',
        ylab = 'Total de Mães', 
        ylim = c(0,200),
        col = hcl.colors(3, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemSH,'%',sep = ''),str_c('',vrPorcentagemUI,'%',sep = ''), str_c('',vrPorcentagemMI,'%',sep = '')))
legend("topright", legend = c('Sem Histórico','Duas ou Mais Incidências','Uma Incidência'), fill = hcl.colors(3, palette = "Peach"))

rm(d1,vrPorcentagemSH, vrPorcentagemMI,vrPorcentagemUI)
##########FIM ANÁLISE SOBRE PARTO PRÉ MATURO

## 6.4) ANÁLISE SOBRE HISTORICO DE HIPERTENSÃO ??? Erro no argumento

d1 <- group_by(.data = data_nascidos, HT) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$HT))

#94 % DAS MÃES NAO TEM HIPERTENSÃO
#06 % DAS MÃES TEM HIPERTENSÃO

vrPorcentagemN <- round((nrow(subset(data_nascidos, HT == 'Não')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_nascidos, HT == 'Sim')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$HT),
        main = 'Mães com Histórico de Hipertensão',
        ylab = 'Total de Mães', 
        ylim = c(0,200),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemN, '%', sep = ''),str_c('',vrPorcentagemS, '%', sep = '')))
legend("topright", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "Peach"))

rm(vrPorcentagemS,vrPorcentagemN,d1)

####################FIM DA ANALISE DE HISTÓRICO DE HIPERTENSÃO

## 6.5 PRESENÇA DE IRRITABILIDADE UTERINA ???? NÃO SAI A PORCENTAGEM
d1 <- group_by(.data = data_nascidos, UI) %>% summarise(count=n())
d1

prop.table(table(data_nascidos$UI))

#85 % DAS MÃES TEM HISTÓRICO DE IRRITABILIDADE UTERINA
#15 % DAS MÃES TEM HISTÓRICO DE IRRITABILIDADE UTERINA

vrPorcentagemN <- round((nrow(subset(data_nascidos, UI == 'Não')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_nascidos, UI == 'Sim')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$UI),
        main = 'Mães com histórico de Irritabilidade Uterina',
        ylab = 'Total de Mães', 
        ylim = c(0,200),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemN, '%', sep = ''),str_c('',vrPorcentagemS, '%', sep = '')))
legend("topright", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "Peach"))

rm(vrPorcentagemN,vrPorcentagemS,d1)

####################FIM DA ANALISE DE HISTÓRICO DE MÃES COM IRRITABILIDADE UTERINA        

## 6.6) ANÁLISE SOBRE O TOTAL DE RECÉM NACIDOS ABAIXO DO PESO

d1 <- group_by(.data = data_nascidos, LOW) %>% summarise(count=n())
d1

#31% DOS NASCIDOS FORAM ABAIXO DO PESO

par(mar=c(6,5,5,5))
vrPorcentagemAP <- round((nrow(subset(data_nascidos, LOW == 'Abaixo do Peso')) / nrow(data_nascidos))*100,digits = 2)
vrPorcentagemPN <- round((nrow(subset(data_nascidos, LOW == 'Peso Normal')) / nrow(data_nascidos))*100,digits = 2)
barplot(table(data_nascidos$LOW),
        main = 'Distribuição de Nascidos Abaixo do Peso',
        ylab = 'Peso em KG',
        ylim = c(0,130),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemAP,'%',sep = ''),str_c('',vrPorcentagemPN,'%',sep = '')))
legend("topleft", legend = c('Abaixo do Peso', 'Peso Normal'), fill = hcl.colors(2, palette = "Peach"))

rm(d1, vrPorcentagemAP, vrPorcentagemPN)

##########FIM ANÁLISE SOBRE O TOTAL DE NASCIDOS POR PESO AO NASCER

## 6.8)ANÁLISE SOBRE A IDADE DAS MÃES
mean(data_nascidos$AGE)
#MÉDIA DE IDADE DAS MÃES É DE 23 ANOS.

par(mar = c(3,4,7,5))
boxplot(data_nascidos$AGE,
        col = 'pink',
        main = 'Análise Univariada. Idade das Mães.', 
        cex.main = 0.9,
        ylab = 'Idade',
        border = 'gray20')

par(mar = c(5,4,7,5))
hist(data_nascidos$AGE,
     main = 'Distribuição Das Mães por Idade.',
     xlab = 'Faixa etária em anos.',
     ylab = 'Total de Mães',
     ylim = c(0,100),
     xlim = c(14,45),
     col = hcl.colors(10, palette = "Red-Blue"))

#####################FIM VISÃO GERAL DAS INFORMAÇÕES#####################################

###########TESTES DE HIPÓTESES 

##1.ESTUDO SOBRE A COR DA PELE DAS MÃES
#Diagrama

#Ho - A cor da pele das mães não tem relação com o baixo peso ao nascer.
#Ha - A cor da pele das mães tem relação com o baixo peso ao nascer.

d1 <- group_by(.data = data_nascidos, RACE, LOW) %>% summarise(count=n(),                                                             media=row_number())
d1

par(las = 1)
par(mar = c(6,5,5,5))
barplot(table(data_nascidos$RACE,data_nascidos$LOW),
        main = 'Análise Bivariada. Nascimentos Abaixo do Peso x Cor da Pele da Mãe.', 
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
#CONCLUSÃO
# Como o p-valor= 0.0905 % é maior que 5% não podemos rejeitar a hipótese Nula, 
#portanto, concluímos que  Não existe dependência entre a cor da pela das mães (Raça) 
#e o baixo peso ao nascer ao nível de significância de 5%. 

########## FIM ANÁLISE SOBRE A COR DA PELE DAS MÃES

##2.ESTUDO SOBRE CONSUMO DE TABACO

# Diagrama

# H0: Não existe dependência entre o consumo de tabaco e o nascimento de bebês abaixo do peso.
# Ha: Existe dependência entre o consumo de tabaco e o nascimento de bebês abaixo do peso.


par(mar = c(4,4,4,2))
barplot(table(data_nascidos$SMOKE,data_nascidos$LOW),
        main = 'Análise Bivariada. Nascimentos Abaixo do Peso x Consumo de Tabaco.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('Mães Fumantes','Mães não Fumantes'), fill = cm.colors(2))


dj <- table(data_nascidos$SMOKE, data_nascidos$LOW)
chisq.test(dj)

#CONCLUSÃO
#Como o p-valor=0.03958 e menor que 5% rejeitamos a hipótese nula. Portanto,
#concluímos que existe dependência entre o consumo de tabaco e o nascimento de bebês abaixo do peso, ao nível de 
#significância de 5%. 

######FIM ESTUDO SE O TABACO INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####


##3.ESTUDO SOBRE HISTORICO DE PARTO PREMATURO

#Diagrama 
# H0: Não existe dependência entre mães que tem histórico de parto prematuro e o nascimento de bebês abaixo do peso.
# Ha: Existe dependência entre mães que tem histórico de parto prematuro e o nascimento de bebês abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$PTL,data_nascidos$LOW),
        main = 'Análise Bivariada. Nascimentos Abaixo do Peso x Historico de Parto Prematuro.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(3), border = 'gray20')
legend("topleft", legend = c('Sem Histórico','Uma Incidência', 'Duas ou mais incidências'), fill = cm.colors(3))

dj <- table(data_nascidos$PTL, data_nascidos$LOW)
chisq.test (dj, simulate.p.value = TRUE)


#CONCLUSÃO
#Como o p-valor=0.00049 e é menor que 5% rejeitamos a hipótese nula. Portanto,
#concluímos que existe dependência entre maès que tem histórico de parto prematuro
#e o nascimento de bebês abaixo do peso, ao nível de significância de 5%. 

######FIM ESTUDO SE O TABACO INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####

##4.ESTUDO SOBRE HISTORICO DE HIPERTENSÃO

#Diagrama 
# H0: Não existe dependência entre ter histórico de hipertensão e o nascimento de bebês abaixo do peso.
# Ha: Existe dependência entre  ter histórico hipertensão e o nascimento de bebês abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$HT,data_nascidos$LOW),
        main = 'Análise Bivariada. Nascimentos Abaixo do Peso x Historico de Hipertensão.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('Não','Sim'), fill = cm.colors(2))

dj <- table(data_nascidos$HT, data_nascidos$LOW)
chisq.test (dj)


#CONCLUSÃO
#Como o p-valor=0.076 e é maior que 5%  nao podemos rejeitamos a hipótese nula. Portanto,
#concluímos que existe não dependência entre mães que tem histórico de hipertensão e o nascimento de bebês abaixo do peso, ao nível de 
#significância de 5%. 

######FIM ESTUDO SE  HISTÓRICO DE HIPERTENSÃO  INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####

##5.ESTUDO SOBRE PRESENÇA DE IRRITABILIDADE UTERINA 

#Diagrama 
# Ha: Não existe dependência entre o fato das mães terem irritabilidade uterina e o nascimento de bebês abaixo do peso.
# Ha: Existe dependência entre o fato das mães terem irritabilidade uterina e o nascimento de bebês abaixo do peso.

par(mar = c(4,4,4,2))
barplot(table(data_nascidos$UI,data_nascidos$LOW),
        main = 'Análise Bivariada. Nascimentos Abaixo do Peso x Irritabilidade Uterina.', 
        cex.main = 0.8,
        xlab = 'Peso ao Nascer',
        ylab = 'Total de Nascidos',
        ylim = c(0, 160),
        col = cm.colors(2), border = 'gray20')
legend("topleft", legend = c('Não','Sim'), fill = cm.colors(2))

dj <- table(data_nascidos$UI, data_nascidos$LOW)
chisq.test (dj)


#CONCLUSÃO
#Como o p-valor=0.035 é menor que 5% rejeitamos a hipótese nula em favor de Ha. 
#Portanto concluímos que existe dependência entre o histórico de irritabilidade uterina
#e o nascimento de bebês abaixo do peso, ao nível de significância de 5%. 

######FIM ESTUDO SE A PRESENÇA DE IRRITABILIDADE UTERINA INTERFERE NO NASCIMENTO COM ABAIXO DO PESO#####


#--------------------------------------------------------------------------------#
# IMC vs Gênero em pacientes com ataque cardíaco
# DATASET: heart_attack.csv
#--------------------------------------------------------------------------------#

# Contexto: Um estudo foi conduzido com pacientes de uma certa região metropoli-
# tana que haviam sofrido ataque cardiáco. Nesse exemplo estamos interessados em
# determinar se há uma relação entre Índice de Massa Corporal e Gênero. Indívi-
# duos que apresentaram no hospital ataque cardíaco foram selecionados aleatoria-
# mente para participar do estudo.


data_heart_attack=read.csv("/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Inferência Estatística/Trabalho1/heart_attack.csv",sep=",",header=TRUE)
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
        main = 'Distribuição por Genêro.',
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

#H0 - A média de IMC de mulheres que sofreram infarto é igual ou inferior à média de IMC de homens na mesma condição.
#Ha - A média de IMC de mulheres que sofreram infarto é superior à média de IMC de homens na mesma condição.

var.test(bmi ~ gender, data = data_heart_attack,  alternative='two.sided')
#P-VALUE < 5%. VARIABILIDADES SÃO DIFERENTES

t.test(bmi ~ gender, data = data_heart_attack, alternative='greater', var.equal=FALSE)
#P-VALUE < 5%. HIPÓTESE NULA REJEITADA EM FAVOR DE HA.

#CONCLUSÃO
#COM O P-VALUE ABAIXO DE 5% REJEITAMOS A HIPÓTESE NULA EM FAVOR DE HA. 
#DESTA FORMA CONCLUÍTMOS QUE A MÉDIA DE IMC DE MULHERES QUE SOFRERAM INFARTO É SUPERIOR
#A MÉDIA DE IMC DE HOMENS NA MESMA CONDICÃO, APONTANDO UMA RELAÇÃO ENTRE IMC E GÊNERO.


#--------------------------------------------------------------------------------#
# Ingestão de álcool vs Direção
# DATASET: beers.csv
#--------------------------------------------------------------------------------#

# Contexto: A ingestão de bebidas alcóolicas é uma das causas principais de aci-
# dentes com veículos. Um amostra de 20 motoristas foi escolhida e seu tempo de
# reação em uma pista de obstáculos foi medido antes e depois da ingestão de 
# duas cervejas. O objetivo do estudo foi o de verificar se os motoristas ficam
# prejudicados após beber duas cervejas


path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Inferência Estatística/Trabalho1/'
data_beers <- read.csv(paste(path_arquivos,'beers.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_beers <- beers
data_antes <- data_beers[,1, drop = F]
colnames(data_antes)[1] <- 'Tempo.De.Reação'
data_antes$alcool='Antes'

data_depois <- data_beers[,2, drop = F]
colnames(data_depois)[1] <- 'Tempo.De.Reação'
data_depois$alcool='Depois'

d3 <- merge(data_antes, data_depois, all = T)

boxplot(Tempo.De.Reação ~ alcool, data = d3,
        main = 'Análise Bivariada: Consulmo de álcool x Tempo de reação', 
        cex.main = 1.5,
        xlab = 'Consumo de Álcool',
        ylab = 'Tempo de reação (em segundos).', 
        cex.axis = 1.2,
        cex.lab = 1.2,
        ylim = c(2,8),
        col = hcl.colors(3, palette = 'Cold'),
        border = 'gray20')

# Diagrama

# H0: media do tempo de reação dos motoristas após o consumo de álcool é igual ou inferior do que 
# a média do tempo de reação antes do consumo de álcool.

# Ha: media do tempo de reação dos motoristas após o consumo de álcool é maior do que 
# a média do tempo de reação antes do consumo de álcool.


t.test(data_beers$Before , data_beers$After, alternative = 'two.sided', paired = TRUE)

#CONSLUSÃO
#Os dados não mostram evidência o suficiente para concluir que o consumo de álcool
#torna o tempo de reação dos motoristas maior do que ante do consumo de álcool.

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

path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Inferência Estatística/Trabalho1/'
data_ratfeed <- read.table(paste(path_arquivos,'ratfeed.txt',sep = ''), sep = '', header = TRUE, stringsAsFactors = TRUE)
data_ratfeed <- ratfeed
data_ratfeed$Type = ifelse(data_ratfeed$Type==1,'Beef',ifelse(data_ratfeed$Type==2,'Pork','Cereal'))
data_ratfeed$Amount = ifelse(data_ratfeed$Amount==1,'High','Low')


colnames(data_ratfeed)[1] = 'Ganho.De.Peso'
colnames(data_ratfeed)[2] = 'Quantidade'
colnames(data_ratfeed)[3] = 'Tipo.De.Alimento'

#ANÁLISE BIVARIADA GANHO DE PESO X QUANTIDADE
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

# H0 - A média de ganho de peso de ratos que receberam muita quantidade de alimentos 
#é igual ou inferior a média de ganho de peso de ratos que receberam pouca quantidade.

# Ha - A média de ganho de peso de ratos que receberam muita quantidade de alimentos 
#é superior a média de ganho de peso de ratos que receberam pouca quantidade.


var.test(Ganho.De.Peso ~ Quantidade, data = data_ratfeed,  alternative =  'two.sided')
#P-VALUE = 0.978. VARIÂNCIAS NÃO SÃO DIFERENTES.

t.test(Ganho.De.Peso ~ Quantidade, data = data_ratfeed, alternative = 'greater', var.equal = TRUE)
#P-VALUE <<<<<<<<< 5%.

#CONCLUSÃO
#COMO P-VALUE É MENOR QUE 5% PODEMOS REJEITAR A HIPÓTESE NULA EM FAVOR DE HA.
#DESTA FORMA CONCLUÍMOS COM 95% DE CONFIANÇA QUE A QUANTIDADE DE ALIMENTO DADA
#AOS RATOS TEM RELAÇAO COM O GANHO DE PESO.


#ANÁLISE BIVARIADA GANHO DE PESO X TIPO
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

# H0 - As médias de ganho de peso não se diferem com relaçào ao tipo de alimento consumido pelos ratos.
# Ha - Ao menos uma das médias de ganho de peso se difere com relaçào aO tipo de alimento consumido pelos ratos.


waov <-  t.test(Ganho.De.Peso ~ Tipo.De.Alimento), data = data_ratfeed)
waov

#CONCLUSÃO
# Considerando as variâncias diferentes os resultados mostram que ao menos uma 
# das médias de ganho de peso se difere com relação ao tipo de alimento consumido 
# pelos ratos.
# Desta forma concluímos que o tipo de alimento dado aos ratos tem relaçào com o ganho de peso.

#ANÁLISE BIVARIADA GANHO DE PESO X QUANTIDADE + TIPO DE ALIMENTO
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

# H0 - As médias de ganho de peso não se diferem com relaçào a quantidade e o tipo de alimento consumido pelos ratos.
# Ha - Ao menos uma das médias de ganho de peso se difere com relaçào a quantidade e o tipo de alimento consumido pelos ratos.

waov <-  welch_anova_test(Ganho.De.Peso ~ (Quantidade + Tipo.De.Alimento), data = data_ratfeed)
waov


#CONCLUSÃO
# Os resultados mostram que ao menos uma das médias de ganho de peso se difere 
# com relação a quantidade e tipo de alimento consumido pelos ratos.
# Desta forma concluímos que a quantidade e o tipo de alimento 
# dado aos ratos tem relaçào com o ganho de peso.


#--------------------------------------------------------------------------------#
# Gordura corporal em pacientes do sexo masculino
# Adaptado de: 
# bodyfat_men.csv
#--------------------------------------------------------------------------------#

# Objetivos: entender se existem relações lineares entre medições de circunfe-
# rência de várias partes do corpo de homens (além de outras informações) com o 
# % de gordura corporal desses indivíduos

# Age (years)
# Weight (lbs)
# Height (inches)
# Abdomen -- Forearm (circunference in cm)
# bodayfat (%)

# Além dos testes individuais, construa também uma regressão linear com todas
# as variávei seguindo a metodologia vista em aula (atente à multicolinearidade
# e significância estatística das variáveis)


path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Inferência Estatística/Trabalho1/'
data_bodyfat <- read.csv(paste(path_arquivos,'bodyfat_men.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_bodyfat <- bodyfat_men
summary(data_bodyfat)

colnames(data_bodyfat)[1] = 'Idade'
colnames(data_bodyfat)[2] = 'Peso'
colnames(data_bodyfat)[3] = 'Altura'
colnames(data_bodyfat)[4] = 'Abdômen'
colnames(data_bodyfat)[5] = 'Quadril'
colnames(data_bodyfat)[6] = 'Pescoço'
colnames(data_bodyfat)[7] = 'Coxa'
colnames(data_bodyfat)[8] = 'Punho'
colnames(data_bodyfat)[9] = 'Bíceps'
colnames(data_bodyfat)[10] = 'Antibraço'
colnames(data_bodyfat)[11] = 'Gordura.Corporal'

#CONVERSÃO DE DADOS
data_bodyfat$Peso = round(as.double(data_bodyfat$Peso * 0.453592),2)
data_bodyfat$Altura = round(as.double(data_bodyfat$Altura * 2.54),2)

#EXCLUSÃO DE OUTLIERS
data_bodyfat <- data_bodyfat %>% subset(Altura > 75.00)

#r1 <- lm(Gordura.Corporal ~ (Idade  + Altura + Pescoço + Punho + Bíceps + Antibraço), data = data_bodyfat)
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
#FORAM OBSERVADAS MULTICOLINARIEDADES DO ALVO COM RELAÇÃO AS VARIÁVEIS:
#PESO, ABDÔMEN, QUADRIL E COXA.


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

#ABDÔMEN
r1 <- lm(Gordura.Corporal ~ Abdômen, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Abdômen, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Abdômen (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Abdômen, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

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

#PESCOÇO
r1 <- lm(Gordura.Corporal ~ Pescoço, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Pescoço, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Pescoço (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Pescoço, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

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

#BÍCEPS
r1 <- lm(Gordura.Corporal ~ Bíceps, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Bíceps, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Bíceps (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Bíceps, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)

#ABNTIBRAÇO
r1 <- lm(Gordura.Corporal ~ Antibraço, data = data_bodyfat)
summary(r1)

par(las=1)
plot(data_bodyfat$Antibraço, data_bodyfat$Gordura.Corporal,
     main = '', 
     xlab = 'Antibraço (cm)', 
     ylab = '% Gordura Corporal', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue') 
abline(lm(Gordura.Corporal ~ Antibraço, data = data_bodyfat), col = 'red', lwd = 2, lty = 2)



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

