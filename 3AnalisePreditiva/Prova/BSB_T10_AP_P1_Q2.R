# QUESTAO 2 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
qq=BSB_T10_AP_P1[,-1]
summary(qq)
str(qq)
# ANALISAR STATUS
w=table(qq$STATUS,useNA = 'ifany');w
#bom mau 
#739 709
if (!require("gmodels")) install.packages("gmodels")
library(gmodels)
CrossTable(qq$STATUS)

# ANALISAR AS VARIAVEIS SEMPRE 

# VAMOS RODAR A REGRESSAO LOGISTICA
names(qq)
qq$target=ifelse(qq$STATUS=="mau",1,0)  # transformar em binário  ## EVENTO RESPOSTA MAU 
qq$STATUS=NULL  ## elimina a variavel STATUS que foi transformada em numerica com o nome target
names(qq)
fit=glm(data = qq, target~., family = binomial()) ## a regressão classifica 
summary(fit)

#NÃO SELECIONAR AS VARIAVEIS 

qq$TESTE=as.numeric(qq$TESTE)
# ESTIMAR A PROBABILIDADE DE A1 SER MAU 
##previsao A1
A1=data.frame(IDADE='25a45',ECIV='casado',DIST_EMP='média',
              TIPORESID='propria', PRIM_EMP='não',TESTE=82)

A1mau=predict(fit, newdata = A1, type = 'response') ## o predict dá a probabilidade
A1mau
#0.338

# ESTIMAR PROBABILIDADE DO INDIVIDUO
Individuo=data.frame(IDADE='25a45',ECIV='solt',DIST_EMP='média',
              TIPORESID='propria', PRIM_EMP='sim',TESTE=76)

#PD : probabilidade do INDIVIDUO SER MAU
PD.individuo=predict(fit, newdata = Individuo, type = 'response') ## o predict dá a probabilidade
PD.individuo
#0.687

# devolvelndo Status
qq=BSB_T10_AP_P1[,-1]
summary(qq)

# ANALISAR E VALIDAR O MODELO  utilizar amostra qq - #PD  quanto maior, maior a prob de ser mau
qq$PD=predict(fit, newdata = qq,type = 'response')


# PONTO DE CORTE = 0.50 = SE > 0.50 CLASSIFICAO MAU 
K50=ifelse(qq$PD>.50, "mau", "bom")
#matriz de classificaÃ§Ã£o
CrossTable(K50, qq$STATUS, prop.t = F, prop.chisq = F)
acc50=(458+506)/1448;acc50
#0.6657
# taxa de erro
0.6657459 - 1
#0.3342541

library(hmeasure)
HMeasure(qq$STATUS, qq$PD)$metric

library(MLmetrics)
AUC(qq$PD, qq$STATUS) 


#RANDON FOREST  - nao divir as amostras
str(qq)
#RF requer que todas as quali sejam FACTOR

qq$STATUS=as.factor(qq$STATUS)
qq$IDADE=as.factor(qq$IDADE)
qq$ECIV=as.factor(qq$ECIV)
qq$DIST_EMP=as.factor(qq$DIST_EMP)
qq$TIPORESID=as.factor(qq$TIPORESID)
qq$PRIM_EMP=as.factor(qq$PRIM_EMP)

qq$PD=NULL

summary(qq)


# Rode o algoritmo randomForest para estimar a probabilidade de ser mau de um indivíduo, 
#considerando B=400 (número de árvores)  e set.seed(789) e  4 variáveis para cada partição. 
#Se Vc achar que necessita outros hiper parâmetros adote-os e especifique-os 
#a seguir:________________________________

names(qq)
install.packages("randomForest")
library(randomForest)
set.seed(789)
RF=randomForest(data=qq, STATUS~.,mtry=4,ntree=400,importance=T)
# numero de variaveis 4 dado no ex. de outra forma seria (default mtry=raisz(12) / ntree=400
RF
prev.RF=predict(RF, newdata=qq, type="prob")
head(prev.RF)
qq$pmau=prev.RF[,2]
library(hmeasure)

HMeasure(qq$STATUS, prev.RF[,2])$metric[[3]] ##Calcular AUROC
varImpPlot(RF)

plot(RF);grid(col=3)



# B4) Qual a  probabilidade estimada de que o indivíduo A1 seja ???mau??? ? 
# Resp:____________________ (3 casas decimais) 

novo=data.frame(IDADE='25a45',ECIV='casado',DIST_EMP='média',
                TIPORESID='propria', PRIM_EMP="não", TESTE=82)
library(dplyr)
novo=novo %>% mutate_if(is.character,as.factor)

levels(novo$IDADE) <- levels(qq$IDADE)
levels(novo$ECIV) <- levels(qq$ECIV)
levels(novo$DIST_EMP) <- levels(qq$DIST_EMP)
levels(novo$TIPORESID) <- levels(qq$TIPORESID)
levels(novo$PRIM_EMP) <- levels(qq$PRIM_EMP)
levels(novo$TESTE) <- levels(qq$TESTE)


novo$IDADE[1]="25a45"
novo$ECIV[1]="casado"
novo$DIST_EMP[1]="média"
novo$TIPORESID[1]="propria"
novo$PRIM_EMP[1]="não"
novo$TESTE[1]="82"
RFA1=predict(RF, newdata =novo, type = 'prob')
RFA1








