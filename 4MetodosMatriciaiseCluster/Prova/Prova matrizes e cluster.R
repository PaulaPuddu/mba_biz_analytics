#DISCIPLINA: Análise de Cluster , Analise de Componentes Principais, Matrizes e Regras de Associação
#Prova Individual

rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(stringr)
library(dplyr)
library(NbClust)
library(cluster)
library(readxl)
library(gmodels)
library(fpc)

BSB_T10_MMAC_P1_21_11_03 <- read_excel("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics
                                       and Big data /0Métodos Matriciais e Clusters/Prova/BSB T10 MMAC P1 21 11 03.xlsx", 
                                       sheet = "ECONOMIA")
economia=BSB_T10_MMAC_P1_21_11_03
names(economia)
glimpse(economia)
summary(economia)
names(economia)
####################Análise das variáveis######################
par(mfrow=c(1,2)) ## colocar 2 graficos um ao lado do outro
boxplot(economia$inflacao  , main= 'inflacao')
hist (economia$inflacao)

par(mfrow=c(1,2)) ## colocar 2 graficos um ao lado do outro
boxplot(economia$desemprego  , main= 'desemprego')
hist (economia$desemprego, main='desemprego')

par(mfrow=c(1,2)) ## colocar 2 graficos um ao lado do outro
boxplot(economia$PIB  , main= 'PIB')
hist (economia$PIB, main='PIB')

par(mfrow=c(1,2)) ## colocar 2 graficos um ao lado do outro
boxplot(economia$reservas  , main= 'reservas')
hist (economia$reservas, main='reservas')

par(mfrow=c(1,2)) ## colocar 2 graficos um ao lado do outro
boxplot(economia$juros  , main= 'juros')
hist (economia$juros, main='juros')

###################################################################################################################

#Questão 1a) Padronizar / qual a correlação entre desemprego e inflação?
names(economia)
economia$ID=NULL
economia.numeric=economia[,sapply(economia,is.numeric)] ## somente as variáveis quantitativas
names(economia.numeric)
economia.num.padr=scale(economia.numeric) ### padronização
head(economia.num.padr)
#Qual a correlação entre desemprego e inflaçao?
round(cor(economia.muneric),3) ## correlaçao entre vars
# resp 0.322

####################################################################################################################

#Questão 1b) Matriz de distancias - função dist / método ward K=3   clusters
#Mostre quantos elementos tem cada cluster que Você determinou.
#Dê o número de elementos que cada cluster contem
#Cluster 1  elementos= 10   Cluster 2  elementos= 2     
#Cluster 3  elementos= 1
economia.num.dist=dist(economia.num.padr) ## calculo matriz de distâncias 
hc1=hclust(economia.num.dist, method = 'ward.D2') 
plot(hc1, hand= -1, lables = F, lwd=2, main = 'aplicação1');grid(col = 4)
abline(h=15, col = 'red', lty=2, lwd=2)

economia$hc1=cutree(hc1, 3) # Solução com 3 clusters 
table(economia$hc1) # tabela mostra quantos elementos em cada cluster

####################################################################################################################

#1.c) Quais os países  que compõem o menor cluster obtido pelo método de Ward.D2 
#(em caso de empate, selecione um deles. Diga qual.)? 
economia$País[economia$hc1 == 1]
economia$País[economia$hc1 == 2]
economia$País[economia$hc1 == 3]

#####################################################################################################################

#1.d) Quais as médias (valores não padronizados) da variável juros 
#nos diferentes clusters?  . 
#Cluster 1  =________   Cluster 2  =________  Cluster 3  =________    (2 casas decimais) 

install.packages("dplyr")
library(dplyr)
economia %>% group_by(cluster = economia$hc1) %>% 
  summarise(media = mean(juros))

#####################################################################################################################

#1.e) Com auxílio de boxplots compare os clusters.  
#Qual apresenta maiores valores de PIB, sem considerar eventuais oultiers?   
#(não cole a figura)
#Maiores valores de PIB  cluster= 1. 
#Quantos  paises há neste cluster? "Brasil" RESPOSTA 10

boxplot(economia$PIB ~ economia$hc1, main='PIB', col=topo.colors(3))
economia$País[economia$hc1 == 1]

####################################################################################################################

#1.f) Utilizando o método k medoids, determine o melhor número de clusters sugerido 
#pela função pamk fazendo o k variar de 2 a 4 e adotando como critério 
#a medida “asw”. Utilize a matriz de distâncias calculada com a função dist. 
#Rode utilizando o set.seed(123) e diga quantos clusters encontrou.  
#Número de clusters com  set.seed(123) 2
#Tamanhos dos clusters obtidos:12 e 1

set.seed(123) # para geração da partição inicial
kmd=pamk(economia.num.dist, k=2:4, criterion = "asw", critout = TRUE) #PARTITION AROUND MEDOID PIECES #SELECIONA POR CRITÉRIOS INTERNOS
kmd$nc #número de clusters selecionados
economia$kmd=kmd$pamobject$clustering #IDENTIFICA OS CLUSTERS
kmd$pamobject$medoids  #identifica os medoids

cs=cluster.stats(d=economia.num.dist, economia$kmd, silhouette = T)
cs$dunn  #relativo à primeira solução utilizada: economia$kmd
cs$avg.silwidth #relativo à primeira solução utilizada: un$kmd

d1 <- group_by(.data = economia, kmd) %>% summarise(count=n())
d1 

#######################COMPONENTES PRINCIPAIS#######################################################################

#Determine  as componentes principais das variáveis quantitativas após padronizá-las (utilizando a opção scale.=T).
#Selecione o menor número de componentes que expliquem pelo menos 90% da variância total. 

#2.a) Quantas componentes principais Você selecionou?  Resp:2
qq=economia[,-1] ## eliminar var quali
qq=qq[,-c(6,7)] ## eliminar vars quali

# importante analisar s correla??es
round(cor(qq),2) ## 2 é o arrendondamento das casas decimais

####################################################################################################################

options(scipen=999) ## altera a anota?ao cientifica 

#Passo 1 - determinar as PC - componentes principais 
#sempre padronizar
pc=prcomp(qq, scale. = T)# ele padroniza as variaveis 

head (qq)
pc$x # MATRIZ COM as 5 colunas representando as CP
#mean(pc$x[,1]) ## primeira coluna / para a prova nao precisa calcular a média
var(pc$x[,1])  # --> 2,76
#mean(pc$x[,2])
var(pc$x[,2])  # --> 1,07
var(pc$x[,3])  # --> 0,89
var(pc$x[,4])  # --> 0,22
var(pc$x[,5])  # --> 0,06
plot(pc, ylim=c(0,5), col=18, main="variancias das CP");grid(col=4) #eixo vertical as variancias das CP

#Passo 2 - regras para determinar com qtas CP ficar BASEADAS NA VARIANCIA
abline(h=1, col=4, lwd=4) # sugere 2 CP
abline(h=.90, col=4, lwd=4)  # sugere 2 CP
#as duas regras sugerem 2 CP; confirmamos com 90% explicado, seja com duas variaveis
# 90% da info sao retidas

##################################################################################################################

#2.b) Qual a proporção da variância explicada por essas componentes? 
#Resp:  0,76 (em porcentagem, com 2 casas decimais) 
#( A variância da 3ª componente deu 0.8893032 O que daria 3 componente  = 0.94 de retenção de info)

summary(pc) ##Proporçao das variaveis
#as duas regras sugerem 2 CP; confirmamos com 90% explicado, seja com duas variaveis
# 90% da info sao retidas
#                         PC1    PC2    PC3     PC4     PC5
#Standard deviation     1.6608 1.0350 0.9430 0.46647 0.25209
#Proportion of Variance 0.5516 0.2143 0.1779 0.04352 0.01271
#Cumulative Proportion  0.5516 0.7659 0.9438 0.98729 1.00000

###################################################################################################################

#2.c) Qual a correlação da variável juros com a terceira componente principal? 
#Resp: ________________________ (3 casas decimais) 

#como interpretar as CP?

trescomp=pc$x[,1:3] #ela contem as 5 CPs; mas nos vamos trabalhar só com as duas primeiras
trescomp

#Icalcular as correlações entre as vars originais e as duas CP  
round(cor(qq, trescomp),3)

####################################MATRIZ DE CORRELAÇAO########################################################

#2.d) Seja R matriz de correlações das variáveis quantitativas. Determine a matriz inversa de R.  
#Qual o primeiro elemento (1ª linha, 1ª coluna)  da diagonal principal da matriz inversa?
#Resp: 10,0465 (4 casas decimais) 
economia
A=economia[,2:6] # somente as quantitativas
names(A)
class(A)
dim(A)
#correlação entre essas vars 
R=cor(A);R # em geral utiliza-se R para denotar a matriz de correlação
round(cor(A),4) # 4 casas decimais
dim(R)
round(solve(R),4)
###################################################################################################################

#2.e) Qual o maior auto valor da matriz de correlações R? 
#Resp: 2,758 (3 casas decimais) 

A=economia[,2:6]; A=as.matrix(A)
R=cor(A)  
round(cor(A),3)

L=eigen(R)  #L : objeto que contem os eigenvalues(autovalor) e eigevectors (autovetor) de R
L$vectors
v1=L$vectors[,1];v1
v2=L$vectors[,2];v2
v3=L$vectors[,3];v3
R%*%v1 # multiplicando uma matriz pelo primeiro autovetor
L$values

options(scipen=999)
t(v1)%*%v2  #autovetores são ortogonais 

######################################################################################################################

#2.f) Qual a correlação entre o 1º e o 3º autovetor de R? Resp: -0.07 (2 casas decimais)

#relação com CP ( Componentes principais)
A.pad=scale(A)
pc=prcomp(A.pad)

quemsera=A.pad%*%L$vectors
head(quemsera,10)
head(pc$x ,10) #10 primeiros elementos de PC1
#   scale(A)%*%L$vectors correspondem às CP!!!
#   os componentes dos autovetores são os coeficientes (wij nos slides)  que geram as CP

cor(v1,v3)

####################################################################################################################

#Questão 3: Considere a seguinte matriz binária de transações (consta do arquivo em Excel da prova como MKT).
#Para facilitar a leitura, colei apenas os valores iguais a 1 (No Excel da prova a matriz está completa)
####################################################################################################################

#3.a) calcule (utilizando as fórmulas)  o suporte, a confiança e o lift para e Re seguinte 
#{ leite, fraldas}  {cerveja}  (dê as respostas com  2 casas decimais)
#Suporte=_________    confiança=_____________ lift=______________ 


mkt=BSB_T10_MMAC_P1_21_11_03[,2:7]
names(mkt)
head(mkt,10)
mkt=mkt[rowSums(mkt)>0,] #eliminar as linhas que só tem zero
which(rowSums(mkt)==0) #verificação
head(mkt)

#criando uma matriz binária indicando se comprou 
#ou não (sem considerar quantos comprou)
mkt2=ifelse(mkt>0,1,0)
head(mkt2)

#antes de usar a base de dados devemos transforma-la em "matrix'
class(mkt2) #já é matriz, nao preciso trasformar em matriz
#e se nao fosse, só para ilustrar
#mkt=as.matrix(mkt2)  # só para vcs entenderem como faríamos
#class(mkt.mat)



is=apriori(data = mkt.tr, 
           parameter = list(supp=.10,conf=0.7, minlen=2, 
                            maxlen=4, maxtime=25, target="frequent itemsets" ))
inspect(sort(is, by="supp"))



####################################################################################################################

#3.b) utilize o pacote arules para determinar as regras de associação com suporte superior a 10% 
#e confiança superior a 70%. Considere apenas regras com 2 a 4 itens. Não esqueça de eliminar a coluna 1 (TID).  
#Quantas regras foram geradas? Resp: 29 
# gerando a matriz de transações para pode rodar o algoritmo
library(arules)
mkt.tr=as(mkt2,"transactions")  #gera um objeto para trabalho do arules
# comando inspopect faz a mesma coisa que o View ou head (+-)
inspect(mkt.tr[1:14]) # mostra os ITEMSETS
summary(mkt.tr)

#analisando os produtos graficamente
itemFrequency(mkt.tr) # suporte de cada item 
itemFrequencyPlot(mkt.tr, col=topo.colors(9)) #não ordena
itemFrequencyPlot(mkt.tr,topN=10, main="item support",ylim=c(0,.6), col=topo.colors(10));grid(col=3)
abline(h=.1,col=2, lwd=4)  #suporte mínimo de 10%, por exemplo
abline(h=.05,col=18, lwd=4)  #suporte mínimo de 10%, por exemplo

#geração das regras
rules=apriori(data = mkt.tr, 
              parameter = list(supp=.10,conf=0.7, #escolha arbitrária
                               minlen=2, maxlen=4, target="rules"))
#maxlen: an integer value for the maximal number of items per item set (default: 10 items)
#maxtime: time limit in seconds for checking subsets. maxtime=0 disables the time limit. (default: 5 seconds)
rules #dá o número de regras
inspect(sort(rules, by="conf")) #ordenado pela conf, coverage é o suporte do LHS
#Coverage (also called cover or LHS-support) is the support of the left-hand-side of the rule, i.e.,
#It represents a measure of to how often the rule can be applied.
####################################################################################################################

#3.c) verifique no output  o suporte, a confiança e o lift para a regra de associação seguinte    
#{FRALDAS,PÃO,QUEIJO}   => {LEITE}
#Suporte=0,214    confiança= 0,750 lift= 1,167  (3 casas decimais)

####################################################################################################################

#3.d) Eliminando as regras redundantes, quantas regras não redundantes sobraram? 
#Resp: 12. Das regras de associação não redundantes, qual a que possui menor lift?
#Dê a confiança dela.

#(escreva a regra utilizado a notação usual {QUEIJO} {LEITE} ) conf= 0,714  LIFT = 1,111

#obtendo regras redundantes
# A rule is more general if it has the same RHS but one or more items removed from the LHS ("menorLHS").
#A rule is redundant if a more general rules with the same or a higher confidence exists.
#A more specific rule is redundant if it is only equally or even less predictive than a more general rule
# a rule X -> Y is redundant if for some X' (subset of) X, conf(X' -> Y) >= conf(X -> Y)

inspect(rules[is.redundant(rules)]) # output em branco se nao houver regras redundantes
rules.pruned=rules[!is.redundant(rules, measure="confidence")]
rules.pruned
inspect(sort(rules.pruned, by="conf"))

#####################################################################################################################

