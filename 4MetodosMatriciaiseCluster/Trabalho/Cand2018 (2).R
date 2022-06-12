#DISCIPLINA: BANCO DE DADOS E VISUALIZAÇÃO
#TRABALHO 1
#MEMBROS: ANA PAULA, FÁBIO MONTEIRO, LUCAS SENA E MARCOS SOARES
rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(stringr)
library(dplyr)
library(NbClust)
library(cluster)
library(readxl)
library(gmodels)
library(fpc)


path_arquivo <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Métodos Matriciais e Análise de Clusters/Trabalho/consulta_cand_2018_BRASIL.xls'
consulta_cand_2018_BRASIL <- read_excel(path = path_arquivo)

consulta_cand_2018_BRASIL <- read_excel("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Métodos Matriciais e Clusters/Trabalho/consulta_cand_2018_BRASIL.xls")
cand=consulta_cand_2018_BRASIL

cand$DS_GRAU_INSTRUCAO = ifelse(cand$DS_GRAU_INSTRUCAO=='ENSINO M<c9>DIO COMPLETO', 'ENSINO MÉDIO COMPLETO',ifelse(cand$DS_GRAU_INSTRUCAO=='L\xca E ESCREVE','LÊ E ESCREVE',cand$DS_GRAU_INSTRUCAO))

head(cand)
glimpse(cand)

##########################ANALISE VARIAVEIS QUALITATIVAS ##########################333
unique(cand$TP_ABRANGENCIA)
cand<-cand %>% filter(cand$TP_ABRANGENCIA == 'ESTADUAL') ## Retirada da abrangência FEDERAL
cand <- cand %>% filter(cand$SG_UF %in% c('BA', 'SP'))
cand<-cand %>% filter(cand$DS_CARGO %in% c('DEPUTADO ESTADUAL','DEPUTADO FEDERAL')) 
cand<-cand %>% filter(cand$SG_UF_NASCIMENTO %in% c('AC', 'AL', 'AM', 'AP', 
                      'BA', 'CE', 'DF', 'ES', 'GO',
                      'MA','MG', 'MS', 'MT', 'PA', 
                      'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO',
                      'RR','RS', 'SC', 'SE', 'SP', 'TO'))
## ELIMINAR VARS 
cand$...11=NULL
cand$...12=NULL
cand$...13=NULL
cand$TP_ABRANGENCIA = NULL
cand$NR_IDADE_DATA_POSSE = NULL

#VARIÁVEIS DRIVERS IDENTIFICADAS
CrossTable(cand$DS_SITUACAO_CANDIDATURA, useNA = 'ifany')
CrossTable(cand$DS_SITUACAO_CANDIDATURA, cand$SG_UF, useNA = 'ifany')


CrossTable(cand$TP_AGREMIACAO, useNA = 'ifany') ### 
CrossTable(cand$TP_AGREMIACAO, cand$SG_UF, useNA = 'ifany')

CrossTable(cand$ST_REELEICAO, useNA = 'ifany')
›CrossTable(cand$ST_REELEICAO, cand$DS_CARGO, useNA = 'ifany')

CrossTable(cand$ST_DECLARAR_BENS, useNA = 'ifany')
CrossTable(cand$ST_DECLARAR_BENS, cand$DS_SITUACAO_CANDIDATURA,useNA = 'ifany')

CrossTable(cand$SG_UF, useNA = 'ifany')

CrossTable(cand$DS_CARGO, useNA = 'ifany')
CrossTable(cand$DS_CARGO, cand$SG_UF, useNA = 'ifany')


CrossTable(cand$SG_UF_NASCIMENTO, useNA = 'ifany')
CrossTable(cand$SG_UF_NASCIMENTO, cand$SG_UF, useNA = 'ifany')

CrossTable(cand$DS_GRAU_INSTRUCAO, useNA = 'ifany')

CrossTable(cand$DS_COR_RACA, useNA = 'ifany')
CrossTable(cand$DS_COR_RACA, cand$DS_SITUACAO_CANDIDATURA,  useNA = 'ifany')
CrossTable(cand$DS_COR_RACA, cand$SG_UF,useNA = 'ifany')

#VARIÁVEIS DISCRIMINADORAS IDENTIFICADAS
CrossTable(cand$DS_ESTADO_CIVIL, useNA = 'ifany')

CrossTable(cand$DS_GENERO, useNA = 'ifany')
CrossTable(cand$DS_GENERO,cand$DS_SITUACAO_CANDIDATURA, useNA = 'ifany')
CrossTable(cand$DS_GENERO,cand$TP_AGREMIACAO, useNA = 'ifany')
CrossTable(cand$DS_GENERO,cand$SG_UF, useNA = 'ifany')
CrossTable(cand$DS_GENERO,cand$DS_CARGO, useNA = 'ifany')

#TRANSFORMAÇÃO DE VARIÁVEIS QUALI PARA BINÁRIA
# cand$DS_SITUACAO_CANDIDATURA=ifelse(cand$DS_SITUACAO_CANDIDATURA=='APTO',1,0)
# colnames(cand)[3]='APTO'
# cand$TP_AGREMIACAO=ifelse(cand$TP_AGREMIACAO=='PARTIDO ISOLADO',1,0)
# colnames(cand)[4]='PARTIDO_ISOLADO'
# cand$DS_GENERO=ifelse(cand$DS_GENERO=='MASCULINO',0,1)
# colnames(cand)[7]='GENERO'
# cand$ST_REELEICAO=as.numeric(ifelse(cand$ST_REELEICAO=='SIM',1,0))
# colnames(cand)[11]='REELEITO'
# cand$ST_DECLARAR_BENS=ifelse(cand$ST_DECLARAR_BENS=='SIM',1,0)
# colnames(cand)[12]=' DECLAROU_BENS'

#FATORIZAR
cand$SG_UF=as.factor(cand$SG_UF)
cand$DS_CARGO=as.factor(cand$DS_CARGO)
cand$DS_SITUACAO_CANDIDATURA=as.factor(cand$DS_SITUACAO_CANDIDATURA)
cand$TP_AGREMIACAO=as.factor(cand$TP_AGREMIACAO)
cand$SG_UF_NASCIMENTO=as.factor(cand$SG_UF_NASCIMENTO)
cand$DS_GENERO=as.factor(cand$DS_GENERO)
cand$DS_GRAU_INSTRUCAO=as.factor(cand$DS_GRAU_INSTRUCAO)
cand$DS_ESTADO_CIVIL=as.factor(cand$DS_ESTADO_CIVIL)
cand$DS_COR_RACA=as.factor(cand$DS_COR_RACA)
cand$ST_REELEICAO=as.factor(cand$ST_REELEICAO)
cand$ST_DECLARAR_BENS=as.factor(cand$ST_DECLARAR_BENS)

sapply(cand, class)

#CLUSTERIZANDO

#mix de vars --> calcular distancia usando daisy
cand.dist=daisy(cand) #matriz de distâncias (Gower)

# com mix de variáveis a padronização é feita pela função daisy
hc2=hclust(cand.dist, method ='ward.D2')

plot(hc2, hang = -1, labels = F, lwd=2, main="aplicação 2") 
#dendrograma sugere 2 ou 3 clusters. 

#vamos analisar a solução com 3 clusters
cand$hc2=cutree(hc2,2)

#########Variáveis drivers#################
CrossTable(cand$DS_SITUACAO_CANDIDATURA, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$TP_AGREMIACAO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$ST_REELEICAO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$ST_DECLARAR_BENS, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$SG_UF, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_CARGO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$SG_UF_NASCIMENTO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_GRAU_INSTRUCAO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_COR_RACA, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_GENERO, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)

######Variaveis descritivas ###########
CrossTable(cand$DS_ESTADO_CIVIL, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_COR_RACA, cand$hc2, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)


###################################Método K-Medoid#####################################################

set.seed(10) # para geração da partição inicial
kmd=pamk(cand.dist, k=2:5, criterion = "asw", critout = TRUE) #PARTITION AROUND MEDOID PIECES #SELECIONA POR CRITÉRIOS INTERNOS
kmd$nc #número de clusters selecionados
cand$kmd=kmd$pamobject$clustering #IDENTIFICA OS CLUSTEEA
kmd$pamobject$medoids  #identifica os medoids

CrossTable(cand$DS_SITUACAO_CANDIDATURA, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$ST_DECLARAR_BENS, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)

CrossTable(cand$SG_UF, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_CARGO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$TP_AGREMIACAO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$ST_REELEICAO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$ST_DECLARAR_BENS, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$SG_UF_NASCIMENTO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_GRAU_INSTRUCAO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_COR_RACA, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)

CrossTable(cand$DS_ESTADO_CIVIL, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)
CrossTable(cand$DS_GENERO, cand$kmd, prop.c = F, prop.chisq = F, prop.t = F, prop.r = T)


#################Comparaçao HC2 e KMD #####################################

CrossTable(cand$hc2, cand$kmd,prop.c = F, prop.chisq = F, prop.t = F, prop.r = F)
 

##############Cruzar os resultados obtidos pelo dois tipos de clusters e calcular adjusted Rand 
##############index da library fpc (vide slides).##############################

cs=cluster.stats(d=cand.dist, cand$hc2, cand$kmd, silhouette = T)
cs$dunn  #relativo à primeira solução utilizada: cand$
cs$avg.silwidth #relativo à primeira solução utilizada: un$kmd

cs$corrected.rand #compar as duas  un$kmd e un$hc2





