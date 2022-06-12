## FGV-Management - 1S2022
## Análise de Mídias Sociais e Mineração de Texto - Tarefa 1 e 2
## Prof. Eduardo de Rezende Francisco
## Entrega : 11/05/2022

##########################################################################################################

#                                    REDE ONE MODE

##########################################################################################################
# QUESTÕES BASE

# •	Utilizando a Rede One Mode do arquivo Rede One Mode_Tarefa Aulas 1 e 2_Brasília T10.xlsx 
# descreva sua estrutura de componentes, nós, arestas, centralidades, cliques, diâmetro, 
#distância média e densidade. Faça pequenas modificações na tabela e veja seus resultados.

#•	Inclua outras análises em seu código (usando as extensões sna, network ou igraph) e 
#comente os resultados (seja criativo!).

# BAIXANDO LIBS
install.packages("network")
library(network)
install.packages("sna")
library(sna)
install.packages("rgl")
library(rgl)
install.packages("igraph")
library(igraph)

# CARREGANDO OS DADOS REDE ONE MODE 
rede1 <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Analise de Midias Sociais_Mineracao_texto/Tarefas1e2/rede1.csv", row.names=1, sep=";")
View(rede1)

# ADAPTANDO O DATA FRAME PARA QUE POSSA SERVIR PARA MONTAGEM DA REDE
#Nao rodei estes scrips . Salvei arquivo xls em csv e na importação adaptei os dados

#grede <- rede[,2:34]
#rownames(grede) <- rede[,1]

# QUANTIDADE DE NÓS E ARESTAS
graph.data.frame(rede1)
graph_from_incidence_matrix(rede1)

# CONSTRUINDO A REDE A PARTIR DA MATRIZ DE RELAÇÕES (0 e 1)
gplot(rede1)
gplot(rede1,gmode="graph",displaylabels = TRUE)
gplot(rede1,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)

# MEDIDA DE CENTRALIDADE  
degree(rede1, gmode = "graph",cmode = "indegree")
closeness(rede1,gmode="graph") # qto maior , mais proximo
betweenness(rede1,gmode="graph") # grau de intermediação , Mais alto , Maior o grau de interm.

## Aprimorando a Rede 
gplot(rede1,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=degree(rede,gmode="graph",cmode="indegree")/3)

gplot(rede1,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=degree(grede,gmode="graph",cmode="indegree"))

gplot(rede1,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=closeness(grede,gmode="graph")*2)

gplot(rede1,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=round(closeness(grede,gmode="graph"),digits=2))

gplot(rede1,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=betweenness(grede,gmode="graph")/3+1)

gplot(rede,gmode="rede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=betweenness(grede,gmode="graph"))

# DENSIDADE DA REDE
gden(rede1, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
# 0.5056818 - densidade alta

# EFICIÊNCIA DA REDE 
efficiency(rede1, g=NULL, diag=FALSE)
#.5097656 - eficiência alta 

connectedness(rede1)
# 1 componente

mutuality(rede) # ??? Não entendi o significado 

# CLIQUES 
clique.census(rede1, mode = "graph", tabulate.by.vertex = FALSE,
              clique.comembership = "sum", enumerate = FALSE,
              na.omit = TRUE)

clique.census(rede1, mode = "graph", tabulate.by.vertex = TRUE,
              clique.comembership = "none", enumerate = FALSE,
              na.omit = TRUE)

clique.census(rede, mode = "graph", tabulate.by.vertex = TRUE,
              clique.comembership = "bysize", enumerate = FALSE,
              na.omit = TRUE)

# MATRIS DE DISTÂNCIA GEODÉSICA 
dg3 <- geodist(rede1)
dg3
dg3$counts # Numero de caminhos 
dg3$gdist # DISTANCIA  distancia 
# VEJA O QUE TEM A MAIOR DISTANCIA NA TABELA GDIST E NA TABELA COUNTS VEJA QUANTOS CAMINHOS


#########################################################################################################

##                                          REDE TWO MODES

#########################################################################################################

#•	Utilizando a Rede Two Mode do arquivo Rede Two Mode_Tarefa Aulas 1 e 2_Brasília T10.xlsx descreva 
#sua estrutura de componentes, nós, arestas, centralidades. 
#Faça pequenas modificações na tabela e veja seus resultados.

# CARREGANDO OS DADOS
rede2 <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Analise de Midias Sociais_Mineracao_texto/Tarefas1e2/rede2.csv", row.names=1, sep=";")
View(rede2)

# QUANTIDADE DE NÓS E LAÇOS 
graph.data.frame(rede2)
graph_from_incidence_matrix(rede2)

# CONSTRUINDO A REDE A PARTIR DA MATRIZ DE RELAÇÕES (0 e 1)
gplot(rede2)
gplot(rede2,gmode="twomode",displaylabels = TRUE)
gplot(rede2,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE)

# MEDIDAS DE CENTRALIDADE
degree(rede2,gmode="twomode",cmode="indegree")
closeness(rede2,gmode="twomode")
betweenness(rede2,gmode="twomode")

# APRIMORANDO A REPRESENTAÇÃO DA REDE 
gplot(rede2,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(rede2,gmode="twomode")*3)

# DENSIDADE DA REDE
gden(rede2, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE)
# 0.2637681 - densidade baixa

# EFICIÊNCIA DA REDE 
efficiency(rede2, g=NULL, diag=FALSE)
#0.7525926 - eficiência alta 

connectedness(rede2)
# 1 componente

mutuality(rede2) # ??? Não entendi o significado
# 273

# CLIQUES 
clique.census(rede2, mode = "graph", tabulate.by.vertex = FALSE,
              clique.comembership = "sum", enumerate = FALSE,
              na.omit = TRUE)

clique.census(rede2, mode = "graph", tabulate.by.vertex = TRUE,
              clique.comembership = "none", enumerate = FALSE,
              na.omit = TRUE)

clique.census(rede2, mode = "graph", tabulate.by.vertex = TRUE,
              clique.comembership = "bysize", enumerate = FALSE,
              na.omit = TRUE)

# MATRIX DE DISTÂNCIA GEODÉSICA 
dg3 <- geodist(rede2)
dg3
dg3$counts # Número de caminhos 
dg3$gdist #  distancia 
# VEJA O QUE TEM A MAIOR DISTANCIA NA TABELA GDIST E NA TABELA COUNTS VEJA QUANTOS CAMINHOS

## Como Calcula a distância Média???
## Como acha o Diametro????

# ANÁLISE DE CLUSTERS
# Implementa o algoritmo hierárquico e apresenta o dendrograma
hc <- hclust(dist(rede2), "average")  # explorar com outros métodos de distância
p <- ggdendrogram(hc, rotate=FALSE)
print(p)
ggdendrogram(hc, rotate=TRUE)

hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrograma Análise de Redes")

# "Cortando" a árvore em 3 e 4 grupos
grupos <- cutree(hc,k=3)
grupos
grupos <- cutree(hc,k=4)
grupos

# Analisando as principais variáveis a partir dos grupos

boxplot(rede2$Vacina ~ grupos, col = "blue", main = 'Box Plot Produto: Vacina')
boxplot(rede2$Máscara ~ grupos, col = "blue", main = 'Box Plot Produto: Máscara')
boxplot(rede2$Álcool.Gel ~ grupos, col = "blue", main = 'Box Plot Produto: Álcool-Gel')

# Color nodes by closeness / clusters
?igraph.plotting

