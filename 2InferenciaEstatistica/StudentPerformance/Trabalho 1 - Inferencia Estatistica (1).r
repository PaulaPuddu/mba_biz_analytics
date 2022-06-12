#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# INFERENCIA ESTATISTICA
# TRABALHO INDIVIDUAL
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#ALUNOS:ANA PAULA, DAVI, F�BIO MONTEIRO, LUCAS SENA E  MARCOS SOARES, 

rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(dplyr)
library('rstatix')
data_students=read.table("/home/lucas/Documentos/FGV/Material_LSA/M02_Analise_Exploratoria/M02_TrabalhoFinal/Trabalho1_StudentPerformance/repo/student-mat.csv",sep=";",header=TRUE)
data_students=read.table("/home/lucas/Documentos/FGV/Material_LSA/M02_Analise_Exploratoria/M02_TrabalhoFinal/Trabalho1_StudentPerformance/repo/student-mat.csv",sep=";",header=TRUE)

data_students <- data_students %>% mutate(rendimento = ifelse(data_students$G3 <= 9,'Fail', 
                                            ifelse(data_students$G3 <= 11,'Sufficient', 
                                                    ifelse(data_students$G3 <= 13,'Satisfactory', 
                                                            ifelse(data_students$G3 <= 15,'Good', 
                                                                    ifelse(data_students$G3 <= 20,'Excellent'))))),
                                          media_alcool_semana = (Dalc + Walc) / 2,
                                          consome_alcool_acima_media = ifelse(media_alcool_semana > 2 ,'sim', 'n�o'),
                                          famsize = ifelse(famsize == 'LE3' ,2, 4), #CONVERS�O DA VARI�VEL PARA BIN�RIA NUM�RICA.
                                          )
                         
data_students$rendimento <- factor(data_students$rendimento)
data_students$sex <- factor(data_students$sex)
data_students$schoolsup <- factor(data_students$schoolsup)
#data_students$Medu = factor(data_students$Medu, levels = c(1,2,3,4), labels = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Curso Superior'))
#data_students$Fedu = factor(data_students$Fedu, levels = c(1,2,3,4), labels = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Curso Superior'))
#data_students$Dalc = factor(data_students$Dalc, levels = c(1,2,3,4,5), labels = c('Muito pouco', 'Pouco','Normal','Alto','Muito Alto'))
#data_students$Walc = factor(data_students$Walc, levels = c(1,2,3,4,5), labels = c('Muito pouco', 'Pouco','Normal','Alto','Muito Alto'))
#data_students$famrel = factor(data_students$famrel, levels = c(1,2,3,4,5), labels = c('Muito ruim', 'Ruim','Normal','Bom','Muito Bom'))
data_students$Medu = factor(data_students$Medu)
data_students$Fedu = factor(data_students$Fedu)
data_students$Dalc = factor(data_students$Dalc)
data_students$Walc = factor(data_students$Walc)
data_students$famrel = factor(data_students$famrel)

data_students$Mjob = factor(data_students$Mjob)
data_students$Fjob = factor(data_students$Fjob)
data_students$reason = factor(data_students$reason)
data_students$paid = factor(data_students$paid)
data_students$famsup = factor(data_students$famsup)
data_students$nursery = factor(data_students$nursery)
data_students$higher = factor(data_students$higher)
data_students$internet = factor(data_students$internet)
data_students$romantic = factor(data_students$romantic)
#data_students$freetime = factor(data_students$freetime, levels = c(1,2,3,4,5), labels = c('Muito pouco', 'Pouco','Normal','Alto','Muito Alto'))

summary(data_students)

##########Como � o rendimento geral dos alunos? (distribui��o e descritiva)

vrPorcentagemE <- round((nrow(subset(data_students, rendimento == 'Excellent')) / nrow(data_students))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_students, rendimento == 'Fail')) / nrow(data_students))*100,digits = 2)
vrPorcentagemG <- round((nrow(subset(data_students, rendimento == 'Good')) / nrow(data_students))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_students, rendimento == 'Satisfactory')) / nrow(data_students))*100,digits = 2)
vrPorcentagemU <- round((nrow(subset(data_students, rendimento == 'Sufficient')) / nrow(data_students))*100,digits = 2)
par(mar=c(8,3,4,5))
par(las=1)
barplot(table(data_students$rendimento),
        main = 'Distribui��o de Alunos por Rendimento Escolar',
        ylab = 'Total de Alunos', 
        xlab = '', 
        col = hcl.colors(5, palette = "Blues 3"),
        ylim = c(0,150),
        names.arg = c(str_c('',vrPorcentagemE,'%',sep = ''),
                      str_c('',vrPorcentagemF,'%',sep = ''),
                      str_c('',vrPorcentagemG,'%',sep = ''),
                      str_c('',vrPorcentagemS,'%',sep = ''),
                      str_c('',vrPorcentagemU,'%',sep = '')))
legend("topleft", legend = c('Fail','Sufficient','Satisfactory','Good','Excellent'), fill = hcl.colors(5, palette = "Blues 3"), )

rm(vrPorcentagemE, vrPorcentagemF, vrPorcentagemG, vrPorcentagemS, vrPorcentagemU)

#M�DIA GERAL DA NOTA DE ALUNOS
mean(data_students$G3)

#DISTRIBUI��O GERAL DAS NOTAS DOS ESTUDANTES
par(mar = c(3,4,7,5))
boxplot(data_students$G3,
        col = terrain.colors(2),
        main = 'An�lise Univariada. Desempenho Escolar', 
        cex.main = 0.9,
        ylab = 'Desempenho',
        border = 'gray20')

hist(data_students$G3,
     main = 'Distribui��o de Notas dos Estudantes',
     ylim = c(0,100),
     col = hcl.colors(10, palette = "Red-Blue"))

##########FIM Como � o rendimento geral dos alunos? (distribui��o e descritiva);


#An�lise explorat�ria univariada dos dados (gr�ficos e tabelas dependendo do contexto), 
#como � o panorama geral dos alunos dessas escolas?
#AN�LISE GERAL SOBRE A BASE DE ESTUDANTES

#DISTRIBUI��O DE ALUNOS POR ESCOLA
vrPorcentagemGP <- round((nrow(subset(data_students, school == 'GP')) / nrow(data_students))*100,digits = 2)
vrPorcentagemMS <- round((nrow(subset(data_students, school == 'MS')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$school),
        main = 'Distribui��o de Alunos por Escola',
        ylab = 'Total de Alunos', 
        ylim = c(0,400),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemGP,'%',sep = ''),str_c('',vrPorcentagemMS,'%',sep = '')))
legend("topright", legend = c('Gabriel Pereira','Mousinho da Silveira'), fill = hcl.colors(2, palette = "Peach"))
rm(vrPorcentagemGP,vrPorcentagemMS)

#DISTRIBUI��O DE ALUNOS POR SEXO
vrPorcentagemF <- round((nrow(subset(data_students, sex == 'F')) / nrow(data_students))*100,digits = 2)
vrPorcentagemM <- round((nrow(subset(data_students, sex == 'M')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$sex),
        main = 'Distribui��o de Alunos por Sexo',
        ylab = 'Total de Alunos', 
        xlab = 'Sexo', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "Red-Blue"),
        names.arg = c(str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemM,'%',sep = '')))
legend("topleft", legend = c('Feminino','Masculino'), fill = hcl.colors(2, palette = "Red-Blue"))
rm(vrPorcentagemF, vrPorcentagemM)

#DISTRIBUI��O DE ALUNOS POR ENDERE�O
vrPorcentagemU <- round((nrow(subset(data_students, address == 'U')) / nrow(data_students))*100,digits = 2)
vrPorcentagemR <- round((nrow(subset(data_students, address == 'R')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$address),
     main = 'Distribui��o de Alunos por Endere�o',
     ylab = 'Total de Alunos', 
     xlab = 'Tipo de Endere�o', 
     col = hcl.colors(2, palette = "Blues 3"),
     names.arg = c(str_c('',vrPorcentagemR,'%',sep = ''),str_c('',vrPorcentagemU,'%',sep = '')))
legend("topleft", legend = c('Rural','Urbano'), fill = hcl.colors(2, palette = "Blues 3"))
rm(vrPorcentagemR, vrPorcentagemU)


#DISTRIBUI��O GERAL POR PSTATUS - SITUA��O CONJUGAL DOS PAIS
par(mar = c(3,4,7,5))
vrPorcentagemA <- round((nrow(subset(data_students, Pstatus == 'A')) / nrow(data_students))*100,digits = 2)
vrPorcentagemT <- round((nrow(subset(data_students, Pstatus == 'T')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Pstatus),
        col = hcl.colors(2, palette = 'ag_Sunset'),
        main = 'An�lise Univariada. Situa��o Conjugal dos Pais.', 
        names.arg = c(str_c('',vrPorcentagemA,'%',sep = ''),str_c('',vrPorcentagemT,'%',sep = '')),
        cex.main = 0.9,
        ylab = 'Desempenho',
        xlab = 'Situa��o',
        border = 'gray20')
legend("topleft", legend = c('Separados','Juntos'), fill = hcl.colors(2, palette = 'ag_Sunset'))
rm(vrPorcentagemR, vrPorcentagemU)


#DISTRIBUI��O GERAL DE NIVEL ESCOLAR DAS M�ES
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Medu == 0)) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Medu == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Medu == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Medu == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Medu == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Medu),
        col = hcl.colors(5, palette = 'ag_Sunset'),
        main = 'Distribui��o de N�vel Escolar das M�es.', 
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = ''),str_c('',vrPorcentagem3,'%',sep = ''),str_c('',vrPorcentagem4,'%',sep = ''),str_c('',vrPorcentagem5,'%',sep = '')),
        cex.main = 0.9,
        ylim = c(0,150),
        border = 'gray20')
legend("topleft", legend = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Ensino M�dio','Curso Superior'), fill = hcl.colors(5, palette = 'ag_Sunset'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrPorcentagem5)


#DISTRIBUI��O GERAL DE NIVEL ESCOLAR DOS PAIS
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Fedu == 0)) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Fedu == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Fedu == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Fedu == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Fedu == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Fedu),
        col = hcl.colors(5, palette = 'Vik'),
        main = 'Distribui��o de N�vel Escolar dos Pais', 
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = ''),str_c('',vrPorcentagem3,'%',sep = ''),str_c('',vrPorcentagem4,'%',sep = ''),str_c('',vrPorcentagem5,'%',sep = '')),
        cex.main = 0.9,
        ylim = c(0,180),
        border = 'gray20')
legend("topright", legend = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Ensino M�dio','Curso Superior'), fill = hcl.colors(5, palette = 'Vik'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUI��O GERAL DE CATEGORIA DE PROFISSAO DAS MAES
par(mar = c(7,4,4,2))
vrPorcentagem1 <- round((nrow(subset(data_students, Mjob == 'at_home')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Mjob == 'health')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Mjob == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Mjob == 'services')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Mjob == 'teacher')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Mjob),
        col = hcl.colors(5, palette = 'Lajolla'),
        main = 'Distribui��o de M�es por Profiss�o.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Em casa', 'Sa�de','Outros','Servi�os', 'Professora'), fill = hcl.colors(5, palette = 'Lajolla'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrPorcentagem5)

#DISTRIBUI��O GERAL DE CATEGORIA DE PROFISSAO DOS PAIS
par(mar = c(4,3,8,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Fjob == 'at_home')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Fjob == 'health')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Fjob == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Fjob == 'services')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Fjob == 'teacher')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Fjob),
        col = hcl.colors(5, palette = 'ag_GrnYl'),
        main = 'Distribui��o de Pais por Profiss�o.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Em casa', 'Sa�de','Outros','Servi�os', 'Professor'), fill = hcl.colors(5, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUI��O GERAL DE ALUNOS POR GUARDA PARENTAL
par(las = 1)
par(mar = c(4,6,5,5))
vrPorcentagem1 <- round((nrow(subset(data_students, guardian  == 'mother')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, guardian == 'father')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, guardian == 'other')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$guardian),
        col = hcl.colors(3, palette = 'ag_GrnYl'),
        main = 'Distribui��o de Alunos por Guarda Parental.', 
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = '')),
        ylim = c(0,300),
        border = 'gray20')
legend("topright", legend = c('M�e', 'Pai','Outros'), fill = hcl.colors(3, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3)

#DISTRIBUI��O GERAL DE ALUNOS POR TAMANHO DA FAM�LIA
par(las = 1)
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, famsize  == '2')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famsize == '4')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famsize),
        col = hcl.colors(2, palette = 'ag_GrnYl'),
        main = 'Distribui��o de Alunos por Tamanho da Fam�lia.', 
        ylim = c(0,300),
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('Menos que 4 membros', 'Acima de 3 membros'), fill = hcl.colors(2, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3)


#DISTRIBUI��O GERAL DE ALUNOS MOTIVO DE ESCOLHA DA ESCOLA
par(mar = c(3,4,7,5))
vrLegenda <- c('Curso', 'Perto de Casa','Outros','Reputa��o')
vrPorcentagem1 <- round((nrow(subset(data_students, reason == 'course')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, reason == 'home')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, reason == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, reason == 'reputation')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$reason),
        col = hcl.colors(4, palette = 'Broc'),
        main = 'Distribui��o de Alunos por Motivo de Escolha da Escola.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Broc'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)

#DISTRIBUI��O GERAL DE ALUNOS POR TEMPO DE LOCOMO��O AT� A ESCOLA
par(mar = c(3,4,7,5))
vrLegenda <- c('< 15 min.', '15 a 30 min.', '30 min a 1hr.','> 1hr.')
vrPorcentagem1 <- round((nrow(subset(data_students, traveltime == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, traveltime == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, traveltime == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, traveltime == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$traveltime),
        col = hcl.colors(4, palette = 'Geyser'),
        main = 'Distribui��o de Alunos por Tempo de Locomo��o Casa x Escola.', 
        cex.main = 0.9,
        ylim = c(0,300),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Geyser'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)


#DISTRIBUI��O GERAL DE ALUNOS POR TEMPO DE ESTUDO SEMANAL
par(mar = c(3,4,7,5))
vrLegenda <- c('< 2hr.', 'De 2hrs a 5 hsr.', 'De 5hrs a 10hrs.','> 10hrs.')
vrPorcentagem1 <- round((nrow(subset(data_students, studytime == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, studytime == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, studytime == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, studytime == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$studytime),
        col = hcl.colors(5, palette = 'Batlow'),
        main = 'Distribui��o de Alunos por Tempo de Estudo Semanal.', 
        cex.main = 0.9,
        ylim = c(0,300),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)

#DISTRIBUI��O GERAL DE ALUNOS POR REPROVA��O NA DISCIPLINA
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, failures == 0)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, failures == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, failures == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, failures == 3)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$failures),
        col = hcl.colors(5, palette = 'Batlow'),
        main = 'Distribui��o de Alunos por Quantidade de Reprova��es.', 
        cex.main = 0.9,
        ylim = c(0,380),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Nenhuma.','Uma.','Duas.','Tr�s ou mais.'), fill = hcl.colors(4, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUI��O GERAL DE ALUNOS COM SUPLEMENTO ESCOLAR
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, schoolsup == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, schoolsup == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$schoolsup),
        col = hcl.colors(2, palette = 'Batlow'),
        main = 'Distribui��o de Alunos por Suplemento Escolar.', 
        cex.main = 0.9,
        ylim = c(0,400),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2)


#DISTRIBUI��O GERAL DE ALUNOS COM SUPORTE EDUCACIONAL FAMILIAR - famsup
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, famsup == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famsup == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famsup),
        col = hcl.colors(2, palette = 'Lajolla'),
        main = 'Distribui��o de Alunos por Suporte Educacional Familiar.', 
        cex.main = 0.9,
        ylim = c(0,250),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = 'Lajolla'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUI��O GERAL DE ALUNOS COM CURSOS EXTRAS NA DISCIPLINA
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, paid == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, paid == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$paid),
        col = hcl.colors(2, palette = 'Turku'),
        main = 'Distribui��o de Alunos com Curso Extra na Disciplina.', 
        cex.main = 0.9,
        ylim = c(0,250),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = 'Turku'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUI��O GERAL DE ALUNOS COM ATIVIDADES EXTRA CURRICULARES.
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, activities == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, activities == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$activities),
        col = hcl.colors(2, palette = 'Vik'),
        main = 'Distribui��o de Alunos com Atividades Extra-Curriculares.', 
        cex.main = 0.9,
        ylim = c(0,240),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = 'Vik'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUI��O DE ALUNOS QUE FREQUENTARAM A ESCOLA MATERNAL
vrPorcentagemCN <- round((nrow(subset(data_students, nursery == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSN <- round((nrow(subset(data_students, nursery == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$nursery),
        main = 'Distribui��o de Alunos que Frequentaram o Maternal',
        cex.main = 0.9,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "Zissou 1"),
        names.arg = c(str_c('',vrPorcentagemSN,'%',sep = ''),str_c('',vrPorcentagemCN,'%',sep = '')))
legend("topright", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "Zissou 1"))
rm(vrPorcentagemCN,vrPorcentagemSN)

#DISTRIBUI��O DE ALUNOS QUE QUEREM FAZER CURSO SUPERIOR
vrPorcentagemS <- round((nrow(subset(data_students, higher == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemN <- round((nrow(subset(data_students, higher == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$higher),
        main = 'Distribui��o de Alunos que Desejam Curso Superior',
        cex.main = 0.9,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "OrYel"),
        names.arg = c(str_c('',vrPorcentagemN,'%',sep = ''),str_c('',vrPorcentagemS,'%',sep = '')))
legend("topleft", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "OrYel"))
rm(vrPorcentagemN,vrPorcentagemS)

#DISTRIBUI��O DE ALUNOS COM E SEM ACESSO A INTERNET
vrPorcentagemCI <- round((nrow(subset(data_students, internet == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSI <- round((nrow(subset(data_students, internet == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$internet),
        main = 'Distribui��o de Alunos com Acesso a Internet em Casa',
        cex.main = 1.0,
        ylab = 'Total de Alunos', 
        ylim = c(0,380),
        col = hcl.colors(2, palette = "PiYG"),
        names.arg = c(str_c('',vrPorcentagemSI,'%',sep = ''),str_c('',vrPorcentagemCI,'%',sep = '')))
legend("topleft", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "PiYG"))
rm(vrPorcentagemCI,vrPorcentagemSI)


#DISTRIBUI��O DE ALUNOS QUE S�O OU N�O ROM�NTICOS
vrPorcentagemCI <- round((nrow(subset(data_students, romantic == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSI <- round((nrow(subset(data_students, romantic == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$romantic),
        main = 'Distribui��o de Alunos que s�o Rom�nticos.',
        cex.main = 1.0,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "PiYG"),
        names.arg = c(str_c('',vrPorcentagemSI,'%',sep = ''),str_c('',vrPorcentagemCI,'%',sep = '')))
legend("topleft", legend = c('N�o','Sim'), fill = hcl.colors(2, palette = "PiYG"))
rm(vrPorcentagemCI,vrPorcentagemSI)


#DISTRIBUI��O GERAL DE ALUNOS POR N�VEL DE RELACIONAMENTO FAMILIAR
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito ruim', 'Ruim','Normal','Bom','Muito Bom')
vrPorcentagem1 <- round((nrow(subset(data_students, famrel == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famrel == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, famrel == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, famrel == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, famrel == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famrel),
        col = hcl.colors(5, palette = 'Broc'),
        main = 'Distribui��o de Alunos por N�vel de Relacionamento Familiar.', 
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = vrLegenda, fill = hcl.colors(5, palette = 'Broc'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS POR TEMPO DISPON�VEL
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco', 'Pouco','Normal','Alto','Muito Alto')
vrPorcentagem1 <- round((nrow(subset(data_students, freetime == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, freetime == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, freetime == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, freetime == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, freetime == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$freetime),
        col = hcl.colors(5, palette = 'Temps'),
        main = 'Distribui��o de Alunos por Tempo Livre Duranta a Semana.', 
        cex.main = 0.9,
        ylim = c(0,200),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = vrLegenda, fill = hcl.colors(5, palette = 'Temps'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS QUE SAEM COM AMIGOS
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco', 'Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, goout == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, goout == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, goout == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, goout == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, goout == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$goout),
        col = hcl.colors(5, palette = 'Emrld'),
        main = 'Distribui��o de Alunos que Saem com Amigos.', 
        cex.main = 0.9,
        ylim = c(0,200),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = vrLegenda, fill = hcl.colors(5, palette = 'Emrld'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS POR N�VEL DE CONSUMO DE �LCOOL DURANTE A SEMANA
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco','Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, Dalc == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Dalc == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Dalc == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Dalc == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Dalc == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Dalc),
        col = hcl.colors(5, palette = 'Emrld'),
        main = 'Distribui��o de Alunos por N�vel de Consumo de �lcool Durante a Semana.', 
        cex.main = 0.9,
        ylim = c(0,340),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(5, palette = 'Emrld'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS POR N�VEL DE CONSUMO DE �LCOOL DURANTE O FINAL DE SEMANA
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco','Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, Walc == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Walc == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Walc == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Walc == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Walc == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Walc),
        col = hcl.colors(5, palette = 'Set 3'),
        main = 'Distribui��o de Alunos por N�vel de Consumo de �lcool no Final de Semana.', 
        cex.main = 0.9,
        ylim = c(0,190),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(5, palette = 'Set 3'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS POR SITUA��O ATUAL DE SA�DE
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito ruim','Ruim','Indo','Boa','Super Boa')
vrPorcentagem1 <- round((nrow(subset(data_students, health == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, health == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, health == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, health == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, health == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$health),
        col = hcl.colors(5, palette = 'Oranges'),
        main = 'Distribui��o de Alunos por N�vel de Consumo de �lcool no Final de Semana.', 
        cex.main = 0.9,
        ylim = c(0,170),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = vrLegenda, fill = hcl.colors(5, palette = 'Oranges'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUI��O GERAL DE ALUNOS POR N�MERO DE FALTAS NAS DISCIPLINA

##########FIM AN�LISE GERAL DOS DADOS


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELA��O AO G�NERO

# Diagrama sobre o rendimento x sexo
# H0: A m�dia de desempenho de alunos do g�nero feminino n�o � inferior a m�dia de alunos do sexo masculino.
# Ha: A m�dia de desempenho de alunos do g�nero feminino � inferior a m�dia de alunos do sexo masculino.

par(las=1)
boxplot(G3 ~ sex, data = data_students,
        main = 'Distribui��o de Rendimento Por Sexo', 
        xlab = 'Sexo ', 
        ylab = 'Rendimento', 
        cex.axis = 1.2, 
        cex.lab = 1.2,
        ylim = c(0,20),
        col = terrain.colors(2))


d1 <- group_by(.data = data_students, sex) %>% summarise(n=n(),
                                                         media=mean(G3),
                                                         sd=sd(G3),
                                                         var=var(G3))
d1

var.test(data_students$G3 ~ data_students$sex, data = data_students,  alternative = 'two.sided')
#0.6989. AS VARI�NCIAS N�O S�O DIFERENTES.

t.test(G3 ~ sex, data = data_students, alternative = 'less', var.equal = TRUE)
#P-value = 0.0199

#CONCLUS�O
# Uma vez que p-valor < 5% temos evidencia para rejeitar H0 em favor de Ha
# de modo que alunos do g�nero feminino possuem m�dia inferior do que alunos do g�nero masculino.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELA��O AO G�NERO


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELA��O A SUA IDADE

#TESTE DE REGRESS�O LINEAR SIMPLES 
#H0: N�o h� rela��o estatisticamente significante entre a idade dos alunos e as m�dias escolares.
#Ha: Existe rela��o estatisticamente significante entre a idade dos alunos e as m�dias escolares.

tt1 <- lm(G3 ~ age, data = data_students)
summary(tt1)
#p-value = 0.001
#Estimate Age = -2
#R2 = 0,026

par(mar=c(5,4,2,5))
plot(data_students$age, data_students$G3,
     main = '', cex.main = 1.0,
     xlab = 'IDADE', 
     ylab = 'DESEMPENHO (0 - 20)', cex.axis = 1.2, 
     cex.axis = 1.2, cex.lab = 1.2, pch = 21, cex = 0.8, 
     col = 'dodgerblue4', bg = 'dodgerblue', ylim = c(0,20), xlim = c(15,22), 
     row.names(c('2','s','s')))
abline(tt1, col = 'red', lwd = 2, lty =2)	

# Com o P-value � menor que 5% identificamos uma rela��o linear inversa entre o desempenho dos alunos
# e sua idade, de modo que, acada ano de idade dos alunos, h� uma redu��o de 0.5 d�cimos da nota final.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O TIPO DE ENDERE�O URBANO X RURAL
# Diagrama sobre o RENDIMENTO x LOCAL DE RESID�NCIA
# H0: Alunos que moram na zona Rural possuem m�dia escolar igual ou superior a m�dia escolar de alunos que moram na zona Urbana.
# Ha: Alunos que moram na zona Rural possuem m�dia escolar menor do que a m�dia escolar de alunos que moram na zona Urbana.

par(mar = c(3,4,7,5))
boxplot(G3 ~ address, data = data_students,
        col = terrain.colors(2),
        main = 'An�lise Bivariada. Tipo de Endere�o x Desempenho Escolar', 
        cex.main = 0.9,
        ylab = 'Desempenho',
        xlab = 'Tipo de Endere�o',
        border = 'gray20')

#O ENDERE�O DOS ALUNOS INFLUENCIA EM SEU RENDIMENTO
tt <- table(data_students$address, data_students$rendimento)
ddt <- as.data.frame(tt)
prop.table(tt)
par(mar=c(8,4,3,1))
par(las=2)
barplot(tt, 
        col = terrain.colors(2),
        main = 'Desempenho escolar. Moradia Rural x Urbana', 
        cex.main= 1.1,
        cex.names = 1.2, 
        cex.lab = 1.2, 
        cex.axis = 1.2, 
        ylim = c(0,180),
        border = 'gray20')
legend("topleft", legend = c('Rural', 'Urbano'), fill = terrain.colors(2))
rm(tt, ddt)

d1 <- group_by(.data = data_students, address) %>% summarise(count=n(),
                                                             media=mean(G3),
                                                             sd=sd(G3),
                                                             var=var(G3))
d1

#TESTANDO A VARIABILIDADE. Ha = S�O DIFERENTES. H0 = S�O IGUAIS
var.test(G3 ~ address, data = data_students, alternative = 'two.sided')
#AS VARIABILIDADES N�O S�O DIFERENTES. P-value=0.988

t.test(G3 ~ address, data = data_students, alternative = 'less', var.equal = TRUE)
#P-value=0.0178

#CONCLUS�O: 
#COMO P-VALUE � MENOR QUE 5%(0.017) PODEMOS REJEITAR A HIP�TESE NULA, DESTA FORMA PODEMOS CONCLUIR 
#COM 95% DE CONFIAN�A QUE A O ENDERE�O DE RESID�NCIA DOS ALUNOS INTERFERE NO SEU DESEMPENHO ESCOLAR
#ALUNOS QUE VIVEM NA CIDADE POSSUEM UMA M�DIA MAIOR DO QUE ALUNOS QUE MORAM NO CAMPO.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O TIPO DE . URBANO X RURAL


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POR TAMANHO DA FAM�LIA (QUANTIDADE DE MEMBROS).
# Diagrama
# Ha: a m�dia escolar de alunos com fam�lia de at� 3 membros n�o difere da m�dia escolar de alunos com fam�lia cima de 3 membros. 
# Ha: a m�dia escolar de alunos com fam�lia de at� 3 membros difere da m�dia escolar de alunos com fam�lia acima de 3 membros. 

d1 <- group_by(.data = data_students, famsize) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1



par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ famsize, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.8,
        main = 'Tamanho da Fam�lia x Desempenho',
        names = c('Menor e igual a 3','Maior que 3'),
        ylab = 'Desempenho Escolar',
        xlab = 'Quantidade de Membros da Fam�lia',
        border = 'gray20')

#TESTANDO A VARI�NCIA
var.test(G3 ~ famsize, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.1882. VARI�NCIAS N�O S�O DIFERENTES.

t.test(G3 ~ famsize, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE 0.1062. HIP�TESE NULA N�O PODE REJEITADA.

#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5%(0.1062) N�O PODEMOS REJEITAR A HIP�TESE NULA, DESTA FORMA CONCLU�MOS 
#COM 95% DE CONFIAN�A QUE A QUANTIDADE DE MEMBROS DA FAM�LIA N�O INTERERE NO SEU DESEMPENHO ESCOLAR DOS ALUNOS.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POR TAMANHO DE MEMBROS DA FAM�LIA.


##########ESTUDO SOBRE O DESEMPENHO DA SITUA�AO CONJUGAL DOS PAIS

# Diagrama
# H0: A situa��o conjugal dos pais n�o interfere no desempenho escolar dos alunos.
# Ha: A situa��o conjugal dos pais interfere no desempenho escolar dos alunos.

d1 <- group_by(.data = data_students, Pstatus) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ Pstatus, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.8,
        main = 'Filho(s) na Situa��o Conjugal x Desempenho',
        names = c('Separados','Juntos'),
        ylab = 'Desempenho Escolar',
        xlab = 'Situa��o Conjugal dos Pais',
        border = 'gray20')


#TESTANDO A VARI�NCIA
var.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.5929. VARI�NCIAS N�O S�O DIFERENTES.

t.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.250

#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5% PODEMOS REJEITAR A HIP�TESE ALTERNATIVA, DESTA FORMA CONCLU�MOS 
#COM 95% DE CONFIAN�A QUE A SITUA��O CONJUGAL DOS PAIS N�O INTERFERE NO DESEMPENHO DOS ALUNOS.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O GRAU DE ESCOLARIDADE DA M�E

# Diagrama
# H0: As m�dias escolares dos alunos n�o diferem com rela��o ao grau de escolaridade das m�es.
# Ha: Ao menos uma das m�dias escolares dos alunos diferem com rela��o ao grau de escolaridade das m�es.

d1 <- group_by(.data = data_students, Medu) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1

par(las=2)
par(mar=c(9,6,2,4))
boxplot(G3 ~ Medu, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x Grau de Escolaridade da M�e.',
        names = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Ensino M�dio','Curso Superior'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20'); grid(col=4)


#TESTE ANOVA. VARI�NCIAS IGUAIS
aovt <- aov(G3 ~ Medu, data = data_students)
summary(aovt)
#P-VALUE <<<<<<<< 5% 
#Pr(>F) 9.24e-05


#TESTE WELCH ANOVA. VARI�NCIAS DIFERENTES
welch_anova_test(G3 ~ Medu, data = data_students)
#P-VALUE < 5%

#CONCLUS�O
# Os resultados mostram que ao menos uma das m�dias se difere com rela��o ao grau de escolaridade das m�es.
# Desta forma conclu�mos que a o grau de escolaridade das m�es influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X O GRAU DE ESCOLARIDADE DA M�E.

##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O GRAU DE ESCOLARIDADE DO PAI

# Diagrama
# H0: As m�dias escolares dos alunos n�o diferem com rela��o ao grau de escolaridade dos pais
# Ha: Ao menos uma das m�dias escolares dos alunos diferem com rela��o ao grau de escolaridade dos pais.

d1 <- group_by(.data = data_students, Fedu) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1

par(las=2)
par(mar=c(9,6,2,4))
boxplot(G3 ~ Medu, data = data_students,
        col = topo.colors(5),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x Grau de Escolaridade do  Pai.',
        names = c('Sem estudo', 'Ensino Prim�rio','Ensino Fundamental','Ensino M�dio','Curso Superior'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')


#TESTE ANOVA. VARI�NCIAS IGUAIS
aovt <- aov(G3 ~ Fedu, data = data_students)
summary(aovt)
#P-VALUE <<<<<<<< 5%


#TESTE WELCH ANOVA. VARI�NCIAS DIFERENTES
welch_anova_test(G3 ~ Fedu, data = data_students)
#P-VALUE < 5%

#CONCLUS�O
# Considerando vari�ncias iguais, os resultados mostram que ao menos uma das m�dias se difere com rela��o ao grau de escolaridade dos pais.
# Desta forma conclu�mos que o grau de escolaridade dos pais influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X O GRAU DE ESCOLARIDADE DO PAI.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME A PROFISS�O DA M�E

# Diagrama
# H0: As m�dias escolares n�o se diferem conforme a categoria da profiss�o das m�es.
# Ha: Ao menos uma das m�dias escolares se diferem conforme a categoria da profiss�o das m�es.

d1 <- group_by(.data = data_students, Mjob) %>% summarise(media=mean(G3),
                                                          sd=sd(G3),
                                                          var = var(G3))
d1

par(las=2)
par(mar=c(6,6,8,4))
boxplot(G3 ~ Mjob, data = data_students,
        col = cm.colors(5),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x Profiss�o da M�e.',
        names = c('Em casa', 'Sa�de','Outros','Servi�os', 'Professora'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARI�NCIAS IGUAIS
aovt <- aov(G3 ~ Mjob, data = data_students)
summary(aovt)
#P-VALUE = 0.0051

#CONSIDERANDO VARI�NCIAS DIFERENTES
welch_anova_test(G3 ~ Mjob, data = data_students)

#P-VALUE=0.006
rm(d1, aovt)

#CONCLUS�O
# Os resultados mostram que ao menos uma das m�dias se difere com rela��o a categoria da profiss�o das m�es.
# Desta forma conclu�mos que a categoria de profiss�o das m�es influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X A PROFISS�O DA M�E.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME A PROFISS�O DA PAI

# Diagrama
# H0: As m�dias escolares n�o se diferem conforme a categoria da profiss�o dos pais.
# Ha: Ao menos uma das m�dias escolares se diferem conforme a categoria da profiss�o dos pais.

d1 <- group_by(.data = data_students, Fjob) %>% summarise(media=mean(G3),
                                                          var = var(G3))
d1

par(las=2)
par(mar=c(6,6,8,4))
boxplot(G3 ~ Fjob, data = data_students,
        col = topo.colors(5),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x Profiss�o do Pai.',
        names = c('Em casa', 'Sa�de','Outros','Servi�os', 'Professor'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARI�NCIAS IGUAIS
aovt <- aov(G3 ~ Fjob, data = data_students)
summary(aovt)
#P-VALUE = 0.268

#CONSIDERANDO VARI�NCIAS DIFERENTES
welch_anova_test(G3 ~ Fjob, data = data_students)
#P-VALUE=0.277

rm(d1, aovt)
#CONCLUS�O
# Os resultados mostram que as m�dias escolares n�o diferem conforme a categoria da prossis�o dos pais.

##########FIM ESTUDO SOBRE O DESEMPENHO X A PROFISS�O DOS PAIS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O MOTIVO DE ESCOLHA DA ESCOLA

# Diagrama
# Ha: As m�dias n�o se diferem conforme o motivo de escolha do col�gio.
# Ha: Ao menos uma das m�dias se diferem conforme o motivo de escolha do col�gio.

d1 <- group_by(.data = data_students, reason) %>% summarise(media=mean(G3),
                                                            var = var(G3))
d1

par(las=2)
par(mar=c(7,6,8,4))
boxplot(G3 ~ reason, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x Motivo Excolha da Escola.',
        names = c('Curso', 'Perto de Casa','Outros','Reputa��o'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARI�NCIAS IGUAIS
aovt <- aov(G3 ~ reason, data = data_students)
summary(aovt)
#P-VALUE = 0.102

#CONSIDERANDO VARI�NCIAS DIFERENTES
welch_anova_test(G3 ~ reason, data = data_students)
#P-VALUE=0.092

rm(d1, aovt)
#CONCLUS�O
# As m�dias escolares n�o diferem com rela��o ao motivo da escolha da escola.

##########FIM ESTUDO SOBRE O DESEMPENHO X MOTIVO DE ESCOLHA DA ESCOLA.


##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X GUARDA PARENTAL

#H0: As m�dias escolares n�o se diferem em fun��o do tipo de guarda parental.
#Ha: Ao menos uma das m�dias escolares se difere em fun��o do tipo de guarda parental.


d1 <- group_by(.data = data_students, guardian) %>% summarise(count=n(),
                                                              media=mean(G3),
                                                              sd=sd(G3),
                                                              var=var(G3))
d1

par(las=1)
par(mar=c(7,5,2,5))
boxplot(data_students$G3 ~ data_students$guardian,
        col = hcl.colors(3, palette = 'Hawaii'),
        names = c('Pai','M�e','Outros'),
        main = 'An�lise Bivariada',
        ylab = 'Desempenho (0 - 20)',
        xlab = '')


#Aplicando a an�lise de ANOVA para a compara��o de mais de 2 m�dias

# Supondo vari�ncias iguais
one_way_anova <- aov(G3 ~ guardian, data = data_students)
summary(one_way_anova)
# p-value = 0.205. Hip�tese nula(H0) n�o pode ser descartada.

# Supondo vari�ncias diferentes
welch_anova_test(G3 ~ guardian, data = data_students)
# p-value = 0.226. Hip�tese nula(H0) n�o pode ser descartada.

#CONCLUS�O
# Os resultados mostram que as m�dias escolares n�o diferem com rela��o ao tipo de guarda parental dos alunos.
# Desta forma conclu�mos que o tipo de guarda parental n�o interefere no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X GUARDA PARENTAL



##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O SUPORTE FAMILIAR EDUCACIONAL

# Diagrama
# H0: A m�dia de alunos que possuem suporte educacional familiar � igual a m�dia de alunos que n�o possuem.
# Ha: A m�dia de alunos que possuem suporte educacional familiar � diferente da m�dia de alunos que n�o possuem.

d1 <- group_by(.data = data_students, famsup) %>% summarise(count=n(),
                                                            media=mean(G3),
                                                            sd=sd(G3),
                                                            var = var(G3))
d1

par(las=1)
par(mar=c(7,6,8,4))
boxplot(G3 ~ famsup, data = data_students,
        col = terrain.colors(2),
        cex.main = 0.9,
        main = 'An�lise Bivariada. Desempenho Escolar x  Suporte Educacional Familiar',
        names = c('N�o', 'Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#TESTANDO AS VARI�NCIAS
var.test(G3 ~ famsup, data = data_students)
#P-VALUE = 0.798. VARI�NCIAS N�O S�OP DIFERENTES.

t.test(G3 ~ famsup, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.437

rm(d1, aovt)
#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5% PODEMOS REJEITAR A HIP�TESE ALTERNATIVA, DESTA FORMA CONCLU�MOS 
#COM 95% DE CONFIAN�A QUE O SUPORTE EDUCACIONAL FAMILIAR N�O INTERFERE NO DESEMPENHO DOS ALUNOS.
##########FIM ESTUDO SOBRE O DESEMPENHO X O SUPORTE EDUCACIONAL FAMILIAR..



##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM OU N�O REFOR�O ESCOLAR
# Diagrama
# H0: A m�dia escolar de alunos que n�o possuem refor�o escolar � menor ou igual a m�dia escolar de alunos que possuem.
# Ha: A m�dia escolar de alunos que n�o possuem refor�o escolar � maior do que a m�dia escolar de alunos que possuem.

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ schoolsup, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.8,
        main = 'An�lise Bivariada. Alunos que Possuem Refor�o Escolar.',
        names = c('N�o','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

d1 <- group_by(.data = data_students, schoolsup) %>% summarise(count=n(),
                                                               media=mean(G3),
                                                               sd=sd(G3),
                                                               var=var(G3))
d1

#TESTANDO A VARI�NCIA
var.test(G3 ~ schoolsup, data = data_students, alternative = 'two.sided')
#P-VALUE <<<<<<<< 5%. VARI�NCIAS S�O DIFERENTES.

t.test(G3 ~ schoolsup, data = data_students, alternative = 'greater', var.equal = FALSE)
#P-VALUE = 0.0098

#Como o P-valor � menor que 5% rejeitarmos a hip�tese nula em favor de Ha. 
#Portanto, conclu�mos que alunos que n�o possuem refor�o escolar possuem m�dia escolar superior 
# a alunos que fazem o refor�o.

##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM REFOR�O ESCOLAR


# Diagrama
# H0: A m�dia escolar de alunos que consomem �lcool acima da m�dia � igual a m�dia escolar de alunos que n�o o fazem.
# Ha: A m�dia escolar de alunos que consomem �lcool acima da m�dia � diferente a m�dia escolar de alunos que n�o o fazem.

#M�DIA GERAL DE CONSUMO DE ALCOOL DURANTE A SEMANA # 1.481013
mean(as.numeric(data_students$Dalc))
#M�DIA GERAL DE CONSUMO DE ALCOOL NO FIM DE SEMANA #2.291139
mean(as.numeric(data_students$Walc))
#M�DIA GERAL DE CONSUMO DE ALCOOL POR SEMANA #1.886076
mean((as.numeric(data_students$Walc) + as.numeric(data_students$Dalc))/2)

d1 <- group_by(.data = data_students, consome_alcool_acima_media) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1

par(mar=c(7,6,6,2))
par(las=2)
boxplot(G3 ~ round(media_alcool_semana), data = data_students,
        col = terrain.colors(9),
        cex.main = 0.8,
        main = 'An�lise Bivariada. N�veis Consumo de �lcool na Semana x Desempenho',
        ylab = 'Desempenho Escolar',
        xlab = '',
        names = c('Muito Pouco','Pouco','Moderado', 'Alto', 'Muito Alto'),
        border = 'gray20'); grid(col = 4)
ttt <- table(data_students$G3, data_students$Dalc)

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ consome_alcool_acima_media, data = data_students,
        col = terrain.colors(2),
        cex.main = 0.8,
        main = 'An�lise Bivariada. Consumo de �lcool Acima da M�dia',
        names = c('N�o','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#TESTANDO A VARI�NCIA
var.test(G3 ~ consome_alcool_acima_media, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.001362. VARI�NCIAS S�O DIFERENTES.

t.test(G3 ~ consome_alcool_acima_media, data = data_students, alternative = 'two.sided', var.equal = FALSE)
#P-VALUE <<<<<< 0.2195.


#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5% PODEMOS REJEITAR A HIP�TESE ALTERNATIVA, DESTA FORMA PODEMOS CONCLUIR 
#COM 95% DE CONFIAN�A QUE A O CONSUMO DE �LCOOL ACIMA DA M�DIA N�O TEM RELA��O NO DESEMPENHO ESCOLAR

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O CONSUMO DE �LCOOL ACIMA DA M�DIA.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.
# Diagrama
# H0: A m�dia escolar de alunos que vivem com pais separados � igual a m�dia escolar de alunos que vivem com pais juntos.
# Ha: A m�dia escolar de alunos que vivem com pais separados � diferente da m�dia escolar de alunos que vivem com pais juntos.

d1 <- group_by(.data = data_students, Pstatus) %>% summarise(count=n(),
                                                             media_notas = mean(G3),
                                                             sd = sd(G3),
                                                             var = var(G3))
d1

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ Pstatus, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.8,
        main = 'Situa��o dos Pais x Desempenho',
        names = c('Separados','Juntos'),
        ylab = 'Desempenho Escolar',
        xlab = 'Situa��o dos Pais',
        border = 'gray20')

#TESTANDO A VARI�NCIA
var.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.5929. VARI�NCIAS N�O S�O DIFERENTES.

t.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.125.
#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5% PODEMOS REJEITAR A HIP�TESE ALTERNATIVA, DESTA FORMA CONCLU�MOS 
#COM 95% DE CONFIAN�A QUE ALUNOS A SITUA��O CONJUGAL DOS PAIS N�O INTERFERE NO DESEMPENHO ESCOLAR DOS ALUNOS.
#AS M�DIA S�O IGUAIS

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM FILHOS NA CRECHE
# Diagrama
# H0: Alunos que possuem filhos na creche escolar possuem desempenho igual do que alunos que n�o possuem.
# Ha: Alunos que possuem filhos na creche escolar possuem desempenho diferente do que alunos que n�o possuem.

d1 <- group_by(.data = data_students, nursery) %>% summarise(count=n(),
                                                                    media_notas = mean(G3),
                                                                    sd = sd(G3),
                                                                    var = var(G3))
d1

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ nursery, data = data_students,
        col = cm.colors(2),
        cex.main = 0.8,
        main = 'Filho(s) na Creche x Desempenho',
        names = c('N�o','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = 'Filho(s) na Creche',
        border = 'gray20')

#TESTANDO SE AS VARI�NCIAS S�O DIFERENTES
var.test(G3 ~ nursery, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.9769. VARI�NCIAS N�O S�O DIFERENTES. > 5%

t.test(G3 ~ nursery, data = data_students, alternative = 'two.sided', var.equal = TRUE)

#P-VALUE = 0.3066
#CONCLUS�O
#COMO P-VALUE � MAIOR QUE 5% PODEMOS REJEITAR A HIP�TESE ALTERNATIVA EM FAVOR DE H0.
#DESTA FORMA CONCLU�MOS COM 95% DE CONFIAN�A QUE ALUNOS POSSUEM FILHOS NA CRECHE ESCOLAR POSSUEM 
#O MESMO RENDIMENTO DE ALUNOS QUE N�O POSSUEM.


##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM FILHOS NA CRECHE.



##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE LOCOMO��O AT� A ESCOLA

#Ha: As m�dias escolares n�o se difere em fun��o do tempo de locomo��o at� a escola.
#Ha: Ao menos uma das m�dias escolares se difere em fun��o do tempo de locomo��o at� a escola.

d1 <- group_by(.data = data_students, traveltime) %>% summarise(count=n(),
                                                              media=mean(G3),
                                                              sd=sd(G3),
                                                              var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$traveltime,
        #col = hcl.colors(4, palette = 'Hawaii'),
        main = 'An�lise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Tempo de Transporte')
legend("topright", legend = c('< 15 min.', '15 a 30 min.', '30 min a 1hr.','> 1hr.'), fill = hcl.colors(4, palette = 'Hawaii'))


#Aplicando a an�lise de ANOVA para a compara��o de mais de 2 m�dias
# Supondo vari�ncias iguais
one_way_anova <- aov(G3 ~ traveltime, data = data_students)
summary(one_way_anova)
# p-value = 0.0199. Hip�tese nula(H0) pode ser descartada em favor de Ha.

# Supondo vari�ncias diferentes
welch_anova_test(G3 ~ traveltime, data = data_students)
# p-value = 0.172. Hip�tese nula(H0) n�o pode ser descartada.

#CONCLUS�O
#Falhamos em rejeitar H0. Considerando vari�ncias diferentes, os testes mostram que todas 
#as m�dias se difere em fun��o do tempo de transporte de casa at� o col�gio. Desta forma 
#conclu�mos que o tempo de casa at� o col�gio n�o interfere no desempenho dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE LOCOMO��O AT� A ESCOLA

##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE ESTUDO DURANTE A SEMANA

#H0: As m�dias escolares se diferem em fun��o do tempo de estudo do aluno durante a semana
#Ha: Ao menos uma das m�dias escolares se difere em fun��o do tempo de estudo do aluno durante a semana

d1 <- group_by(.data = data_students, studytime) %>% summarise(count=n(),
                                                                media=mean(G3),
                                                                sd=sd(G3),
                                                                var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$studytime,
        col = hcl.colors(4, palette = 'Cividis'),
        main = 'An�lise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Tempo de Estudo')
legend("bottomright",  legend = c('1 - Menos que 2hrs.', '2 - 2 a 5hrs', '3 - 5 a 10hrs.','4 - > 10hrs.'), fill = hcl.colors(4, palette = 'Cividis'))


#Aplicando a an�lise de ANOVA para a compara��o de mais de 2 m�dias
# Supondo vari�ncias iguais
one_way_anova <- aov(G3 ~ studytime, data = data_students)
summary(one_way_anova)
# p-value = 0.0521. Hip�tese nula(H0) n�o pode ser descartada.

# Supondo vari�ncias diferentes
welch_anova_test(G3 ~ studytime, data = data_students)
# p-value = 0.196. Hip�tese nula(H0) n�o pode ser descartada.

#CONCLUS�O
# Os estudos mostram que o tempo de estudo do aluno durante a semana n�o interfere no seu desempenho escolar.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE ESTUDO DURANTE A SEMANA


##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X QUANTIDADE DE REPROVA��ES ANTERIORES

#H0: As m�dias n�o se diferem em fun��o de reprova��es anteriores na mesma disciplina.
#Ha: Ao menos uma das m�dias se diferem em fun��o de reprova��es anteriores na mesma disciplina.

d1 <- group_by(.data = data_students, failures) %>% summarise(count=n(),
                                                               media=mean(G3),
                                                               sd=sd(G3),
                                                               var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$failures,
        col = hcl.colors(4, palette = 'Berlin'),
        main = 'An�lise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Quantidade de Reprova��es')
legend("topright",  legend = c('0 - Nenhuma reprova��o.', '1 - 1 Reprova��o.', '2 - 2 Reprovac�es.','3 - 3 ou mais Reprova��es.'), fill = hcl.colors(4, palette = 'Berlin'))


#Aplicando a an�lise de ANOVA para a compara��o de mais de 2 m�dias

# Supondo vari�ncias iguais
one_way_anova <- aov(G3 ~ failures, data = data_students)
summary(one_way_anova)
# p-value <<<<<<<<<< 5%. Hip�tese nula(H0) pode ser descartada em favor da Ha.

# Supondo vari�ncias diferentes
welch_anova_test(G3 ~ failures, data = data_students)
# p-value <<<<<<<< 5% Hip�tese nula(H0) pode ser descartada em favor de Ha.

#CONCLUS�O
#Os testes comprovam que o desempenho dos alunos se difere em fun��o de reprova��es anteriores na mesma disciplina. 
#Desta forma podemos concluir que reprova��es na disciplina tem rela��o com o desempenho do aluno.

##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X QUANTIDADE DE REPROVA��ES ANTERIORES

##########ESTUDO SOBRE A RELA��O LINEAR DO DESEMPENHO DOS ALUNOS X TOTAL DE FALTAS NAS DISCIPLINA.



lll <- lm(G3 ~  absences, data = data_students)
summary(lll)

plot(data_students$absences, data_students$G3, 
     xlab = 'N�mero de Faltas', 
     ylab = 'Desempenho', 
     pch = 21, 
     cex = 0.8, 
     col = 'dodgerblue4', 
     bg = 'dodgerblue')
abline(lll, col = 'red', lwd = 2, lty = 2)	

lll <- lm(G3 ~  traveltime, data = data_students)
summary(lll)
plot(data_students$traveltime, data_students$G3, 
     xlab = 'Tempo de Estudo', 
     ylab = 'Desempenho', 
     pch = 21, 
     cex = 0.8, 
     col = 'dodgerblue4', 
     bg = 'dodgerblue')
abline(lll, col = 'red', lwd = 2, lty = 3)	

lll <- lm(G3 ~  studytime, data = data_students)
summary(lll)
plot(data_students$studytime, data_students$G3, 
     xlab = 'Tempo de Estudo', 
     ylab = 'Desempenho', 
     pch = 21, 
     cex = 0.8, 
     col = 'dodgerblue4', 
     bg = 'dodgerblue')
abline(lll, col = 'red', lwd = 2, lty = 3)	

lll <- lm(G3 ~  famrel, data = data_students)
summary(lll)
plot(data_students$famrel, data_students$G3, 
     xlab = 'Tempo de Estudo', 
     ylab = 'Desempenho', 
     pch = 21, 
     cex = 0.8, 
     col = 'dodgerblue4', 
     bg = 'dodgerblue')
abline(lll, col = 'red', lwd = 2, lty = 3)	

plot(G3 ~ age,data = data_students,  col = 'blue', lwd = 2, lty = 2)



##########ESTUDO COMPARATIVO DE DESEMPENHO ENTRE AS ESCOLAS

# Gabriel Pereira / Mousinho da Silveira
d1 <- group_by(.data = data_students, school) %>% summarise(count=n(),
                                                            media=mean(G3),
                                                            sd=sd(G3),
                                                            var=var(G3))

d1


boxplot(data_students$G3 ~ data_students$school, 
        col= hcl.colors(5, palette = 'PuBu'),
        xlab = 'Escola',
        ylab = 'Desempenho')
legend("bottomright", legend = c('Gabriel Pereira','Mousinho da Silveira'), fill = hcl.colors(5, palette = 'PuBu'))

# Diagrama
# H0: As m�dias escolares dos alunos s�o iguais entre as duas escolas.
# Ha: As m�dias escolares dos alunos s�o diferentes entre as duas escolas.

#TESTANDO A VARIABILIDADE
var.test(G3 ~ school, data = data_students, alternative = 'two.sided')
#P-VALUE=0.4803. HIP�TESE NULA N�O DESCARTADA. VARIABILDIADE N�O S�O DIFERENTES.

#TESTANDO A HIP�TESE DE QUE AS M�DIAS ESCOLARES DOS ALUNOS S�O DIFERENTES ENTRE AS DUAS ESCOLAS
t.test(G3 ~ school, data = data_students,  alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.3722
glimpse(data_students)
#CONCLUS�O
#Como o p-value � maior que 5% n�o podemos rejeitar a hip�tese nula. 
# Portanto, conclu�mos que as m�dias escolares dos alunos s�o iguais nas duas escolas.

##########FIM ESTUDO COMPARATIVO DE DESEMPENHO ENTRE AS ESCOLAS



########## VERIFICANDO A RELA��O LINEAR ENTRE AS VARI�VEIS


#N�MERO DE REPETI��ES DA MESMA DISCIPLINA
tt1 <- lm(G3 ~ failures, data = data_students)
summary(tt1)
#P-VALUE <<<<<<< 5%
#R2 0,13

d1 <- group_by(.data = data_students, failures) %>% summarise(count=n())
d1

plot(data_students$failures, data_students$G3,
     main = '',
     xlab = 'TOTAL REPROVA��ES NA DISCIPLINA', 
     ylab = 'DESEMPENHO (0 - 20)', 
     cex.lab = 1.2, pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue',
     ylim = c(0,20), xlim = c(0,3))
abline(lm(G3 ~ failures, data = data_students), col = 'red', lwd=2,lty=2)	

#OBSERVAMOS UMA RELA��O LINEAR INVERSA

#N�MERO FALTAS NA DISCIPLINA
#H0: N�o h� rela��o estatisticamente significante entre as faltas dos alunos e as m�dias escolares.
#Ha: Existe rela��o estatisticamente significante entre as faltas dos alunos e as m�dias escolares.

tt1 <- lm(G3 ~ absences, data = data_students)
summary(tt1)
#P-value = 0.497.
#R2 = 0
# Com o P-value maior que 5% identicamos que n�o h� rela��o linear entrea a quantidade da faltas e 
#o desempenho do aluno da disciplina matem�tica.

plot(data_students$absences, data_students$G3,
     main = '', 
     xlab = 'N�MERO DE FALTAS NA DISCIPLINA', 
     ylab = 'DESEMPENHO (0 - 20)', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue', 
     ylim = c(0,20), 
     xlim = c(0,70))
abline(lm(G3 ~ absences, data = data_students), col = 'red', lwd = 2, lty =2)

#CONCLUS�O
#P-valor > que 5%, N�o rejeita H0. N�o apresenta um padr�o de lineariedade entre as faltas e o desempenho escolar

