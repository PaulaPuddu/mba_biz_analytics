#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# INFERENCIA ESTATISTICA
# TRABALHO INDIVIDUAL
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#ALUNOS:ANA PAULA, DAVI, FÁBIO MONTEIRO, LUCAS SENA E  MARCOS SOARES, 

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
                                          consome_alcool_acima_media = ifelse(media_alcool_semana > 2 ,'sim', 'não'),
                                          famsize = ifelse(famsize == 'LE3' ,2, 4), #CONVERSÃO DA VARIÁVEL PARA BINÁRIA NUMÉRICA.
                                          )
                         
data_students$rendimento <- factor(data_students$rendimento)
data_students$sex <- factor(data_students$sex)
data_students$schoolsup <- factor(data_students$schoolsup)
#data_students$Medu = factor(data_students$Medu, levels = c(1,2,3,4), labels = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Curso Superior'))
#data_students$Fedu = factor(data_students$Fedu, levels = c(1,2,3,4), labels = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Curso Superior'))
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

##########Como é o rendimento geral dos alunos? (distribuição e descritiva)

vrPorcentagemE <- round((nrow(subset(data_students, rendimento == 'Excellent')) / nrow(data_students))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_students, rendimento == 'Fail')) / nrow(data_students))*100,digits = 2)
vrPorcentagemG <- round((nrow(subset(data_students, rendimento == 'Good')) / nrow(data_students))*100,digits = 2)
vrPorcentagemS <- round((nrow(subset(data_students, rendimento == 'Satisfactory')) / nrow(data_students))*100,digits = 2)
vrPorcentagemU <- round((nrow(subset(data_students, rendimento == 'Sufficient')) / nrow(data_students))*100,digits = 2)
par(mar=c(8,3,4,5))
par(las=1)
barplot(table(data_students$rendimento),
        main = 'Distribuição de Alunos por Rendimento Escolar',
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

#MÉDIA GERAL DA NOTA DE ALUNOS
mean(data_students$G3)

#DISTRIBUIÇÃO GERAL DAS NOTAS DOS ESTUDANTES
par(mar = c(3,4,7,5))
boxplot(data_students$G3,
        col = terrain.colors(2),
        main = 'Análise Univariada. Desempenho Escolar', 
        cex.main = 0.9,
        ylab = 'Desempenho',
        border = 'gray20')

hist(data_students$G3,
     main = 'Distribuição de Notas dos Estudantes',
     ylim = c(0,100),
     col = hcl.colors(10, palette = "Red-Blue"))

##########FIM Como é o rendimento geral dos alunos? (distribuição e descritiva);


#Análise exploratória univariada dos dados (gráficos e tabelas dependendo do contexto), 
#como é o panorama geral dos alunos dessas escolas?
#ANÁLISE GERAL SOBRE A BASE DE ESTUDANTES

#DISTRIBUIÇÃO DE ALUNOS POR ESCOLA
vrPorcentagemGP <- round((nrow(subset(data_students, school == 'GP')) / nrow(data_students))*100,digits = 2)
vrPorcentagemMS <- round((nrow(subset(data_students, school == 'MS')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$school),
        main = 'Distribuição de Alunos por Escola',
        ylab = 'Total de Alunos', 
        ylim = c(0,400),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemGP,'%',sep = ''),str_c('',vrPorcentagemMS,'%',sep = '')))
legend("topright", legend = c('Gabriel Pereira','Mousinho da Silveira'), fill = hcl.colors(2, palette = "Peach"))
rm(vrPorcentagemGP,vrPorcentagemMS)

#DISTRIBUIÇÃO DE ALUNOS POR SEXO
vrPorcentagemF <- round((nrow(subset(data_students, sex == 'F')) / nrow(data_students))*100,digits = 2)
vrPorcentagemM <- round((nrow(subset(data_students, sex == 'M')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$sex),
        main = 'Distribuição de Alunos por Sexo',
        ylab = 'Total de Alunos', 
        xlab = 'Sexo', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "Red-Blue"),
        names.arg = c(str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemM,'%',sep = '')))
legend("topleft", legend = c('Feminino','Masculino'), fill = hcl.colors(2, palette = "Red-Blue"))
rm(vrPorcentagemF, vrPorcentagemM)

#DISTRIBUIÇÃO DE ALUNOS POR ENDEREÇO
vrPorcentagemU <- round((nrow(subset(data_students, address == 'U')) / nrow(data_students))*100,digits = 2)
vrPorcentagemR <- round((nrow(subset(data_students, address == 'R')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$address),
     main = 'Distribuição de Alunos por Endereço',
     ylab = 'Total de Alunos', 
     xlab = 'Tipo de Endereço', 
     col = hcl.colors(2, palette = "Blues 3"),
     names.arg = c(str_c('',vrPorcentagemR,'%',sep = ''),str_c('',vrPorcentagemU,'%',sep = '')))
legend("topleft", legend = c('Rural','Urbano'), fill = hcl.colors(2, palette = "Blues 3"))
rm(vrPorcentagemR, vrPorcentagemU)


#DISTRIBUIÇÃO GERAL POR PSTATUS - SITUAÇÃO CONJUGAL DOS PAIS
par(mar = c(3,4,7,5))
vrPorcentagemA <- round((nrow(subset(data_students, Pstatus == 'A')) / nrow(data_students))*100,digits = 2)
vrPorcentagemT <- round((nrow(subset(data_students, Pstatus == 'T')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Pstatus),
        col = hcl.colors(2, palette = 'ag_Sunset'),
        main = 'Análise Univariada. Situação Conjugal dos Pais.', 
        names.arg = c(str_c('',vrPorcentagemA,'%',sep = ''),str_c('',vrPorcentagemT,'%',sep = '')),
        cex.main = 0.9,
        ylab = 'Desempenho',
        xlab = 'Situação',
        border = 'gray20')
legend("topleft", legend = c('Separados','Juntos'), fill = hcl.colors(2, palette = 'ag_Sunset'))
rm(vrPorcentagemR, vrPorcentagemU)


#DISTRIBUIÇÃO GERAL DE NIVEL ESCOLAR DAS MÃES
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Medu == 0)) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Medu == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Medu == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Medu == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Medu == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Medu),
        col = hcl.colors(5, palette = 'ag_Sunset'),
        main = 'Distribuição de Nível Escolar das Mães.', 
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = ''),str_c('',vrPorcentagem3,'%',sep = ''),str_c('',vrPorcentagem4,'%',sep = ''),str_c('',vrPorcentagem5,'%',sep = '')),
        cex.main = 0.9,
        ylim = c(0,150),
        border = 'gray20')
legend("topleft", legend = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Ensino Médio','Curso Superior'), fill = hcl.colors(5, palette = 'ag_Sunset'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrPorcentagem5)


#DISTRIBUIÇÃO GERAL DE NIVEL ESCOLAR DOS PAIS
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Fedu == 0)) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Fedu == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Fedu == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Fedu == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Fedu == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Fedu),
        col = hcl.colors(5, palette = 'Vik'),
        main = 'Distribuição de Nível Escolar dos Pais', 
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),str_c('',vrPorcentagem2,'%',sep = ''),str_c('',vrPorcentagem3,'%',sep = ''),str_c('',vrPorcentagem4,'%',sep = ''),str_c('',vrPorcentagem5,'%',sep = '')),
        cex.main = 0.9,
        ylim = c(0,180),
        border = 'gray20')
legend("topright", legend = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Ensino Médio','Curso Superior'), fill = hcl.colors(5, palette = 'Vik'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUIÇÃO GERAL DE CATEGORIA DE PROFISSAO DAS MAES
par(mar = c(7,4,4,2))
vrPorcentagem1 <- round((nrow(subset(data_students, Mjob == 'at_home')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Mjob == 'health')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Mjob == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Mjob == 'services')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Mjob == 'teacher')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Mjob),
        col = hcl.colors(5, palette = 'Lajolla'),
        main = 'Distribuição de Mães por Profissão.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Em casa', 'Saúde','Outros','Serviços', 'Professora'), fill = hcl.colors(5, palette = 'Lajolla'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrPorcentagem5)

#DISTRIBUIÇÃO GERAL DE CATEGORIA DE PROFISSAO DOS PAIS
par(mar = c(4,3,8,5))
vrPorcentagem1 <- round((nrow(subset(data_students, Fjob == 'at_home')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Fjob == 'health')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Fjob == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Fjob == 'services')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Fjob == 'teacher')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Fjob),
        col = hcl.colors(5, palette = 'ag_GrnYl'),
        main = 'Distribuição de Pais por Profissão.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Em casa', 'Saúde','Outros','Serviços', 'Professor'), fill = hcl.colors(5, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUIÇÃO GERAL DE ALUNOS POR GUARDA PARENTAL
par(las = 1)
par(mar = c(4,6,5,5))
vrPorcentagem1 <- round((nrow(subset(data_students, guardian  == 'mother')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, guardian == 'father')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, guardian == 'other')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$guardian),
        col = hcl.colors(3, palette = 'ag_GrnYl'),
        main = 'Distribuição de Alunos por Guarda Parental.', 
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = '')),
        ylim = c(0,300),
        border = 'gray20')
legend("topright", legend = c('Mãe', 'Pai','Outros'), fill = hcl.colors(3, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3)

#DISTRIBUIÇÃO GERAL DE ALUNOS POR TAMANHO DA FAMÍLIA
par(las = 1)
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, famsize  == '2')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famsize == '4')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famsize),
        col = hcl.colors(2, palette = 'ag_GrnYl'),
        main = 'Distribuição de Alunos por Tamanho da Família.', 
        ylim = c(0,300),
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('Menos que 4 membros', 'Acima de 3 membros'), fill = hcl.colors(2, palette = 'ag_GrnYl'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3)


#DISTRIBUIÇÃO GERAL DE ALUNOS MOTIVO DE ESCOLHA DA ESCOLA
par(mar = c(3,4,7,5))
vrLegenda <- c('Curso', 'Perto de Casa','Outros','Reputação')
vrPorcentagem1 <- round((nrow(subset(data_students, reason == 'course')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, reason == 'home')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, reason == 'other')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, reason == 'reputation')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$reason),
        col = hcl.colors(4, palette = 'Broc'),
        main = 'Distribuição de Alunos por Motivo de Escolha da Escola.', 
        cex.main = 0.9,
        ylim = c(0,180),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Broc'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)

#DISTRIBUIÇÃO GERAL DE ALUNOS POR TEMPO DE LOCOMOÇÃO ATÉ A ESCOLA
par(mar = c(3,4,7,5))
vrLegenda <- c('< 15 min.', '15 a 30 min.', '30 min a 1hr.','> 1hr.')
vrPorcentagem1 <- round((nrow(subset(data_students, traveltime == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, traveltime == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, traveltime == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, traveltime == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$traveltime),
        col = hcl.colors(4, palette = 'Geyser'),
        main = 'Distribuição de Alunos por Tempo de Locomoção Casa x Escola.', 
        cex.main = 0.9,
        ylim = c(0,300),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Geyser'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)


#DISTRIBUIÇÃO GERAL DE ALUNOS POR TEMPO DE ESTUDO SEMANAL
par(mar = c(3,4,7,5))
vrLegenda <- c('< 2hr.', 'De 2hrs a 5 hsr.', 'De 5hrs a 10hrs.','> 10hrs.')
vrPorcentagem1 <- round((nrow(subset(data_students, studytime == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, studytime == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, studytime == 3)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, studytime == 4)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$studytime),
        col = hcl.colors(5, palette = 'Batlow'),
        main = 'Distribuição de Alunos por Tempo de Estudo Semanal.', 
        cex.main = 0.9,
        ylim = c(0,300),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = vrLegenda, fill = hcl.colors(4, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4, vrLegenda)

#DISTRIBUIÇÃO GERAL DE ALUNOS POR REPROVAÇÃO NA DISCIPLINA
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, failures == 0)) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, failures == 1)) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, failures == 2)) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, failures == 3)) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$failures),
        col = hcl.colors(5, palette = 'Batlow'),
        main = 'Distribuição de Alunos por Quantidade de Reprovações.', 
        cex.main = 0.9,
        ylim = c(0,380),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Nenhuma.','Uma.','Duas.','Três ou mais.'), fill = hcl.colors(4, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3, vrPorcentagem4)

#DISTRIBUIÇÃO GERAL DE ALUNOS COM SUPLEMENTO ESCOLAR
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, schoolsup == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, schoolsup == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$schoolsup),
        col = hcl.colors(2, palette = 'Batlow'),
        main = 'Distribuição de Alunos por Suplemento Escolar.', 
        cex.main = 0.9,
        ylim = c(0,400),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Não','Sim'), fill = hcl.colors(2, palette = 'Batlow'))
rm(vrPorcentagem1, vrPorcentagem2)


#DISTRIBUIÇÃO GERAL DE ALUNOS COM SUPORTE EDUCACIONAL FAMILIAR - famsup
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, famsup == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famsup == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famsup),
        col = hcl.colors(2, palette = 'Lajolla'),
        main = 'Distribuição de Alunos por Suporte Educacional Familiar.', 
        cex.main = 0.9,
        ylim = c(0,250),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('Não','Sim'), fill = hcl.colors(2, palette = 'Lajolla'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUIÇÃO GERAL DE ALUNOS COM CURSOS EXTRAS NA DISCIPLINA
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, paid == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, paid == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$paid),
        col = hcl.colors(2, palette = 'Turku'),
        main = 'Distribuição de Alunos com Curso Extra na Disciplina.', 
        cex.main = 0.9,
        ylim = c(0,250),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topright", legend = c('Não','Sim'), fill = hcl.colors(2, palette = 'Turku'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUIÇÃO GERAL DE ALUNOS COM ATIVIDADES EXTRA CURRICULARES.
par(mar = c(3,4,7,5))
vrPorcentagem1 <- round((nrow(subset(data_students, activities == 'no')) / nrow(data_students))*100, digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, activities == 'yes')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$activities),
        col = hcl.colors(2, palette = 'Vik'),
        main = 'Distribuição de Alunos com Atividades Extra-Curriculares.', 
        cex.main = 0.9,
        ylim = c(0,240),
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = c('Não','Sim'), fill = hcl.colors(2, palette = 'Vik'))
rm(vrPorcentagem1, vrPorcentagem2)

#DISTRIBUIÇÃO DE ALUNOS QUE FREQUENTARAM A ESCOLA MATERNAL
vrPorcentagemCN <- round((nrow(subset(data_students, nursery == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSN <- round((nrow(subset(data_students, nursery == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$nursery),
        main = 'Distribuição de Alunos que Frequentaram o Maternal',
        cex.main = 0.9,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "Zissou 1"),
        names.arg = c(str_c('',vrPorcentagemSN,'%',sep = ''),str_c('',vrPorcentagemCN,'%',sep = '')))
legend("topright", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "Zissou 1"))
rm(vrPorcentagemCN,vrPorcentagemSN)

#DISTRIBUIÇÃO DE ALUNOS QUE QUEREM FAZER CURSO SUPERIOR
vrPorcentagemS <- round((nrow(subset(data_students, higher == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemN <- round((nrow(subset(data_students, higher == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$higher),
        main = 'Distribuição de Alunos que Desejam Curso Superior',
        cex.main = 0.9,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "OrYel"),
        names.arg = c(str_c('',vrPorcentagemN,'%',sep = ''),str_c('',vrPorcentagemS,'%',sep = '')))
legend("topleft", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "OrYel"))
rm(vrPorcentagemN,vrPorcentagemS)

#DISTRIBUIÇÃO DE ALUNOS COM E SEM ACESSO A INTERNET
vrPorcentagemCI <- round((nrow(subset(data_students, internet == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSI <- round((nrow(subset(data_students, internet == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$internet),
        main = 'Distribuição de Alunos com Acesso a Internet em Casa',
        cex.main = 1.0,
        ylab = 'Total de Alunos', 
        ylim = c(0,380),
        col = hcl.colors(2, palette = "PiYG"),
        names.arg = c(str_c('',vrPorcentagemSI,'%',sep = ''),str_c('',vrPorcentagemCI,'%',sep = '')))
legend("topleft", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "PiYG"))
rm(vrPorcentagemCI,vrPorcentagemSI)


#DISTRIBUIÇÃO DE ALUNOS QUE SÃO OU NÃO ROMÂNTICOS
vrPorcentagemCI <- round((nrow(subset(data_students, romantic == 'yes')) / nrow(data_students))*100,digits = 2)
vrPorcentagemSI <- round((nrow(subset(data_students, romantic == 'no')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$romantic),
        main = 'Distribuição de Alunos que são Românticos.',
        cex.main = 1.0,
        ylab = 'Total de Alunos', 
        ylim = c(0,nrow(data_students)),
        col = hcl.colors(2, palette = "PiYG"),
        names.arg = c(str_c('',vrPorcentagemSI,'%',sep = ''),str_c('',vrPorcentagemCI,'%',sep = '')))
legend("topleft", legend = c('Não','Sim'), fill = hcl.colors(2, palette = "PiYG"))
rm(vrPorcentagemCI,vrPorcentagemSI)


#DISTRIBUIÇÃO GERAL DE ALUNOS POR NÍVEL DE RELACIONAMENTO FAMILIAR
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito ruim', 'Ruim','Normal','Bom','Muito Bom')
vrPorcentagem1 <- round((nrow(subset(data_students, famrel == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, famrel == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, famrel == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, famrel == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, famrel == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$famrel),
        col = hcl.colors(5, palette = 'Broc'),
        main = 'Distribuição de Alunos por Nível de Relacionamento Familiar.', 
        cex.main = 0.9,
        names.arg = c(str_c('',vrPorcentagem1,'%',sep = ''),
                      str_c('',vrPorcentagem2,'%',sep = ''),
                      str_c('',vrPorcentagem3,'%',sep = ''),
                      str_c('',vrPorcentagem4,'%',sep = ''),
                      str_c('',vrPorcentagem5,'%',sep = '')),
        border = 'gray20')
legend("topleft", legend = vrLegenda, fill = hcl.colors(5, palette = 'Broc'))
rm(vrPorcentagem1, vrPorcentagem2, vrPorcentagem3,vrPorcentagem4,vrPorcentagem5)

#DISTRIBUIÇÃO GERAL DE ALUNOS POR TEMPO DISPONÍVEL
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco', 'Pouco','Normal','Alto','Muito Alto')
vrPorcentagem1 <- round((nrow(subset(data_students, freetime == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, freetime == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, freetime == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, freetime == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, freetime == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$freetime),
        col = hcl.colors(5, palette = 'Temps'),
        main = 'Distribuição de Alunos por Tempo Livre Duranta a Semana.', 
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

#DISTRIBUIÇÃO GERAL DE ALUNOS QUE SAEM COM AMIGOS
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco', 'Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, goout == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, goout == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, goout == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, goout == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, goout == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$goout),
        col = hcl.colors(5, palette = 'Emrld'),
        main = 'Distribuição de Alunos que Saem com Amigos.', 
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

#DISTRIBUIÇÃO GERAL DE ALUNOS POR NÍVEL DE CONSUMO DE ÁLCOOL DURANTE A SEMANA
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco','Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, Dalc == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Dalc == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Dalc == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Dalc == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Dalc == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Dalc),
        col = hcl.colors(5, palette = 'Emrld'),
        main = 'Distribuição de Alunos por Nível de Consumo de Álcool Durante a Semana.', 
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

#DISTRIBUIÇÃO GERAL DE ALUNOS POR NÍVEL DE CONSUMO DE ÁLCOOL DURANTE O FINAL DE SEMANA
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito pouco','Pouco','Normal','Muito','Em Excesso')
vrPorcentagem1 <- round((nrow(subset(data_students, Walc == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, Walc == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, Walc == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, Walc == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, Walc == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$Walc),
        col = hcl.colors(5, palette = 'Set 3'),
        main = 'Distribuição de Alunos por Nível de Consumo de Álcool no Final de Semana.', 
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

#DISTRIBUIÇÃO GERAL DE ALUNOS POR SITUAÇÃO ATUAL DE SAÚDE
par(mar = c(3,4,7,5))
vrLegenda <- c('Muito ruim','Ruim','Indo','Boa','Super Boa')
vrPorcentagem1 <- round((nrow(subset(data_students, health == '1')) / nrow(data_students))*100,digits = 2)
vrPorcentagem2 <- round((nrow(subset(data_students, health == '2')) / nrow(data_students))*100, digits = 2)
vrPorcentagem3 <- round((nrow(subset(data_students, health == '3')) / nrow(data_students))*100, digits = 2)
vrPorcentagem4 <- round((nrow(subset(data_students, health == '4')) / nrow(data_students))*100, digits = 2)
vrPorcentagem5 <- round((nrow(subset(data_students, health == '5')) / nrow(data_students))*100, digits = 2)
barplot(table(data_students$health),
        col = hcl.colors(5, palette = 'Oranges'),
        main = 'Distribuição de Alunos por Nível de Consumo de Álcool no Final de Semana.', 
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

#DISTRIBUIÇÃO GERAL DE ALUNOS POR NÚMERO DE FALTAS NAS DISCIPLINA

##########FIM ANÁLISE GERAL DOS DADOS


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELAÇÃO AO GÊNERO

# Diagrama sobre o rendimento x sexo
# H0: A média de desempenho de alunos do gênero feminino não é inferior a média de alunos do sexo masculino.
# Ha: A média de desempenho de alunos do gênero feminino é inferior a média de alunos do sexo masculino.

par(las=1)
boxplot(G3 ~ sex, data = data_students,
        main = 'Distribuição de Rendimento Por Sexo', 
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
#0.6989. AS VARIÂNCIAS NÃO SÃO DIFERENTES.

t.test(G3 ~ sex, data = data_students, alternative = 'less', var.equal = TRUE)
#P-value = 0.0199

#CONCLUSÃO
# Uma vez que p-valor < 5% temos evidencia para rejeitar H0 em favor de Ha
# de modo que alunos do gênero feminino possuem média inferior do que alunos do gênero masculino.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELAÇÃO AO GÊNERO


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS COM RELAÇÃO A SUA IDADE

#TESTE DE REGRESSÃO LINEAR SIMPLES 
#H0: Não há relação estatisticamente significante entre a idade dos alunos e as médias escolares.
#Ha: Existe relação estatisticamente significante entre a idade dos alunos e as médias escolares.

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

# Com o P-value é menor que 5% identificamos uma relação linear inversa entre o desempenho dos alunos
# e sua idade, de modo que, acada ano de idade dos alunos, há uma redução de 0.5 décimos da nota final.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O TIPO DE ENDEREÇO URBANO X RURAL
# Diagrama sobre o RENDIMENTO x LOCAL DE RESIDÊNCIA
# H0: Alunos que moram na zona Rural possuem média escolar igual ou superior a média escolar de alunos que moram na zona Urbana.
# Ha: Alunos que moram na zona Rural possuem média escolar menor do que a média escolar de alunos que moram na zona Urbana.

par(mar = c(3,4,7,5))
boxplot(G3 ~ address, data = data_students,
        col = terrain.colors(2),
        main = 'Análise Bivariada. Tipo de Endereço x Desempenho Escolar', 
        cex.main = 0.9,
        ylab = 'Desempenho',
        xlab = 'Tipo de Endereço',
        border = 'gray20')

#O ENDEREÇO DOS ALUNOS INFLUENCIA EM SEU RENDIMENTO
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

#TESTANDO A VARIABILIDADE. Ha = SÃO DIFERENTES. H0 = SÃO IGUAIS
var.test(G3 ~ address, data = data_students, alternative = 'two.sided')
#AS VARIABILIDADES NÃO SÃO DIFERENTES. P-value=0.988

t.test(G3 ~ address, data = data_students, alternative = 'less', var.equal = TRUE)
#P-value=0.0178

#CONCLUSÃO: 
#COMO P-VALUE É MENOR QUE 5%(0.017) PODEMOS REJEITAR A HIPÓTESE NULA, DESTA FORMA PODEMOS CONCLUIR 
#COM 95% DE CONFIANÇA QUE A O ENDEREÇO DE RESIDÊNCIA DOS ALUNOS INTERFERE NO SEU DESEMPENHO ESCOLAR
#ALUNOS QUE VIVEM NA CIDADE POSSUEM UMA MÉDIA MAIOR DO QUE ALUNOS QUE MORAM NO CAMPO.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O TIPO DE . URBANO X RURAL


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POR TAMANHO DA FAMÍLIA (QUANTIDADE DE MEMBROS).
# Diagrama
# Ha: a média escolar de alunos com família de até 3 membros não difere da média escolar de alunos com família cima de 3 membros. 
# Ha: a média escolar de alunos com família de até 3 membros difere da média escolar de alunos com família acima de 3 membros. 

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
        main = 'Tamanho da Família x Desempenho',
        names = c('Menor e igual a 3','Maior que 3'),
        ylab = 'Desempenho Escolar',
        xlab = 'Quantidade de Membros da Família',
        border = 'gray20')

#TESTANDO A VARIÂNCIA
var.test(G3 ~ famsize, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.1882. VARIÂNCIAS NÃO SÃO DIFERENTES.

t.test(G3 ~ famsize, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE 0.1062. HIPÓTESE NULA NÃO PODE REJEITADA.

#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5%(0.1062) NÃO PODEMOS REJEITAR A HIPÓTESE NULA, DESTA FORMA CONCLUÍMOS 
#COM 95% DE CONFIANÇA QUE A QUANTIDADE DE MEMBROS DA FAMÍLIA NÃO INTERERE NO SEU DESEMPENHO ESCOLAR DOS ALUNOS.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POR TAMANHO DE MEMBROS DA FAMÍLIA.


##########ESTUDO SOBRE O DESEMPENHO DA SITUAÇAO CONJUGAL DOS PAIS

# Diagrama
# H0: A situação conjugal dos pais não interfere no desempenho escolar dos alunos.
# Ha: A situação conjugal dos pais interfere no desempenho escolar dos alunos.

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
        main = 'Filho(s) na Situação Conjugal x Desempenho',
        names = c('Separados','Juntos'),
        ylab = 'Desempenho Escolar',
        xlab = 'Situação Conjugal dos Pais',
        border = 'gray20')


#TESTANDO A VARIÂNCIA
var.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.5929. VARIÂNCIAS NÃO SÃO DIFERENTES.

t.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.250

#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5% PODEMOS REJEITAR A HIPÓTESE ALTERNATIVA, DESTA FORMA CONCLUÍMOS 
#COM 95% DE CONFIANÇA QUE A SITUAÇÃO CONJUGAL DOS PAIS NÃO INTERFERE NO DESEMPENHO DOS ALUNOS.

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O GRAU DE ESCOLARIDADE DA MÃE

# Diagrama
# H0: As médias escolares dos alunos não diferem com relação ao grau de escolaridade das mães.
# Ha: Ao menos uma das médias escolares dos alunos diferem com relação ao grau de escolaridade das mães.

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
        main = 'Análise Bivariada. Desempenho Escolar x Grau de Escolaridade da Mãe.',
        names = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Ensino Médio','Curso Superior'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20'); grid(col=4)


#TESTE ANOVA. VARIÂNCIAS IGUAIS
aovt <- aov(G3 ~ Medu, data = data_students)
summary(aovt)
#P-VALUE <<<<<<<< 5% 
#Pr(>F) 9.24e-05


#TESTE WELCH ANOVA. VARIÂNCIAS DIFERENTES
welch_anova_test(G3 ~ Medu, data = data_students)
#P-VALUE < 5%

#CONCLUSÃO
# Os resultados mostram que ao menos uma das médias se difere com relação ao grau de escolaridade das mães.
# Desta forma concluímos que a o grau de escolaridade das mães influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X O GRAU DE ESCOLARIDADE DA MÃE.

##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O GRAU DE ESCOLARIDADE DO PAI

# Diagrama
# H0: As médias escolares dos alunos não diferem com relação ao grau de escolaridade dos pais
# Ha: Ao menos uma das médias escolares dos alunos diferem com relação ao grau de escolaridade dos pais.

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
        main = 'Análise Bivariada. Desempenho Escolar x Grau de Escolaridade do  Pai.',
        names = c('Sem estudo', 'Ensino Primário','Ensino Fundamental','Ensino Médio','Curso Superior'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')


#TESTE ANOVA. VARIÂNCIAS IGUAIS
aovt <- aov(G3 ~ Fedu, data = data_students)
summary(aovt)
#P-VALUE <<<<<<<< 5%


#TESTE WELCH ANOVA. VARIÂNCIAS DIFERENTES
welch_anova_test(G3 ~ Fedu, data = data_students)
#P-VALUE < 5%

#CONCLUSÃO
# Considerando variãncias iguais, os resultados mostram que ao menos uma das médias se difere com relação ao grau de escolaridade dos pais.
# Desta forma concluímos que o grau de escolaridade dos pais influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X O GRAU DE ESCOLARIDADE DO PAI.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME A PROFISSÃO DA MÃE

# Diagrama
# H0: As médias escolares não se diferem conforme a categoria da profissão das mães.
# Ha: Ao menos uma das médias escolares se diferem conforme a categoria da profissão das mães.

d1 <- group_by(.data = data_students, Mjob) %>% summarise(media=mean(G3),
                                                          sd=sd(G3),
                                                          var = var(G3))
d1

par(las=2)
par(mar=c(6,6,8,4))
boxplot(G3 ~ Mjob, data = data_students,
        col = cm.colors(5),
        cex.main = 0.9,
        main = 'Análise Bivariada. Desempenho Escolar x Profissão da Mãe.',
        names = c('Em casa', 'Saúde','Outros','Serviços', 'Professora'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARIÂNCIAS IGUAIS
aovt <- aov(G3 ~ Mjob, data = data_students)
summary(aovt)
#P-VALUE = 0.0051

#CONSIDERANDO VARIÂNCIAS DIFERENTES
welch_anova_test(G3 ~ Mjob, data = data_students)

#P-VALUE=0.006
rm(d1, aovt)

#CONCLUSÃO
# Os resultados mostram que ao menos uma das médias se difere com relação a categoria da profissão das mães.
# Desta forma concluímos que a categoria de profissão das mães influencia no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO X A PROFISSÃO DA MÃE.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME A PROFISSÃO DA PAI

# Diagrama
# H0: As médias escolares não se diferem conforme a categoria da profissão dos pais.
# Ha: Ao menos uma das médias escolares se diferem conforme a categoria da profissão dos pais.

d1 <- group_by(.data = data_students, Fjob) %>% summarise(media=mean(G3),
                                                          var = var(G3))
d1

par(las=2)
par(mar=c(6,6,8,4))
boxplot(G3 ~ Fjob, data = data_students,
        col = topo.colors(5),
        cex.main = 0.9,
        main = 'Análise Bivariada. Desempenho Escolar x Profissão do Pai.',
        names = c('Em casa', 'Saúde','Outros','Serviços', 'Professor'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARIÂNCIAS IGUAIS
aovt <- aov(G3 ~ Fjob, data = data_students)
summary(aovt)
#P-VALUE = 0.268

#CONSIDERANDO VARIÂNCIAS DIFERENTES
welch_anova_test(G3 ~ Fjob, data = data_students)
#P-VALUE=0.277

rm(d1, aovt)
#CONCLUSÃO
# Os resultados mostram que as médias escolares não diferem conforme a categoria da prossisão dos pais.

##########FIM ESTUDO SOBRE O DESEMPENHO X A PROFISSÃO DOS PAIS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O MOTIVO DE ESCOLHA DA ESCOLA

# Diagrama
# Ha: As médias não se diferem conforme o motivo de escolha do colégio.
# Ha: Ao menos uma das médias se diferem conforme o motivo de escolha do colégio.

d1 <- group_by(.data = data_students, reason) %>% summarise(media=mean(G3),
                                                            var = var(G3))
d1

par(las=2)
par(mar=c(7,6,8,4))
boxplot(G3 ~ reason, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.9,
        main = 'Análise Bivariada. Desempenho Escolar x Motivo Excolha da Escola.',
        names = c('Curso', 'Perto de Casa','Outros','Reputação'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#CONSIDERANDO VARIÂNCIAS IGUAIS
aovt <- aov(G3 ~ reason, data = data_students)
summary(aovt)
#P-VALUE = 0.102

#CONSIDERANDO VARIÂNCIAS DIFERENTES
welch_anova_test(G3 ~ reason, data = data_students)
#P-VALUE=0.092

rm(d1, aovt)
#CONCLUSÃO
# As médias escolares não diferem com relação ao motivo da escolha da escola.

##########FIM ESTUDO SOBRE O DESEMPENHO X MOTIVO DE ESCOLHA DA ESCOLA.


##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X GUARDA PARENTAL

#H0: As médias escolares não se diferem em função do tipo de guarda parental.
#Ha: Ao menos uma das médias escolares se difere em função do tipo de guarda parental.


d1 <- group_by(.data = data_students, guardian) %>% summarise(count=n(),
                                                              media=mean(G3),
                                                              sd=sd(G3),
                                                              var=var(G3))
d1

par(las=1)
par(mar=c(7,5,2,5))
boxplot(data_students$G3 ~ data_students$guardian,
        col = hcl.colors(3, palette = 'Hawaii'),
        names = c('Pai','Mãe','Outros'),
        main = 'Análise Bivariada',
        ylab = 'Desempenho (0 - 20)',
        xlab = '')


#Aplicando a análise de ANOVA para a comparação de mais de 2 médias

# Supondo variâncias iguais
one_way_anova <- aov(G3 ~ guardian, data = data_students)
summary(one_way_anova)
# p-value = 0.205. Hipótese nula(H0) não pode ser descartada.

# Supondo variâncias diferentes
welch_anova_test(G3 ~ guardian, data = data_students)
# p-value = 0.226. Hipótese nula(H0) não pode ser descartada.

#CONCLUSÃO
# Os resultados mostram que as médias escolares não diferem com relação ao tipo de guarda parental dos alunos.
# Desta forma concluímos que o tipo de guarda parental não interefere no desempenho escolar dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X GUARDA PARENTAL



##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O SUPORTE FAMILIAR EDUCACIONAL

# Diagrama
# H0: A média de alunos que possuem suporte educacional familiar é igual a média de alunos que não possuem.
# Ha: A média de alunos que possuem suporte educacional familiar é diferente da média de alunos que não possuem.

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
        main = 'Análise Bivariada. Desempenho Escolar x  Suporte Educacional Familiar',
        names = c('Não', 'Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#TESTANDO AS VARIÂNCIAS
var.test(G3 ~ famsup, data = data_students)
#P-VALUE = 0.798. VARIÂNCIAS NÃO SÃOP DIFERENTES.

t.test(G3 ~ famsup, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.437

rm(d1, aovt)
#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5% PODEMOS REJEITAR A HIPÓTESE ALTERNATIVA, DESTA FORMA CONCLUÍMOS 
#COM 95% DE CONFIANÇA QUE O SUPORTE EDUCACIONAL FAMILIAR NÃO INTERFERE NO DESEMPENHO DOS ALUNOS.
##########FIM ESTUDO SOBRE O DESEMPENHO X O SUPORTE EDUCACIONAL FAMILIAR..



##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM OU NÃO REFORÇO ESCOLAR
# Diagrama
# H0: A média escolar de alunos que não possuem reforço escolar é menor ou igual a média escolar de alunos que possuem.
# Ha: A média escolar de alunos que não possuem reforço escolar é maior do que a média escolar de alunos que possuem.

par(mar=c(6,7,6,2))
par(las=1)
boxplot(G3 ~ schoolsup, data = data_students,
        col = terrain.colors(5),
        cex.main = 0.8,
        main = 'Análise Bivariada. Alunos que Possuem Reforço Escolar.',
        names = c('Não','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

d1 <- group_by(.data = data_students, schoolsup) %>% summarise(count=n(),
                                                               media=mean(G3),
                                                               sd=sd(G3),
                                                               var=var(G3))
d1

#TESTANDO A VARIÂNCIA
var.test(G3 ~ schoolsup, data = data_students, alternative = 'two.sided')
#P-VALUE <<<<<<<< 5%. VARIÂNCIAS SÃO DIFERENTES.

t.test(G3 ~ schoolsup, data = data_students, alternative = 'greater', var.equal = FALSE)
#P-VALUE = 0.0098

#Como o P-valor é menor que 5% rejeitarmos a hipótese nula em favor de Ha. 
#Portanto, concluímos que alunos que não possuem reforço escolar possuem média escolar superior 
# a alunos que fazem o reforço.

##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM REFORÇO ESCOLAR


# Diagrama
# H0: A média escolar de alunos que consomem álcool acima da média é igual a média escolar de alunos que não o fazem.
# Ha: A média escolar de alunos que consomem álcool acima da média é diferente a média escolar de alunos que não o fazem.

#MÉDIA GERAL DE CONSUMO DE ALCOOL DURANTE A SEMANA # 1.481013
mean(as.numeric(data_students$Dalc))
#MÉDIA GERAL DE CONSUMO DE ALCOOL NO FIM DE SEMANA #2.291139
mean(as.numeric(data_students$Walc))
#MÉDIA GERAL DE CONSUMO DE ALCOOL POR SEMANA #1.886076
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
        main = 'Análise Bivariada. Níveis Consumo de Álcool na Semana x Desempenho',
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
        main = 'Análise Bivariada. Consumo de Álcool Acima da Média',
        names = c('Não','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = '',
        border = 'gray20')

#TESTANDO A VARIÂNCIA
var.test(G3 ~ consome_alcool_acima_media, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.001362. VARIÂNCIAS SÃO DIFERENTES.

t.test(G3 ~ consome_alcool_acima_media, data = data_students, alternative = 'two.sided', var.equal = FALSE)
#P-VALUE <<<<<< 0.2195.


#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5% PODEMOS REJEITAR A HIPÓTESE ALTERNATIVA, DESTA FORMA PODEMOS CONCLUIR 
#COM 95% DE CONFIANÇA QUE A O CONSUMO DE ÁLCOOL ACIMA DA MÉDIA NÃO TEM RELAÇÃO NO DESEMPENHO ESCOLAR

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS CONFORME O CONSUMO DE ÁLCOOL ACIMA DA MÉDIA.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.
# Diagrama
# H0: A média escolar de alunos que vivem com pais separados é igual a média escolar de alunos que vivem com pais juntos.
# Ha: A média escolar de alunos que vivem com pais separados é diferente da média escolar de alunos que vivem com pais juntos.

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
        main = 'Situação dos Pais x Desempenho',
        names = c('Separados','Juntos'),
        ylab = 'Desempenho Escolar',
        xlab = 'Situação dos Pais',
        border = 'gray20')

#TESTANDO A VARIÂNCIA
var.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.5929. VARIÂNCIAS NÃO SÃO DIFERENTES.

t.test(G3 ~ Pstatus, data = data_students, alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.125.
#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5% PODEMOS REJEITAR A HIPÓTESE ALTERNATIVA, DESTA FORMA CONCLUÍMOS 
#COM 95% DE CONFIANÇA QUE ALUNOS A SITUAÇÃO CONJUGAL DOS PAIS NÃO INTERFERE NO DESEMPENHO ESCOLAR DOS ALUNOS.
#AS MÉDIA SÃO IGUAIS

##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE MORAM COM PAIS CASADOS OU SEPARADOS.


##########ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM FILHOS NA CRECHE
# Diagrama
# H0: Alunos que possuem filhos na creche escolar possuem desempenho igual do que alunos que não possuem.
# Ha: Alunos que possuem filhos na creche escolar possuem desempenho diferente do que alunos que não possuem.

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
        names = c('Não','Sim'),
        ylab = 'Desempenho Escolar',
        xlab = 'Filho(s) na Creche',
        border = 'gray20')

#TESTANDO SE AS VARIÃNCIAS SÃO DIFERENTES
var.test(G3 ~ nursery, data = data_students, alternative = 'two.sided')
#P-VALUE = 0.9769. VARIÂNCIAS NÃO SÃO DIFERENTES. > 5%

t.test(G3 ~ nursery, data = data_students, alternative = 'two.sided', var.equal = TRUE)

#P-VALUE = 0.3066
#CONCLUSÃO
#COMO P-VALUE É MAIOR QUE 5% PODEMOS REJEITAR A HIPÓTESE ALTERNATIVA EM FAVOR DE H0.
#DESTA FORMA CONCLUÍMOS COM 95% DE CONFIANÇA QUE ALUNOS POSSUEM FILHOS NA CRECHE ESCOLAR POSSUEM 
#O MESMO RENDIMENTO DE ALUNOS QUE NÃO POSSUEM.


##########FIM ESTUDO SOBRE O DESEMPENHO DOS ALUNOS QUE POSSUEM FILHOS NA CRECHE.



##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE LOCOMOÇÃO ATÉ A ESCOLA

#Ha: As médias escolares não se difere em função do tempo de locomoção até a escola.
#Ha: Ao menos uma das médias escolares se difere em função do tempo de locomoção até a escola.

d1 <- group_by(.data = data_students, traveltime) %>% summarise(count=n(),
                                                              media=mean(G3),
                                                              sd=sd(G3),
                                                              var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$traveltime,
        #col = hcl.colors(4, palette = 'Hawaii'),
        main = 'Análise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Tempo de Transporte')
legend("topright", legend = c('< 15 min.', '15 a 30 min.', '30 min a 1hr.','> 1hr.'), fill = hcl.colors(4, palette = 'Hawaii'))


#Aplicando a análise de ANOVA para a comparação de mais de 2 médias
# Supondo variâncias iguais
one_way_anova <- aov(G3 ~ traveltime, data = data_students)
summary(one_way_anova)
# p-value = 0.0199. Hipótese nula(H0) pode ser descartada em favor de Ha.

# Supondo variâncias diferentes
welch_anova_test(G3 ~ traveltime, data = data_students)
# p-value = 0.172. Hipótese nula(H0) não pode ser descartada.

#CONCLUSÃO
#Falhamos em rejeitar H0. Considerando variâncias diferentes, os testes mostram que todas 
#as médias se difere em função do tempo de transporte de casa até o colégio. Desta forma 
#concluímos que o tempo de casa até o colégio não interfere no desempenho dos alunos.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE LOCOMOÇÃO ATÉ A ESCOLA

##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE ESTUDO DURANTE A SEMANA

#H0: As médias escolares se diferem em função do tempo de estudo do aluno durante a semana
#Ha: Ao menos uma das médias escolares se difere em função do tempo de estudo do aluno durante a semana

d1 <- group_by(.data = data_students, studytime) %>% summarise(count=n(),
                                                                media=mean(G3),
                                                                sd=sd(G3),
                                                                var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$studytime,
        col = hcl.colors(4, palette = 'Cividis'),
        main = 'Análise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Tempo de Estudo')
legend("bottomright",  legend = c('1 - Menos que 2hrs.', '2 - 2 a 5hrs', '3 - 5 a 10hrs.','4 - > 10hrs.'), fill = hcl.colors(4, palette = 'Cividis'))


#Aplicando a análise de ANOVA para a comparação de mais de 2 médias
# Supondo variâncias iguais
one_way_anova <- aov(G3 ~ studytime, data = data_students)
summary(one_way_anova)
# p-value = 0.0521. Hipótese nula(H0) não pode ser descartada.

# Supondo variâncias diferentes
welch_anova_test(G3 ~ studytime, data = data_students)
# p-value = 0.196. Hipótese nula(H0) não pode ser descartada.

#CONCLUSÃO
# Os estudos mostram que o tempo de estudo do aluno durante a semana não interfere no seu desempenho escolar.

##########FIM ESTUDO SOBRE O DESEMPENHO ESCOLAR X TEMPO DE ESTUDO DURANTE A SEMANA


##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X QUANTIDADE DE REPROVAÇÕES ANTERIORES

#H0: As médias não se diferem em função de reprovações anteriores na mesma disciplina.
#Ha: Ao menos uma das médias se diferem em função de reprovações anteriores na mesma disciplina.

d1 <- group_by(.data = data_students, failures) %>% summarise(count=n(),
                                                               media=mean(G3),
                                                               sd=sd(G3),
                                                               var=var(G3))
d1

par(mar=c(4,5,2,5))
boxplot(data_students$G3 ~ data_students$failures,
        col = hcl.colors(4, palette = 'Berlin'),
        main = 'Análise Bivariada',
        ylab = 'Desempenho(0 - 20)',
        xlab = 'Quantidade de Reprovações')
legend("topright",  legend = c('0 - Nenhuma reprovação.', '1 - 1 Reprovação.', '2 - 2 Reprovacões.','3 - 3 ou mais Reprovações.'), fill = hcl.colors(4, palette = 'Berlin'))


#Aplicando a análise de ANOVA para a comparação de mais de 2 médias

# Supondo variâncias iguais
one_way_anova <- aov(G3 ~ failures, data = data_students)
summary(one_way_anova)
# p-value <<<<<<<<<< 5%. Hipótese nula(H0) pode ser descartada em favor da Ha.

# Supondo variâncias diferentes
welch_anova_test(G3 ~ failures, data = data_students)
# p-value <<<<<<<< 5% Hipótese nula(H0) pode ser descartada em favor de Ha.

#CONCLUSÃO
#Os testes comprovam que o desempenho dos alunos se difere em função de reprovações anteriores na mesma disciplina. 
#Desta forma podemos concluir que reprovações na disciplina tem relação com o desempenho do aluno.

##########ESTUDO SOBRE O DESEMPENHO ESCOLAR X QUANTIDADE DE REPROVAÇÕES ANTERIORES

##########ESTUDO SOBRE A RELAÇÃO LINEAR DO DESEMPENHO DOS ALUNOS X TOTAL DE FALTAS NAS DISCIPLINA.



lll <- lm(G3 ~  absences, data = data_students)
summary(lll)

plot(data_students$absences, data_students$G3, 
     xlab = 'Número de Faltas', 
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
# H0: As médias escolares dos alunos são iguais entre as duas escolas.
# Ha: As médias escolares dos alunos são diferentes entre as duas escolas.

#TESTANDO A VARIABILIDADE
var.test(G3 ~ school, data = data_students, alternative = 'two.sided')
#P-VALUE=0.4803. HIPÓTESE NULA NÃO DESCARTADA. VARIABILDIADE NÃO SÃO DIFERENTES.

#TESTANDO A HIPÓTESE DE QUE AS MÉDIAS ESCOLARES DOS ALUNOS SÃO DIFERENTES ENTRE AS DUAS ESCOLAS
t.test(G3 ~ school, data = data_students,  alternative = 'two.sided', var.equal = TRUE)
#P-VALUE = 0.3722
glimpse(data_students)
#CONCLUSÃO
#Como o p-value é maior que 5% não podemos rejeitar a hipótese nula. 
# Portanto, concluímos que as médias escolares dos alunos são iguais nas duas escolas.

##########FIM ESTUDO COMPARATIVO DE DESEMPENHO ENTRE AS ESCOLAS



########## VERIFICANDO A RELAÇÃO LINEAR ENTRE AS VARIÁVEIS


#NÚMERO DE REPETIÇÕES DA MESMA DISCIPLINA
tt1 <- lm(G3 ~ failures, data = data_students)
summary(tt1)
#P-VALUE <<<<<<< 5%
#R2 0,13

d1 <- group_by(.data = data_students, failures) %>% summarise(count=n())
d1

plot(data_students$failures, data_students$G3,
     main = '',
     xlab = 'TOTAL REPROVAÇÕES NA DISCIPLINA', 
     ylab = 'DESEMPENHO (0 - 20)', 
     cex.lab = 1.2, pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue',
     ylim = c(0,20), xlim = c(0,3))
abline(lm(G3 ~ failures, data = data_students), col = 'red', lwd=2,lty=2)	

#OBSERVAMOS UMA RELAÇÃO LINEAR INVERSA

#NÚMERO FALTAS NA DISCIPLINA
#H0: Não há relação estatisticamente significante entre as faltas dos alunos e as médias escolares.
#Ha: Existe relação estatisticamente significante entre as faltas dos alunos e as médias escolares.

tt1 <- lm(G3 ~ absences, data = data_students)
summary(tt1)
#P-value = 0.497.
#R2 = 0
# Com o P-value maior que 5% identicamos que não há relação linear entrea a quantidade da faltas e 
#o desempenho do aluno da disciplina matemática.

plot(data_students$absences, data_students$G3,
     main = '', 
     xlab = 'NÚMERO DE FALTAS NA DISCIPLINA', 
     ylab = 'DESEMPENHO (0 - 20)', 
     pch = 21, 
     col = 'dodgerblue4', bg = 'dodgerblue', 
     ylim = c(0,20), 
     xlim = c(0,70))
abline(lm(G3 ~ absences, data = data_students), col = 'red', lwd = 2, lty =2)

#CONCLUSÃO
#P-valor > que 5%, Não rejeita H0. Não apresenta um padrão de lineariedade entre as faltas e o desempenho escolar

