#DISCIPLINA: BANCO DE DADOS E VISUALIZAÇÃO
#TRABALHO 1
#MEMBROS: ANA PAULA, FÁBIO MONTEIRO, LUCAS SENA E MARCOS SOARES
rm(list = ls())      # Clear all variables  
graphics.off()       # Close graphics windows  

library(dplyr)
library(stringr)
library(gmodels)
library(ggplot2)
library(lubridate)

#PATH PARA LEITURA DOS ARQUIVOS
path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Banco de Dados e Visualização/Trabalho/dados/'

#CLIENT
data_client=read.table(paste(path_arquivos,'client.asc',sep=''),sep=";",header=TRUE)

data_client <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/client.asc", sep=";")
View(data_client)
summary(data_client)
data_client = data_client %>% mutate(sexo = ifelse(as.numeric(substr(data_client$birth_number,3,4)) > 12,'F','M'))
data_client$ano_nascimento = as.numeric(substr(data_client$birth_number,1,2))+1900
#data_client$data_nascimento = ifelse(data_client$sexo == 'F', paste(as.numeric(substr(data_client$birth_number,3,4))-50,'/',  str_c(substr(data_client$birth_number,1,2) - 50,sep='')) ,'M')
#data_nascimento=paste(substr(data_client$birth_number,4,5),as.numeric(substr(data_client$birth_number,3,4))-50,sep = ''))
#ifelse(as.numeric(substr(data_client$birth_number,3,4)) > 12,paste(substr(data_client$birth_number,0,2), as.numeric(substr(data_client$birth_number,3,4))-50,substr(data_client$birth_number,4,5),''), data_client$birth_number))
summary(data_client)

#ACCOUNT
data_account=read.table(paste(path_arquivos,'account.asc',sep=''),sep=";",header=TRUE)

data_account <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/account.asc", sep=";")
View(data_account)
summary(data_account)
data_account$frequency = ifelse(data_account$frequency == 'POPLATEK MESICNE','MENSAL',ifelse(data_account$frequency == 'POPLATEK PO OBRATU','TRANSAÇÃO', 'SEMANAL'))
colnames(data_account)[3]='extrato'
#data_account$date=format(as.Date.POSIXct(data_account$date, format = "%y%d%m"))
data_account$extrato = factor(data_account$extrato)
levels(data_account$frequency)

#DISP
data_disp=read.table(paste(path_arquivos,'disp.asc',sep=''),sep=";",header=TRUE)

data_disp<- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/disp.asc", sep=";")
View(data_disp)
data_disp$type = ifelse(data_disp$type=='DISPONENT','DEPENDENTE', ifelse(data_disp$type == 'OWNER', 'PROPRIETÁRIO',''))
data_disp$type = factor(data_disp$type)
summary(data_disp)
    
#CARD
data_card=read.table(paste(path_arquivos,'card.asc',sep=''),sep=";",header=TRUE)

data_card <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/card.asc", sep=";")
View(data_card)
summary(data_card)
data_card$type = factor(data_card$type)
data_card$issued <- paste0("19", data_card$issued) #ADICIONANDO 19 NO ANO....
data_card$issued <- ymd_hms(data_card$issued)

#DISTRICT
data_district=read.table(paste(path_arquivos,'district.asc',sep=''),sep=";",header=TRUE)

data_district <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/district.asc", sep=";")
View(data_district)
colnames(data_district)[1]="distric_id"
colnames(data_district)[2]="nome"
colnames(data_district)[3]="regiao"
colnames(data_district)[4]="habitantes"
colnames(data_district)[5]="mun_menos_500"
colnames(data_district)[6]="mun_500_1999"
colnames(data_district)[7]="mun_2000_9999"
colnames(data_district)[8]="mun_maior_1000"
colnames(data_district)[9]="num_cidades"
colnames(data_district)[10]="proporcao_habitantes_urbanos"
colnames(data_district)[11]="media_salarial"
colnames(data_district)[12]="taxa_desemprego_95"
colnames(data_district)[13]="taxa_desemprego_96"
colnames(data_district)[14]="num_empre_por_1000_hab"
colnames(data_district)[15]="num_crimes_95"
colnames(data_district)[16]="num_crimes_96"
summary(data_district)

#LOAN
data_loan=read.table(paste(path_arquivos,'loan.asc',sep=''),sep=";",header=TRUE)

data_loan <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/loan.asc", sep=";")
View(data_loan)
data_loan$status = ifelse(data_loan$status=='A','FINALIZADO',ifelse(data_loan$status=='B','INADIMPLENTE',ifelse(data_loan$status=='C','ADIMPLENTE','ATRASADO')))
data_loan$status = factor(data_loan$status)
data_loan$date = ymd(data_loan$date)
data_loan$amount = as.double(data_loan$amount)

#ORDER
data_order=read.table(paste(path_arquivos,'order.asc',sep=''),sep=";",header=TRUE)

data_order <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/order.asc", sep=";")
View(data_order)
data_order$k_symbol =  factor(data_order$k_symbol)
data_order$amount =  as.double(data_order$amount)
data_order$k_symbol = ifelse(data_order$k_symbol=='POJISTNE', 'SEGURO',ifelse(data_order$k_symbol=='SIPO', 'DESPESAS GERAIS',ifelse(data_order$k_symbol=='UVER', 'EMPRÉSTIMO','NA')))

#TRANSCTION
data_trans=read.table(paste(path_arquivos,'trans.asc',sep=''),sep=";",header=TRUE)


data_trans <- read.csv("/Volumes/GoogleDrive/Meu Drive/0CIENCIA DE DADOS/MBA FGV Analytics and Big data /0Banco de Dados/czech_data/trans.asc", sep=";")
View (data_trans)
data_trans$type = ifelse(data_trans$type == 'PRIJEM', 'CRÉDITO','DÉBITO')
#data_trans$k_symbol = ifelse(data_trans$k_symbol == "",'',data_trans$k_symbol)
data_trans$k_symbol = factor(data_trans$k_symbol)
levels(data_trans$k_symbol)
summary(data_trans)


data_trans$operation = ifelse(data_trans$operation == 'VYBER KARTOU', 'SAQUE CARTÃO DE CRÉDITO',ifelse(data_trans$operation == 'VKLAD', 'DEPÓSITO EM DINHEIRO',ifelse(data_trans$operation == 'PREVOD Z UCTU', 'RECEBIMENTO DE BANCO',ifelse(data_trans$operation == 'VYBER', 'SAQUE EM DINHEIRO','REMESSA'))))
data_trans$operation = factor(data_trans$operation)
data_trans$date = ymd(data_trans$date)
data_trans$amount = as.double(data_trans$amount)
data_trans$balance = as.double(data_trans$balance)

#ANÁLISE DA CARTEIRA DE CLIENTES
par(mar = c(5,3,2,3))
par(las = 1)
vrPorcentagemM <- round((nrow(subset(data_client, sexo == 'M')) / nrow(data_client))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_client, sexo == 'F')) / nrow(data_client))*100,digits = 2)
barplot(table(data_client$sexo),
        main = 'Distribui??o de Clientes por Sexo.',
        ylab = 'Total de Clientes', 
        ylim = c(0,3000),
        col = hcl.colors(2, palette = "Peach"),
        names.arg = c(str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemM,'%',sep = '')))
legend("topright", legend = c('Feminino', 'Masculino'), fill = hcl.colors(2, palette = "Peach"))

par(mar = c(5,4,7,5))
hist(data_client$ano_nascimento,
     main = 'Distribui??o de Clientes por Ano de Nascimento.',
     xlab = 'Ano de Nascimento.',
     ylab = 'Total de Clientes',
     ylim = c(10,600),
     col = hcl.colors(20, palette = "Red-Blue"))

#DISTRIBUIÇÃO DE CLIENTES POR DISTRITO

data_client_district = merge(data_client, data_district, all = T)
d1 <- data_client %>% group_by(district_id) %>% summarise(count = n())
d1$nome = data_district$nome
d1=head(d1,20)#TOP 20 dis
par(mar = c(8,6,3,3))
par(las = 2)
barplot(height=d1$count,
        main = 'Top 20 Distritos por Total de Clientes.',
        names=d1$nome, 
        col= as.vector(terrain.colors(20)), 
        yaxp=c(0, max(d1$count), 10))

#MÉDIA DE RENDA POR REGIAO

par(mar=c(10,6,3,3))
par(las=2)
boxplot(media_salarial ~ regiao, data = data_district,
        main = 'M?dia de Renda Por Regi?o',
        cex.main = 1.5,
        xlab = '',
        ylab = '',
        col = hcl.colors(10, palette = 'Cold'),
        border = 'gray20',
        cex.axis = 1.2,
        cex.lab = 1.2,
        ylim = c(min(data_district$media_salarial), max(data_district$media_salarial)))

#TOTAL DE CLIENTES x TOTAL DE HABITANTES POR REGIÃO
d1 <- data_client_district %>% group_by(regiao) %>% summarise(clientes = n())
d2 <- data_district %>% group_by(regiao) %>% summarise(total_habitantes = sum(habitantes))
d1$habitantes = d2$total_habitantes
barplot(t(as.matrix(d1[, 2:3])), 
        beside = TRUE,
        main = 'Total de Clientes x Total de Habitantes por Regi?o.',
        cex.main = 1.2,
        names.arg = d1$regiao,
        col = hcl.colors(2, palette = "Temps"),
        ylab = "",
        xlab = "")
legend("topleft", legend = c('Clientes', 'Habitantes'), fill = hcl.colors(2, palette = "Temps"))

#PERCENTUAL DE CLIENTES COM CARTAO DE CREDITO

dc1 <- data_client %>% inner_join(data_disp, by ="client_id") %>% left_join(data_card, by ="disp_id")
dc2 <- group_by(.data = dc1, POSSUI_CARTAO=is.na(card_id)) %>% summarise(count=n())
par(las = 1)
vrPorcentagemM <- round((nrow(subset(dc1, is.na(card_id))) / nrow(dc1))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(dc1, !is.na(card_id))) / nrow(dc1))*100,digits = 2)
par(las = 1)
barplot(table(dc2$POSSUI_CARTAO), height = dc2$count,
        main = 'Distribui?o de Clientes com Cart?o de Cr?dito.',
        xlab = 'Possui Cart?o', 
        ylab = 'Total de Clientes', 
        ylim = c(0,5000),
        col = hcl.colors(2, palette = "Cividis"),
        names.arg = c(str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemM,'%',sep = '')))
legend("topleft", legend = c('N?O', 'SIM'), fill = hcl.colors(2, palette = "Cividis"))

#DISTRIBUIÇÃO DE CLIENTES COM CARTÃO POR TIPO
dc4 = filter(.data = dc1, !is.na(card_id))
dc5 <- filter(.data = dc4, !is.na(card_id)) %>% group_by(type.y) %>% summarise(count = n())
vrPorcentagemC <- round((nrow(subset(dc4, type.y == 'classic')) / nrow(dc4))*100,digits = 2)
vrPorcentagemG <- round((nrow(subset(dc1, type.y == 'gold')) / nrow(dc1))*100,digits = 2)
vrPorcentagemJ <- round((nrow(subset(dc1, type.y == 'junior')) / nrow(dc1))*100,digits = 2)
par(las = 1)
barplot(table(dc5$type.y), height = dc5$count,
        main = 'Distribui??o de Clientes com Cart?o por Tipo',
        xlab = 'Tipo de  Cart?o', 
        ylab = 'Total de Clientes', 
        ylim = c(0,800),
        col = hcl.colors(3, palette = "Emrld"),
        names.arg = c(str_c('',vrPorcentagemC,'%',sep = ''),str_c('',vrPorcentagemG,'%',sep = ''),str_c('',vrPorcentagemJ,'%',sep = '')))
legend("topright", legend = c('Classic', 'Gold', 'Junior'), fill = hcl.colors(3, palette = "Emrld"))

#CLIENTES QUE MAIS MOVIMENTARAM
options("scipen" = 100, "digits"= 2)
ddx <- data_client %>% inner_join(data_disp, by ="client_id") %>% left_join(data_trans, by ="account_id")
ddx <- subset(ddx, type.y == 'CRÉDITO') #SOMENTE TRANSAÇÕES DE CRÉDITO
d1 <- ddx %>% group_by(client_id) %>% summarise(total_transacoes = n(),
                                                montante_transacoes = as.double(sum(amount))) %>%  arrange(desc(montante_transacoes))
d1 = head(d1,30)
par(mar = c(4,6,5,2))
par(las = 2)
barplot(d1$montante_transacoes, 
        main = 'Top 30 Clientes que mais movimentaram.',
        cex.main = 1.2,
        names.arg = d1$client_id,
        col = hcl.colors(1),
        ylab = "",
        ylim = c(0,4000000),
        xlab = "Id do Cliente")

#PERCENTUAL DE CLIENTES COM EMPRÉSTIMO

d1 <- data_client %>% inner_join(data_disp, by = "client_id") %>% left_join(data_loan, by = "account_id")
d1
vrPorcentagemP <- round((nrow(subset(d1, !is.na(loan_id))) / nrow(d1))*100,digits = 2)
vrPorcentagemN <- round((nrow(subset(d1, is.na(loan_id))) / nrow(d1))*100,digits = 2)
d2 <- group_by(.data = d1, POSSUI_EMPRESTIMO = !is.na(loan_id)) %>% summarise(count=n())
d2
par(las=1)
barplot(d2$count, 
        main = 'Percentual de Clientes que possuem e/ou J? realizaram empr?stimo.',
        cex.main = 1,
        col = hcl.colors(2, palette = "Vik"),
        ylim = c(0,5000),
        ylab = "",
        names.arg = c(str_c('',vrPorcentagemN,'%',sep = ''),str_c('',vrPorcentagemP,'%',sep = '')))
legend("topright", legend = c('N?o Possui', 'Possui/J? realizou'), fill = hcl.colors(2, palette = "Vik"))

#TOTAL DE EMPRÉSTIMOS x POR SITUAÇÃO

par(las=1)
vrPorcentagemI <- round((nrow(subset(data_loan, status == 'INADIMPLENTE')) / nrow(data_loan))*100,digits = 2)
vrPorcentagemAT <- round((nrow(subset(data_loan, status == 'ATRASADO')) / nrow(data_loan))*100,digits = 2)
vrPorcentagemF <- round((nrow(subset(data_loan, status == 'FINALIZADO')) / nrow(data_loan))*100,digits = 2)
vrPorcentagemAD <- round((nrow(subset(data_loan, status == 'ADIMPLENTE')) / nrow(data_loan))*100,digits = 2)
d1 <- data_loan %>% group_by(status) %>% summarise(count=n(), soma_valor=sum(amount)) %>% arrange(soma_valor)
par(mar=c(4,6,4,4))
barplot(d1$soma_valor, 
        main = 'Distribui??o de Empr?stimos por Status.',
        cex.main = 1.2,
        col = hcl.colors(4, palette = "Temps"),
        ylim = c(0,70000000),
        ylab = "",
        names.arg = c(str_c('',vrPorcentagemI,'%',sep = ''),str_c('',vrPorcentagemAT,'%',sep = ''),str_c('',vrPorcentagemF,'%',sep = ''),str_c('',vrPorcentagemAD,'%',sep = '')),
        xlab = "")
legend("topleft", legend = d1$status , fill = hcl.colors(4, palette = "Temps"))

#DISTRIBUIÇÃO DE TRANSAÇÕES POR QUANTIDADE E VALOR
d1 <- data_trans %>% group_by(type) %>% summarise(count = n(), valor = sum(amount))
d1 #TABELA EXPLICATIVA
d1$valor = log(d1$valor)
d1$count = log(d1$count)
par(mar=c(5,3,3,3))
par(las=1)
barplot(t(as.matrix(d1[, 2:3])), 
        beside = TRUE,
    main = 'Distribui??o de Transa??es por Quantidade e Valor.',
        cex.main = 1.2,
        names.arg = d1$type,
        col = hcl.colors(2, palette = "Blue-Red 3"),
        ylab = "#Freq. Log",
        ylim = c(0,25),
        xlab = "Escala Logaritimica")
legend("topleft", legend = c('Total Transa??es', 'Valor Total'), fill = hcl.colors(2, palette = "Blue-Red 3"))

#DISTRIBUIÇÃO DE TRANSAÇÕES POR OPERATION
d1 <- data_trans %>% group_by(operation) %>% summarise(count = n(), valor = sum(amount))
d1 #TABELA EXPLICATIVA
d1$count = log(d1$count)
d1$valor = log(d1$valor)
par(mar=c(12,3,3,3))
par(las=2)
barplot(t(as.matrix(d1[, 2:3])), 
        beside = TRUE,
        main = 'Distribui??o de Transa??es por Opera??o',
        cex.main = 1.2,
        names.arg = d1$operation,
        ylim = c(0,30),
        col = hcl.colors(2, palette = "Blue-Red 3"),
        xlab = "")
legend("topleft", legend = c('Total Transa??es', 'Valor Total'), fill = hcl.colors(2, palette = "Blue-Red 3"))

#DISTRIBUIÇÃO DE TRANSAÇÕES POR TIPO

d1 <- data_trans %>% group_by(type) %>% summarise(valor = sum(amount))
d1
par(mar=c(3,8,3,3))
par(las=1)
dc = subset(data_trans, type == 'CR?DITO')
dd = subset(data_trans, type == 'D?BITO')
vrPorcentagemC <- round(sum(dc$amount)/sum(data_trans$amount),2)
vrPorcentagemD <- round(sum(dd$amount)/sum(data_trans$amount),2)
barplot(d1$valor, 
        main = 'Distribui??o de Transa??es por Tipo e Valor.',
        cex.main = 1.2,
        names.arg = c(str_c('',vrPorcentagemC,'%',sep = ''),str_c('',vrPorcentagemD,'%',sep = '')),
        yaxp=c(0, max(d1$valor), 5),
        col = hcl.colors(2, palette = "Terrain 2"),
        xlab = "")
legend("topright", legend = d1$type, fill = hcl.colors(2, palette = "Terrain 2"))

#MONTANTE DE TRANSAÇÕES ANUAL
d1 <- data_trans %>% group_by(ano=year(data_trans$date)) %>% summarise(valor_anual = sum(amount))
d1
par(mar=c(3,8,3,3))
par(las=1)
barplot(d1$valor_anual, 
        main = 'Distribui??o de Transa??es por Ano.',
        cex.main = 1.2,
        names.arg = d1$ano,
        yaxp=c(0, max(d1$valor_anual), 10),
        col = hcl.colors(6, palette = "GnBu"),
        xlab = "")
legend("topleft", legend = d1$ano, fill = hcl.colors(6, palette = "GnBu"))

#PROXIMO ENCONTRO... ANALISAR KSYMBOL
levels(data_trans$k_symbol)

