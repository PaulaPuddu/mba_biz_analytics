#DISCIPLINA: ANÁLISE EXPLORATÓRIA DE DADOS
#TRABALHO 1
#INTEGRANTES:Ana Paula, Fábio Monteiro, Lucas Sena, Marcos Soares.
#11/07/2021

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(rcompanion)
library(stringr)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(rcompanion)
library(summarytools)



#ARQUIVOS ORIGEM
path_arquivos <- '/Users/soares/Documents/pessoal/fgv/mba bussiness analytics & BigData/Disciplinas/Análise exploratória dos dados/Trabalho 1/archive/'
data_airlines <- read.csv(paste(path_arquivos,'nyc_airlines.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_airlines <- na.omit(data_airlines)
data_flights <- read.csv(paste(path_arquivos,'nyc_flights.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_flights <- na.omit(data_flights)
data_planes <- read.csv(paste(path_arquivos,'nyc_planes.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
#data_planes <- na.omit(data_planes)
data_wheather <- read.csv(paste(path_arquivos,'nyc_weather.csv',sep = ''), sep = ',', header = TRUE, stringsAsFactors = TRUE)
data_wheather <- na.omit(data_wheather)

#TRATAMENTO DA BASE DE VOOS
#CONSIDERAR PARA O ESTUDO SOMENTE VOOS REALIZADOS(AIRTIME > 0)
data_flights <- data_flights %>% filter(!is.na(data_flights$air_time) & as.numeric(data_flights$air_time) > 0)
data_flights$distance <- data_flights$distance * 1.609344 #CONVERSAO PARA KM
data_flights <- data_flights %>% mutate(data_hora_voo_programada = as.POSIXct(paste(str_trim(data_flights$year),'-',str_trim(data_flights$month),'-', str_trim(data_flights$day),' ', str_trim(data_flights$hour),':', str_trim(data_flights$minute), ':00', sep = ""), format = "%Y-%m-%d %H:%M:%OS"))
data_flights <- data_flights %>% mutate(data_hora_voo_real = ymd_hms(data_flights$data_hora_voo_programada) + dminutes(data_flights$dep_delay))
data_flights <- data_flights %>% mutate(data_hora_chegada_real = ymd_hms(data_flights$data_hora_voo_real) + dminutes(data_flights$air_time))
data_flights <- data_flights %>% mutate(dia_da_semana = weekdays(data_flights$data_hora_voo_real, abbreviate = TRUE))
data_flights$year = factor(data_flights$year)
data_flights$month = factor(data_flights$month)
data_flights$day  <- factor(data_flights$day)
data_flights$hour <- factor(data_flights$hour)
data_flights$minute  <- factor(data_flights$minute)
data_flights$carrier <- factor(data_flights$carrier)
data_flights$origin  <- factor(data_flights$origin)
data_flights$dest  <- factor(data_flights$dest)
data_flights$tempo_em_voo  <- as.numeric(data_flights$air_time / 60)
data_flights$tailnum  <- factor(data_flights$tailnum)
data_flights$flight  <- factor(data_flights$flight)
data_flights$month = factor(data_flights$month,c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Jan', 'Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez'))
data_flights$dia_da_semana  <- factor(data_flights$dia_da_semana)
data_flights <- na.omit(data_flights)

summary(data_flights)

#TRATAMENTO DA BASE DE AERONAVES
data_planes$tailnum <- factor(data_planes$tailnum)
data_planes$year <- factor(data_planes$year)
data_planes$type  <- factor(data_planes$type)
data_planes$manufacturer <- factor(data_planes$manufacturer)
data_planes$model <- factor(data_planes$model)
data_planes$engines <- factor(data_planes$engine)
data_planes$seats <- factor(data_planes$seats)
data_planes$speed <- factor(data_planes$speed)#TODO CONVERTER EM KM/H
data_planes$engine <- factor(data_planes$engine)

summary(data_planes)

#TRATAMENTO DA BASE DE DADOS DE PLANOS
data_planes <- data_planes %>% filter(!is.na(data_planes$year) & data_planes$year != "")

#TRATAMENTO DOS DADOS DE  CLIMA
data_wheather <- data_wheather %>% filter(!is.na(data_wheather$temp) & data_wheather$temp != "")
data_wheather$visib <- factor(data_wheather$visib)
data_wheather$year <- factor(data_wheather$year)
data_wheather$month <- factor(data_wheather$month)
data_wheather$day <- factor(data_wheather$day)
data_wheather$wind_speed <- data_wheather$wind_speed * 1.609344 #CONVERSAO PARA KM
data_wheather <- data_wheather %>% mutate("data" = as.POSIXct(paste(str_trim(data_wheather$year),'-',str_trim(data_wheather$month),'-', str_trim(data_wheather$day),' ', str_trim(data_wheather$hour),':00:00', sep = ""), format = "%Y-%m-%d %H:%M:%OS"))

#ESTUDO GERAL SOBRE OS VOOS
#HORÁRIO DE MAIOR DEMANDA DE VOOS
dd <- data_flights %>% count(hour) %>% arrange(n)
plot(dd, main = 'Horário de maior demanda de voos', cex.main = 1.5,
     xlab = 'Horários', cex.lab = 1.2,
     ylab = 'Voos')

#DIA DA SEMANA DE MAIOR MOVIMENTO
data_flights_por_dia_semana <- data_flights %>% count(data_flights$dia_da_semana) %>% arrange((n))
data_flights_por_dia_semana$`data_flights$dia_da_semana`  <- factor(data_flights_por_dia_semana$`data_flights$dia_da_semana`)

barplot(table(data_flights$dia_da_semana),
        main = 'Dia da semana de maior movimento de voos',
        cex.main = 1.2,
        xlab = 'Dia da Semana',
        ylab = 'Total de Voos',
        cex.lab = 1.2,
        ylim = c(0,50000), 
        col = 'cyan', 
        border = 'black')

plot(data_flights_por_dia_semana, frame = TRUE,
     main = 'Dia da semana de maior movimento de voos',
     cex.main = 1.5,
     xlab = 'Dia da Semana',
     ylab = 'Total de Voos',
     cex.lab = 1.2,
     lwd = 2, col = 'darkorange')

polygon(density(data_flights_por_dia_semana$`data_flights$n`))

plot(data_flights_por_dia_semana, frame = FALSE)

hist(data_flights_por_dia_semana$n,breaks = 10)

     hist(data_flights_por_dia_semana$n,
     main = 'Histograma',
     cex.main = 1.2,
     xlab = "CIA Aérea",
     ylab = "Total de Voos",
     cex.axis = 1.0, 
     cex.lab = 1.0,
     ylim = c(0,50000), col = "red", border = "blue")

transform(data_flights,year = as.numeric(year),
          month = as.numeric(month),
          day = as.numeric(day),
          hour = as.numeric(hour),
          time_hour = as_datetime(data_flights$time_hour))

#MES DE MAIOR DEMANDADE VOOS
data_mes_maior_demanda <- data_flights %>% count(origin, month)
data_mes_maior_demanda <- data_mes_maior_demanda %>% arrange(origin,n)
data_mes_maior_demanda$month <- factor(data_mes_maior_demanda$month)
data_mes_maior_demanda2 <- data_flights %>% count(month) %>% arrange(n)

#MÉDIA DE DURAÇÃO DE VOOS POR EMPRESA E AEROPORTO
View(data_flights  %>% group_by(origin) %>% 
       summarise(media_duracao_em_voo = (median(air_time, na.rm = TRUE))/60) %>%
       arrange(origin, media_duracao_em_voo))

data_flights.media_voos <- data_flights %>% summarise(data_flights$air_time)
mean(data_flights$air_time)
data_flights$air_time / 60

#MÉDIA DA DURAÇÃO DE VOO POR EMPRESA AÉREA E AEROPORTO
View(data_flights  %>% group_by(carrier,origin) %>% 
       summarise(media_duracao_voo = median(air_time, na.rm = TRUE)) %>%
       arrange(origin, media_duracao_voo))

#MÉDIA DAS DISTÂNCICAS DE VOOS POR AEROPORTO
View(data_flights  %>% group_by(origin) %>% 
  summarise(media_distancia = median(distance, na.rm = TRUE)) %>%
  arrange(origin, media_distancia))

#MÉDIA DAS DISTÂNCICAS DE VOOS POR EMPRESA AÉREA E AEROPORTO
View(data_flights  %>% group_by(origin, carrier) %>% 
       summarise(media_distancia = median(distance, na.rm = TRUE)) %>%
       arrange(origin, media_distancia))

dd1 <- data_flights %>% filter(data_flights$distance > 0)
dd1 <- data_flights %>% count(data_flights$carrier)

dd1 %>% summarise(data_flights$distance)
mean(dd1$distance, na.rm = TRUE)

#MÉDIA MENSAL DE VOOS
mean(data_mes_maior_demanda$n)
#MÁXIMO DE VOOS EM UM MES
max(data_mes_maior_demanda$n)
#MÍNIMO DE VOOS EM UM MES
min(data_mes_maior_demanda$n)

#DESVIO PADRÃO
sd(data_mes_maior_demanda$n)
#VARIÂNCIA MENSAL DE VOOS
var(data_mes_maior_demanda$n)


#ESTUDO SOBRE ATRASOS DE VOOS
#QUANTIDADE DE VOOS ATRASADOS POR CARRIER
data_voos_atrasados_por_carrier <- data_flights %>% filter(data_flights$dep_delay > 0)

dd1 <- count(data_voos_atrasados_por_carrier, carrier)
dd2 <- data_flights %>% count(carrier)
dd3 <- merge(dd1, dd2, by = 'carrier')

#MELHOR E PIOR AEROPORTO EM PONTUALIDADE

dd1 <- data_flights %>% count(origin)
dd2 <- data_flights %>% filter(dep_delay > 0 ) %>% count(origin)
dd3 <- merge(dd1,dd2, by = 'origin')
dd3 <- dd3 %>% mutate(percentual= (n.x / n.y))


td <- prop.table(table(data_mes_maior_demanda$origin, data_mes_maior_demanda$month))
hist(data_mes_maior_demanda$month)

#MÉDIA DE TEMPERATURA DOS VOOS
mean(data_wheather$temp)
summary(data_wheather)
str(data_wheather)
#data_airlines <- data_airlines %>% arrange(data_airlines$carrier) #[order(data_airlines$carrier),]

#ESTUDO SOBRE OS VOOS
t_voos_origin <- table(data_flights$origin)
t_voos_origin
addmargins(t_voos_origin)
round(prop.table(t_voos_origin),digits = 2)
#PERCENTUAL DE VOOS POR ORIGEM(AEROPORTO)
round(100*prop.table(t_voos_origin),digits=0)

t_voos_origin_x_carrier <- table(data_flights$origin, data_flights$carrier)
t_voos_origin_x_carrier
addmargins(t_voos_origin_x_carrier)
round(prop.table(t_voos_origin_x_carrier,2)*100,digits=0)
#PERCENTUAL DE VOOS POR ORIGEM(AEROPORTO) E CARRIER
barplot(t_voos_origin_x_carrier, xlab = 'Carrier',ylab = 'Origem',main = "Distribuição de voos por aeroporto e carrier",
        col = c("darkblue","lightcyan","red")
        ,legend = rownames(t_voos_origin_x_carrier), args.legend = list(x = "topleft"))

barplot(prop.table(t_voos_origin_x_carrier,2)*100,
        xlab = 'Carrier',ylab = 'Percentuais',main = "Percentual por carrier nos aeroportos",beside = T,col = c("darkblue","lightcyan"),
        legend = rownames(t_voos_origin_x_carrier), args.legend = list(x = "topleft"))

#ESTUDO SOBRE OS ATRASOS DOS VOOS
data_flights_lazy <- data_flights %>% filter(!is.na(data_flights$dep_delay) & data_flights$dep_delay > 0)
data_flights_lazy_by_carrier <- data_flights_lazy %>% group_by(data_flights_lazy$carrier) %>% arrange(data_flights_lazy$month)

View(data_flights %>% count(data_flights$carrier) %>% summarise(data_flights$month))
summary(data_flights)
str(data_flights)
levels(data_flights$origin) # checking
levels(data_flights$dest) # checking
#data_flights = data_flights %>% mutate(flight_time =ymd(day + '/' + month + '/' + year + ' ' + hour + ':' + minute + ':00'))
#df$Invoice <- ymd(df$Invoice) # data da fatura
#df$Due     <- ymd(df$Due)     # data do vencimento
#df$Payment <- ymd(df$Payment) # data de pagamento
mean(data_flights$month)


#Análise de voos originados do aeroporto JFK
#ITEM 1.1.1.1
data_jfk_aero <- data_flights %>% filter(data_flights$origin == 'JFK' & data_flights$arr_time > 0)
data_jfk_aero <- data_jfk_aero %>% filter(!is.na(data_jfk_aero$origin) | data_jfk_aero$origin != "")
data_jfk_aero <- data_jfk_aero %>% filter(!is.na(data_jfk_aero$carrier) | data_jfk_aero$carrier != "")
levels(data_jfk_aero$month)
levels(data_jfk_aero$origin)
data_jfk_aero %>%
  group_by(data_jfk_aero$month) %>%
  summarize(origin = sum(data_jfk_aero$origin)) %>%
  ungroup()

dd1 <- data_jfk_aero %>% group_by_all(data_jfk_aero$carrier)


ttt <- prop.table(table(data_jfk_aero$origin,data_jfk_aero$carrier),margin = 2)
barplot(ttt,
        main = 'BARPLOT: Análise Bivariada', cex.main= 1.5,
        cex.names = 1.2, cex.lab = 1.2, cex.axis = 1.2, ylim = c(0,1),
        xlab = 'Aeroporto', ylab = 'Frequência de Voos (%)',
        col = c('chartreuse','chartreuse4'), border = 'gray20')
legend("left", legend = row.names(ttt), fill = c('chartreuse','chartreuse4'))

data_jfk_aero <- data_jfk_aero %>% count(data_jfk_aero$carrier) %>% arrange(n)
colnames(data_jfk_aero)[1] <- "carrier"
data_jfk_aero <- left_join(data_airlines,data_jfk_aero,by = "carrier",all.x = T)
View(data_jfk_aero)


#tbl <- prop.table(table(data_flights$carrier,data_flights$origin),margin = 2)
#barplot(tbl,
#        main = 'BARPLOT: Análise Bivariada', cex.main= 1.5,
#        cex.names = 1.2, cex.lab = 1.2, cex.axis = 1.2, ylim = c(0,1),
#        xlab = 'Aeroporto', ylab = 'Frequência de Voos (%)',
#        col = c('chartreuse','chartreuse4'), border = 'gray20')
#legend("left", legend = row.names(tbl), fill = c('chartreuse','chartreuse4'))

barplot(table(data_jfk_aero$carrier),
        main = 'Aeroporto JFK: Distribuição de voos por carrier', 
        cex.main = 1.5,
        cex.names = 1.2,
        xlab = 'Carrier',
        ylab = 'Total de Voos', 
        cex.axis = 1.0,
        ylim = c(0,60000), col = 'chartreuse', border = 'chartreuse4')

barplot(table(data_ewr_aereo$carrier),
        main = 'Aeroporto EWR: Distribuição de voos por carrier', 
        cex.main = 1.5,
        cex.names = 1.2,
        xlab = 'Carrier',
        ylab = 'Total de Voos', 
        cex.axis = 1.0,
        ylim = c(0,60000), col = 'chartreuse', border = 'chartreuse4')

barplot(table(data_lga_aereo$carrier),
        main = 'Aeroporto LGA: Distribuição de voos por carrier', 
        cex.main = 1.5,
        cex.names = 1.2,
        xlab = 'Carrier',
        ylab = 'Total de Voos', 
        cex.axis = 1.0,
        ylim = c(0,60000), col = 'chartreuse', border = 'chartreuse4')

hist(data_flights$mont,
     main = 'Histograma',
     cex.main = 1.2,
     xlab = "CIA Aérea",
     ylab = "Total de Voos",
     cex.axis = 1.0, 
     cex.lab = 1.0,
     ylim = c(0,50000), col = "red", border = "blue")

data_lga_aereo <- data_flights %>% filter(data_flights$origin == 'LGA' & data_flights$arr_time > 0)
View(data_lga_aereo %>% count(data_lga_aereo$carrier) %>% arrange(n))

data_ewr_aereo <- data_flights %>% filter(data_flights$origin == 'EWR' & data_flights$arr_time > 0)
View(data_ewr_aereo %>% count(data_ewr_aereo$carrier) %>% arrange(n))

addmargins(table(data_flights$carrier, data_flights$origin), 1:2)
addmargins(prop.table(table(data_flights$carrier, data_flights$origin), 1)*100, 2)
data_carrier_origem_props <- prop.table(table(data_flights$carrier, data_flights$origin), 1)*100 
data_carrier_origem_props <- data_carrier_origem_props %>% count(data_carrier_origem_props$carrier)
View(prop.table(table(data_flights$carrier, data_flights$origin), 1)*100)
cramerV(data_flights$carrier, data_flights$origin)
barplot(prop.table(table(data_flights$carrier,data_flights$origin), margin = 1))

P <- prop.table(table(data_flights$carrier,data_flights$carrier), margin=1)
ggplot(as.data.frame(P), aes(x = data_flights$flight, y = 'data_flights$day')) 

data_flights_lazy_by_carrier <- data_flights %>% 
  count(data_flights$origin, data_flights$carrier) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(data_flights_lazy)           # s


gg_prop <- ggplot(data = data.frame()
                  , aes(x = data_flights_lazy_by_carrier$origin, y = data_flights_lazy_by_carrier$prop, fill = data_flights_lazy_by_carrier$carrier)) + 
  scale_y_continuous(labels = data_flights$origin) +    
  labs(x = 'Origens', y = 'NULL', fill = 'Carries', title = 'Proporção de voos por origem e carrier')

gg_prop %+%   # use %+% to add...
  data_flights_lazy_by_carrier    # a dataframe

View(data_jfk_aero %>% group_by(data_jfk_aero$carrier))

#summary(data_jfk_aero$origin)/1e3
#sd(data_jfk_aero$origin)/1e3
#(max(data_jfk_aero$origin) - min(data_jfk_aero$origin))/1e3

#data_jfk_aero %>% 
#  group_by(data_jfk_aero$carrier) %>% 
#  summarise(mean_inc = mean(data_jfk_aero$origin),
#                                       median_inc = median(data_jfk_aero$origin))




View(data_flights_aeros)
#ITEM 1.1.1.2
#QUANTIDADE DE VOOS POR AEROPORTO ORIGEM
#View(data_flights_aeros %>% filter() count(data_flights_aeros$origin) %>% arrange(n))
#QUANTIDADE DE VOOS POR AEROPORTO DESTINO
data_flights_aeros2 = data_flights_aeros %>% group_by(month) %>% 
      summarise(carrier = sum(carrier))
View(data_flights_aeros %>% count(data_flights_aeros$origin,data_flights_aeros$dest) %>% arrange(n))


#ITEM 1.1.1.3
#data_flights_of_airlines <- merge(data_airlines,data_flights,by = "carrier",all.x = FALSE)
data_voos_por_cia_aerea <- left_join(data_airlines,data_flights_aeros,by = "carrier",all.x = FALSE)
View(data_voos_por_cia_aerea)
hist(data_voos_por_cia_aerea$n,
     main = 'Histograma',
     cex.main = 1.5,
     xlab = "CIA Aérea",
     ylab = "Total de Voos",
     cex.axis = 1.2, cex.lab = 1.2,
     ylim = c(0,50000), col = "dodgerblue", border = 'dodgerblue4')


#QUANTIDADE DE VOOS AEROPORTO
#group_by(data_jfk_aero$month,data_jfk_aero$carrier)
data_jfk_voos_por_cia_aerea_por_mes <- data_jfk_aero %>% count(data_jfk_aero$month)

#QUANTIDADE DE VOOS AEROPORTO POR CIA ÁREA NO PERÍODO DE MAIOR MOVIMENTO
data_jfk_voos_por_cia_per_max <- data_jfk_aero %>% filter(data_jfk_aero$month == 7 | data_jfk_aero$month == 8)
barplot(table(data_jfk_voos_por_cia_per_max$carrier),
        main = 'Aeroporto JFK: Demanda de voos dos mêses Jul e Ago.', 
        cex.main = 1.5,
        cex.names = 1.2,
        xlab = 'Carrier',
        ylab = 'Total de Voos', 
        cex.axis = 1.0,
        ylim = c(0,9000), col = 'chartreuse', border = 'chartreuse4')


#GRÁFICO DE 
barplot(table(data_flights$hour),
        main = 'Aeroporto JFK: Movimentação diária de voos.', 
        cex.main = 1.5,
        cex.names = 1.2,
        xlab = 'Carrier',
        ylab = 'Total de Voos', 
        cex.axis = 1.0,
        ylim = c(0,30000), col = 'chartreuse', border = 'chartreuse4')

View(data_jfk_voos_por_cia_aerea %>% count('data_flights$month'))

#data_flights_of_airlines %>% count(data_flights_of_airlines$tailnum)
#TOTAL DE VOOS POR CIA AÉREA
data_total_voo_por_cia_aerea <- data_flights_of_airlines %>% count(data_flights_of_airlines$carrier)
colnames(data_total_voo_por_cia_aerea)[1] <- "cia"
colnames(data_total_voo_por_cia_aerea)[2] <- "total"


View(data_flights_aeros %>% count(data_flights_aeros$carrier)) %>% arrange(n)
#ITEM 1.1.1.3
View(data_flights_aeros %>% group_by(data_total_voo_por_cia_aerea$carrier, origin))

vff <- data_total_voo_por_cia_aerea %>% arrange(desc(data_total_voo_por_cia_aerea$total))
View(vff)
vff$total <- factor(vff$total, levels = vff$total)

vff %>% ggplot( aes(x = vff$total, y = vff$cia) ) +
  geom_segment( aes(x = vff$total ,xend = vff$total, y = vff$cia, yend = vff$cia), color = "grey") +
  geom_point(size = 5, color = "#69b3a2") +
  coord_flip() +
  xlab("")

# lock in factor level order
data_total_voo_por_cia_aerea$cia <- factor(data_total_voo_por_cia_aerea$cia, levels = data_total_voo_por_cia_aerea$cia)
# plot
data_total_voo_por_cia_aerea <- data_total_voo_por_cia_aerea %>% arrange(data_total_voo_por_cia_aerea$total)
ggplot(data_total_voo_por_cia_aerea, aes(x = data_total_voo_por_cia_aerea$total, y = data_total_voo_por_cia_aerea$cia)) + 
  geom_bar(stat = "identity") + coord_flip()

data_total_voo_por_cia_aerea <- arrange(data_total_voo_por_cia_aerea,data_total_voo_por_cia_aerea$total)
data_total_voo_por_cia_aerea %>%
  arrange(data_total_voo_por_cia_aerea$cia) %>%
  ggplot( aes(x = data_total_voo_por_cia_aerea$total, y = data_total_voo_por_cia_aerea$cia) ) +
  geom_segment( aes(x = data_total_voo_por_cia_aerea$total ,xend = data_total_voo_por_cia_aerea$total, y = data_total_voo_por_cia_aerea$cia, yend = data_total_voo_por_cia_aerea$cia), color = "grey") +
  geom_point(size = 5, color = "#69b3a2") +
  coord_flip() +
  xlab("")

utils::str(hist(data_total_voo_por_cia_aerea$total, col = "red", labels = TRUE))



data_total_voo_por_cia_aerea %>%
  arrange(data_total_voo_por_cia_aerea$total,data_total_voo_por_cia_aerea$cia) %>%
  ggplot( aes(x = cia, y = total) ) +
  geom_segment( aes(x = cia ,xend = cia, y = total, yend = total), color = "grey") +
  geom_point(size = 5, color = "#69b3a2") +
  coord_flip() +
  xlab("")


hist(data_total_voo_por_cia_aerea$total)


qplot(data_total_voo_por_cia_aerea$total, geom="histogram") 

qplot(data_total_voo_por_cia_aerea$total,
      geom="histogram",
      binwidth= 30,  
      main = "Histogram for Airlines", 
      xlab = "data_total_voo_por_cia_aerea$total", 
      ylab = "n",
      fill=I("blue"), 
      col=I("red"), 
      alpha=.2,
      xlim=c(10,50))

data_total_voo_por_cia_aerea %>%
  mutate(name = fct_reorder(name, desc(val))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

data_flights_groupby_carrier <- data_flights %>% group_by(data_flights$carrier)
subset(data_flights_groupby_carrier, select = c("dep_time", "carrier"))

data_flights_groupby_carrier <- data_flights %>% group_by(data_flights$carrier)
View(subset(data_flights_groupby_carrier, select = c("dep_time", "carrier")))

#ESTUDO SOBRE FROTAS
#DISTANCIAS
#DURACOES
#CARACTERIZACAO
#TIPO E IDADE DA FROTA
data_voos_com_avioes <- right_join(data_flights, data_planes, by = 'tailnum')

dd1 <- group_by(data_voos_com_avioes,data_voos_com_avioes$carrier,data_voos_com_avioes$year.y) %>% 
  summarise(count = n())
write.csv(dd1, str_c(path_arquivos, 'arquivo1.csv', 
                    sep = ''), row.names = FALSE)

dd2 <- group_by(data_voos_com_avioes,data_voos_com_avioes$carrier,data_voos_com_avioes$type) %>% 
  summarise(count = n())
write.csv(dd2, str_c(path_arquivos, 'arquivo2.csv', 
                     sep = ''), row.names = FALSE)

dd3 <- group_by(data_voos_com_avioes,data_voos_com_avioes$carrier,data_voos_com_avioes$manufacturer) %>% 
  summarise(count = n())
write.csv(dd3, str_c(path_arquivos, 'arquivo3.csv', 
                     sep = ''), row.names = FALSE)

data_voos_com_avioes_por_modelo <- data_voos_com_avioes %>% count(data_voos_com_avioes$model)

data_voos_com_avioes_por_ano <- data_voos_com_avioes %>% count(data_voos_com_avioes$year.y) %>% arrange(1)


#ESTUDO SOBRE O CLIMA DURANTE
data_voo_com_clima <- merge(data_flights %>% filter(data_flights$dep_delay >= 1), data_wheather, by.x = "data_hora_voo_real", by.y = "data" )

data_voo_com_clima <- subset(data_voo_com_clima, select = c(dep_delay, visib))

data_voo_com_clima$visib <- ifelse((as.numeric(data_voo_com_clima$visib) <= 5), 'RUIM', 'BOA')
data_voo_com_clima$visib <- factor(data_voo_com_clima$visib)

dd1 <- dd1 %>% count(`data_voo_com_clima$visib`)

plot(table(data_voo_com_clima$visib, data_voo_com_clima$dep_delay))

dd1 <- group_by(data_voo_com_clima, data_voo_com_clima$visib) %>% summarize(n = n())
colnames(dd1)[1] <- "visibilidade"
colnames(dd1)[2] <- "total"

bp <- ggplot(data=dd1, aes(x=visibilidade, y=total, fill=total)) + geom_boxplot()
bp

ggplot(data = dd1, mapping = aes(x = visibilidade, y = total, fill = total)) +
  geom_bar(stat = "identity") +
  labs(x = "Visibilidade", y = "Total de Voos")
legend = rownames(dd1) args.legend = list(x = "topleft")

barplot(dep_delay ~ visib,
        data = data_voo_com_clima,
        main = 'Relação entre Clima x Atrasos de voos', cex.main = 1.2,
        xlab = 'Visibilidade',
        ylab = 'Atraso em minutos', cex.axis = 1.2,
        horizontal = F, cex.lab = 1.2,
        ylim = c(0,100),
        col = 'red', border = 'black')

view(frequency(dd1$visibilidade, report.nas = F, style = "rmarkdown"))
plot(dd1, main = 'Relação Visibilidade x Atraso de Voos',
     ylim = c(0,70))

dd2 <- data_voo_com_clima %>% count(dd2$visib) 

#data_voo_com_clima$wind_speed <- factor(data_voo_com_clima$wind_speed)
#data_voo_com_clima$visib <- factor(data_voo_com_clima$visib)
#data_voo_com_clima$dep_delay <- factor(data_voo_com_clima$dep_delay)
tb <- prop.table()

plot(table(data_voo_com_clima$visib,data_voo_com_clima$dep_delay), 
     main = "Atrasos de voos por visibilidade",
     xlab = 'Visibilidade',
     ylab = 'Atrasos em minutos',
     ccol = c("darkblue","lightcyan"))

barplot(dd1$atraso ~ dd1$visibilidade,
        main = 'Relação entre Clima x Atrasos de voos', cex.main = 1.2,
        xlab = 'Visibilidade',
        ylab = 'Atraso em minutos', cex.axis = 1.2,
        cex.lab = 1.2,
        ylim = c(0,100),
        col = 'red', border = 'black')





# Atribui data_wheather a variável xx e data_flights a variável yy
xx<- data_wheather
ff <- data_flights
#converte temperatura de fahrenheit para celsius 
xx$temp <- (xx$temp-32)*(5/9)
#Atribuindo data_hora_voo_real ao dataFrame xx data_wheather para inner join
xx <- xx %>% mutate(data_hora_voo_real = as.POSIXct(paste(str_trim(xx$year),'-',str_trim(xx$month),'-', str_trim(xx$day),' ', str_trim(xx$hour),':00',':00', sep = ""), format = "%Y-%m-%d %H:%M:%S"))
#convertendo
xx$data_hora_voo_real <- as.character(ymd_hms(xx$data_hora_voo_real))
ff$data_hora_voo_real <- as.character(ymd_hms(ff$data_hora_voo_real))

#Análise da temperatura
media=mean(xx$temp)
xx_q1 <- xx %>% filter(xx$temp < q1_)
summary(xx$temp)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-11.10    1.70    8.30   10.33   18.30   37.80 
q1_ <- 1.70
#Graficos dos dados amostrais da temperatura
boxplot(xx$temp)
hist(xx$temp,
     main = 'Atrasos de chegada e temperatura abaixo de 1,7°C', cex.main= 1.5,
     xlab = 'Frequencia de ocorrencia da temperatura',
     ylab = 'Temperatura em Graus Celsius',)
abline(v=media, col="blue",lwd=2)
abline(v=q1_, col="red", lwd=4)


# Inner Join entre data_flights e data_wheather por meio da data_hora_voo_real
#Junção das datasFrames data_wheather e data_flights
data_wheather$visib <- numeric(data_wheather$visib)
df <- merge(x=ff,y=xx,by="data_hora_voo_real")
#Criando dataFrame de análise dfy
dfy <- data.frame(df$data_hora_voo_real, df$dep_delay, df$arr_delay, df$temp, df$visib, df$precip)
#dfy$data_hora_voo_real <- df$data_hora_voo_real
dfy$flag_dep_delay <- (ifelse(df$dep_delay <= 0, FALSE, TRUE)) #df$dep_delay
dfy$flag_arr_delay  <- (ifelse(df$arr_delay <= 0, FALSE, TRUE))  #df$arr_delay
dfy$flag_temp  <- (ifelse(df$temp < -1.70, TRUE, FALSE))
dfy$flag_visib  <- (ifelse(df$visib > 3, TRUE, FALSE))
#dfy$flag_precip  <- (ifelse(df$precip < 0, TRUE, FALSE))
#flag resultante do relacionamento entre os boolean
dfy$bool_temp_dep <- (ifelse(dfy$flag_dep_delay & dfy$flag_temp, TRUE, FALSE))
#dfy$bool_visib  <- (ifelse(dfy$flag_dep_delay & dfy$flag_visib, TRUE, FALSE))
dfy$bool_temp_arr <- (ifelse(dfy$flag_arr_delay & dfy$flag_temp, TRUE, FALSE))
dfy$validacao_atraso <- (ifelse(dfy$bool_temp_arr & dfy$bool_temp, TRUE, FALSE))
dfy$bool_novos_atrasos_saida <- (ifelse(dfy$flag_dep_delay &dfy$flag_arr_delay, TRUE, FALSE))
#dfy$bool_precip  <- (ifelse(dfy$flag_dep_delay & dfy$flag_visib, TRUE, FALSE))

#GRÁFICOS
#'Atrasos ocasionadas nos aeroportos da amostra de wheather', cex.main= 1.5,
barplot(table(dfy$bool_novos_atrasos_saida),
        main = 'Atrasos ocasionadas nos aeroportos da amostra de wheather', cex.main= 1.5,
        xlab = 'TRUE são atrasos que aconteceram nos aeroportos',
        ylab = 'Frequencia trasos ocasionados nos aeroportos',
        col = c('green','red'), border = 'gray20')
#Atrasos de CHEGADA em temperatura abaixo de 1,7°C
tbl2 <- barplot(table(dfy$bool_temp_dep),
                main = 'Atrasos de CHEGADA em temperatura abaixo de 1,7°C', cex.main= 1.5,
                xlab = 'Atrasos com temperatúra inferiores a 1,7°C',
                ylab = 'Frequencias de ocorrência',
                col = c('darkcyan','cyan'), border = 'gray20')
#Arrtrasos de SAÍDA em temperatura abaixo de 1,7°C
tbl <- barplot(table(dfy$bool_temp_arr),
               main = 'Arrtrasos de SAÍDA em temperatura abaixo de 1,7°C', cex.main= 1.5,
               xlab = 'Atrasos com temperatúra inferiores a 1,7°C',
               ylab = 'Frequencias de ocorrência',
               col = c('darkcyan','cyan'), border = 'gray20')

