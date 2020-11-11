library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)
library(reshape2)
library(openxlsx)

data <- read.delim("coronavirus argentina.txt")
data <- arrange(data, confirmados)
datats <- ts(data[c(1:nrow(data)-1),2], frequency = 365, start = 1) #Se saca el ultimo dato (hoy) para ver como predice



fit1 <- auto.arima(datats) #Se le deja al programa que elija que modelo arima usar
summary(fit1) # Se analizan mínimamente el resultado y los estadisticos
forecast(fit1,20) #se proyecta para los próximos 20 días (hoy+19)
#En base a analisis anteriores, los paises en momentos de control siguen un arima (1,2,0), los que no un arima(0,2,0)

fit2 <- arima(datats, c(1,2,0)) #Se le pide que ajuste a un modelo en particular (arima 1,2,0), el mismo que el automatico
summary(fit2)
forecast(fit2,20)

fit3 <- arima(datats, c(0,2,0))
summary(fit3)
forecast(fit3,20)

###############################

fitpesimista <- arima(data$confirmados[1:15], c(0,2,0))
summary(fitpesimista)
fore1 <- forecast(fitpesimista,20)
fore1 <- as.data.frame(fore1)


fitoptimista <- arima(data$confirmados[1:15], c(1,2,0))
summary(fitoptimista)
fore2 <- forecast(fitoptimista,20)
fore2 <- as.data.frame(fore2)

fore <- cbind(fore1[,c(1,5)], fore2[,c(1,5)])
colnames(fore) <- c("Pesimista", "Pesimista High limit 95%", "Optimista", "Optimista High Limit 95%")
fecha <- as.Date("19-03", format="%d-%m")
fore$fecha <- rep(fecha, 20)
fore$fecha <- as.Date(fore$fecha+c(0:19))
fore <- fore[c(5,1:4)]

data2 <- data
data2$Fecha <- as.Date(data2$Fecha, format="%d/%m")
data2[,c(3,4,5)] <- data2$confirmados
colnames(data2) <- c("fecha","Pesimista", "Pesimista High limit 95%", "Optimista", "Optimista High Limit 95%")

fore <- rbind(data2,fore)
##################################

foreg <- melt(fore, id="fecha")
foreg$fecha <- as.Date(foreg$fecha)
windows()
ggplot(foreg, aes(x=fecha, y=value, colour=variable, group=variable))+
  geom_line()+
  scale_y_continuous(name = "Casos", breaks = c(seq(0, 1100,100)))+
  scale_x_date(date_breaks = "5 days")+
  ggtitle("Escenarios coronavirus")

################
datamundi <- read.csv("C:/Users/Usuario/Documents/walter docs/coronavirus/covid_19_clean_complete.csv")
datamundi <- datamundi %>%
  group_by(Country.Region, Date) %>%
  summarise(sum(Confirmed))
datamundi$Date <- as.Date(datamundi$Date,format="%m/%d/20")
datamundi <- arrange(datamundi, Date)
datamundi <- datamundi[
  c(datamundi$Country.Region=="Spain"|datamundi$Country.Region=="Italy"|
      datamundi$Country.Region=="Brazil"|datamundi$Country.Region=="Mexico")
  ,c(2,1,3)]
datamundi <- datamundi[datamundi$`sum(Confirmed)`!=0,c(1,2,3)]
colnames(datamundi) <- c("Fecha","Pais", "Casos")



ldf <- split(datamundi, datamundi$Pais)
ldf <- list(Spain=ldf$Spain, Italy=ldf$Italy, Brazil=ldf$Brazil, Mexico=ldf$Mexico)
list2env(ldf, .GlobalEnv)

Brazil$Fecha <- 1+Brazil$Fecha-min(Brazil$Fecha)
Mexico$Fecha <- 1+Mexico$Fecha-min(Mexico$Fecha)
Italy$Fecha <- 1+Italy$Fecha-min(Italy$Fecha)
Spain$Fecha <- 1+Spain$Fecha-min(Spain$Fecha)

Argentina_pesimista95 <- data.frame(Fecha= seq(1,36,1),Pais=rep("Argentina(PesimistaH95%)", 36), Casos=c(1,1,fore$`Pesimista High limit 95%`))
Argentina_pesimista <- data.frame(Fecha= seq(1,36,1),Pais=rep("Argentina(Pesimista)", 36), Casos=c(1,1,fore$Pesimista))
datamundi <- rbind(Argentina_pesimista95, Argentina_pesimista,Brazil, Italy, Mexico, Spain)

ggplot(datamundi[c(datamundi$Fecha<17 & datamundi$Pais!="Argentina(PesimistaH95%)"),], aes(x=Fecha, y=Casos, colour=Pais, group=Pais))+
  geom_line()+
  scale_y_continuous(name = "Casos", breaks = c(seq(0, 100,10)))+
  scale_x_continuous(breaks = seq(1,16,1))+
  ggtitle("Cantidad de casos en los primeros 16 días")

ggplot(datamundi[c(datamundi$Fecha<22 & datamundi$Fecha>16 ) ,], aes(x=Fecha, y=Casos, colour=Pais, group=Pais))+
  geom_line()+
  scale_y_continuous(name = "Casos",breaks = c(seq(0,350,50)))+
  ggtitle("Cantidad de casos entre los días 16 y 21 días")

ggplot(datamundi[c(datamundi$Fecha>21 & datamundi$Fecha<37),], aes(x=Fecha, y=Casos, colour=Pais, group=Pais))+
  geom_line()+
  scale_y_continuous(name = "Casos", breaks = c(seq(0,4500,500)))+
  scale_x_continuous(breaks = seq(22,36,1))+
  ggtitle("Cantidad de casos entre los días 21 y 36")

ggplot(datamundi[datamundi$Fecha>36,], aes(x=Fecha, y=Casos, colour=Pais, group=Pais))+
  geom_line()+
  scale_y_continuous(name = "Casos", breaks = c(seq(0,32000,5000)))+
  scale_x_continuous(breaks = seq(36,51,1))+
  ggtitle("Cantidad de casos desde el día 36")




latin <-read.csv("C:/Users/Usuario/Documents/walter docs/coronavirus/covid_19_clean_complete.csv") 
latin <- latin[c(latin$Country.Region=="Argentina"|latin$Country.Region=="Brazil"|
                 latin$Country.Region=="Chile"|latin$Country.Region=="Uruguay"|
                 latin$Country.Region=="Mexico"|latin$Country.Region=="Colombia"|
                 latin$Country.Region=="Peru"|latin$Country.Region=="Ecuador"),c(5,2,6)]
latin <- latin[latin$Confirmed!=0,]
latin$Date <- as.Date(latin$Date, format="%m/%d/20")
latin <- arrange(latin, latin$Date)
colnames(latin) <- c("Fecha", "Pais", "Casos")

lala <- latin %>%
  group_by(Pais) %>%
  summarise(min(Fecha))

latin <- merge(latin, lala, by="Pais")
latin$Fecha <- latin$Fecha+1-latin$`min(Fecha)`
latin <- latin[,c(1:3)]
latin$Fecha <- as.numeric(latin$Fecha)

ggplot(latin, aes(x=Fecha, y=Casos, colour=Pais, group=Pais))+
  geom_line(size=1)+
  scale_y_continuous(name="Casos", breaks = c(seq(0,350,50)))+
  scale_x_continuous(breaks = c(seq(0,21,1)))+
  ggtitle("Cantidad de casos acumulados en Latinoamerica")+
  scale_color_manual(values = c("Steelblue", "Green", "Purple", "Black", "Yellow", "DarkGreen", "Red", "Brown"))
  
####################################
data <- read.delim("coronavirus argentina.txt")
data <- arrange(data, confirmados)

x <- NULL
y <- NULL

for (i in 3:nrow(data)) {
  x <- data$confirmados[1:i]
fit <- arima(x, c(0,2,0))
fit <- forecast(fit,1)
fit <- as.data.frame(fit[["mean"]])
y[i] <- as.numeric(fit)
}

y <- c(0, y[1:length(y)-1])
y <- as.data.frame(y)
y <- cbind(data, y)
y$dif <- y$confirmados-y$y
y$error <- y$dif/ y$confirmados
