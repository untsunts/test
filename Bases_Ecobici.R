##Bases de datos Ecobici
rm(list=ls())
library(readr)
octubre<-read_csv('Ecobici/2016-10.csv',col_types = cols(
  Genero_Usuario = col_factor(c('F','M')),
  Fecha_Retiro = col_date(format='%d/%m/%Y'),
  Hora_Retiro = col_time(format= '%H:%M:%S %p'),
  Fecha_Arribo = col_date(format='%d/%m/%Y'),
  Hora_Arribo = col_time(format= '%H:%M:%S %p')
))
noviembre<-read_csv('Ecobici/2016-11.csv',col_types = cols(
  Genero_Usuario = col_factor(c('F','M')),
  Fecha_Retiro = col_date(format='%d/%m/%Y'),
  Hora_Retiro = col_time(format= '%H:%M:%S %p'),
  Fecha_Arribo = col_date(format='%d/%m/%Y'),
  Hora_Arribo = col_time(format= '%H:%M:%S %p')
))
diciembre<-read_csv('Ecobici/2016-12.csv',col_types = cols(
  Genero_Usuario = col_factor(c('F','M')),
  Fecha_Retiro = col_date(format='%d/%m/%Y'),
  Hora_Retiro = col_time(format= '%H:%M:%S %p'),
  Fecha_Arribo = col_date(format='%d/%m/%Y'),
  Hora_Arribo = col_time(format= '%H:%M:%S %p')
))
estaciones_json<-read_csv('Ecobici/estaciones_csv.csv')
lluvia_oct<-read_tsv('Ecobici/lluv_oct_2016.tsv')
lluvia_nov<-read_tsv('Ecobici/lluv_nov_2016.tsv')
lluvia_dic<-read_tsv('Ecobici/lluv_dic_2016.tsv')
library(reshape2)
library(tidyr)
library(dplyr)
colnames(lluvia_oct)<-c('Fecha','TempExtr','Precip','Nieve','Prediccion','TempExtrProm')
colnames(lluvia_nov)<-c('Fecha','TempExtr','Precip','Nieve','Prediccion','TempExtrProm')
colnames(lluvia_dic)<-c('Fecha','TempExtr','Precip','Nieve','Prediccion','TempExtrProm')

lluvia_oct<-separate(data=lluvia_oct,col=Fecha, into= c('dia_sem','dia'), sep=" ")
lluvia_oct<-separate(data=lluvia_oct, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_oct<-separate(data=lluvia_oct, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))
lluvia_oct<-separate(data=lluvia_oct,col=Precip, into= c('cantidad.ll','medida.ll'), sep=" ")
lluvia_nov<-separate(data=lluvia_nov,col=Fecha, into= c('dia_sem','dia'), sep=" ")
lluvia_nov<-separate(data=lluvia_nov, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_nov<-separate(data=lluvia_nov, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))
lluvia_nov<-separate(data=lluvia_nov,col=Precip, into= c('cantidad.ll','medida.ll'), sep=" ")
lluvia_dic<-separate(data=lluvia_dic,col=Fecha, into= c('dia_sem','dia'), sep=" ")
lluvia_dic<-separate(data=lluvia_dic, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_dic<-separate(data=lluvia_dic, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))
lluvia_dic<-separate(data=lluvia_dic,col=Precip, into= c('cantidad.ll','medida.ll'), sep=" ")

library(lubridate)
library(ggplot2)
#pegar 3 meses
bicis = rbind(octubre,noviembre,diciembre)
print(bicis, n=9,width = 200)
bicis_tiempos<-bicis %>% mutate(minutos_dif = ceiling((Hora_Arribo- Hora_Retiro)/60))
hist(as.numeric(bicis_tiempos$minutos_dif))
#al parecer hay algún tipo de ruido en los datos ya que existen valores negativos en tiempos de uso
#contar número de bicis usadas por día
bicis_fecha<-bicis_tiempos%>%mutate(dia_semana = weekdays(Fecha_Retiro), 
                                  mes = month(Fecha_Retiro), dia_mes = day(Fecha_Retiro))
bicis_fecha<-bicis_fecha%>%mutate(conteo = )
bicis_estacion_retiro <- bicis_fecha%>%group_by(Ciclo_Estacion_Retiro)
bicis_estacion_arribo <- bicis_fecha%>%group_by(Ciclo_Estacion_Arribo)
bicis_dia_sem<- bicis_fecha%>% group_by(dia_semana)
bicis_dia_mes<- bicis_fecha%>%group_by(dia_mes)
bicis_mes<-bicis_fecha%>%group_by(mes)

conteo_retiro<-bicis_estacion_retiro%>%count(Fecha_Retiro)
conteo_arribo<-bicis_estacion_arribo%>%count(Fecha_Arribo)
conteo_rprom<-conteo_retiro%>%summarise(conteo_medio = mean(n))
conteo_aprom<-conteo_arribo%>%summarise(conteo_medio = mean(n))
conteo_estacion<-cbind(conteo_rprom,conteo_aprom)
ggplot(conteo_estacion, aes(x=Ciclo_Estacion_Retiro, y=Ciclo_Estacion_Arribo)) + geom_point()
#al ver que son las mismas, puedo seguir
library(gridExtra)
colnames(conteo_estacion)<- c('EstRet','contRet','EstArr','contArr')
ggplot(conteo_estacion, aes(x=contRet, y=contArr)) + geom_point()
#parece ser que las estaciones que son muy usadas para agarrar bicis, también son muy usadas para dejar bicis
conteo_estacion_uso<-conteo_estacion%>%mutate(uso_tot = contRet + contArr )
est.est<-ggplot(conteo_estacion, aes(x=contRet, y=contArr, colour=EstRet/1000) ) + 
  geom_point(alpha = 0.8)
est.uso<-ggplot(conteo_estacion_uso, aes(x=EstRet, y =uso_tot)) + geom_point()
grid.arrange(est.est,est.uso,nrow=2)
#seria bueno ver si el número de la estación depende de qué tan vieja es, si así fuera podría parecer
#que las estaciones más viejas son las más usadas, si no fuera así tal vez es por ubicación y eso nos diría
#mucho de qué tipo de flujo existe en la ciudad, por zona afecta más, eso lo podremos ver con ggmap usando los datos
#del json descargable.


#juntar y resumir lluvias y temperaturas

lluvia_oct_aux<- lluvia_oct[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvia_nov_aux<- lluvia_nov[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvia_dic_aux<- lluvia_dic[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvias<-rbind(lluvia_oct_aux,lluvia_nov_aux,lluvia_dic_aux)
lluvias<-separate(data=lluvias,col=dia, into= c('mes','dia'), sep="/")

#unir bases




