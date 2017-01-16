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
#contar número de bicis usadas por diferentes categorías
bicis_fecha<-bicis_tiempos%>%mutate(dia_semana = weekdays(Fecha_Retiro), 
                                  mes = month(Fecha_Retiro), dia_mes = day(Fecha_Retiro))
#bicis_fecha<-bicis_fecha%>%mutate(conteo_dia = tally(group_by()), conteo_estacion = count(Ciclo_Estacion_Retiro))
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
#faltaría analizar las horas pero no logro obtener las horas de manera adecuada... analizaré la diferencia en minutos y los segundos totales
tiempo_uso.dia_semana<-bicis_fecha%>%group_by(minutos_dif)%>%count(dia_semana)
uso.dia_semana<-tiempo_uso.dia_semana%>%group_by(dia_semana)%>%summarise(prom_cont = mean(n))
uso.dia_semana<-uso.dia_semana[1:7,]
uso.dia_semana$dia_semana<-factor(uso.dia_semana$dia_semana, 
                                     levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

ggplot(uso.dia_semana, aes(x=dia_semana, y =prom_cont)) + geom_point()

conteo_dia_semana<-bicis_dia_sem%>%tally()
conteo_dia_semana<-conteo_dia_semana[1:7,]
conteo_dia_semana$dia_semana<-factor(conteo_dia_semana$dia_semana, 
                                     levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
cont.ds<-ggplot(conteo_dia_semana, aes(x=dia_semana, y=n)) + geom_point()

conteo_dia_mes<-bicis_dia_mes%>%tally()
cont.dm<-ggplot(conteo_dia_mes, aes(x=dia_mes, y=n)) + geom_point()

grid.arrange(cont.ds,cont.dm,nrow=2)
#hay una estacionalidad por día de la semana, sería bueno ver si este patrón se repite en las estaciones viejas para clasificar su uso.

conteo_dia_estacion<-bicis_fecha%>%group_by(Ciclo_Estacion_Retiro,dia_semana)%>%tally()
#(ignorar)conteo_dia_estacion<-conteo_dia_estacion%>%filter(dia_semana == c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
conteo_dia_estacion$dia_semana<-factor(conteo_dia_estacion$dia_semana, 
                                     levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

ggplot(conteo_dia_estacion, aes(y=n, x=dia_semana, colour=Ciclo_Estacion_Retiro)) + geom_point()
#puede ser que los NA  (muy probablemente) nos estén metiendo mucho ruido, el problema está en la lectura de los datos... :S
#Realmente no es tan buena idea ignorar esos NA, sin embargo (por la falta de tiempo), creo que tendré que hacerlo por ahora...

#hay una tendencia similar en tiempo de uso a la de uso total en cada día de la semana:
prom.dia<-ggplot(uso.dia_semana, aes(x=dia_semana, y =prom_cont, colour=dia_semana)) + geom_point(size=2.0)
cont.ds<-ggplot(conteo_dia_semana, aes(x=dia_semana, y=n, colour=dia_semana)) + geom_point(size=2.0)

grid.arrange(prom.dia, cont.ds, nrow =2)
#en conclusión se puede decir que hay dos tendencias, una por día de la semana y otra por el número de la estación
#que el número de la estación parezca explicar la tendencia de uso podría ser por dos razones, longevidad o cercanía de ciertos puntos
#a continuación usaré información del clima (que siento que podría explicar mejor y ayudarnos a clasificar)
#y después haré dos cosas: 1.- clasificar (manualmente) en categorías de uso (mucho o poco) y de tendencias temporales (creciente o decreciente)
#y correr un árbol aleatorio para ver qué variables parecen describir estos movimientos.
#la segunda va a ser un método clasificatorio de aprendizaje no supervisado (no sé muy bien todavía si svm o algo más sencillo como pca y poner rangos)
#sin embargo, la verdad, creo que a esa parte no llegaré a tiempo dado que (aunque entiendo cómo funcionan), a la fecha no he hecho experimentos con ellos y datos...

#juntar y resumir lluvias y temperaturas

lluvia_oct_aux<- lluvia_oct[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvia_nov_aux<- lluvia_nov[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvia_dic_aux<- lluvia_dic[,c('dia','T.baja','T.alta','cantidad.ll','TProm.baja','TProm.alta')]
lluvias<-rbind(lluvia_oct_aux,lluvia_nov_aux,lluvia_dic_aux)
lluvias<-separate(data=lluvias,col=dia, into= c('mes','dia'), sep="/")

#unir bases



#hetmap entradas_salidas


# (ignorar) ggplot(data = df.team_data, aes(x = metrics, y = teams)) +
#   geom_tile(aes(fill = performance)) 
