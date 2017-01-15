##Bases de datos Alvaro Obregon
rm(list=ls())
library(readr)
#leer tablas de natalidad/mortalidad y de población por Alvaro Obregon
mortalidad<-read_csv('AlvaroObregon//INEGI_Exporta_mortalidad.csv')
natalidad<-read_csv('AlvaroObregon//INEGI_Exporta_natalidad.csv')
natalidad<-natalidad[-2,]
AlvaroObregon<- read_csv('AlvaroObregon/iter2010_ageb.csv')

#tasa crecimiento estimada 2000 - 2020 Álvaro Obregón (http://www.dao.gob.mx/delegacion/encifras/proyecciones.php)
tc_1020<-0.002 #tasa anual

#filtrar bases
library(dplyr)
library(tidyr)
library(magrittr)
summary(mortalidad)
mortalidad_AO<- mortalidad %>% filter(X1>2009)
m_1016<-mortalidad_AO[,c(1:2,(ncol(mortalidad_AO)-5):ncol(mortalidad_AO))]
natalidad_AO<- natalidad[219:231,]
colnames(natalidad_AO[,(ncol(natalidad_AO)-20):ncol(natalidad_AO)])
n_1016<- natalidad_AO[,c(3,(ncol(natalidad_AO)-15):ncol(natalidad_AO))]
#ajustar apuradamente las natalidades...
n_1016$diez = as.numeric(n_1016$`2010`)
n_1016$once = as.numeric(n_1016$`2011`)
n_1016$doce = as.numeric(n_1016$`2012`)
n_1016$trece = as.numeric(n_1016$`2013`)
n_1016$catorce = as.numeric(n_1016$`2014`)
n_1016$quince = as.numeric(n_1016$`2015`)

##Estimacion Frecuencias
#estimar tamaños de cada AGEB 
AGEB <-AlvaroObregon%>%filter(NOM_LOC == 'Total AGEB urbana')
rAGEB <- AGEB%>% mutate(total = sum(P_TOTAL), size = P_TOTAL/total)

#estimar frecuencias por mes
n_aux<-n_1016%>% filter(X3!='Total')
rn_aux<-n_aux%>%mutate(total=sum(quince),size = quince/total)
colnames(m_1016)<-c('ocurrencia','mes','diez','once','doce','trece','catorce','quince')
m_aux<-m_1016%>%filter(ocurrencia==2015, mes!='Total')
rm_aux<-m_aux%>%mutate(total=sum(quince), size = quince/total)

#estimación frec 6 meses
frec_nat_6<-sum(rn_aux[7:12,25])
frec_mort_6<-sum(rm_aux[7:12,10])

##Explicacion
#Para calcular el num por AGEB primero voy a calcular el num para toda la delegacion y
#estimare cada AGEB por la frecuencia relativa obtenida del 2010.
#Para calcular el num de los meses julio-diciembre (aprox, ya que ya llevamos 15 días de enero, le quitaré la mitad de julio y pondré la mitad de enero)
#calcularé el de todo el 2015 y estimaré la proporción con los meses del 2015.
#Para calcular la población total del 2016 usaré las estimaciones hechas por la delegación y, con una tasa equivalente, aproximaré la población total de la delegación en el 2016.
#Para calcular la población total de niños entre 0 y un años usaré información de nacimientos y muertes históricas del INEGI.
#Gracias a haber encontrado datos de nacimiento por delegación (gracias busquedas avanzadas del INEGI, nunca cambien)
#Sólo necesitaré estimar los totales de nacimientos y no estimarlos según número de habitantes, sin embargo, para la migración sí tendré que hacer un cálculo por población total
#multiplicada por la tasa de nacimientos por hogares en la Ciudad de México (estimada con un modelo arima y datos del INEGI).
#Me enfocaré en 3 partes: 
#Nacimientos en los últimos 6 meses de gente que vive en AO.
#Muertes de niños entre 0 y 6 meses (usaré la proporción correspondiente a Enero-Junio calculada anteriormente de las muertes infantiles de gente que habita AO)
#Migración de niños de esa edad (estimando por el crecimiento poblacional estimado por la delegación, para evitar separar en entradas y salidas)
#La fórmula final será: Estimación = Nacimientos - Muertes + Crecimiento * Tasa.Natalidad por persona


#tasas natalidad CdMX
tasa_nat<-read_tsv('AlvaroObregon/tasa_natalidad.tsv')
library(forecast)
head(tasa_nat)
ARIMA_tasaN<-auto.arima(tasa_nat$`Tasa Natalidad`)
summary(ARIMA_tasaN)
pred<-forecast(ARIMA_tasaN)
tasaN_pred<-pred$mean[2]/(100*1000)

##Estimacion final
#Estimacion migracion
AO_2010<-698000 #www.dao.gob.mx
AO_2016<- round(AO_2010*(1+tc_1020)^6)
AO_2017<- round(AO_2016*(1+tc_1020))
Crec_AO<- AO_2017- AO_2016
PobObjetivo_CrecAO <- round(Crec_AO * tasaN_pred)

#Estimacion mortalidad
totales_mort<-mortalidad_AO[1,3:28]
tiempo<-1990:2015
totales_mort<-rbind(tiempo,totales_mort)
plot(1:26,totales_mort)
pred_aux<-as.data.frame(t(totales_mort))
colnames(pred_aux)<-c('yr','mort')
lin.mod<-lm(pred_aux$mort ~ pred_aux$yr)
pr.lm <- predict(lin.mod)
ls <- loess(pred_aux$mort~pred_aux$yr)
pr.loess <- predict(ls)
library(ggplot2)
pred_aux$pr<-pr.lm
pred_aux$loess<-pr.loess
ggplot(pred_aux,aes(x=yr, y=mort)) + geom_point(size = 4.5, col = 'darkgray') +
  geom_line(y=pred_aux$pr, col = 'red', size = 0.8) +geom_smooth(size=0.5)
#tomar valores del 2010 en adelante (ya que tiene tendencia polinomial)
pred_aux2<-pred_aux%>%filter(yr>=2010)
lin.mod<-lm(pred_aux2$mort ~ pred_aux2$yr)
pr.lm <- predict(lin.mod)
pred_aux2$pr<-pr.lm
ggplot(pred_aux2,aes(x=yr, y=mort)) + geom_point(size = 4.5, col = 'darkgray') +
  geom_line(y=pred_aux2$pr, col = 'red', size = 0.8) +geom_smooth(size=0.5)
pred_mort_2016<-round(as.numeric(pr.lm[6]/pr.lm[5])*pred_aux2[4,2]*frec_mort_6)

#Estimacion natalidad
#(ignorar) totales_nat<-n_1016%>%select(ends_with('_1'))
#(ignorar) colnames(totales_nat)<-c('v_2010','v_2011','v_2012','v_2013','v_2014','v_2015')
totales_CdMX<-natalidad[101,4]
totales_AO <- natalidad[218,4]
#comprobacion supuesto 8%
tasa_AO<-10539/122416
#como queda cercano, usaré esa aproximación, estimaré contra el total de nacimientos en la CdMX
totales_nat_CdMX<-natalidad[101,]
totales_nat_CdMX<-totales_nat_CdMX%>%select(starts_with('20'))
totales_nat_CdMX<-totales_nat_CdMX%>%select(ends_with('_1'))
#también usaré sólo datos desde el 2010 (parecen más estables, no sé el porqué, antes del 2009 los nacimientos son MUY pocos, me parece que puede haber un error)
totales_nat_CdMX<-totales_nat_CdMX%>%select(starts_with('201'))
totales_nat_CdMX<-c(706, 1220,1557,2326,22189,89752)
#como para 2014 y 2015 los datos parecen crecer demasiado, usaré los datos de http://www.beta.inegi.org.mx/app/statisticsexplorer/09/index.html y veré cuál me hace más sentido
totales_nat_AO<-c(13100,12656,12323,11557,11737,11104)
#definitivamente me hacen más sentido los datos de la tabla interactiva del iNEGI, usaré esa información.
plot(2010:2015,totales_nat_AO)
#de nuevo parece tener una tendencia polinomial, haré el mismo análisis que para mortalidad.
tiempo<-2010:2015
totales_nat<-rbind(tiempo,totales_nat_AO)
pred_aux<-as.data.frame(t(totales_nat))
colnames(pred_aux)<-c('yr','nat')
lin.mod<-lm(pred_aux$nat ~ pred_aux$yr)
pr.lm <- predict(lin.mod)
pred_aux$pr<-pr.lm
ggplot(pred_aux,aes(x=yr, y=nat)) + geom_point(size = 4.5, col = 'darkgray') +
  geom_line(y=pred_aux$pr, col = 'red', size = 0.8) +geom_smooth(size=0.5)
pred_nat_2016<-round(as.numeric(pr.lm[6]/pr.lm[5])*pred_aux[4,2]*frec_nat_6)

Est6meses<- pred_nat_2016 - pred_mort_2016 + PobObjetivo_CrecAO
AGEB_Est6meses<-data.frame(AGEB = rAGEB$AGEB , estimado = round(Est6meses*rAGEB$size), size = rAGEB$size)
