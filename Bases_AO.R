#Bases de datos Alvaro Obregon
rm(list=ls())
library(readr)
#leer tablas de natalidad/mortalidad y de población por Alvaro Obregon
mortalidad<-read_csv('AlvaroObregon//INEGI_Exporta_mortalidad.csv')
natalidad<-read_csv('AlvaroObregon//INEGI_Exporta_natalidad.csv')
natalidad<-natalidad[-2,]
AlvaroObregon<- read_csv('AlvaroObregon/iter2010_ageb.csv')
#tasa crecimiento estimada 2000 - 2020 Álvaro Obregón (www.dao.gob.mx)
tc_1020<-0.002
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

