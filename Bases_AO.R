#Bases de datos Alvaro Obregon

library(readr)
#leer tablas de natalidad/mortalidad y de población por Alvaro Obregon
mortalidad<-read_csv('AlvaroObregon//INEGI_Exporta_mortalidad.csv')
natalidad<-read_csv('AlvaroObregon//INEGI_Exporta_natalidad.csv')
AlvaroObregon<- read_csv('AlvaroObregon/iter2010_ageb.csv')
#tasa crecimiento estimada 2000 - 2020 Álvaro Obregón (www.dao.gob.mx)
tc_10-20<-0.002
#filtrar bases
library(dplyr)
library(tidyr)
library(magrittr)
summary(mortalidad)
mortalidad_AO<- mortalidad %>% filter(X1>2009)
m1016<-mortalidad_AO[,c(1:2,(ncol(mortalidad_AO)-5):ncol(mortalidad_AO))]
natalidad_AO<- natalidad[219:231,]
colnames(natalidad_AO[,(ncol(natalidad_AO)-20):ncol(natalidad_AO)])
n_1016<- natalidad_AO[,c(3,(ncol(natalidad_AO)-15):ncol(natalidad_AO))]
#estimar tamaños de cada AGEB 
AGEB <-AlvaroObregon%>%filter(NOM_LOC == 'Total AGEB urbana')
rAGEB <- AGEB%>% mutate(total = sum(P_TOTAL), size = P_TOTAL/total)
