##Bases de datos Ecobici
rm(list=ls())
library(readr)
octubre<-read_csv('Ecobici/2016-10.csv', col_types = c())
noviembre<-read_csv('Ecobici/2016-11.csv')
diciembre<-read_csv('Ecobici/2016-12.csv')
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

lluvia_oct<-separate(data=lluvia_oct, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_oct<-separate(data=lluvia_oct, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))
lluvia_nov<-separate(data=lluvia_nov, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_nov<-separate(data=lluvia_nov, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))
lluvia_dic<-separate(data=lluvia_dic, col = TempExtr , into = c('T.alta','T.baja'))
lluvia_dic<-separate(data=lluvia_dic, col = TempExtrProm , into = c('TProm.alta','TProm.baja'))




