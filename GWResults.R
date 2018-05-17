#Limpa o Workspace
rm(list=ls())

#Chama a biblioteca
library(RColorBrewer)
library(maptools)
library(rgdal)
library(rgeos)
library(RgoogleMaps)
library(sp)
library(spdep)
library(ggmap)
library(plyr)
library(Hmisc)
library(tidyverse)
library(spgwr)

#Lê os dados
dados<-read.csv("Data\\Dados.csv")
dados$MUNICDV<-as.numeric(dados$codigo_ibg)

#Carrega o objeto SpatialPolygon
mun <- readOGR(dsn = "Malhas", "NORTE_MUN_region", verbose=FALSE)
uf <- readOGR(dsn = "Malhas", "NORTE_UF_region", verbose=FALSE)

#Cria uma variável ID para o georeferenciamento
mun@data$id <- rownames(mun@data)

#Define a projeção (Shapefile)
mun <- spTransform(mun, CRS("+proj=longlat +datum=WGS84"))

#Create a SpatialPolygonDataFrame
mun.join<-merge(mun,dados,by="MUNICDV",all=F)
#######################################################################################
#################################### Full Model        ################################
#######################################################################################
gbwG <- ggwr.sel(Beta ~ Área_prop + Orient_tecn + Queimada + Agricultura_.organ +
                   Trabalho_infantil + Laço_parentesco + Financiamento + Sabem_.ler +
                   Köppen + Altitude + T_Mean, 
                   data = mun.join, family = "gaussian", gweight = gwr.Gauss)


ggwrG <- ggwr(Beta ~ Área_prop + Orient_tecn + Queimada + Agricultura_.organ +
                     Trabalho_infantil + Laço_parentesco + Financiamento + Sabem_.ler +
                     Köppen + Altitude + T_Mean, 
                     data = mun.join, family = "gaussian", bandwidth = gbwG, gweight = gwr.Gauss)

#Pega os resultados
ggwrG.df<-as.data.frame(ggwrG$SDF)
ggwrG.df$MUNICDV<-mun@data$MUNICDV
ggwrG.mun<-merge(mun, ggwrG.df, by="MUNICDV", all=F)


#Plota o mapa
ggwrG.df <- fortify(ggwrG, region="MUNICDV")

gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$coefUnauthAbsenceSchools11))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

#Trasnforma o objeto espacial em um objeto que pode ser lido pelo ggplot2
sfn.df <- fortify(sfn, region="CD_GEOCMU")

