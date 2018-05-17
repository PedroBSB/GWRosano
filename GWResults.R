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
dados$CD_GEOCMU<-as.numeric(dados$codigo_ibg)

#Carrega o objeto SpatialPolygon
mun <- readOGR(dsn = "Malhas", "DetermEcoE", verbose=FALSE)
uf <- readOGR(dsn = "Malhas", "NORTE_UF_region", verbose=FALSE)

#Cria uma variável ID para o georeferenciamento
mun@data$id <- rownames(mun@data)

#Define a projeção (Shapefile)
mun <- spTransform(mun, CRS("+proj=longlat +datum=WGS84"))

#Create a SpatialPolygonDataFrame
mun.join<-merge(mun,dados,by="CD_GEOCMU",all=F)
mun.join@data$id <- rownames(mun.join@data)
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

#Pega os resultados (!!!! Trocar mun.join por mun !!!!)
ggwrG.df<-as.data.frame(ggwrG$SDF)
colnames(ggwrG.df)<-paste0("B_",colnames(ggwrG.df))
ggwrG.df$CD_GEOCMU<-as.numeric(mun.join@data$CD_GEOCMU)

#Plota o mapa
sfn.df <- fortify(mun.join, region="CD_GEOCMU")
sfn.df$CD_GEOCMU<-sfn.df$id

#Merge 
sfn.df<-merge(sfn.df, ggwrG.df,by="CD_GEOCMU",all=T)

#Escolhe as cores
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

#Aumenta ou diminui a Bounding Box (aumenta em 1%)
bbox <- ggmap::make_bbox(sfn.df$long, sfn.df$lat, f = 0.1)

#Obtêm o mapa do Google Maps (Existem outras possibilidades...)
map <- get_map(location=bbox, source='google', maptype = 'terrain', color='bw')

#Constrói o mapa:
map <- ggmap(map, base_layer=ggplot(data=sfn.df, aes(x=long, y=lat)), 
             extent = "normal", maprange=FALSE)
map <- map + geom_polygon(data=sfn.df,aes(x = long, y = lat, group = group, fill=B_Financiamento), alpha = .6)  
map <- map +   geom_path(aes(x = long, y = lat, group = group),
                         data = sfn.df, colour = "grey50", alpha = .7, size = .4, linetype=2)  
map <- map + coord_equal() 
map <- map + scale_fill_gradientn(colours = myPalette(4))
map <-map +  ggtitle("Financiamento") +  labs(x="Longitude",y="Latitude")
ggsave(filename = "B_Financiamento.png", map,
       width = 10, height = 8, dpi = 150, units = "in", device='png')