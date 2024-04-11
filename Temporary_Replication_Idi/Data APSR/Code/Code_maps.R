library(ggmap)
library(sf)
library(mapview)
library(tidyverse)
library(ggplot2)
library(maptools)
library(rgdal)

rm(list=ls())

#IMPORT DATA
setwd("/Data (APSR)")

bolivia <- read.csv("Data/bol_census_elections.csv")


# SHAPEFILE DATA
shape_muni <- read_sf('Data/bolivia_shapes/bol_admbnda_adm3_gov_2020514.shp')
shape_dep <- read_sf('Data/bolivia_shapes/bol_admbnda_adm1_gov_2020514.shp')

# convert to a data.frame for use with ggplot2/ggmap and plot
shape_muni <- fortify(shape_muni)
shape_dep <- fortify(shape_dep)

shape_muni <- shape_muni %>%
  rename(Municipio = ADM3_ES)

shape_muni <- shape_muni %>%
  rename(Departamento = ADM1_ES)

shape_dep <- shape_dep %>%
  rename(Departamento = ADM1_ES)


# CLEAN DATA
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "ú", "u")  
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "í", "i")  
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "ó", "o")  
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "é", "e")  
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "á", "a")  
bolivia$Municipio <- str_replace_all(bolivia$Municipio, "ñ", "n")  


shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "ú", "u")  
shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "í", "i")  
shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "ó", "o")  
shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "é", "e")  
shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "á", "a")  
shape_muni$Municipio <- str_replace_all(shape_muni$Municipio, "ñ", "n")  
shape_muni$Departamento <- str_replace_all(shape_muni$Departamento, "í", "i")  

shape_dep$Departamento <- str_replace_all(shape_dep$Departamento, "í", "i")  

# Adjust Municipio names to match

bolivia$Municipio[bolivia$Municipio=="Camataqui (Villa Abecia)"] <- "Villa Abecia"
bolivia$Municipio[bolivia$Municipio=="Saavedra"] <- "General Saavedra"
bolivia$Municipio[bolivia$Municipio=="Ancoraimes"] <- "Villa Ancoraimes"
bolivia$Municipio[bolivia$Municipio=="Juan Jose Perez (Charazani)"] <- "Charazani"
bolivia$Municipio[bolivia$Municipio=="Vitichi"] <- "Vitiche"
bolivia$Municipio[bolivia$Municipio=="Villa de Sacaca"] <- "Sacaca"
bolivia$Municipio[bolivia$Municipio=="Villa Nueva (Loma Alta)"] <- "Villa Nueva"
bolivia$Municipio[bolivia$Municipio=="Postrer Valle"] <- "Postrervalle"
bolivia$Municipio[bolivia$Municipio=="Villamontes"] <- "Villa Montes"
bolivia$Municipio[bolivia$Municipio=="Santiago de Andamarca"] <- "Andamarca"
bolivia$Municipio[bolivia$Municipio=="Sopachuy"] <- "Sopachui"
bolivia$Municipio[bolivia$Municipio=="Soracachi (*)"] <- "Paria"
bolivia$Municipio[bolivia$Municipio=="Santiago de Sebastian Pagador"] <- "Santiago de Huari"
bolivia$Municipio[bolivia$Municipio=="Muyupampa"] <- "Villa Vaca Guzman"
bolivia$Municipio[bolivia$Municipio=="Ayopaya (Villa de Independencia)" ] <- "Ayopaya"
bolivia$Municipio[bolivia$Municipio=="Villa San Lorenzo"] <- "San Lorenzo"
bolivia$Municipio[bolivia$Municipio=="S.P. De Buena Vista"] <- "Buena Vista"
bolivia$Municipio[bolivia$Municipio=="Ascension de Guarayos"] <- "Ascencion de Guarayos"



shape_muni$Municipio[shape_muni$Municipio=="Alcala"] <- "Villa Alcala"
shape_muni$Municipio[shape_muni$Municipio=="Puerto Menor de Rurrenabaque"] <- "Rurrenabaque"
shape_muni$Municipio[shape_muni$Municipio=="Puerto Mayor de Guaqui"] <- "Guaqui"
shape_muni$Municipio[shape_muni$Municipio=="El Carmen Rivero Torrez"] <- "Carmen Rivero Torrez"
shape_muni$Municipio[shape_muni$Municipio=="Chuquihuta Ayllu Jucumani"] <- "Chuquiuta"
shape_muni$Municipio[shape_muni$Municipio=="Choquecota"] <- "Choque Cota"
shape_muni$Municipio[shape_muni$Municipio=="Jesus de Machaka"] <- "Jesus de Machaca"
shape_muni$Municipio[shape_muni$Municipio=="Nuestra Senora de La Paz"] <- "La Paz"
shape_muni$Municipio[shape_muni$Municipio=="Moromoro"] <- "Moro Moro"
shape_muni$Municipio[shape_muni$Municipio=="Yunguyo del Litoral"] <- "Yunguyo de Litoral"
shape_muni$Municipio[shape_muni$Municipio=="La (Marka) San Andres de Machaca"] <- "San Andres de Machaca"
shape_muni$Municipio[shape_muni$Municipio=="Puerto Mayor de Carabuco"] <- "Pto. Carabuco"
shape_muni$Municipio[shape_muni$Municipio=="Villa Ricardo Mugia - Icla"] <- "Icla"
shape_muni$Municipio[shape_muni$Municipio=="Sicasica"] <- "Sica Sica"
shape_muni$Municipio[shape_muni$Municipio=="San Miguel"] <- "San Miguel de Velasco"
shape_muni$Municipio[shape_muni$Municipio=="San Pablo"] <- "San Pablo de Lopez"
shape_muni$Municipio[shape_muni$Municipio=="San Jose"] <- "San Jose de Chiquitos"
shape_muni$Municipio[shape_muni$Municipio=="Santa Ana"] <- "Santa Ana de Yacuma"
shape_muni$Municipio[shape_muni$Municipio=="Villa Gualberto Villarroel"] <- "Cuchumuela"
shape_muni$Municipio[shape_muni$Municipio=="Alto Beni"] <- "Caranavi"
shape_muni$Municipio[shape_muni$Municipio=="Chua Cocani"] <- "Achacachi"
shape_muni$Municipio[shape_muni$Municipio=="Huatajata"] <- "Achacachi"
shape_muni$Municipio[shape_muni$Municipio=="Villa Charcas"] <- "Incahuasi"

shape_muni$Municipio <- ifelse(shape_muni$Departamento=="Santa Cruz" & 
                                 shape_muni$Municipio=="San Ignacio", "San Ignacio de Velasco",
                               shape_muni$Municipio)

shape_muni$Municipio <- ifelse(shape_muni$Departamento=="Santa Cruz" & 
                                 shape_muni$Municipio=="Santa Rosa", "Santa Rosa del Sara",
                               shape_muni$Municipio)




#### MERGE SHAPEFILES WITH CSV DATA

muni_merge <- left_join(shape_muni, bolivia, by=c('Departamento', 'Municipio'))


###### PREPARE DATA FOR PLOT

muni_merge$rural_muni <- as.factor(ifelse(muni_merge$PobRuralProp2001 >= .75, 1, 0)) # creates rural filter for org presence
muni_merge <- muni_merge %>%
  mutate(csutcb = case_when(Departamento == "Beni" |  Departamento == "La Paz" ~ "MIP",
                            Departamento == "Cochabamba" ~ "ASP/MAS-IPSP",
                            Departamento == "Oruro" ~ "MIP/MAS-IPSP",
                            Departamento == "Chuquisaca"| Departamento == "Pando" | Departamento == "Potos" | Departamento == "Santa Cruz" | Departamento == "Tarija" ~ "MAS-IPSP"))

muni_merge$csutcb1 <- as.factor(ifelse(muni_merge$rural_muni==1, muni_merge$csutcb, 0))
muni_merge$csutcb1[is.na(muni_merge$csutcb1)] <- 0



# FIGURE 6

p1 <- ggplot() + 
  geom_sf(data=muni_merge, color="gray90", mapping=aes(fill=csutcb1)) +
  scale_fill_manual(values = c("white","grey45","black","grey75","gray90"), name="Political Party",
  labels=c("Urban","ASP/MAS-IPSP","MAS-IPSP","MIP","MIP/MAS-IPSP"))+ 
  geom_sf(data = shape_dep, color='black', alpha=0)+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        plot.title=element_text(family="Times", face="bold", size=14))
print(p1)

ggsave(p1, filename = '../Data (APSR)/Figures/Fig6.tiff', width = 8, height = 8, device='tiff', dpi=1000)

dev.off()


p1_c <- ggplot() + 
  geom_sf(data=muni_merge, color="gray90", mapping=aes(fill=csutcb1)) +
  scale_fill_manual(values = c("white","darkgoldenrod1","blue3","red","yellow"), 
                    name="Political Party",
                    labels=c("Urban","ASP/MAS-IPSP","MAS-IPSP","MIP","MIP/MAS-IPSP")) +
  geom_sf(data = shape_dep, color='black', alpha=0)+
  theme(panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        plot.title=element_text(family="Times", face="bold", size=14))
print(p1_c)

ggsave(p1_c, filename = '../Data (APSR)/Figures/Fig6_color.tiff', width = 8, height = 8, device='tiff', dpi=1000)

dev.off()



# FIGURE 8

p2 <- ggplot() +
  geom_sf(data = muni_merge,color='gray29', mapping=aes(fill=mas_per2002)) +
  scale_fill_distiller(palette ="Greys", direction = 1, na.value="white")+
  geom_sf(data = shape_dep,  color="black", alpha=0) + 
  theme(legend.position= "none",
        panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
  plot.title=element_text(family="Times", face="bold", size=14))
print(p2)


ggsave(p2, filename = '../Data (APSR)/Figures/Fig8.tiff', width = 8, height = 8, device='tiff', dpi=1000)
dev.off()

