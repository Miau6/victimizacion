#### será el script de los delitos de las zonas metropolitanas
#vamos a crear una base de datos para relacionar municipios con zonas metro
#para el área de puerto vallarta, vamos a tener que meter el municipio de 
#bahía de banderas, pensar bien en como relacionarla

#vamos a mter la base de datos de delitos municipal, vieja metodología
library(readxl)
delitos_vieja <- read_excel("C:/Users/luis.orduna/Downloads/Incidencia municipal 2011 - 2017 agosto2020.xlsx")
library(dplyr)
library(tidyr)
#el objetivo será hacer una gráfica mensual del total de incidencia delictiva de la zona
#después una tabla con el total de la zona de los delitos descritos
#vamos a filtrar para jalisco y nayarit
delitos_vieja <- delitos_vieja %>%
  gather(Mes, Carpetas, ENERO:DICIEMBRE) %>% 
  filter(ENTIDAD=="JALISCO")
#vamos a crear la base de datos de las areas metropolitanas

# delitos_vieja <- delitos_vieja %>%
#   mutate(area=case_when(
#     INEGI==14039 | INEGI==14120 | INEGI==14101 | INEGI==14098 |
#       INEGI==14097 | INEGI==14070 | INEGI==14044 | INEGI==14051 |
#       INEGI==14124 | INEGI==14002 ~ "AMG",
#     INEGI==14067 | INEGI==18020 ~ "Puerto Vallarta",
#     INEGI==14063 | INEGI==14047 | INEGI==14066 ~ "Ocotlán",
#     INEGI==14015 | INEGI==14037 | INEGI==14054 ~ "Autlán",
#     INEGI==14121 | INEGI==14023 | INEGI==14079 ~ "Sur"
#   ))
#vamos a crear la variabe de fecha
library(lubridate)
delitos_vieja <- delitos_vieja %>%
  mutate(Mes=factor(Mes, 
                    levels = c("ENERO", "FEBRERO", "MARZO", "ABRIL",
                               "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE",
                               "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")),
         Fecha=ymd(paste0(AÑO, Mes, "01")))
#ponemos 0 en los valores faltantes
delitos_vieja$Carpetas <- ifelse(is.na(delitos_vieja$Carpetas),
                                 0, delitos_vieja$Carpetas)
#metemos el filtro para las ciudades medias
#las ciudades que nos interesan son 4: Arandas, Lagos de Moreno, San  juan de los lagos y Tepatitlán
ciudades_medias <- c("ARANDAS", "LAGOS DE MORENO", 
                     "SAN JUAN DE LOS LAGOS", "TEPATITLAN DE MORELOS")
delitos_vieja_area <- delitos_vieja %>% filter(MUNICIPIO==ciudades_medias) %>% 
  group_by(Fecha, MUNICIPIO) %>%
  summarise(Total=sum(Carpetas))

#vamos a meter la base de datos de la nueva metodología y hacer el mismo proceso 
delitos_nueva <- read.csv("C:/Users/luis.orduna/Downloads/Municipal-Delitos-2015-2020_dic2020/Municipal-Delitos-2015-2020_dic2020.csv")

delitos_nueva <- delitos_nueva %>%
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  filter(Entidad=="Jalisco")
#vamos a crear la base de datos de las areas metropolitanas

delitos_nueva <- delitos_nueva %>%
  mutate(area=case_when(
    Cve..Municipio==14039 | Cve..Municipio==14120 | Cve..Municipio==14101 | Cve..Municipio==14098 |
      Cve..Municipio==14097 | Cve..Municipio==14070 | Cve..Municipio==14044 | Cve..Municipio==14051 |
      Cve..Municipio==14124 | Cve..Municipio==14002 ~ "AMG",
    Cve..Municipio==14067 | Cve..Municipio==18020 ~ "Puerto Vallarta",
    Cve..Municipio==14063 | Cve..Municipio==14047 | Cve..Municipio==14066 ~ "Ocotlán",
    Cve..Municipio==14015 | Cve..Municipio==14037 | Cve..Municipio==14054 ~ "Autlán",
    Cve..Municipio==14121 | Cve..Municipio==14023 | Cve..Municipio==14079 ~ "Sur"
  ))
#vamos a crear la variabe de fecha
library(lubridate)
delitos_nueva <- delitos_nueva %>%
  mutate(Mes=factor(Mes, 
                    levels = c("Enero", "Febrero", "Marzo", "Abril",
                               "Mayo", "Junio", "Julio", "Agosto", "Septiembre",
                               "Octubre", "Noviembre", "Diciembre")),
         Fecha=ymd(paste0(Año, Mes, "01")))
#creamos la base de datos por area metropolitana
#metemos el filtro de ciudades medias
ciudades_medias_nueva <- c("Arandas", "Lagos de Moreno", 
                           "San Juan de los Lagos", "Tepatitlán de Morelos")
delitos_nueva_area <- delitos_nueva %>% filter(Municipio==ciudades_medias_nueva) %>% 
  group_by(Fecha, Municipio, Año) %>%
  summarise(Total=sum(Carpetas))
#eliminamos las observaciones del area vieja por amg del 15 al 17
delitos_vieja_area <- delitos_vieja_area %>%
  mutate(Año=year(Fecha)) %>% filter(Año<2015)
#combinamos ambas bases de datos
#convertimos los nombres de los municipios de la vieja metodología
delitos_vieja_area$MUNICIPIO[delitos_vieja_area$MUNICIPIO=="ARANDAS"] <- "Arandas"
delitos_vieja_area$MUNICIPIO[delitos_vieja_area$MUNICIPIO=="LAGOS DE MORENO"] <- "Lagos de Moreno"
delitos_vieja_area$MUNICIPIO[delitos_vieja_area$MUNICIPIO=="SAN JUAN DE LOS LAGOS"] <- "San Juan de los Lagos"
delitos_vieja_area$MUNICIPIO[delitos_vieja_area$MUNICIPIO=="TEPATITLAN DE MORELOS"] <- "Tepatitlán de Morelos"
delitos_vieja_area <- delitos_vieja_area %>%
  rename(Municipio=MUNICIPIO)

delitos_total_area <- rbind(delitos_vieja_area, delitos_nueva_area)
delitos_total_area <- delitos_total_area %>%
  drop_na(Municipio)
library(ggplot2)
library(ggrepel)
library(scales)
#vamos a realizar un filtro únicamente a Tepa porque no tiene registros en 2013. 
delitos_total_area <- delitos_total_area %>%
  filter()
#vamos a crear el ciclo para que se realicen las gráfica 
#creamos el vector de area
area <- c(
  "AMG", "Autlán", "Ocotlán", "Puerto Vallarta", "Sur")
grafica <- list()
for(i in 1:5){
  
grafica[[i]] <- delitos_total_area %>% 
  filter(Municipio==ciudades_medias_nueva[i]) %>%
  ggplot(aes(Fecha, Total)) +
  geom_point(colour="#FBBB27", size=1.5) +
  geom_line(size=.4) + 
  geom_text_repel(aes(label=format(Total,big.mark=",",scientific=FALSE)),
                  size=2.8) +
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = "gam", )
}
#vamos a graficar Arandas
grafica_tepa <- delitos_total_area %>% 
  filter(Municipio=="Tepatitlán de Morelos" & Año>2013) %>%
  ggplot(aes(Fecha, Total)) +
  geom_point(colour="#FBBB27", size=1.5) +
  geom_line(size=.4) + 
  geom_text_repel(aes(label=format(Total,big.mark=",",scientific=FALSE)),
                  size=2.8) +
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = "gam", )
ggsave(grafica_sur, filename = paste0("Sur", ".png"),
       width = 16, height = 7)
 for(i in 1:5){
   ggsave(grafica[[i]], 
          filename = paste0(ciudades_medias_nueva[i], ".png"),
          width = 14, height = 7)
   
 }



grafica_todas <- delitos_total_area %>%
  ggplot(aes(Fecha, Total)) +
  geom_point(colour="#FBBB27", size=1.5) +
  geom_line(size=.4) + 
  #geom_text_repel(aes(label=format(Total,big.mark=",",scientific=FALSE)),
                  #size=2.8) +
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(method = "gam", ) +
  facet_grid(Municipio~., scales = "free")


###vamos a crear la base de datos de delitos por amg. Será el total
#eliminamos del 15 al 17 de la vieja base de datos y colapsamos por delito y amg

delitos_vieja <- delitos_vieja %>%
  filter(AÑO<2015)

#vamos a agrupar por area y subtipode delito
delito_vieja_subtipo <- delitos_vieja %>%
  filter(SUBTIPO=="EXTORSION" | SUBTIPO=="FRAUDE" |
           SUBTIPO=="A NEGOCIO" | SUBTIPO=="A TRANSEUNTES" |
           grepl("\\w VEHICULOS",SUBTIPO))
delito_vieja_subtipo <- delito_vieja_subtipo %>% 
  mutate(SUBTIPO=ifelse(grepl("\\w VEHICULOS",SUBTIPO),
                        "VEHICULOS", SUBTIPO)) %>%
  filter(MUNICIPIO %in% ciudades_medias) %>% 
  group_by(SUBTIPO, MUNICIPIO) %>%
  summarise(Total=sum(Carpetas))
 #unicamente para Tepatitlán
#vamos a agrupar por area y subtipode delito
delito_vieja_subtipo <- delitos_vieja %>%
  filter(SUBTIPO=="EXTORSION" | SUBTIPO=="FRAUDE" |
           SUBTIPO=="A NEGOCIO" | SUBTIPO=="A TRANSEUNTES" |
           grepl("\\w VEHICULOS",SUBTIPO))
delito_vieja_subtipo <- delito_vieja_subtipo %>% 
  mutate(SUBTIPO=ifelse(grepl("\\w VEHICULOS",SUBTIPO),
                        "VEHICULOS", SUBTIPO)) %>%
  filter(MUNICIPIO %in% ciudades_medias) %>% 
  group_by(SUBTIPO, MUNICIPIO) %>%
  summarise(Total=sum(Carpetas)) %>%
  filter(MUNICIPIO!="TEPATITLAN DE MORELOS")
##########vamos a crear la base a partir de tipo
delitos_vieja_tipo <- delitos_vieja %>%
  filter(TIPO=="DOLOSOS" | TIPO=="DOLOSAS") %>%
  filter(MUNICIPIO %in% ciudades_medias) %>% 
  group_by(TIPO, MUNICIPIO) %>%
  summarise(Total=sum(Carpetas)) %>%
  filter(MUNICIPIO!="TEPATITLAN DE MORELOS")
delitos_vieja_tipo$TIPO[delitos_vieja_tipo$TIPO=="DOLOSOS"] <- "Homicidio doloso" 
delitos_vieja_tipo$TIPO[delitos_vieja_tipo$TIPO=="DOLOSAS"] <- "Lesiones dolosas" 
delitos_vieja_tipo <- delitos_vieja_tipo %>%
  rename(SUBTIPO=TIPO)


#juntamos ambas bases de datos (vieja)
delitos_vieja_total <- rbind(delitos_vieja_tipo, delito_vieja_subtipo)

delitos_vieja_total <- delitos_vieja_total %>% 
  filter(MUNICIPIO!="TEPATITLAN DE MORELOS")
delitos_vieja_total2 <- rbind(delitos_vieja_total, delitos_vieja_total_tepa)
#vamos a hacer el mismo tratamiento para la nueva metodologia
delito_nueva_subtipo <- delitos_nueva %>%
  filter(Subtipo.de.delito=="Extorsión" | Subtipo.de.delito=="Fraude" |
           Subtipo.de.delito=="Robo a negocio" | Subtipo.de.delito=="Robo de vehículo automotor" |
           Subtipo.de.delito=="Homicidio doloso" | Subtipo.de.delito=="Lesiones dolosas" |
           Subtipo.de.delito=="Feminicidio" |
           grepl("\\w transeúnte",Subtipo.de.delito))
delito_nueva_subtipo <- droplevels(delito_nueva_subtipo)

levels(delito_nueva_subtipo$Subtipo.de.delito)[7] <- "Robo a transeúnte"
levels(delito_nueva_subtipo$Subtipo.de.delito)[8] <- "Robo a transeúnte"
levels(delito_nueva_subtipo$Subtipo.de.delito)[2] <- "Homicidio doloso"
delito_nueva_subtipo$Subtipo.de.delito[delito_nueva_subtipo$Subtipo.de.delito=="Robo a transeúnte en espacio abierto al público"] <- "Robo a transeúnte"
delito_nueva_subtipo$Subtipo.de.delito[delito_nueva_subtipo$Subtipo.de.delito=="Robo a transeúnte en vía pública"] <- "Robo a transeúnte"
delito_nueva_subtipo$Subtipo.de.delito[delito_nueva_subtipo$Subtipo.de.delito=="Feminicidio"] <- "Homicidio doloso"

delito_nueva_subtipo2 <- delito_nueva_subtipo %>% 
  filter(Municipio %in% ciudades_medias_nueva) %>% 
  group_by(Subtipo.de.delito, Municipio) %>%
  summarise(Total=sum(Carpetas)) %>%
  rename(SUBTIPO=Subtipo.de.delito)

#vamos a modificar la base de datos de la nueva metodología para adecuarla a la vieja
delito_nueva_subtipo <- delito_nueva_subtipo %>%
  rename(SUBTIPO=Subtipo.de.delito)
delitos_vieja_total2$SUBTIPO[delitos_vieja_total2$SUBTIPO=="A NEGOCIO"] <- "Robo a negocio"
delitos_vieja_total2$SUBTIPO[delitos_vieja_total2$SUBTIPO=="A TRANSEUNTES"] <- "Robo a transeúnte"
delitos_vieja_total2$SUBTIPO[delitos_vieja_total2$SUBTIPO=="EXTORSION"] <- "Extorsión"
delitos_vieja_total2$SUBTIPO[delitos_vieja_total2$SUBTIPO=="FRAUDE"] <- "Fraude"
delitos_vieja_total2$SUBTIPO[delitos_vieja_total2$SUBTIPO=="VEHICULOS"] <- "Robo de vehículo automotor"
delitos_vieja_total2 <- delitos_vieja_total2 %>%
  rename(Municipio=MUNICIPIO)
#modificamos la variable municipio
delitos_vieja_total2$Municipio[delitos_vieja_total2$Municipio=="ARANDAS"] <- "Arandas"
delitos_vieja_total2$Municipio[delitos_vieja_total2$Municipio=="LAGOS DE MORENO"] <- "Lagos de Moreno"
delitos_vieja_total2$Municipio[delitos_vieja_total2$Municipio=="SAN JUAN DE LOS LAGOS"] <- "San Juan de los Lagos"
delitos_vieja_total2$Municipio[delitos_vieja_total2$Municipio=="TEPATITLAN DE MORELOS"] <- "Tepatitlán de Morelos"

delito_tipo_total <- rbind(delito_nueva_subtipo2, delitos_vieja_total2)
#vamos a adarle formato para poder agrupar
#lesiones y homicidio están bien todas las demás necesitarán modificarse


delito_tipo_total <- delito_tipo_total %>%
  group_by(Municipio, SUBTIPO) %>%
  summarise(Total=sum(Total)) %>%
  rename(Delito=SUBTIPO)
#vamos a realizar el spread para dejar la base lista
delito_tipo_total <- delito_tipo_total %>%
  spread(Delito, Total)

write.csv(delito_tipo_total, "tipos_delito_por_ciudades_medias.csv")

#falta el total de delito para realizar la resta y saber cuanto corresponde a todos
total_vieja <- delitos_vieja %>% filter(MUNICIPIO=="TEPATITLAN DE MORELOS" & AÑO==2014) %>% 
  group_by(MUNICIPIO) %>% summarise(Total=sum(Carpetas)) 

total_nueva <- delitos_nueva %>% filter(Municipio %in% ciudades_medias_nueva) %>% 
  group_by(Municipio) %>% summarise(Total=sum(Carpetas))

total_vieja$MUNICIPIO[total_vieja$MUNICIPIO=="ARANDAS"] <- "Arandas"
total_vieja$MUNICIPIO[total_vieja$MUNICIPIO=="LAGOS DE MORENO"] <- "Lagos de Moreno"
total_vieja$MUNICIPIO[total_vieja$MUNICIPIO=="SAN JUAN DE LOS LAGOS"] <- "San Juan de los Lagos"
total_vieja$MUNICIPIO[total_vieja$MUNICIPIO=="TEPATITLAN DE MORELOS"] <- "Tepatitlán de Morelos"
total_vieja <- total_vieja %>% 
  rename(Municipio=MUNICIPIO)

total_area <- rbind(total_vieja, total_nueva)
total_area <- total_area %>%
  group_by(Municipio) %>% summarise(Total=sum(Total))
