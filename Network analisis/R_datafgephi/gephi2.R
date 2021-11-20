

farmacias = read.csv("../farmacias.csv")
farmacias_stgo = farmacias[farmacias$fk_region == 7,]
data_farmacias = data.frame( id = 1:length(farmacias_stgo$fecha),
                             farmacias_stgo$local_nombre,
                             farmacias_stgo$comuna_nombre, 
                             farmacias_stgo$local_lat, 
                             farmacias_stgo$local_lng)
data_farmacias$farmacias_stgo.local_lat[data_farmacias$farmacias_stgo.local_lat == ""] <- NA
data_farmacias$farmacias_stgo.local_lng[data_farmacias$farmacias_stgo.local_lng == ""] <- NA
farmaciasclean <- na.omit(data_farmacias)


library(mice)
md.pattern(farmaciasclean)

write.csv(x = farmaciasclean,"../farmaciascleaned.csv")

############Nodos##############

nodos <- read.csv("../Nodosfinal2.csv")
nodos2 <- data.frame(id = 1:length(farmaciasclean$id),
                     farmacia = nodos$id,
                     label = nodos$label,
                     latitude =farmaciasclean$farmacias_stgo.local_lat,
                     longitude = farmaciasclean$farmacias_stgo.local_lng
                     )

nodos3 <- data.frame(id = nodos$label,
                     latitude =farmaciasclean$farmacias_stgo.local_lat,
                     longitude = farmaciasclean$farmacias_stgo.local_lng,
                     label = nodos$id)

write.csv(nodos2, "../nodos2.csv")
write.csv(nodos3, "../nodos3")
###########Lon Lng#############

aristas <- data.frame(Source = nodos2$id,
                      Target = nodos2$farmacia)

write.csv(aristas, "../aristas.csv")

########Aristas arregladas por zona#########

zonanodos <- read.csv("../aristas2.csv")
aristaszonas <- read.csv("../comunasurnorte.csv", sep = ";")


mergezonas <- merge(zonanodos, aristaszonas, by = "Source")

aristasxzona <- data.frame(Target = mergezonas$X,
                           Source = mergezonas$Zonas,
                           Comunas = mergezonas$Source)
aristasxzona2 <- aristasxzona[order(aristasxzona$Target),]
  
write.csv(aristasxzona2, "../aristasporzona.csv")

#####Remplazar farmacias#########

excelgephi <- read.csv("../Nodosxzona.csv")

columna1 <- ifelse(excelgephi$Farm != "AHUMADA" &
                     excelgephi$Farm != "CRUZ VERDE" &
                     excelgephi$Farm != "SALCOBRAND",
                   yes  = "OTRO",
                   no  = fifelse(excelgephi$Farm == "AHUMADA", "AHUMADA", 
                                 fifelse(excelgephi$Farm == "CRUZ VERDE", "CRUZ VERDE", "SALCOBRAND")))

View(columna1)

excelgephi2 <- cbind(excelgephi, columna1)

write.csv(excelgephi2, "../Nodosxzona2.csv")


########Tamaño por nodo########


nodosportamano <- read.csv("../nodosxzona4.csv")
columna2 <- ifelse(nodosportamano$columna1 != "AHUMADA" &
                     nodosportamano$columna1 != "CRUZ VERDE" &
                     nodosportamano$columna1 != "SALCOBRAND",
                   yes  = 1,
                   no  = ifelse(nodosportamano$columna1 == "AHUMADA", 1.1, 
                                 ifelse(nodosportamano$columna1 == "CRUZ VERDE", 1.15, 1.2)))

nodosportamano2 <- cbind(nodosportamano, columna2)

write.csv(nodosportamano2, "../nodosportamano2.csv")


#########añadir peso a las aristas############

nodos3 <- read.csv("../aristas2.csv")
poblacion <- read.csv("../poblacionporcomuna.csv")
poblacion_filtrada <- data.frame(id =poblacion$Id,)


##########Medir distancias##########
install.packages("geosphere")
install.packages("scales")

library(geosphere)
library(tidyr)
library(scales)

distancias <- read.csv("../nodos2.csv")
distancias2 <- distancias
names(distancias2)[names(distancias2) == "id"] <- "id2"
identificadores <- crossing(id = distancias$id, id2 =distancias$id)
coordenadas <- merge(identificadores, distancias, by = "id")
names(coordenadas)[names(coordenadas) == "latitude"] <- "latitudeA"
names(coordenadas)[names(coordenadas) == "longitude"] <- "longitudeA"
coordenadas2 <- merge(coordenadas, distancias2, by = "id2" )


paramedirdist <- data.frame(id=coordenadas2$id,id2=coordenadas2$id2, 
                            latitudeA = coordenadas2$latitudeA, longitudeA = coordenadas2$longitudeA,
                            latitudeB = coordenadas2$latitude, longitudeB = coordenadas2$longitude)
paramedirdistbackup <- paramedirdist

paramedirdist2 <- paramedirdist[order(paramedirdist$id),]



rownames(paramedirdist2, 1:length(paramedirdist2))


distanciascalculadas <- distm(paramedirdist2[1:10,c("longitudeA","latitudeA")],
                              paramedirdist2[1:10,c("longitudeB","latitudeB")])

medicion <- distm(distancias[, c("longitude", "latitude")])

distasvector <- as.vector(medicion)

aristas_distancia <- data.frame(id1 = identificadores$id,  id2 =identificadores$id2, distancias = distasvector)

longitud <- ifelse(aristas_distancia$distancias ==0,yes = "identico",
                   no =ifelse(aristas_distancia$distancias < 500, yes = "A",
                   no = ifelse(aristas_distancia$distancias < 1000, yes = "B",
                               no = ifelse(aristas_distancia$distancias < 2000, yes = "C", no ="D"))))

aristas_distancia$longitudclase <- longitud
pesos_aristas <- rescale(distasvector, to = c(1,10))
aristas_distancia$pesos <- 1/pesos_aristas

write.csv(aristas_distancia, "../aristas_distancias.csv")

aristas_distancias_filtrado <- aristas_distancia[aristas_distancia$longitudclase == "A",]
aristas_distancias_filtrado2 <- aristas_distancia[aristas_distancia$longitudclase == "B",]
aristas_distancias_filtrado3 <- aristas_distancia[aristas_distancia$longitudclase == "C",]

write.csv(aristas_distancias_filtrado, "../aristas_distancias_A.csv")
write.csv(aristas_distancias_filtrado2, "../aristas_distancias_B.csv")
write.csv(aristas_distancias_filtrado3, "../aristas_distancias_C.csv")

aristas_distancias_filtrado_ABC <- aristas_distancia[aristas_distancia$longitudclase == "A" |
                                                       aristas_distancia$longitudclase == "B" |
                                                       aristas_distancia$longitudclase == "C",]

pesos_aristas2 <- rescale(aristas_distancias_filtrado_ABC$distancias, to = c(1,10))
aristas_distancias_filtrado_ABC$pesos <- 1/pesos_aristas2

write.csv(aristas_distancias_filtrado_ABC, "../aristas_distancias_ABC.csv")


################Calculo centralidades###################
library(tidyverse)
library(tibble)

excel_centralidades <- read.csv("../farmacias_con_distancia_pesos_centralidades.csv")

tabla_filtrocol <- excel_centralidades %>%
  select(Id, Label, farmacia, Degree, Weighted.Degree) %>%
  slice_head(n = 713) %>%
  group_by(Label)
  
farmacias_separadas <- ifelse(tabla_filtrocol$Label != "AHUMADA" &
                                  tabla_filtrocol$Label != "CRUZ VERDE" &
                                  tabla_filtrocol$Label != "SALCOBRAND",
                                   yes  = "OTRO",
                                   no  = ifelse(tabla_filtrocol$Label == "AHUMADA", "AHUMADA", 
                                  ifelse(tabla_filtrocol$Label == "CRUZ VERDE", "CRUZ VERDE", "SALCOBRAND")))
tabla_filtrocol$Label <- farmacias_separadas

#GRADO POR FARMACIA

tabla_filtrocol2 <- as_tibble(tabla_filtrocol) %>%
  group_by(Label) %>%
  summarise(mean_degree = mean(Degree),
            mean_wdegree = mean(Weighted.Degree), .groups = 'drop')  %>%
  gather(key , value, -Label)

trans <- function(x){pmin(x,2) + 0.05*pmax(x-2,0)}
trans(c(0, 1, 2, 10,15,20,25,30))



ggplot(tabla_filtrocol2, aes(x = reorder(Label, value), y = value, fill = key)) +
  geom_col(width = 0.7, color = "black") +
  theme_light()+
  geom_text(aes(label=round(value,2)),position = position_stack(vjust = 1), vjust=1.5, size=3.5)+
  labs(title ="Degree mean by drugstore", x = "Drugstore", y = "Degree mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black"))+
  scale_y_continuous(trans ="log2", labels = c("","","","","")) +
  scale_fill_discrete(name = "Centrality", labels = c("Mean degree", "Mean weigthed degree"))



ggplot(tabla_filtrocol2, aes(x = reorder(Label, mean_wdegree), y = mean_wdegree)) +
  geom_col(width = 0.7, color = c("darkred","darkgreen","black","darkblue"),
           fill = c("darkred","darkgreen","black","darkblue"),
           position=position_dodge(0.5)) +
  theme_light()+
  geom_text(aes(label=round(mean_wdegree,2)), vjust=-0.3, size=3.5)+
  labs(title ="Weigthed degree mean by drugstore", x = "Drugstore", y = "Weigthed degree mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black"))


#GRADO POR COMUNA

tabla_filtrocol3 <- as_tibble(tabla_filtrocol) %>%
  group_by(farmacia) %>%
  summarise(mean_degree = mean(Degree),
            mean_wdegree = mean(Weighted.Degree), .groups = 'drop') 

tabla_filtrocol4 <- tabla_filtrocol3%>%
  arrange(mean_degree) %>%
  top_n(10, wt = mean_degree)

tabla_filtrocol5 <- tabla_filtrocol3%>%
  arrange(mean_wdegree) %>%
  top_n(10, wt = mean_wdegree) %>%
  gather(key  , value, -farmacia)

ggplot(tabla_filtrocol5, aes(x = reorder(farmacia, value), y = value, fill = key)) +
  geom_col(width = 0.7, color ="black") +
  theme_minimal()+
  geom_text(aes(label=round(value,2)), position = position_stack(vjust = 1),vjust=0.4,hjust = 1.3, size=3.5, color = "black")+
  labs(title ="Degree mean by commune", x = "Commune", y = "Degree mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1.9, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black")) +
  coord_flip()+
  scale_y_continuous(trans ="log2", labels =c("","","","")) +
  scale_fill_discrete(name = "Centrality", labels = c("Mean degree", "Mean weigthed degree"))


ggplot(tabla_filtrocol5, aes(x = reorder(farmacia, mean_wdegree), y = mean_wdegree)) +
  geom_col(width = 0.7,position=position_dodge(0.5), fill = "steelblue") +
  theme_minimal()+
  geom_text(aes(label=round(mean_wdegree,2)), vjust=0.4,hjust = 1.3, size=3.5, color = "white")+
  labs(title ="Weigthed degree mean by commune", x = "Commune", y = "Weigthed degree mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1.9, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black")) +
  coord_flip() 


#EIGEN VECTOR, clossnes y betweeness#

tabla_filtrocol_ev <- excel_centralidades %>%
  select(Id, Label, farmacia, eigencentrality, closnesscentrality, betweenesscentrality) %>%
  slice_head(n = 713) %>%
  group_by(Label)

tabla_filtrocol_ev$Label <- ifelse(tabla_filtrocol_ev$Label != "AHUMADA" &
                                     tabla_filtrocol_ev$Label != "CRUZ VERDE" &
                                     tabla_filtrocol_ev$Label != "SALCOBRAND",
                              yes  = "OTRO",
                              no  = ifelse(tabla_filtrocol_ev$Label == "AHUMADA", "AHUMADA", 
                                           ifelse(tabla_filtrocol_ev$Label == "CRUZ VERDE", "CRUZ VERDE", "SALCOBRAND")))

#Por farmacia#

tabla_filtrocol2_ev <- as_tibble(tabla_filtrocol_ev) %>%
  group_by(Label) %>%
  summarise(mean_ev = mean(eigencentrality),
            mean_cls = mean(closnesscentrality),
            mean_btw = mean(betweenesscentrality),
            .groups = 'drop')

ggplot(tabla_filtrocol2_ev, aes(x = reorder(Label, mean_ev), y = mean_ev)) +
  geom_col(width = 0.7, color = c("darkred","darkgreen","black","darkblue"),
           fill = c("darkred","darkgreen","black","darkblue"),
           position=position_dodge(0.5)) +
  theme_minimal()+
  geom_text(aes(label=round(mean_ev,2)), vjust=-0.3, size=3.5)+
  labs(title ="Eigenvector centrality mean by drugstore", x = "Drugstore", y = "Eigenvector centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black"))

ggplot(tabla_filtrocol2_ev, aes(x = reorder(Label, mean_cls), y = mean_cls)) +
  geom_col(width = 0.7, color = c("darkred","darkgreen","black","darkblue"),
           fill = c("darkred","darkgreen","black","darkblue"),
           position=position_dodge(0.5)) +
  theme_minimal()+
  geom_text(aes(label=round(mean_cls,2)), vjust=-0.3, size=3.5)+
  labs(title ="Closeness centrality mean by drugstore", x = "Drugstore", y = "Closeness centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black"))

ggplot(tabla_filtrocol2_ev, aes(x = reorder(Label, mean_btw), y = mean_btw)) +
  geom_col(width = 0.7, color = c("darkred","darkgreen","black","darkblue"),
           fill = c("darkred","darkgreen","black","darkblue"),
           position=position_dodge(0.5)) +
  theme_minimal()+
  geom_text(aes(label=round(mean_btw,2)), vjust=-0.3, size=3.5)+
  labs(title ="Betweenness centrality mean by drugstore", x = "Drugstore", y = "Betweenness centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black"))


#Por comuna#

tabla_filtrocol3_ev <- as_tibble(tabla_filtrocol_ev) %>%
  group_by(farmacia) %>%
  summarise(mean_ev = mean(eigencentrality),
            mean_cls = mean(closnesscentrality),
            mean_btw = mean(betweenesscentrality),
            .groups = 'drop')

tabla_filtrocol4_ev <- tabla_filtrocol3_ev %>%
  arrange(mean_ev) %>%
  top_n(10, wt = mean_ev)

ggplot(tabla_filtrocol4_ev, aes(x = reorder(farmacia, mean_ev), y = mean_ev)) +
  geom_col(width = 0.7,position=position_dodge(0.5), fill = "steelblue") +
  theme_minimal()+
  geom_text(aes(label=round(mean_ev,2)), vjust=0.4,hjust = 1.3, size=3.5, color = "white")+
  labs(title ="Eigenvector centrality mean by communes", x = "Communes", y = "Eigenvector centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1.3, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black")) +
  coord_flip() 

tabla_filtrocol5_ev <- tabla_filtrocol3_ev %>%
  arrange(mean_cls) %>%
  top_n(10, wt = mean_cls)

ggplot(tabla_filtrocol5_ev, aes(x = reorder(farmacia, mean_cls), y = mean_cls)) +
  geom_col(width = 0.7,position=position_dodge(0.5), fill = "steelblue") +
  theme_minimal()+
  geom_text(aes(label=round(mean_cls,2)), vjust=0.4,hjust = 1.3, size=3.5, color = "white")+
  labs(title ="Closeness centrality mean by communes", x = "Communes", y = "Closeness centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1.3, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black")) +
  coord_flip() 

tabla_filtrocol6_ev <- tabla_filtrocol3_ev %>%
  arrange(mean_btw) %>%
  top_n(10, wt = mean_btw)

ggplot(tabla_filtrocol6_ev, aes(x = reorder(farmacia, mean_btw), y = mean_btw)) +
  geom_col(width = 0.7,position=position_dodge(0.5), fill = "steelblue") +
  theme_minimal()+
  geom_text(aes(label=round(mean_btw,2)), vjust=0.4,hjust = 1.3, size=3.5, color = "white")+
  labs(title ="Betweenness centrality mean by comunnes", x = "Communes", y = "Betweenness centrality mean")+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1.3, axis.title.x = element_text(vjust=-0.5),
        title = element_text(face = "bold", color = "black")) +
  coord_flip() 
