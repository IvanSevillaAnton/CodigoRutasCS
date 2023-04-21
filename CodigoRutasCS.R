
library(shiny)
library(TSP)
library(gdata)
library(readxl)
library(gor)
library(leaflet)
library(openrouteservice)


MatrizDistancias <- read_excel("MatrizDistanciasnueva.xlsx")
Distancias<-as.matrix(MatrizDistancias)
rownames(Distancias)<-colnames(Distancias)

Coordenadas<-read_excel("CoordenadasIvan.xlsx",col_names = TRUE,sheet=3)
names(Coordenadas)<-c("LATITUD","LONGITUD") 
Coordenadas<-as.matrix(Coordenadas)
coordenadas<-Coordenadas

ID1=rownames(coordenadas)<-rownames(Distancias)



ui<-fluidPage(
  titlePanel("RUTA DE REPARTO DE MATERIAL SANITARIO EN ZARAGOZA"),
  sidebarPanel(
    tabsetPanel(
      tabPanel(title="Sector Zaragoza I",
               checkboxGroupInput("variable1","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. ACTUR NORTE"="C.S. ACTUR NORTE","C.S. AMPARO POCH (ACTUR OESTE)"="C.S. AMPARO POCH (ACTUR OESTE)",
                                            "C.S. ACTUR SUR"="C.S. ACTUR SUR","C.S. ALFAJARÍN"="C.S. ALFAJARÍN",
                                            "C.S. ARRABAL"="C.S. ARRABAL","C.S. LA JOTA"="C.S. LA JOTA",
                                            "C.S. BUJARALOZ"="C.S. BUJARALOZ","C.S. LUNA"="C.S. LUNA",
                                            "C.S. PARQUE GOYA"="C.S. PARQUE GOYA","C.S. SANTA ISABEL"="C.S. SANTA ISABEL","C.S. VILLAMAYOR"="C.S. VILLAMAYOR",
                                            "C.S. PICARRAL"="C.S. PICARRAL","C.S. ZUERA"="C.S. ZUERA"),
                                  selected=c("C.S. ACTUR NORTE"="C.S. ACTUR NORTE","C.S. LUNA"="C.S. LUNA",
                                             "C.S. PARQUE GOYA"="C.S. PARQUE GOYA","C.S. SANTA ISABEL"="C.S. SANTA ISABEL","C.S. VILLAMAYOR"="C.S. VILLAMAYOR",
                                             "C.S. PICARRAL"="C.S. PICARRAL"),inline=TRUE)),
      
      
      tabPanel(title="Sector Zaragoza II",
               checkboxGroupInput("variable2","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. ALMOZARA"="C.S. ALMOZARA","C.S. CAMPO DE BELCHITE"="C.S. CAMPO DE BELCHITE",
                                            "C.S. CASABLANCA"="C.S. CASABLANCA","C.S. FERNANDO EL CATÓLICO"="C.S. FERNANDO EL CATÓLICO",
                                            "C.S. FUENTES DE EBRO"="C.S. FUENTES DE EBRO","C.S. PARQUE ROMA"="C.S. PARQUE ROMA",
                                            "C.S. PUERTA DEL CARMEN"="C.S. PUERTA DEL CARMEN","C.S. LAS FUENTES NORTE"="C.S. LAS FUENTES NORTE",
                                            "C.S. JOSÉ R. MUÑOZ FERNÁNDEZ (SAGASTA)"="C.S. JOSÉ R. MUÑOZ FERNÁNDEZ (SAGASTA)","C.S. REBOLERÍA"="C.S. REBOLERÍA",
                                            "C.S. ROMAREDA (SEMINARIO)"="C.S. ROMAREDA (SEMINARIO)","C.S. SAN JOSÉ"="C.S. SAN JOSÉ",
                                            "C.S. CANAL IMPERIAL"="C.S. CANAL IMPERIAL","C.S. SAN PABLO"="C.S. SAN PABLO",
                                            "C.S. SÁSTAGO"="C.S. SÁSTAGO","C.S. TORRE RAMONA"="C.S. TORRE RAMONA",
                                            "C.S. TORRERO-LA PAZ"="C.S. TORRERO-LA PAZ","C.S. VALDESPARTERA"="C.S. VALDESPARTERA"),
                                  selected=c("C.S. ALMOZARA"="C.S. ALMOZARA"),inline=TRUE)),
      
      
      tabPanel(title="Sector Zaragoza III",
               checkboxGroupInput("variable3","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. ALAGÓN"="C.S. ALAGÓN","C.S. BOMBARDA"="C.S. BOMBARDA",
                                            "C.S. BORJA"="C.S. BORJA","C.S. CARIÑENA"="C.S. CARIÑENA",
                                            "C.S. CASETAS"="C.S. CASETAS","C.S. DELICIAS NORTE"="C.S. DELICIAS NORTE",
                                            "C.S. DELICIAS SUR"="C.S. DELICIAS SUR",
                                            "C.S. VIRGEN DE LA OLIVA-EJEA DE LOS CABALLEROS"="C.S. VIRGEN DE LA OLIVA-EJEA DE LOS CABALLEROS","C.S. ÉPILA"="C.S. ÉPILA",
                                            "C.S. GALLUR"="C.S. GALLUR","C.S. HERRERA DE LOS NAVARROS"="C.S. HERRERA DE LOS NAVARROS",
                                            "C.S. LA ALMUNIA DE DOÑA GODINA"="C.S. LA ALMUNIA DE DOÑA GODINA","C.S. MARÍA DE HUERVA"="C.S. MARÍA DE HUERVA",
                                            "C.S. MIRALBUENO-GARRAPINILLOS"="C.S. MIRALBUENO-GARRAPINILLOS",
                                            "C.S. OLIVER"="C.S. OLIVER","C.S. SÁDABA"="C.S. SÁDABA",
                                            "C.S. SOS DEL REY CATÓLICO"="C.S. SOS DEL REY CATÓLICO","C.S. SAN ATILANO- TARAZONA"="C.S. SAN ATILANO- TARAZONA",
                                            "C.S. TAUSTE"="C.S. TAUSTE","C.S. UNIVERSITAS"="C.S. UNIVERSITAS",
                                            "C.S. UTEBO"="C.S. UTEBO",
                                            "C.S. VALDEFIERRO"="C.S. VALDEFIERRO"),
                                  selected=c("C.S. ALAGÓN"="C.S. ALAGÓN"),inline=TRUE)),
      
      
      tabPanel(title="Sector Alcañiz",
               checkboxGroupInput("variable4","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. AMANDO LORIGA-CASPE"="C.S. AMANDO LORIGA-CASPE",
                                            "C.S. MAELLA"="C.S. MAELLA"),
                                  selected=c("C.S. AMANDO LORIGA-CASPE"="C.S. AMANDO LORIGA-CASPE"),inline=TRUE)),
      
      
      tabPanel(title="Sector Barbastro",
               checkboxGroupInput("variable5","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. MEQUINENZA"="C.S. MEQUINENZA"),
                                  selected=c("C.S. MEQUINENZA"="C.S. MEQUINENZA"),inline=TRUE)),
      
      
      tabPanel(title="Sector Calatayud",
               checkboxGroupInput("variable6","Seleccione los centros de salud que desea abastecer",
                                  choices=c("C.S. ALHAMA DE ARAGÓN"="C.S. ALHAMA DE ARAGÓN",
                                            "C.S. ARIZA"="C.S. ARIZA","C.S. ATECA"="C.S. ATECA",
                                            "C.S. CALATAYUD"="C.S. CALATAYUD","C.S. DAROCA"="C.S. DAROCA","C.S. ILLUECA"="C.S. ILLUECA",
                                            "C.S. MORATA DE JALÓN"="C.S. MORATA DE JALÓN","C.S. SABIÑAN"="C.S. SABIÑAN",
                                            "C.S. VILLARROYA DE LA SIERRA"="C.S. VILLARROYA DE LA SIERRA"),
                                  selected=c("C.S. ALHAMA DE ARAGÓN"="C.S. ALHAMA DE ARAGÓN"),inline=TRUE)),
      
      
      tabPanel(title="Parámetros algoritmo genético",
        numericInput("Npop","Seleccione la población inicial",value=10, min=2,max=100, step=1),
        numericInput("Ngen","Seleccione el número de generaciones",value=1, min=1,max=100, step=1),
        numericInput("Local","Seleccione el parámetro Local",value=1, min=0,max=1, step=0.05),
        numericInput("Elite","Seleccione el parámetro Elite",value=2, min=1,max=6, step=1))),
    
    width=45),
  
  submitButton("Calcular ruta"),
  
  mainPanel(
            tableOutput("values"),
            leafletOutput("mapa"))
  )




server<-function(input,output){
  
  slidervalues<-reactive({
    
   MatrizTrabajo<-Distancias[c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                     c(input$variable2),c(input$variable3),
                                     c(input$variable4),c(input$variable5),c(input$variable6)),
                                   c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                     c(input$variable2),c(input$variable3),
                                     c(input$variable4),c(input$variable5),
                                     c(input$variable6))]
   
   
   if(length(c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
               c(input$variable2),c(input$variable3),
               c(input$variable4),c(input$variable5),
               c(input$variable6)))<2){
     stop("Elegir como mínimo un centro de salud para obtener ruta")}
   
     else{
       
    if(length(c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
              c(input$variable2),c(input$variable3),
              c(input$variable4),c(input$variable5),
              c(input$variable6)))<10){
      
    MatrizTrabajo[lower.tri(MatrizTrabajo)]=t(MatrizTrabajo)[lower.tri(MatrizTrabajo)]
    MatrizTSP<-TSP(as.matrix(MatrizTrabajo), labels = NULL)
    initialtour<-as.integer(which(labels(MatrizTrabajo)[[1]]=="PLATAFORMA LOGÍSTICA DEL SALUD"))
    
    solucion1<-solve_TSP((MatrizTSP),method="farthest_insertion",control=list(start=initialtour))
    solucion2<-solve_TSP((MatrizTSP),method="nearest_insertion",control=list(start=initialtour))
    Longitud1<-tour_length(solucion1)
    Longitud2<-tour_length(solucion2)
    
    Ruta<-list(c(paste("Longitud (km):",round(Longitud1,digits=2)),labels(solucion1)),c(paste("Longitud (km):",round(Longitud2,digits=2)),labels(solucion2)))
   
    Longitudes<-c(Longitud1,Longitud2)
    Seleccion<-which.min(Longitudes)
    
    Ruta}
      
       
    else {
      MatrizTrabajo[lower.tri(MatrizTrabajo)]=t(MatrizTrabajo)[lower.tri(MatrizTrabajo)]
      isSymmetric(MatrizTrabajo)
      MatrizAG<-MatrizTrabajo
      colnames(MatrizAG)<-NULL
      MatrizTSP<-TSP(as.matrix(MatrizTrabajo), labels = NULL)
      initialtour<-as.integer(which(labels(MatrizTrabajo)[[1]]=="PLATAFORMA LOGÍSTICA DEL SALUD"))
      
      solucion1<-solve_TSP((MatrizTSP),method="farthest_insertion",control=list(start=initialtour))
      solucion2<-solve_TSP((MatrizTSP),method="nearest_insertion",control=list(start=initialtour))
      solucion3<-search_tour_genetic(MatrizAG,dim(MatrizAG)[1],input$Npop,input$Ngen,input$Local,input$Elite)
      Longitud1<-tour_length(solucion1)
      Longitud2<-tour_length(solucion2)
      Longitud3<-solucion3$distance

      Ruta<-list(c(paste("Longitud (km):",round(Longitud1,digits=2)),labels(solucion1)),c(paste("Longitud (km):",round(Longitud2,digits=2)),labels(solucion2)),c(paste("Longitud (km):",round(Longitud3,digits = 2)),colnames(MatrizTrabajo)[solucion3$tour]))
      
      Longitudes<-c(Longitud1,Longitud2,Longitud3)
      Seleccion<-which.min(Longitudes)
      
      Ruta
      }
  }})
  
  
  
  mapaselect<-reactive({
    
    if(length(c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                c(input$variable2),c(input$variable3),
                c(input$variable4),c(input$variable5),
                c(input$variable6)))<10){
      
      coordenadas<-coordenadas[c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                 c(input$variable2),c(input$variable3),
                                 c(input$variable4),c(input$variable5),
                                 c(input$variable6)),]
      
      coordenadas<-as.data.frame(coordenadas)
      
      MatrizTrabajo<-Distancias[c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                  c(input$variable2),c(input$variable3),
                                  c(input$variable4),c(input$variable5),c(input$variable6)),
                                c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                  c(input$variable2),c(input$variable3),
                                  c(input$variable4),c(input$variable5),
                                  c(input$variable6))]
                                                      
      
      MatrizTrabajo[lower.tri(MatrizTrabajo)]=t(MatrizTrabajo)[lower.tri(MatrizTrabajo)]
      MatrizTSP<-TSP(as.matrix(MatrizTrabajo), labels = NULL)
      initialtour<-as.integer(which(labels(MatrizTrabajo)[[1]]=="PLATAFORMA LOGÍSTICA DEL SALUD"))
      
      solucion1<-solve_TSP((MatrizTSP),method="farthest_insertion",control=list(start=initialtour))
      solucion2<-solve_TSP((MatrizTSP),method="nearest_insertion",control=list(start=initialtour))
      Longitud1<-tour_length(solucion1)
      Longitud2<-tour_length(solucion2)
      
      Ruta<-list(labels(solucion1),labels(solucion2))
      Longitudes<-c(Longitud1,Longitud2)
      Seleccion<-which.min(Longitudes)
      Rutafinal<-Ruta[[Seleccion]]
      coordenadas<-coordenadas[order(Rutafinal),]
      coordenadas<-as.data.frame(coordenadas)
      ruta<-data.frame(coordenadas$LONGITUD,coordenadas$LATITUD)
      names(ruta)<-c("LONGITUD","LATITUD")
      ID1=rownames(coordenadas)
      ors_api_key('5b3ce3597851110001cf6248c461a41c55e84e7fac8efbda9e23fc4e')
      route <- ors_directions(asplit(ruta,1), format="geojson",radiuses=1000000)
      mapa <- leaflet(data = coordenadas) %>%
        addTiles() %>% 
        setView(lng = coordenadas$LONGITUD[1], lat = coordenadas$LATITUD[1] , zoom = 1) %>% 
        addMarkers(~LONGITUD, ~LATITUD, popup = ~as.character(ID1), label = ~as.character(ID1))%>%
        addGeoJSON(route, fill=FALSE) %>%
        fitBBox(route$bbox)}
    
    else{
    coordenadas<-coordenadas[c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                               c(input$variable2),c(input$variable3),
                               c(input$variable4),c(input$variable5),
                               c(input$variable6)),]
    
    coordenadas<-as.data.frame(coordenadas)
    MatrizTrabajo<-Distancias[c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                c(input$variable2),c(input$variable3),
                                c(input$variable4),c(input$variable5),c(input$variable6)),
                              c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                                c(input$variable2),c(input$variable3),
                                c(input$variable4),c(input$variable5),
                                c(input$variable6))]
    
    MatrizTrabajo[lower.tri(MatrizTrabajo)]=t(MatrizTrabajo)[lower.tri(MatrizTrabajo)]
    isSymmetric(MatrizTrabajo)
    MatrizAG<-MatrizTrabajo
    colnames(MatrizAG)<-NULL
    MatrizTSP<-TSP(as.matrix(MatrizTrabajo), labels = NULL)
    initialtour<-as.integer(which(labels(MatrizTrabajo)[[1]]=="PLATAFORMA LOGÍSTICA DEL SALUD"))
    
    solucion1<-solve_TSP((MatrizTSP),method="farthest_insertion",control=list(start=initialtour))
    solucion2<-solve_TSP((MatrizTSP),method="nearest_insertion",control=list(start=initialtour))
    solucion3<-search_tour_genetic(MatrizAG,dim(MatrizAG)[1],input$Npop,input$Ngen,input$Local,input$Elite)
    
    Longitud1<-tour_length(solucion1)
    Longitud2<-tour_length(solucion2)
    Longitud3<-solucion3$distance
    
    Ruta<-list(labels(solucion1),labels(solucion2),colnames(MatrizTrabajo)[solucion3$tour])
    Longitudes<-c(Longitud1,Longitud2,Longitud3)
    Seleccion<-which.min(Longitudes)
    Rutafinal<-Ruta[[Seleccion]]
    coordenadas<-coordenadas[order(Rutafinal),]
    coordenadas<-as.data.frame(coordenadas)
    ruta<-data.frame(coordenadas$LONGITUD,coordenadas$LATITUD)
    names(ruta)<-c("LONGITUD","LATITUD")
    ID1=rownames(coordenadas)
    ors_api_key('5b3ce3597851110001cf6248c461a41c55e84e7fac8efbda9e23fc4e')
    route <- ors_directions(asplit(ruta,1), format="geojson",radiuses=1000000)
    mapa <- leaflet(data = coordenadas) %>%
      addTiles() %>% 
      setView(lng = coordenadas$LONGITUD[1], lat = coordenadas$LATITUD[1] , zoom = 1) %>% 
      addMarkers(~LONGITUD, ~LATITUD, popup = ~as.character(ID1), label = ~as.character(ID1))%>%
      addGeoJSON(route, fill=FALSE) %>%
      fitBBox(route$bbox)
    }})
  
  
  output$values<-renderTable({
    
    Tabla<-data.frame(slidervalues())
    
    if(length(c("PLATAFORMA LOGÍSTICA DEL SALUD",c(input$variable1),
                c(input$variable2),c(input$variable3),
                c(input$variable4),c(input$variable5),
                c(input$variable6)))<10){
      
      names(Tabla)<-c("ALGORITMO DEL VECINO MÁS LEJANO","ALGORITMO DEL VECINO MÁS CERCANO")
                Tabla}
    
    else{
      names(Tabla)<-c("ALGORITMO DEL VECINO MÁS LEJANO","ALGORITMO DEL VECINO MÁS CERCANO","ALGORITMO GENÉTICO")
    Tabla}
    
     })
  
  output$mapa<-renderLeaflet({mapaselect()})
}

shinyApp(ui = ui, server = server)

  