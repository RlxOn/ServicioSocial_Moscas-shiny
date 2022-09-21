#### bibliotecas -----------
library(ggplot2)
library(dplyr)
library(car)
library(plotly)
library(readxl)
library(fossil)
library(BiodiversityR)
library(sp)
library(sf)
library(shapefiles)
library(raster)
library(rgdal)
library(leaflet)
library(broom)
library(cat)
library(leaflet.providers)
library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(htmltools)
library(bslib)
setwd("C:\\Users\\raulb\\Downloads\\")

##### Construir el encabezado -----------
header <- dashboardHeader(
  title = HTML("Familias Sarcophagidae y Phoridae en el centro de México"),
  disable = FALSE,
  titleWidth = 600
)

##### Sidebar -------------
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = 'sidebar',
    style = 'height: 90vh; overflow-y: auto;',

    ## Primer tab
    menuItem("Estadísticas generales", tabName = 'general_tab', startExpanded = F, icon = icon("chart-area"),
             menuSubItem('Estimadores', tabName = "estim_tab", icon = icon("chart-line")),
             menuSubItem("Abundancia por zona", tabName = "abzona_tab", icon = icon("chart-bar")),
             menuSubItem("Abundancia por género y especie", tabName = "genesp_tab", icon = icon("chart-bar"))),
    
    ##Segundo tab
    menuItem("Localización", tabName = "mapa_tab", startExpanded = F, icon = icon("globe")),
    useShinyjs()
  )
)

###### body --------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "estim_tab", h2("Estimadores e índices"),
    h3("Índices de abundancia"),
    fluidRow(
    plotlyOutput("plot_indices")),
            h3("Estimadores de abundancia"),
    fluidRow(
      plotlyOutput("plot_estimadores")
    )
    ),
    tabItem(tabName = "abzona_tab", h2("Abundancia por Zona"),
            fluidRow(
              valueBox("OXT", "La especie con más presencia en Cantera Oriente (UNAM).",
                       icon = icon("binoculars"), color = "olive"),
              valueBox("Cantera Oriente","La localidad con mayor número de individuos encontrados (122).",
                       icon = icon("seedling"), color = "blue"),
              valueBox("Más hembras","Se encontraron en la mayoría de las localidades.",
                       icon = icon("venus"), color = "yellow") 
            ),
            h3("Mapa de calor"),
            fluidRow(
              plotOutput("plot_tabasco")
            ),
            h3("Abundancia total por zona"),
            fluidRow(
              plotlyOutput("plot_indzona")
            )
            ),
    tabItem(tabName = "genesp_tab", h2("Abundancia por género y especie"),
            fluidRow(
              valueBox("SAS", "La especie con más individuos.", color = "green",
                       icon = icon("bug")),
              valueBox("Oxysarcodexia", "El género predominante.", color = "blue",
                       icon = icon("plus")),
              valueBox("Peckia","Género compuesto en su mayoría por hembras",
                       color = "orange", icon = icon("venus")),
              valueBox("Ravinia","Género compuesto casi en su totalidad por machos",
                       color = "red", icon = icon("mars"))
            ),
            h2("Abundancia por especie"),
            fluidRow(
              plotlyOutput("plot_especie", width = "auto", height = "600px")
            ),
            h2("Abundancia por género"),
            fluidRow(
              plotlyOutput("plot_genero")
            ),
            h2("Abundancia por especie (Comparación de hembras y machos)"),
            fluidRow(
              plotlyOutput("plot_genesp", width = "auto", height = "1000px")
            )),
    tabItem(tabName = "mapa_tab", h2("Localidades de muestreo"), 
            fluidRow(
              leafletOutput("leaf_mapa")),
            h4("Especies presentes en la localidad elegida"),
            fluidRow(
              column(4,
                     plotOutput("pie_total")), 
              br(), br(), br(),
              column(8,
                     valueBox("Especies", h4(textOutput("text_espec")), color = "aqua",
                              icon = icon("bug"), width = 10))
                
              )
            )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "green")

server <- function(input,output){
  ##### indices ######
  estim <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Estimatessarcophagidae.xlsx",
                     sheet = "FANY", range = cell_limits(ul = c(1,3), lr = c(182,57)))
  
  estim$ZONA <- as.factor(estim$ZONA)
  
  num_especie <- specnumber(estim[2:52]) #Numero de especies en cada muestreo
  index_shanon <- diversity(estim[2:52], index = "shannon")
  index_simpson <- diversity(estim[2:52], index = "simpson")
  index_simpinv <- diversity(estim[2:52], index = "invsimpson")
  index_pielou <- index_shanon/num_especie
  muestra <- seq(1,dim(estim)[1],1)
  
  indexes <- data.frame(muestra, num_especie, index_shanon, index_simpson, 
                        index_simpinv, index_pielou)
  
  fig_index <- plot_ly(indexes, x = ~muestra)
  fig_index <- fig_index %>% add_trace(y =~ num_especie, name = "Número de especies", mode = "lines") %>%
    add_trace(y =~ index_shanon, name = "Índice de Shanon", mode = "lines") %>%
    add_trace(y =~ index_simpson, name = "Índice de Simpson", mode = "lines") %>%
    add_trace(y =~ index_simpinv, name = "Índice de Simpson Inverso", mode = "lines") %>%
    add_trace(y =~ index_pielou, name = "Índice de Pielou", mode = "lines") %>%
    layout(yaxis = list(title = ""))
  
  output$plot_indices <- renderPlotly({fig_index})
  
  ######## Estimadores #######
  estim2 <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Estimatessarcophagidae.xlsx",
                      sheet = "Gráfica(2)", range = cell_limits(ul = c(1,1), lr = c(183,15)))
  
  fig_estim <- plot_ly(estim2, x = ~Samples)
  fig_estim <- fig_estim %>% add_trace(y = ~`S(est)`, name = 'S est',mode = 'lines') %>%
    add_trace(y = ~`ACE Mean`, name = 'ACE Mean',mode = 'lines') %>%
    add_trace(y = ~`ICE Mean`, name = 'ICE Mean',mode = 'lines') %>%
    add_trace(y = ~`Chao 1 Mean`, name = 'Chao 1 Mean',mode = 'lines') %>%
    add_trace(y = ~`Chao 2 Mean`, name = 'Chao 2 Mean',mode = 'lines') %>%
    add_trace(y = ~`Jack 1 Mean`, name = 'Jackk 1 Mean',mode = 'lines') %>%
    add_trace(y = ~`Jack 2 Mean`, name = 'Jackk 2 Mean',mode = 'lines') %>%
    add_trace(y = ~`Bootstrap Mean`, name = 'Bootstrap Mean',mode = 'lines') %>%
    layout(yaxis = list(title = "Estimación")) %>%
    layout(xaxis = list(title = "Muestras"))
  
  fig_estim <- fig_estim %>% add_trace(y =~ `S(est) 95% CI Lower Bound`, 
                                       name = "S 95% CI Lower Bound", mode = 'lines',
                                       line = list(color = "#004080", width = 1, dash = "dash")) %>%
    add_trace(y=~ `S(est) 95% CI Upper Bound`, name = "S 95% CI Upper Bound", mode = "lines",
              line = list(color = "#004080", width = 1, dash = "dash")) %>%
    add_trace(y=~ `Chao 1 95% CI Lower Bound`, name = "Chao 1 95% CI Lower Bound", mode = "lines",
              line = list(color = "#de3163", width = 1, dash = "dash")) %>%
    add_trace(y=~ `Chao 1 95% CI Upper Bound`, name = "Chao 1 95% CI Upper Bound", mode = "lines",
              line = list(color = "#de3163", width = 1, dash = "dash")) %>%
    add_trace(y=~ `Chao 2 95% CI Lower Bound`, name = "Chao 2 95% CI Lower Bound", mode = "lines",
              line = list(color = "#b5a2c8", width = 1, dash = "dash")) %>%
    add_trace(y=~ `Chao 2 95% CI Upper Bound`, name = "Chao 2 95% CI Upper Bound", mode = "lines",
              line = list(color = "#b5a2c8", width = 1, dash = "dash"))
  
  output$plot_estimadores <- renderPlotly({fig_estim})
  
  #### Abundancia de especies por ZONA
  Hoja1 <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Estimatessarcophagidae.xlsx",
                     sheet = "Hoja1", range = cell_limits(ul = c(1,1), lr = c(22,55)))
  Hoja1$ZONA <- as.factor(Hoja1$ZONA)
  
  output$plot_tabasco <- renderPlot({
    tabasco(Hoja1[3:52],labCol = Hoja1$ZONA)
  })
  
  ##Abundancia de Individuos por ZONA
  fig_total <- plot_ly(Hoja1, x =~ ZONA, y =~ TOTAL, type = "bar", name = "Total de individuos") %>%
    add_trace(y =~ `TOTAL HEMBRA`, name = "Total de hembras") %>%
    add_trace(y =~ `TOTAL MACHO`, name = "Total de machos") %>%
    layout(yaxis = list(title = "Número de individuos"),barmode = "group")
  
  output$plot_indzona <- renderPlotly({fig_total})
  
  #Numero de ind por especie
  nomb_especies <- colnames(Hoja1[3:52])
  no_esp <- c(6,31,32,11,14,1,2,7,1,1,1,3,1,4,2,1,6,5,22,21,20,2,1,15,1,1,8,1,17,
              8,2,1,1,6,27,98,40,2,5,1,11,25,4,15,7,6,161,1,4,8)
  df_aux <- data.frame(nomb_especies,no_esp)
  
  fig_espec <- plot_ly(data = df_aux, y=~ nomb_especies, x=~ no_esp, type = "bar", 
                       name = "Total de individuos", color = I("#a55184"))  %>%
    layout(yaxis = list(title = "Nombre de la especie")) %>%
    layout(xaxis = list(title = "Número de individuos"))
  
  output$plot_especie <- renderPlotly({fig_espec})
  
  ####Abundancia por genero #####
  genero <- c("Blaesoxipha","Boettcheria","Helicobia","Oxysarcodexia","Peckia",
              "Ravinia","Sarcophaga", "Tripanurga","Titanogrypa")
  contador <- c(97,68,9,189,9,82,180,17,1)
  
  fig_genero <- ggplotly(
    ggplot(data = NULL, aes(x = genero, y = contador)) + geom_bar(stat = "identity", fill = "#74d147") +
      theme_bw() + xlab("Género") + ylab("Numero de individuos")
  )
  
  output$plot_genero <- renderPlotly({fig_genero})
  
  #### Sexo y Genero ####
  data01 <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Estimatessarcophagidae.xlsx",
                      sheet = "Original")
  
  data01$Sexo <- as.factor(data01$Sexo)
  data01$`Género/ Especie` <- as.factor(data01$`Género/ Especie`)
  
  especie_ggbar <- ggplot(data = data01, aes(y = `Género/ Especie`, fill = Sexo)) + 
    geom_bar(position=position_dodge()) +
    theme_bw() + 
    scale_fill_manual(values=c("#d14774", "#47d1a4")) + xlab("Numero de individuos")
  
  fig_genesp <- ggplotly(especie_ggbar)
  output$plot_genesp <- renderPlotly({fig_genesp})
  
  #####  MAPA   ####

  ### Mapa de los estados necesarios
  sp_data <- readOGR(dsn = ".", layer = "inegi_refcenmuni_2010")
  
  sp_centro <- subset(sp_data, sp_data$nom_ent %in% c(
    "Distrito Federal", "Guanajuato", "Hidalgo", "México",
    "Michoacán de Ocampo", "Morelos", "Querétaro", "Tlaxcala", "Puebla"
  ))
  
  ##Paleta para los poligonos
  paleta_centro <- colorFactor(
    palette = "Paired",
    domain = sp_centro$nom_ent)
  
  ##Hacer el icono
  PhoridaeIcon <- makeIcon(
    iconUrl = "1648868676284.png",
    iconWidth = 45, iconHeight = 42
  )
  
  PhagoIcon <- makeIcon(
    iconUrl = "1648869011510.png",
    iconWidth = 45, iconHeight = 42
  )
  
  ##Mapa con leaflet
  sp_muni <- subset(sp_centro, sp_centro$nom_mun %in% c(
    "Tlalpujahua", "Hidalgo", "San Miguel de Allende", "Ocoyoacac",
    "Coacalco de Berriozábal", "Malinalco", "Metepec", "Miguel Hidalgo",
    "Coyoacán","Xochimilco","San Felipe", "Teolocholco","San Pedro Cholula",
    "Chignahuapan","Xicotepec","Mineral del Chico", "Tizayuca",
    "Metztitlán","Cuernavaca","San Juan del Río"
  ))
  
  ##Paleta para los poligonos
  paleta_centro <- colorFactor(
    palette = "Paired",
    domain = sp_muni$nom_ent)
  
  #### Descripcion de lugares de muestreo
  
  ## Tesis Nuple
  n_chap <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Bosque de Chapultepec",
                  "<a>Tipo de vegetación</a>","Bosque cultivado",
                  "<a>Altura msnm</a>", "2310")
  
  n_coyo <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Coyoacán",
                  "<a>Tipo de vegetación</a>","Matorral desértico urbano",
                  "<a>Altura msnm</a>", "2268")
  n_coacalco <- paste(sep = "<br/>",
                      "<a>Localidad </a>", "Coacalco",
                      "<a>Tipo de vegetación</a>","Pastizal cultivado",
                      "<a>Altura msnm</a>", "2428")
  n_chico <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Parque Nacional El Chico",
                   "<a>Tipo de vegetación</a>","Bosque de táscate",
                   "<a>Altura msnm</a>", "3090")
  n_piedra <- paste(sep = "<br/>",
                    "<a>Localidad </a>", "Piedra Canteada",
                    "<a>Tipo de vegetación</a>","Bosque de pino",
                    "<a>Altura msnm</a>", "2871")
  n_ocoy <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Ocoyoacac",
                  "<a>Tipo de vegetación</a>","Bosque de pino",
                  "<a>Altura msnm</a>", "2750")
  n_tiza <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Tizayuca",
                  "<a>Tipo de vegetación</a>","Matorral crasicaule-cultivo",
                  "<a>Altura msnm</a>", "2322") 
  n_xochi <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Xochimilco",
                   "<a>Tipo de vegetación</a>","Tular-urbano",
                   "<a>Altura msnm</a>", "2235")
  
  #### Tesis rodriguez
  r_cerrosan <- paste(sep = "<br/>",
                      "<a>Localidad </a>", "Cerro San Andrés",
                      "<a>Tipo de vegetación</a>","Matorral espinoso y pastizal",
                      "<a>Altura msnm</a>", "2952")
  r_rayon<- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Parque Nacional Rayón",
                  "<a>Tipo de vegetación</a>","Bosque de pino",
                  "<a>Altura msnm</a>", "2423")
  r_sanjuan <- paste(sep = "<br/>",
                     "<a>Localidad </a>", "Barranca de cocheros, San Juan del Río",
                     "<a>Tipo de vegetación</a>","Bosque de pino-encino, matorral",
                     "<a>Altura msnm</a>", "2115")
  r_chico <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Parque Nacional El Chico",
                   "<a>Tipo de vegetación</a>","Bosque de pino-encino",
                   "<a>Altura msnm</a>", "3090")
  r_tiza <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "El Carmen Tizayuca",
                  "<a>Tipo de vegetación</a>","Bosque de pino-encino, matorral",
                  "<a>Altura msnm</a>", "2322")
  r_metz <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Reserva de la biósfera Metztitlán",
                  "<a>Tipo de vegetación</a>","Matorral xerófilo",
                  "<a>Altura msnm</a>", "1305")
  r_charco <- paste(sep = "<br/>",
                    "<a>Localidad </a>", "El Charco, Ingenio Jardín botánico",
                    "<a>Tipo de vegetación</a>","Matorral xerófilo y pastizal",
                    "<a>Altura msnm</a>", "2027")
  r_hondito <- paste(sep = "<br/>",
                     "<a>Localidad </a>", "Río Hondito",
                     "<a>Tipo de vegetación</a>","Bosque de coníferas, agricultura",
                     "<a>Altura msnm</a>", "2750")
  r_coacalco <- paste(sep = "<br/>",
                      "<a>Localidad </a>", "Sierra de Guadalupe",
                      "<a>Tipo de vegetación</a>","Bosque de coníferas",
                      "<a>Altura msnm</a>", "2428")
  r_mali <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Malinalco (Malikualli)",
                  "<a>Tipo de vegetación</a>","Selva baja caducifolia",
                  "<a>Altura msnm</a>", "1729")
  r_mete <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Metepec",
                  "<a>Tipo de vegetación</a>","Vegetación de ribera",
                  "<a>Altura msnm</a>", "2570")
  r_chapu <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Chapultepec",
                   "<a>Tipo de vegetación</a>","Bosque de pino-encino",
                   "<a>Altura msnm</a>", "2310")
  r_bota <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Jardín Botánico IBUNAM",
                  "<a>Tipo de vegetación</a>","Bosque templado",
                  "<a>Altura msnm</a>", "2320")
  r_cantera <- paste(sep = "<br/>",
                     "<a>Localidad </a>", "Cantera Oriente",
                     "<a>Tipo de vegetación</a>","Matorral xerófilo",
                     "<a>Altura msnm</a>", "2310")
  r_xochi <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "San Gregorio, Ejidos Xochimilco",
                   "<a>Tipo de vegetación</a>","Humedal",
                   "<a>Altura msnm</a>", "2235")
  r_canteada <- paste(sep = "<br/>",
                      "<a>Localidad </a>", "Piedra Canteada",
                      "<a>Tipo de vegetación</a>","Bosque de pino-oyamel",
                      "<a>Altura msnm</a>", "2871")
  r_malinche <- paste(sep = "<br/>",
                      "<a>Localidad </a>", "La Malinche",
                      "<a>Tipo de vegetación</a>","Bosque de pino-encino-oyamel",
                      "<a>Altura msnm</a>", "3117")
  r_cuerna <- paste(sep = "<br/>",
                    "<a>Localidad </a>", "Cuernavaca",
                    "<a>Tipo de vegetación</a>","Vegetación inducida",
                    "<a>Altura msnm</a>", "1439")
  r_cholula <- paste(sep = "<br/>",
                     "<a>Localidad </a>", "Jardín etnobotánico Francisco Peláez",
                     "<a>Tipo de vegetación</a>","Bosque de encino",
                     "<a>Altura msnm</a>", "2142")
  r_chig <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Parque Tulimán",
                  "<a>Tipo de vegetación</a>","Bosque de pino-encino y oyamel",
                  "<a>Altura msnm</a>", "2196")
  r_xico <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Mi Ranchito, Xicotepec",
                  "<a>Tipo de vegetación</a>","Selva alta perennifolia secundaria",
                  "<a>Altura msnm</a>", "1160")
  r_chapu <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Chapultepec",
                   "<a>Tipo de vegetación</a>","Bosque de pino-encino",
                   "<a>Altura msnm</a>", "2310")
  r_botan <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Jardín Botánico IBUNAM",
                   "<a>Tipo de vegetación</a>","Bosque templado",
                   "<a>Altura msnm</a>", "2320")
  r_cante <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Cantera Oriente",
                   "<a>Tipo de vegetación</a>","Matorral xerófilo",
                   "<a>Altura msnm</a>", "2299")
  r_xochi <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "San Gregorio, Ejidos Xochimilco",
                   "<a>Tipo de vegetación</a>","Humedal",
                   "<a>Altura msnm</a>", "2235")
  r_piedra <- paste(sep = "<br/>",
                    "<a>Localidad </a>", "Piedra Canteada",
                    "<a>Tipo de vegetación</a>","Bosque de pino-oyamel",
                    "<a>Altura msnm</a>", "2871")
  r_mali <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "La Malinche",
                  "<a>Tipo de vegetación</a>","Bosque pino-encino-oyamel",
                  "<a>Altura msnm</a>", "2871")
  r_cuerna <- paste(sep = "<br/>",
                    "<a>Localidad </a>", "Cuernavaca",
                    "<a>Tipo de vegetación</a>","Vegetación inducida",
                    "<a>Altura msnm</a>", "1439")
  r_cholu <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Cholula )(Jardín etnobotánico F. Peláez",
                   "<a>Tipo de vegetación</a>","Bosque de encino",
                   "<a>Altura msnm</a>", "2142")
  r_chign <- paste(sep = "<br/>",
                   "<a>Localidad </a>", "Chignahuapan (Parque Tulimán)",
                   "<a>Tipo de vegetación</a>","Bosque de pino-encino-oyamel",
                   "<a>Altura msnm</a>", "2196")
  r_xico <- paste(sep = "<br/>",
                  "<a>Localidad </a>", "Xicotepec (Mi ranchito)",
                  "<a>Tipo de vegetación</a>","Selva alta perennifolia secundaria",
                  "<a>Altura msnm</a>", "1160")
  
  ##Creamos un valor reactivo que guardara la posicion del click
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
  ##Mapa con leaflet
  mapa_muni <- leaflet(sp_muni)
  
  mapa_muni <- mapa_muni %>%
    addScaleBar() %>%
    addMarkers(lat = 19.423377050731126, lng = -99.1825786193375, popup = n_chap,
               label = "Bosque de Chapultepec", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.319053952277386, lng = -99.1449144383446, popup = n_coyo,
               label = "Coyoacán", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.617688173700582, lng =-99.1236875993371, popup = n_coacalco,
               label = "Coacalco", icon = PhoridaeIcon) %>%
    addMarkers(lat = 20.20533604057292, lng =-98.73794599006965, popup = n_chico,
               label = "El chico", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.45663167474952, lng =-98.6002670744013, popup = n_piedra,
               label = "Piedra Canteada", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.26642278513548, lng = -99.4612027936422, popup = n_ocoy,
               label = "Ocoyoacac", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.83462652009159, lng = -98.99363822828849, popup = n_tiza,
               label = "Tizayuca", icon = PhoridaeIcon) %>%
    addMarkers(lat = 19.26508753181893, lng = -99.0623552508639, popup= n_xochi,
               label = "Xochimilco", icon = PhoridaeIcon) %>%
    
    addMarkers(lat = 19.8063980102631, lng = -100.59649499020016, popup = r_cerrosan,
               label = "Cerro San Andrés", icon = PhagoIcon, layerId = "CSA" ) %>%
    addMarkers(lat = 19.756031036308958, lng = -100.14989893206835, popup = r_rayon,
               label = "Parque Nacional Rayón", icon = PhagoIcon, layerId = "PNR") %>%
    addMarkers(lat = 20.33192726471521, lng = -100.10183311281116, popup = r_sanjuan,
               label = "Barranca de Cocheros", icon = PhagoIcon, layerId = "BC") %>%
    addMarkers(lat = 20.198282831712724, lng = -98.71590896089705, popup = r_chico,
               label = "Parque Nacional El chico", icon = PhagoIcon, layerId = "MCH") %>%
    addMarkers(lat = 19.898863293574973, lng = -98.95221147438097, popup = r_tiza,
               label = "El Carmen Tizayuca", icon = PhagoIcon, layerId = "TH") %>%
    addMarkers(lat = 20.589784899932177, lng = -98.7585635455464, popup = r_metz,
               label = "Reserva de la biósfera Metztitlán", icon = PhagoIcon, layerId = "ME") %>%
    addMarkers(lat = 20.91800612935388, lng = -100.72744683204867, popup = r_charco,
               label = "El Charco del Ingenio Jardín Botánico", icon = PhagoIcon) %>%
    addMarkers(lat = 19.287259093895138, lng = -99.45095830323467, popup = r_hondito,
               label = "Río Hondito", icon = PhagoIcon, layerId = "RH") %>%
    addMarkers(lat = 19.61053044994308, lng = -99.08892108974283, popup = r_coacalco,
               label = "Sierra de Guadalupe", icon = PhagoIcon, layerId = "SG") %>%
    addMarkers(lat = 18.951844426718626, lng = -99.4863800743955, popup = r_mali,
               label = "Malinalco", icon = PhagoIcon, layerId = "M") %>%
    addMarkers(lat = 19.270332180186603, lng = -99.58915973207625, popup = r_mete,
               label = "Metepec", icon = PhagoIcon, layerId = "MT") %>%
    addMarkers(lat = 19.410561119833268, lng = -99.2102670744014, popup = r_chapu,
               label = "Bosque de Chapultepec", icon = PhagoIcon, layerId = "CH") %>%
    addMarkers(lat = 19.319844929554712, lng = -99.19351744556742, popup = r_bota,
               label = "Jardín Botánico", icon = PhagoIcon, layerId = "CU") %>%
    addMarkers(lat = 19.32064430536781, lng = -99.17382743207547, popup = r_cantera,
               label = "Cantera Oriente", icon = PhagoIcon, layerId = "CO") %>%
    addMarkers(lat = 19.27301072090702, lng= -99.05248927440421, popup = r_xochi,
               label = "San Gregorio", icon = PhagoIcon, layerId = "X") %>%
    addMarkers(lat = 19.45658109397299, lng = -98.60029926090928, popup = r_canteada,
               label = "Piedra Canteada", icon = PhagoIcon, layerId = "PC") %>%
    addMarkers(lat = 19.24442649929791, lng = -97.99085476090681, popup = r_malinche,
               label = "La Malinche", icon = PhagoIcon, layerId = "TL") %>%
    addMarkers(lat = 18.899533447084636, lng = -99.22884133208214, popup = r_cuerna,
               label = "Cuernavaca", icon = PhagoIcon, layerId = "C") %>%
    addMarkers(lat = 19.04299250443897, lng = -98.30146036091584, popup = r_cholula,
               label = "Jardín Etnobotánico", icon = PhagoIcon) %>%
    addMarkers(lat = 19.871671506932586, lng = -97.97843746090251, popup = r_chign,
               label = "Parque Tulimán", icon = PhagoIcon, layerId = "CT") %>%
    addMarkers(lat =20.261312545833597, lng = -97.96400646089599, popup = r_xico,
               label = "Xicotepec", icon = PhagoIcon, layerId = "XI") %>%
    
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.5)) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik,
                     options = providerTileOptions(opacity = 0.3)) %>%
    
    setView(lat = 19.34179152029062, lng = -98.9856431559105, zoom = 7) %>%
    
    addPolygons(weight = 1,
                stroke = TRUE,
                color = "black",
                fillColor = ~paleta_centro(nom_ent),
                fillOpacity = 0.8,
                dashArray = "1",
                label = ~nom_mun,
                popup = ~paste(sep = "<br/>", 
                               "<a>Entidad: </a>", nom_ent,
                               "<a> Poblacion masc:</a>", pobmas,
                               "<a> Poblacion femenina: </a>", pobfem),
                highlight = highlightOptions(
                  weight = 2,
                  dashArray = "",
                  color = "grey",
                  bringToFront = TRUE
                ))
  
  output$leaf_mapa <- renderLeaflet({mapa_muni})
  
  ##Guardamos el click
  observeEvent(input$leaf_mapa_marker_click,{
    data_of_click$clickedMarker <- input$leaf_mapa_marker_click
  })
  
  ## Sumario por localidad
  data03 <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Estimatessarcophagidae.xlsx",
                      sheet = "Hoja3")
  data03$ESPECIES <- as.factor(data03$ESPECIES)
  data03$ESPECIES
  
  summary_df <- data.frame(Hoja1$ZONA, Hoja1$TOTAL, Hoja1$`TOTAL HEMBRA`, Hoja1$`TOTAL MACHO`,
                           data03$ESPECIES)
  colnames(summary_df) <- c("ZONA","TOTAL", "TOTAL_HEMBRA","TOTAL_MACHO","ESPECIES")
  
  ##Elaboramos el grafico de hembras y machos
  SEXO <- c("HEMBRAS", "MACHOS")
  plet <- c("#de3163","#63de31")
  
  output$pie_total = renderPlot({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="CU"}
    
    if(my_place=="CU"){
      pie(x = c(24,11), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[1,][5]})
    }
    
    if(my_place == "CO"){
      pie(x = c(91,31), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[2,][5]})
    }
    
    if(my_place == "MO"){
      pie(x = c(13,2), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[3,][5]})
    }
    
    if(my_place == "CH"){
      pie(x = c(31,25), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[4,][5]})
    }
    
    if(my_place == "TH"){
      pie(x = c(41,13), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
    output$text_espec <- renderPrint({summary_df[5,][5]})
    }
    
    if(my_place == "MCH"){
      pie(x = c(14,6), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[6,][5]})
    }
    
    if(my_place == "TL"){
      pie(x = c(36,46), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[7,][5]})
    }
    
    if(my_place == "X"){
      pie(x = c(40,21), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[8,][5]})
    }
    
    if(my_place == "PC"){
      pie(x = c(7,2), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[9,][5]})
    }
    
    if(my_place == "SG"){
      pie(x = c(27,12), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[10,][5]})
    }
    
    if(my_place == "RH"){
      pie(x = c(2,0), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[11,][5]})
    }
    
    if(my_place == "ME"){
      pie(x = c(18,4), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[12,][5]})
    }
    
    if(my_place == "CT"){
      pie(x = c(25,8), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[13,][5]})
    }
    
    if(my_place == "XI"){
      pie(x = c(17,7), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[14,][5]})
    }
    
    if(my_place == "CSA"){
      pie(x = c(5,15), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[15,][5]})
    }
    
    if(my_place == "PNR"){
      pie(x = c(5,7), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[16,][5]})
    }
    
    if(my_place == "BC"){
      pie(x = c(4,0), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[17,][5]})
    }
    
    if(my_place == "M"){
      pie(x = c(4,1), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[18,][5]})
    }
    
    if(my_place == "C"){
      pie(x = c(5,6), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[19,][5]})
    }
    
    if(my_place == "MT"){
      pie(x = c(1,1), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[20,][5]})
    }
    
    if(my_place == "SM"){
      pie(x = c(0,38), labels = SEXO, radius = 1, col = plet,
          density = 40, border = "black", main = "Proporción de hembras y machos")
      output$text_espec <- renderPrint({summary_df[21,][5]})
    }
  })
  
  
}

shinyApp(ui,server)

