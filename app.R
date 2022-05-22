#----load library-----
library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)
library(DT)
library(plotly)
library(leaflet)
library(glue)
setwd("G:/DATA SCIENTIST/TETRIS PROGRAM/PROJECT/CAPSTONE/dataset/R/Fauzan")
#-----list provinsi-----
#----data sawit----
sawit <- read_delim("sawit2.csv", delim = ";")
areasawit <- read_delim("luas_tanaman_sawit.csv", delim = ";")
#------ mencari min, max, dan mean tiap provinsi-----

#----dashboard shiny-----
ui<-fluidPage( 
  dashboardPage( skin = "red",
                 dashboardHeader(title = "Informasi Produksi Kelapa Sawit Nasional", titleWidth = 650),
                 dashboardSidebar(
                   sidebarMenu(id = 'sidebarmenu',
                               # first menu item
                               menuItem("Tentang", tabName = "penjelasan1", icon = icon("question-circle")),
                               # second menu item with 2 sub menus
                               menuItem('Grafik',
                                        icon = icon('bar-chart-o'),
                                        menuSubItem('Produksi Skala Nasional',
                                                    tabName = 'chart1',
                                                    icon = icon('line-chart')),
                                        menuSubItem('Produksi Skala Provinsi',
                                                    tabName = 'chart2',
                                                    icon = icon('line-chart'))),
                               menuItem("Database", tabName = "db", icon = icon("database"))
                   )),
                 dashboardBody(
                   tabItems(
                     tabItem("penjelasan1", 
                             h1(strong("Produktifitas Perkebunan Kelapa Sawit Nasional"), align = "center"),
                             p(style="text-align:justify;","Kelapa sawit merupakan salah satu komoditas perkebunan yang memiliki peran strategis dalam pembangunan ekonomi Indonesia. Sebagai penghasil kelapa sawit terbesar di dunia, industri kelapa sawit telah menyediakan lapangan pekerjaan sebesar 16 juta tenaga kerja baik secara langsung maupun tidak langsung."),
                             p(style="text-align:justify;","Produksi minyak sawit dan inti sawit pada tahun 2018 tercatat sebesar 48,68 juta ton, yang terdiri dari 40,57 juta ton crude palm oil (CPO) dan 8,11 juta ton palm kernel oil (PKO). Jumlah produksi tersebut berasal dari Perkebunan Rakyat sebesar 16,8 juta ton (35%), Perkebunan Besar Negara sebesar 2,49 juta ton (5%,) dan Perkebunan Besar Swasta sebesar 29,39 juta ton (60%)."),
                             p(style="text-align:justify;","Meskipun tahun 2020, Indonesia dilanda pandemi Covid-19, nilai ekspor produk kelapa sawit tetap menunjukkan angka positif di kisaran US$ 22,97 miliar (Rp 332 triliun), atau tumbuh sebesar 13,6% dari tahun 2019."),
                             p(style="text-align:justify;","Meningkatnya nilai ekspor produk kelapa sawit di Indonesia tidak lepas dari meningkatnya produksi sawit tahunan yang senantiasa meningkat disetiap tahunnya, tercatat mulai tahun 2016 hingga 2020 terjadi trend kenaikan produksi yang signifikan, hal ini didukung dengan data luas tanaman perkebunan kelapa sawit yang turut meningkat pada rentang waktu tersebut.")),
                     tabItem(tabName = "chart1",
                             # First Row
                             fluidRow(selectInput("tahun", label = h4("Tahun:"), 
                                                  choices = list("2008" = 2008, "2009" = 2009, "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,"2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,"2020" = 2020), 
                                                  selected = 2020),
                                      box(title = "Peta Persebaran Produksi Kelapa Sawit", leafletOutput("mymap", height = 250), width = 12),
                                      box(title = "Grafik Produksi Kelapa Sawit Indonesia", plotlyOutput("plot2", height = 250),
                                          width=6, solidHeader = F),
                                      box(title = "Luas Tanaman Perkebunan Kelapa Sawit Indonesia", plotlyOutput("plot3", height = 250)
                                      ))),
                     tabItem(tabName = "chart2",
                              # First Row
                             selectInput("provs", label = h4("Provinsi"), 
                                         list("Aceh"="Aceh",
                                              "Sumatera Utara"="Sumatera Utara",
                                              "Sumatera Barat"="Sumatera Barat",
                                              "Riau"="Riau",
                                              "Jambi"="Jambi",
                                              "Sumatera Selatan"="Sumatera Selatan"	,
                                              "Bengkulu"="Bengkulu",
                                              "Lampung	"="Lampung	",
                                              "Bangka Belitung"="Bangka Belitung"	,
                                              "Kepulauan Riau"="Kepulauan Riau",
                                              "Jakarta Raya"="Jakarta Raya",
                                              "Jawa Barat"="Jawa Barat",
                                              "Jawa Tengah"="Jawa Tengah",
                                              "Yogyakarta"="Yogyakarta",
                                              "Jawa Timur"="Jawa Timur",
                                              "Banten"="Banten",
                                              "Bali"="Bali",
                                              "Nusa Tenggara Barat"="Nusa Tenggara Barat",
                                              "Nusa Tenggara Timur"="Nusa Tenggara Timur",
                                              "Kalimantan Barat"="Kalimantan Barat"	,
                                              "Kalimantan Tengah"="Kalimantan Tengah"	,
                                              "Kalimantan Selatan"="Kalimantan Selatan",
                                              "Kalimantan Timur"="Kalimantan Timur",
                                              "Kalimantan Utara"="Kalimantan Utara",
                                              "Sulawesi Utara"="Sulawesi Utara"	,
                                              "Sulawesi Tengah"="Sulawesi Tengah"	,
                                              "Sulawesi Selatan"="Sulawesi Selatan"	,
                                              "Sulawesi Tenggara"="Sulawesi Tenggara"	,
                                              "Gorontalo" = "Gorontalo"	,
                                              "Sulawesi Barat"="Sulawesi Barat"	,
                                              "Maluku"="Maluku"	,
                                              "Maluku Utara"="Maluku Utara",
                                              "Papua Barat"="Papua Barat",
                                              "Papua"="Papua"), 
                                         selected = "Riau"),
                                      box(title = NULL, plotlyOutput("plot4", height = 350), width = 12)),
                     
                   tabItem(tabName = "db",
                           # First Row
                           fluidRow(tabBox(id="tabchart1",
                                           tabPanel("Produksi",DT::dataTableOutput("Tab1", height = "450px"), width = 9),
                                           tabPanel("Luas Tanaman",DT::dataTableOutput("Tab2", height = "450px"), width = 9), width = 12)))
                   ))))

server<-shinyServer(function(input, output, session){
  output$plot3 <- renderPlotly({
    c<- areasawit %>% ggplot(aes(x=tahun, y=luas_tanaman)) 
    c<- c + geom_line(color = "Red")
    c<- c + geom_point()
    c<- c + scale_x_continuous(breaks=seq(2008,2020,1))
    c<- c + xlab("Tahun Panen")
    c<- c + ylab("Luas Tanaman (Ribu Hektar)")
    c <- c + theme(axis.text.x = element_text(angle = 90))
    c<- ggplotly(c)
    c
    
  })
  
  output$plot2 <- renderPlotly({
    prov1 <- "Indonesia"
    a<- sawit %>% filter(Provinsi == prov1) %>% ggplot(aes(x=Tahun, y=kelapa_sawit)) 
    a<- a + geom_line(color = "Red")
    a<- a + geom_point()
    a<- a + scale_x_continuous(breaks=seq(2008,2020,1))
    a<- a + xlab("Tahun Panen")
    a<- a + ylab("Kuantitas (Ribu Ton)")
    a<- a + theme(axis.text.x = element_text(angle = 90))
    a <- ggplotly(a)
    a
  })
  
  output$mymap <- renderLeaflet({
    #---------------read GADM----------------
    #peta persebaran produksi per provinsi
    indo_sf <- readRDS("gadm36_IDN_1_sp.rds")
    #join indo_sf dengan sawit tapi di filter tahun 2020 saja (nanti harus ganti variable)
    indo_sf@data <- indo_sf@data %>% 
      left_join(filter(sawit, Tahun == input$tahun ), by = c("NAME_1"="Provinsi"))
    # visualisasi
    ## Label
    labels_map <- glue("<b>{indo_sf@data$NAME_1}</b><br>
     Produksi Sawit (ribu ton) : {indo_sf@data$kelapa_sawit}") %>% 
      lapply(htmltools::HTML)
    #-----membuat map untuk produksi tahun 2020-----
    #membuat map untuk produksi tahun 2020
    rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
    dom <- rescale(indo_sf@data$kelapa_sawit)
    pal <- colorNumeric(palette= "Reds", domain = dom)
    indo_sf %>% 
      leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pal(dom),
                  fillOpacity = 0.5,
                  weight = 1,
                  label = labels_map,
                  color = "white",
                  highlightOptions = highlightOptions(
                    color = "Red",
                    weight = 3,
                    bringToFront = TRUE,
                    opacity = 0.8
                  ))
  })
  
  output$plot4 <- renderPlotly({
    prov2 <- toString(input$provs)
    judul2 <- glue("Produksi Kelapa Sawit Di Provinsi {input$provs}")
    b<- sawit %>% filter(Provinsi == input$provs) %>% ggplot(aes(x=Tahun, y=kelapa_sawit)) 
    b<- b + geom_line(color = "Red")
    b<- b + geom_point()
    b<- b + scale_x_continuous(breaks=seq(2008,2020,1))
    b<- b + xlab("Tahun Panen")
    b<- b + ylab("Kuantitas (Ribu Ton)")
    b<- b + ggtitle(judul2)
    b <- ggplotly(b)
    b
  })
  
  output$Tab1 <- DT::renderDataTable(DT::datatable({
    data <-sawit }))
  
  output$Tab2 <- DT::renderDataTable(DT::datatable({
    data <-areasawit }))
})

shinyApp(ui,server)