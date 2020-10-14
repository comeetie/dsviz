#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(cartography)

# dansmarue = read_csv("./data/dansmarue_small.csv")
# dansmarue.sf = st_as_sf(dansmarue,coords = c("long","lat"),crs = 4326) %>% st_transform(2154)
# gr = st_make_grid(dansmarue.sf  ,400)
# carr= st_within(dansmarue.sf,gr)
# dansmarue.sf$id_car=sapply(carr,function(a){a[1]})
# gr_p = st_sf(data.frame(id_car=1:length(gr)),gr %>% st_centroid())


# bb      <- gr_p %>% st_transform(4326) %>% st_bbox()
# q       <- opq(bbox = bb,timeout = 180)
# qm      <- add_osm_feature (q, key = 'highway',value = 'motorway', value_exact = FALSE)
# qt      <- add_osm_feature (q, key = 'highway',value = 'trunk', value_exact = FALSE)
# qp      <- add_osm_feature (q, key = 'highway',value = 'primary', value_exact = FALSE)
# 
# motorway<- osmdata_sf(qm)
# trunk   <- osmdata_sf(qt)
# primary <- osmdata_sf(qp)
# 
# roads    <- c(primary,trunk,motorway)$osm_lines %>% st_transform(st_crs(gr_p))
# roads.geom = st_geometry(roads)
# rm(list=c("qm","qt","qp","motorway","trunk","primary","roads"))

# qn = q %>% add_osm_feature(key = "place",value="suburb")
# places <- osmdata_sf(qn)
# places.geom = places$osm_points %>% filter(!is.na(name),!duplicated(name)) %>% select(name) %>% st_transform(st_crs(gr_p))
# rm(list=c("qn","places"))
# # Get the shape of the main river "La seine" 
# qr <- q %>% 
#     add_osm_feature (key = 'waterway') %>% 
#     add_osm_feature(key = "name:fr", value = "La Seine")
# river <- osmdata_sf(qr)
# river.geom <- st_geometry(river$osm_lines %>% filter(name.fr == "La Seine")) %>%
#     st_transform(st_crs(gr_p))
# rm(list=c("q","qr","bb","river"))


load("./dansmarue.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dans ma rue"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "cat",label = "Catégories : ",choices = unique(dansmarue$TYPE),selected = unique(dansmarue$TYPE)[1]),
            selectInput(inputId = "inter",label = "Intervenants : ",choices = unique(dansmarue$INTERVENANT),selected = unique(dansmarue$INTERVENANT),multiple = TRUE),
            dateRangeInput(inputId = "dates",label="Dates : ", min=min(dansmarue.sf$DATEDECL),max=max(dansmarue.sf$DATEDECL),start=min(dansmarue.sf$DATEDECL),end=max(dansmarue.sf$DATEDECL)),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h1(textOutput("titre")),
           plotOutput("carte",height="600px"),
           plotOutput("lignes",height="250px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data = reactive({dansmarue.sf %>% filter(TYPE %in% input$cat,INTERVENANT %in% input$inter,DATEDECL >= input$dates[1],DATEDECL <= input$dates[2])})
    output$carte <- renderPlot({
        #ov = st_contains(gr,data())
        #data.sf=st_sf(data.frame(n=sapply(ov,length)),gr)
        #choroLayer(x = data.sf %>% filter(n>0), var = "n",
                 #  method = "quantile", nclass = input$nbcol,col = carto.pal(pal1="red.pal",n1=input$nbcol))
        data.sf = gr_p %>% left_join(data() %>% st_drop_geometry()%>% group_by(id_car) %>% count()) %>% filter(!is.na(n))
        plot(st_geometry(roads.geom),col="#666666",lwd = 0.8,bg="#ffffff")
        plot(st_geometry(river.geom),col="#aaccff",lwd = 3,add=TRUE)
        propSymbolsLayer(x = data.sf, var = "n",
                         legend.title.txt = "Nombre de signalements",
                         col = "#aa4444",inches = 0.2,legend.pos = )
        labelLayer(places.geom[1:25,],txt="name",
            halo = TRUE,
            col = "white", 
            bg = "black", 
            cex = c(.8,.8,1)
        ) 
    })
    output$lignes <- renderPlot({
        ggplot(data() %>% st_drop_geometry() %>% group_by(DATEDECL,TYPE) %>% count())+
            geom_line(aes(group=TYPE,y=n,x=DATEDECL,color=TYPE))+
            theme_light()+scale_color_discrete(guide="none")+
            scale_x_date("Date de Déclaration")
    })
    output$titre <- renderText(input$cat)
}

# Run the application 
shinyApp(ui = ui, server = server)
