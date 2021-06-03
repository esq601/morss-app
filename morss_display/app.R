library(shiny)
library(mongolite)
library(tidyverse)
library(shinydashboard)
library(rhandsontable)
library(AzureStor)
library(shinyjs)
library(slickR)
library(gt)
library(leaflet)


load("creds.rda")
# Define UI for application that draws a histogram
constring <- mongo_con

ui <- dashboardPage(
    
    dashboardHeader(title = "Patrol Dashboard"),
    
    dashboardSidebar(
        uiOutput("report_select")
    ),
    
    dashboardBody(
        column(6,
               gt_output("table"),br(),
               leafletOutput("leafmap")),
        column(6,
               slickROutput("slickr"),br(),
               gt_output("engstable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    con <- mongo(collection = "patrol_collection",db = "patrol_db", #Main db connection
                 url = constring)
    
    con_locs <- mongo(collection = "patrol_locations",db = "patrol_db", #Locations db connection
                      url = constring)

    blob_endp <- blob_endpoint( #Connection to blob for file upload.
        "https://aztxupadhocdata01.blob.core.usgovcloudapi.net/",
        key = blob_key
    )%>%
        blob_container("morss-app")
    
    reports <- con$find('{}')
    
    output$report_select <- renderUI({
        selectInput("report_input", "Select Report:", choices = reports$blobkey )
    })
    
    reports_selected <- reactive({
        reports %>%
            filter(blobkey == input$report_input)
    })
    
    
    observeEvent(input$report_input, {
        req(input$report_input)
        
        unlink("img_temp", recursive = TRUE)
        unlink("tracks_temp", recursive = TRUE)
        
        dir.create("img_temp")
        dir.create("tracks_temp")
        
        names_df <- blob_endp %>%
            
            list_blobs(input$report_input)
        
        names <- names_df$name[grepl(".jpg|.jpeg|.png",names_df$name)]
        
        
        for(i in names){
            
            blob_endp %>%
                download_blob(i, dest = paste("img_temp",i,sep = "/"), overwrite = TRUE)
        }
        
        tracks <- names_df$name[grepl(".gpx",names_df$name)]
        
        print(tracks)
        
        if(length(tracks) > 0){
            
            for(i in tracks){
                
                blob_endp %>%
                    download_blob(i, dest = "tracks_temp/track.gpx", overwrite = TRUE)
            }
        }

        
        
        
        
    })
    
    
    output$slickr <- renderSlickR({
        
        pic_files <- paste0(paste0("img_temp/",input$report_input,"/"),list.files(paste0("img_temp/",input$report_input)))
        
        slickR(pic_files, height = "400px")
        
    })

    
    output$table <- render_gt({
        
        reports_selected() %>%
            select(-engs, -blobkey, -blob_upload) %>%
            unchop(cols = c(unit)) %>%
            t() %>%
            as.data.frame() %>%
            rownames_to_column(var = "Category") %>%
            tibble() %>%
            gt()
        
    })
    
    output$engstable <- render_gt({
        reports_selected()$engs[[1]] %>%
            gt()
    })
    

    tracksreac <- eventReactive(input$report_input,{
        
        coords <- xml2::read_xml("tracks_temp/track.gpx")
        
        coord1 <- xml2::as_list(coords)
        
        coord2 <- coord1[["gpx"]][["trk"]][["trkseg"]]
        
        df_tracks <- data.frame()
        
        for(i in 1:length(coord2)) {
            
            temp <- data.frame(lat = attr(coord2[[i]],"lat"), long = attr(coord2[[i]],"lon"), time = coord2[[i]]$time[[1]])
            
            df_tracks <- bind_rows(df_tracks,temp)
            
        }
        
        df_tracks %>%
            mutate(lat = as.numeric(lat), long = as.numeric(long), time = as.POSIXct(time))
    })
    
    output$leafmap <- renderLeaflet({

        leaflet() %>%
            addPolylines(lng = tracksreac()$long, lat = tracksreac()$lat) %>%
            addProviderTiles("Stamen.TonerLite")
    })
    
}

shinyApp(ui = ui, server = server)