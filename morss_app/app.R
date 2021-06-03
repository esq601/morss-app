library(shiny)
library(mongolite)
library(tidyverse)
library(shinydashboard)
library(rhandsontable)
library(AzureStor)
library(shinyjs)

load("creds.rda")

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #JS Code to reset application after submission of report

constring <- mongo_con

ui <- dashboardPage(
  
  dashboardHeader(title = "Patrol Tracker"),
  
  dashboardSidebar(
    p("Input your debrief here.")
  ),
  
  dashboardBody(
    
    useShinyjs(),  #Code needed for shinyjs to run                                
    extendShinyjs(text = jsResetCode, functions = "reset"),
    
    fluidRow(
      uiOutput("bde_row"),
      uiOutput("bn_row"),
      uiOutput("co_row"),
      uiOutput("plt_row")
    ),
    fluidRow(
      column(3,textInput("name_input","Name")),
      column(3,uiOutput("location_select")),
      column(3,dateInput("date_input", "Date")),
      column(3,selectInput("type_input", "Mission Type", choices = c("US","Joint","Combined")))
    ),
    fluidRow(
      column(6,textAreaInput("long_input","Narrative", width = "100%")),
      column(6,rHandsontableOutput("eng_table",width = "100%"))
    ),
    
    fluidRow(
      fileInput("file_upload","Upload Pictures", multiple = TRUE),
      fileInput("tracks_upload", "Upload Tracks",multiple = FALSE, accept = ".gpx"),
      uiOutput("files_ui")
    ),
    fluidRow(
      actionButton("submit_pre","Submit")
    )
  )
)


server <- function(input, output, session) {
  
  con <- mongo(collection = "patrol_collection",db = "patrol_db", #Main db connection
               url = constring)
  
  con_locs <- mongo(collection = "patrol_locations",db = "patrol_db", #Locations db connection
                    url = constring)
  
  #### Blob connection ####
  
  blob_endp <- blob_endpoint( #Connection to blob for file upload.
    "https://aztxupadhocdata01.blob.core.usgovcloudapi.net/",
    key = blob_key
  )%>%
    blob_container("morss-app")
  
  ####  Read in Data ####
  
  locations <- con_locs$find(query = '{}', fields = '{"names":1}') #Query locations in Locations db
  
  units <- read_csv("data/units.csv") #Read csv with units.  Stored locally.
  
  ### Unit reactives ####
  
  bdes <- units %>% #Brigade selections
    select(bde) %>%
    distinct()
  
  bns <- reactive({ #Battalion selections
    units %>%
      filter(bde %in% input$bde_select) %>%
      select(bn) %>%
      distinct()
  })
  
  companies <- eventReactive(input$bn_select,{ #Company selections
    units %>%
      filter(bde %in% input$bde_select & bn %in% input$bn_select) %>%
      select(co) %>%
      distinct()
  })
  
  plts <- eventReactive(input$co_select,{ #Platoon selections
    units %>%
      filter(bde %in% input$bde_select & bn %in% input$bn_select & co %in% input$co_select) %>%
      select(plt) %>%
      distinct()
  })
  
  #### Render UI for unit selections ####
  
  output$bde_row <- renderUI({
    column(3,selectInput("bde_select","BDE",choices = bdes$bde))
  })
  
  output$bn_row <- renderUI({
    column(3,selectInput("bn_select","BN",choices = bns()$bn))
  })
  
  output$co_row <- renderUI({
    column(3,selectInput("co_select","CO",choices = companies()$co))
  })
  
  output$plt_row <- renderUI({
    column(3,selectInput("plt_select","plt",choices = plts()$plt))
  })
  
  ### Uploaded Files Logic ####
  
  files <- reactive({
    data.frame(
      files = list.files(temp_loc)
    )
  })
  
  output$files_ui <- renderUI({
    
    p(glue::glue_collapse(input$file_upload$name, sep = ", "))
  })
  
  
  ### Event Table ####
  
  hot_starter <- tibble(Who = rep(NA_character_,10),What = rep(NA_character_,10),Where = rep(NA_character_,10),
                        Type = rep(NA_character_,10), Severity = rep(NA_character_,10))
  
  
  output$location_select <- renderUI({
    selectInput("location_input","Location:",choices = locations$names)
  })
  
  
  output$eng_table <- renderRHandsontable({
    
    rhandsontable(hot_starter) %>%
      hot_col("Type", type = "dropdown", source = c("Direct Fire", "IDF", "Engagement", "Obstacle","Opportunity")) %>%
      hot_col("Severity", type = "dropdown", source = c(1,2,3,4,5)) %>%
      hot_cols(colWidths = "150px")
  })
  
  #### Data submission ####
  
  observeEvent(input$submit_pre,{ #Observe "Submit" button click
    
    showModal(modalDialog(size = "s",  #Confirmation Modal
                          title = "Are you sure you want to submit?",
                          actionButton("submit_real", "Yes, Submit")
    ))
  })
  
  
  observeEvent(input$submit_real,{  #Observe final "Submit button click
    
    keyval <- paste(input$bde_select,input$bn_select,input$co_select,input$plt_select,Sys.time())  #Value for UID
    
    df_table <- hot_to_r(input$eng_table) %>%  #Hot_to_r takes client side handsontable and converts to R object
      filter(is.na(Who) == FALSE) %>%  #Remove blanks in 'Name'
      nest(engs = everything())  #Nest into dataframe
    
    df_unit <- tibble(  #Created nested dataframe from unit input
      bde = input$bde_select,
      bn = input$bn_select,
      co = input$co_select,
      plt = input$plt_select
    ) %>%
      nest(unit = everything())
    
    df_insert <- tibble(  #Combine all inputs into single row dataframe
      name = input$name_input,
      date = input$date_input,
      location = input$location_input,
      unit = df_unit$unit,
      type = input$type_input,
      narrative = input$long_input,
      blobkey = keyval #Nested dataframe
    ) %>%
      mutate(blob_upload = case_when(  #Boolean if files were uploaded
        is.null(input$file_upload) == TRUE ~ "false",
        T ~ "true"
      ))
    
    if(nrow(df_table) > 0) { #If table was empty, skip. Otherwise add to dataframe
      df_insert <- bind_cols(df_insert,df_table)
    }
    
    #print(toJSON(df_insert))  #Error checking
    con$insert(df_insert)  #Create new document in main db for event
    
    if(is.null(input$file_upload) == FALSE) { #If no files uploaded skip
      
      for(i in 1:nrow(input$file_upload)){ #For each document uploaded, upload
        #print(input$file_upload[i]) #Error checking
        
        upload_blob(blob_endp,input$file_upload$datapath[i],dest = paste0(keyval,"/",input$file_upload$name[i]))
      }
      
    }
    
    if(is.null(input$tracks_upload) == FALSE) { #If no files uploaded skip
      
      for(i in 1:nrow(input$tracks_upload)){ #For each document uploaded, upload
        #print(input$file_upload[i]) #Error checking
        
        upload_blob(blob_endp,input$tracks_upload$datapath[i],dest = paste0(keyval,"/",input$tracks_upload$name[i]))
      }
      
    }
    
    Sys.sleep(1)
    
    js$reset() #Restart application to clear inputs
    
  })
}

shinyApp(ui = ui, server = server)