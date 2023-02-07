#This app will allow detection of spatial-temporal patterns for any phenomena of interest
#This app will allow users to see how clusters of an issue of interest develop and collapse both in space and across time

#Skills demostrated
#1. Development of shiny apps (user interface, integrating of algorithms)
#2. Detection of patterns across space and time
#3. Visualization of spatio-temporal patterns

#----   Loading of data packages  

pacman::p_load(sf,               # Spatial files
               shiny,
               shinyFiles,
               tmap,
               raster,
               leaflet,
               sp
)

#-----         UI (Appearance)                                                     ##

options(shiny.maxRequestSize = 100000*1024^2)

ui <- fluidPage(
  titlePanel("Imagination inspired by reading"),
  shinythemes::themeSelector(),
  tabsetPanel(
    tabPanel("Image Selection",
             sidebarLayout(
               sidebarPanel(
                 shinyDirButton("folder", "Select data folder:", "please select a folder", FALSE),
               )
               
               #If pre-trained model ask to select model
               #If model building take the user to model development
             ),
             mainPanel(
               
               tmapOutput("map")
               
             )
    )
  )
  
  ################    
  
)


################################################################################
#         Server (the brain, generates outputs)                               ##
################################################################################
#Brain of the app or Logic #Generate outputs

server <- function(input,output, session){
  volumes=getVolumes()
  observe({
    shinyDirChoose(input, 'folder', roots = volumes, filetypes = c('', 'jpg'))
    print(input$folder)
  })
  #----Select a folder with data (two folders, one which is true and the other being false)
  
} 
shinyApp(ui = ui, server = server)