#nhsAdminAreasGUI6/ui.r CURRENT
#andy south 31/5/2013

#to run type this in R console
#library(shiny)
#setwd('C:\\rProjects\\shiny\\')
#runApp('nhsAdminAreasGUI6')

library(shiny)

shinyUI(pageWithSidebar(
  
  #worked but logo too big, height & width have no effect
  list(HTML('<img src="logo2smaller.png"/>'), "CCGsee : for viewing Clinical Commissioning Group data"),
   
  # Sidebar with controls 
  sidebarPanel(
    
    uiOutput("choose_dataset"),
    
    #option to load example QOF data
    wellPanel(
      checkboxInput("loadExData", "load example QOF data", FALSE) #var,name
    ), #end of wellPanel 
     
    selectInput("regionToPlot", "Plot which region ?",
                c("England" = "England",
                  "North" = "North",
                  "South" = "South",
                  "London" = "London",
                  "Midlands and East" = "Midlands and East"
                )),    
    
    checkboxInput("addLegend", "add Legend to maps", TRUE), #var,name

    checkboxInput("addCodes", "add CCG codes to maps", FALSE), #var,name
        
    wellPanel( 
      fileInput('file1', 'Load a CCG csv - ccg code in column 1')
    )  #end of wellPanel   
  ), #end of sidebarPanel
  
  
  mainPanel(
    
    h5("A NHSHackDay project 2013 by Andy South, in dev., southandy@gmail.com"),
    h5("Winner of prize from Chief Data Officer of the NHS in England, May 2013"),
    
    tabsetPanel(
      tabPanel("Map", plotOutput("mapPlot")),
      tabPanel("Bar Plot", plotOutput("barPlot")),
      tabPanel("Bubble Map", plotOutput("mapBubblesPlot")),      
      tabPanel("Table", tableOutput("tablePlot")),
      #data from a loaded file
      tabPanel("Uploaded attribute data", tableOutput("filetable")),   
      tabPanel("About", verbatimTextOutput("textAbout"))     
      #tabPanel("demo Zoom map", includeHTML("olayer.html"))      
    ) # end of tabsetPanel
        
  )
))
