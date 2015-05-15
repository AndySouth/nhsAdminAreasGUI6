#nhsAdminAreasGUI6/ui.r CURRENT
#andy south 31/5/2013

#to run type this in R console
#library(shiny)
#setwd('C:\\rProjects\\shiny\\')
#runApp('nhsAdminAreasGUI6')

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  #headerPanel("nhsmaps.co.uk CCGsee : for viewing Clinical Commissioning Group data"),
  #h4("nhsmaps.co.uk   -   CCGsee   -   for viewing Clinical Commissioning Group data"),
  #trying to include a logo, must be stored in www folder 
  #logo nice small size but couldn't put text after it
  #img(src="logo2.png",height=72,width=72),
  #headerPanel("For viewing Clinical Commissioning Group data"),
  
  #worked but logo too big, height & width have no effect
  list(HTML('<img src="logo2smaller.png"/>'), "CCGsee : for viewing Clinical Commissioning Group data"),
  
  #logo & text too big !
  #headerPanel(
  #  list(HTML('<img src="logo2.png",height=72,width=72/>'), "CCGsee : for viewing Clinical Commissioning Group data"),
  #  windowTitle="CCGsee : for viewing Clinical Commissioning Group data"
  #),
  
  # Sidebar with controls 
  sidebarPanel(
    
    uiOutput("choose_dataset"),
    
    #option to load example QOF data
    wellPanel(
    checkboxInput("loadExData", "load example QOF data", FALSE) #var,name
    ), #end of wellPanel 
    
    #actionButton("loadExData", "load example QOF data"), #var,name
    
    selectInput("regionToPlot", "Plot which region ?",
                c("England" = "England",
                  "North" = "North",
                  "South" = "South",
                  "London" = "London",
                  "Midlands and East" = "Midlands and East"
                )),    
    
    checkboxInput("addLegend", "add Legend to maps", TRUE), #var,name

    checkboxInput("addCodes", "add CCG codes to maps", FALSE), #var,name
    
    
    #File upload button
    #works differently if on server to local
    #if (interactive() == TRUE) {
    #  actionButton('upload', 'Load a CCG csv - ccg code in col 1')
    #} else {
    #  fileInput('file1', 'Load a CCG csv - ccg code in col 1')
    #}

    
    wellPanel(
      
    fileInput('file1', 'Load a CCG csv - ccg code in column 1')
    
    ) #end of wellPanel   

    
    ##may2013 this bit seems to have stopped working
    ##the pasteing of data is no longer listened to
    #*  h5("Paste data to plot from Excel"), 
    #*  h6("in columns with headers, 1st column to contain CCG codes"),
    #*  tags$textarea(id="tr_copyAndPaste", rows=8, cols=999, ""),
    #*  h6("or type above with commas between columns, e.g.:"),
    #*  h6("ccgcode,testData"),
    #*  h6("00L,1"),
    #*  h6("06W,2")    
    
      #submitButton("Join Data To Map")

    ),
  
  
  mainPanel(
    
    h5("A NHSHackDay project 2013 by Andy South, in dev., southandy@gmail.com"),
    h5("Winner of prize from Chief Data Officer of the NHS in England, May 2013"),
    #textOutput("testReactivity"),
    
    tabsetPanel(
      tabPanel("Map", plotOutput("mapPlot")),
      tabPanel("Bar Plot", plotOutput("barPlot")),
      tabPanel("Bubble Map", plotOutput("mapBubblesPlot")),      
      tabPanel("Table", tableOutput("tablePlot")),
      #data from paste
      #tabPanel("Pasted attribute data", tableOutput("pastedData")),
      #data from a loaded file
      tabPanel("Uploaded attribute data", tableOutput("filetable")),
      
      #tabPanel("About", tableOutput("textAbout"))      
      tabPanel("About", verbatimTextOutput("textAbout"))
      
      #tabPanel("demo Zoom map", includeHTML("olayer.html"))      
    ) # end of tabsetPanel
        
  )
))
