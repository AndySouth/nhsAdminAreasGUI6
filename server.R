#nhsAdminAreasGUI6/server.r CURRENT
#andy south 31/5/2013

#to run type this in R console
#library(shiny)
#setwd('C:\\rProjects\\shiny\\')
#runApp('nhsAdminAreasGUI6')

library(shiny)
library(rworldmap)

#load the data stored as files in the app folder
load('sPDF_CCG2.rda')

#whether the example data is loaded
exDataLoaded <- FALSE

#global variable to store uploaded user data
dFuserData <- NULL

shinyServer(function(input, output) {
 
  #browser() #stop execution use n-stepthrough, Q-finish
  
  #for Barrys openLayers tab
  #addResourcePath("maps","maps")

  #creating a reactive variable for the map+data
  #so that whenever it is changed anything using it updates
  #v <- reactiveValues( sPDF_CCGlocal = sPDF_CCG2 )
  #don't think I want exDataLoaded to create reaction
  #v <- reactiveValues( sPDF_CCGlocal = sPDF_CCG2
  #                   , exDataLoaded = exDataLoaded)  
  v <- reactiveValues( sPDF_CCGlocal = sPDF_CCG2
                     , dFuserData = dFuserData)  
  
  
  ################################################
  # Drop-down selection box for columns determined
  output$choose_dataset <- renderUI( {  
    
    input$loadExData #to try to make sure this gets triggered when input$loadExData changes
    
    cat("in choose_dataset input$loadExData=",input$loadExData," exDataLoaded=", exDataLoaded,"\n")
    
    selectInput("attributeToPlot", "Plot which attribute of CCGs ?", choices=getColumnNames())                  
  })

  
  ################################################################
  # to provide a list of selected species based on input parameter
  # **KEY TO HOW APP WORKS***
  getColumnNames <- reactive( {  
    
    cat("in getColumnNames input$loadExData=",input$loadExData," exDataLoaded=", exDataLoaded,"\n")
    
    #check whether example data check box is selected & whether it's not loaded already
    #if so load & join data
    if ( input$loadExData && !exDataLoaded ) 
    {
      # <<- for global assignment
      exDataLoaded <<- loadExDataFunc()
      
    } else if ( !input$loadExData && exDataLoaded ) 
    {
      #set table back to original
      v$sPDF_CCGlocal <- sPDF_CCG2
      exDataLoaded <<- FALSE # <<- for global assignment
    }
    
    #reload user data if a file has been loaded
    if (!is.null(input$file1) && nrow(input$file1) > 0)
    {
      values$fpath <- input$file1[1,'datapath']
      loadUserDataFunc(values$fpath)
    }
    
    #returning column names to go in selectInput
    #miss off first 2 containing ccg code & name
    as.character( names(v$sPDF_CCGlocal)[c(3:ncol(v$sPDF_CCGlocal))])   
    
    }) #end of getColumnNames 

  
  ###########################################
  #creating a command string for map plotting
  formulaText <- reactive({
    
    trueOrFalse <- "FALSE"
    if (input$addLegend) trueOrFalse <- "TRUE"
 
    textOut <- paste("mapPolys(sPDF, nameColumnToPlot='",input$attributeToPlot,"', addLegend=",trueOrFalse,", catMethod='pretty', numCats=5)",sep='') #, mapTitle=''   
    
    
    if (input$addCodes)
    {
      textAdd <- "\ntext( coordinates(sPDF), sPDF$CCG.code, cex=0.8 )"
      textOut <- paste(textOut,textAdd)
    }
    
    if ( input$regionToPlot != 'England' )
    {    
      textAdd <- paste("sPDF <- sPDF[ which(sPDF$Region =='", input$regionToPlot,"'), ]\n", sep='')
      textOut <- paste(textAdd, textOut)
    }
    
    textOut 
    }) # end of formulaText()

  
  # Return the formula text for printing as a caption
  output$caption <- renderText( {
    formulaText()
  })
 


  
  #######################################################
  #to allow reading in of a file with multiple separators
  readMulti <- function(x, sep, replace="\t", as.is=TRUE, header=TRUE)
  {
    dat <- readLines(x)
    dat <- gsub(sep, replace, dat)
    dat <- textConnection(dat)
    dat <- read.table(dat, sep = replace, as.is = as.is)
    
    return(dat)
  } 
   

#may 2013 new code to facilitate file upload, from Joe Cheng 20/4/2013
#https://groups.google.com/forum/?fromgroups#!topic/shiny-discuss/wcOedExyByw
#  values <- reactiveValues()
#  if (interactive() == TRUE) {
#    observe({
#      if (input$upload != 0) {
#        values$fpath <- try(file.choose(), silent=TRUE)
#      }
#    })
#  } else {
#    observe({
#      if (is.null(input$file1) || nrow(input$file1) == 0)
#        values$fpath <- NULL
#      else
#        values$fpath <- input$file1[1,'datapath']
#    })
#  }  

  #########################
  #listens to the file load
  #reads file & joins to ccg data
  values <- reactiveValues() 
   observe({
    
    if (is.null(input$file1) || nrow(input$file1) == 0)
      values$fpath <- NULL
    else
    {
      values$fpath <- input$file1[1,'datapath']
      print(paste("values$fpath=",values$fpath)) 
      
      #@ cool yes !
      #@ this may not be needed here because it may be dealt with by getColumnNames()
      #@ v$sPDF_CCGlocal <- sPDF_CCG2 #set back to the base file
      #@ exDataLoaded <- FALSE #set to F so that loadExDataFunc() assesses whether to add ex data    
      #@ loadExDataFunc() #load ex data if checkbox is ticked
      #@ loadUserDataFunc(values$fpath)
      
      #|replaced with loadUserDataFunc()
      #|dFuserData <- read.csv(values$fpath)
      #|v$dFuserData <- dFuserData #to prompt reactive listeners
      #|print(dFuserData[c(1:2),])     
      #|joinCCGData(dFuserData)
    }
  })
 
  #######################
  # to load & join user data 
  loadUserDataFunc <- function(inFile) { 

    dFuserData <- read.csv(inFile)
    
    v$dFuserData <- dFuserData #to prompt reactive listeners
    
    #print(dFuserData[c(1:2),])     
    
    joinCCGData(dFuserData)
    
  } #end of loadUserDataFunc  
 
  
  #######################
  # to load example data
  #loadExData <- reactive( {  
  loadExDataFunc <- function() { 
    
    #cat("in loadExData exDataLoaded=",exDataLoaded,"\n")
    cat("in loadExData input$loadExData=",input$loadExData," exDataLoaded=", exDataLoaded,"\n")
    
    #only load if not loaded already
    if (input$loadExData && !exDataLoaded )
    {
      inFile <- "qofCCGdata.csv" 
      dF <- read.csv(inFile)
      
      print(dF[c(1:2),]) 
      
      v$sPDF_CCGlocal <- joinCCGData(dF)
      
      exDataLoaded <- TRUE
    }
    
    cat("in loadExData returning exDataLoaded as", exDataLoaded,"\n")
    
    return(exDataLoaded)
  } #end of load ex data       

  
##########################  
#join data to the CCG table 
#(taken out from filetable so it can be re-used)
joinCCGData <- function(dFtoJoin)
{
  #join data onto the map (changing the global sPDF in this function)
  #WEHAY! the isolate stops the reactive variable from setting up an infinite reactivity loop !
  #sPDF_CCG3 <- joinData2Map(dF=dFuserData, nameMap=isolate(v$sPDF_CCGlocal), nameJoinIDMap='CCG.code', nameJoinColumnData='CCG.code')
  
  #set joinColumn to name of first
  nameJoinColumnData <- names(dFtoJoin)[1]
  
  sPDF_CCG3 <- joinData2Map(dF=dFtoJoin, nameMap=isolate(v$sPDF_CCGlocal), nameJoinIDMap='CCG.code', nameJoinColumnData=nameJoinColumnData)
    
  v$sPDF_CCGlocal <- sPDF_CCG3
  
} #end of joinCCGData

  
  ##################################
  ### START OF CODE FOR EACH TAB ###
  ##################################  
  
  
  #####################
  #chloropleth map plot 
  output$mapPlot <- renderPlot( {
    
    
    if( length(input$attributeToPlot) == 0 )
    {
      plot.new()
      mtext("please wait for map to load (~20 seconds)",cex=2,col='purple')
    }  
    
    #to stop it trying to plot map before an option has been chosen
    if( length(input$attributeToPlot) > 0 )
    { 
      #sPDF <- v$sPDF_CCGlocal
      #30/5/13 trying isolate here to stop infinite reactivity loops
      sPDF <- isolate(v$sPDF_CCGlocal)
      
      oldpar <- par(mar=c(4,0,1,0)) # bottom, left, top, right  
      eval(parse(text=formulaText()))
      
      mtext("map made using rworldmap at nhsmaps.co.uk", line=-1.5, side=1, cex=0.8, col='purple') #adj=0 left align, adj=1 right align, default=centre
      
      par(oldpar)
    }
    
  }) #end of mapPlot
  
  
  ############
  # bar plot ranked 
  output$barPlot <- renderPlot( {
    
    #sPDF <- v$sPDF_CCGlocal
    #30/5/13 trying isolate here to stop infinite reactivity loops
    sPDF <- isolate(v$sPDF_CCGlocal)
    
    if ( input$regionToPlot != 'England' )
    {     
      sPDF <- sPDF[ which(sPDF$Region == input$regionToPlot),]     
    }
    
    oldpar <- par(mar=c(0,0,0,0)) # no gap therefore no room for axis
    
    #trying out barplotCountryData
    #! could remove NHS & CCG from the names
    sPDF$CCG.nameShort <- gsub('CCG','',sPDF$CCG.name)
    sPDF$CCG.nameShort <- gsub('NHS ','',sPDF$CCG.nameShort)
    
    numPanels = round( nrow(sPDF) / 25 )
    
    #keep arguments the same as for the map tab
    barplotCountryData( sPDF
                        , nameColumnToPlot = input$attributeToPlot
                        , nameCountryColumn = 'CCG.nameShort'                   
                        , numPanels = numPanels  
                        , scaleSameInPanels = TRUE
                        #, main=''
                        , numCats = 5  
                        , catMethod="pretty" )#   
    
    
    mtext("made using rworldmap at nhsmaps.co.uk", line=-1.5, side=1, cex=0.8, col='purple') #adj=0 left align, adj=1 right align, default=centre
    
    par(oldpar)
  }) #end of barPlot    
  
  
  #############
  # bubble plot 
  output$mapBubblesPlot <- renderPlot( {
    
    #sPDF <- v$sPDF_CCGlocal
    #30/5/13 trying isolate here to stop infinite reactivity loops
    sPDF <- isolate(v$sPDF_CCGlocal)
    
    if ( input$regionToPlot != 'England' )
    {     
      sPDF <- sPDF[ which(sPDF$Region == input$regionToPlot),]     
    }
    
    oldpar <- par(mar=c(0,0,0,0)) # no gap therefore no room for axis
    mapBubbles(sPDF, nameZSize=input$attributeToPlot, fill=FALSE )
    
    if (input$addCodes)
    {
      text( coordinates(sPDF), sPDF$CCG.code, cex=0.8 )
    }  
    
    mtext("map made using rworldmap at nhsmaps.co.uk", line=-1.5, side=1, cex=0.8, col='purple') #adj=0 left align, adj=1 right align, default=centre
    
    par(oldpar)
  }) #end of mapBubblesPlot 
  
  
  #######
  # table
  output$tablePlot <- renderTable( {
    
    #sPDF <- v$sPDF_CCGlocal
    #30/5/13 trying isolate here to stop infinite reactivity loops
    sPDF <- isolate(v$sPDF_CCGlocal)
    
    if ( input$regionToPlot != 'England' )
    {     
      sPDF <- sPDF[ which(sPDF$Region == input$regionToPlot),]     
    }
    
    sPDF@data    
  }) #end of tablePlot   

  
  #############################
  # to put user data in a table
  output$filetable <- renderTable( {
    
    if (is.null(values$fpath)) { # User has not uploaded a file yet
      return(NULL)
    }   
    
    #what to print in the table
    v$dFuserData #just the user data
    
  }) # end of filetable 
  

  ###########################
  # text to go in About panel
  output$textAbout <- renderText( {
    #output$textAbout <- renderTable( {    
    
    #B. Rowlingson, C. Martin and R. Aldridge
    #h5("A NHSHackDay project 2013 by Andy South, B. Rowlingson, C. Martin and R. Aldridge"),
    #h5("In development at NHS HackDay London May 2013, come back for updates, contact southandy@gmail.com"),
    
    t000 <- "Uploaded & example data are added to the selection list top left."
    
    t00 <- "View 'Bar Plot' to check colour scale employed in 'Map'"    
    
    t0 <- "Thanks to ..."   
    t01 <- "NHSHackday contributors :"
    t02 <- "Barry Rowlingson, Chris Martin, Rob Aldridge, Geraint Lewis, Tom Fowler and Marcus Baw"
    
    t03 <- "NHSHackday organisers : particularly Carl Reynolds and David Miller"    
    
    t04 <- "Gavin Jamie : for the QOF data, see www.gpcontract.co.uk"
    
    t1 <- "made using R - http://www.r-project.org/"
    t2 <- "and the packages : shiny, sp and rworldmap. Thanks to all developers!"
    
    t3 <- "I'm a freelance data analyst and am looking to develop this further."
    t4 <- "http://www.linkedin.com/pub/andy-south/54/838/314"
    
    t5 <- "If you'd like to be added to a list to be notified of developments, do get in touch."
    
    textOut <- paste(t000,"\n\n",t00,"\n\n",t0,"\n",t01,"\n",t02,"\n\n",t03,"\n\n",t04
                     ,"\n\n",t1,"\n",t2,"\n\n",t3,"\n",t4,"\n\n",t5)
    
    #textOut <- rbind(t1,t2)
    
    textOut
  })  #end of textAbout  
  
  # to test checkbox reactivity
  #output$testReactivity <- renderText({
  #  onOff <- "OFF"
  #  if (input$loadExData) onOff <- "ON" 
  #  onOff
  #})  
  
  
}) #end of shinyServer
