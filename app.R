############################################
#A shiny that looks into the masterfile and brings up cool information about a specific band
############################################

#Apparently I'm trying to see just how many packages I can fit into this app - i'll take bets
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(data.table)
library(DT)
library(foreign)
library(dplyr)
library(htmltools)
library(stringr)
library(leaflet)

#?? use dyplr::filter for subsetting? faster?

#Read in the master file
#master <- read.dbf("X:\\Public\\Data_proofing_scripts\\Sage\\Shiny\\Masterfile_Shiny\\data\\MASTER6.8.16.dbf")
master <- read.dbf("data/mASTER6.8.16.dbf")

#Read in the file that have all of our location codes with coordinates
#loc_all <- read.csv("X:\\Public\\Data_proofing_scripts\\Sage\\Shiny\\Masterfile_Shiny\\data\\all_location_codes.csv")
loc_all <- read.csv("data/all_location_codes.csv")

#These are the fields we will want to display in the table
masterfields <- c("BAND", grep(pattern = "^BD", colnames(master), value = TRUE),
                  grep(pattern = "^(N|n)\\d[0-9]", colnames(master), value = TRUE),
                  grep(pattern = "^(NBC|nbc)\\d[0-9]", colnames(master), value = TRUE))

#alldata <- read.csv("X:\\Public\\Data_proofing_scripts\\Sage\\Shiny\\Masterfile_Shiny\\data\\masterLocData.csv")
alldata <- read.csv("data/masterLocData.csv")

allpoints <- merge(alldata, loc_all, by.x = "Location", by.y = "LOC")
allpoints$lat <- jitter(allpoints$lat, factor = 0.3)  #make sure the factor is ok
allpoints$long <- jitter(allpoints$long, factor = 0.3)

ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
  theme = shinytheme("flatly"),
    
  title = "Band Lookup",
  
  tabPanel("Band Lookup", 
          titlePanel("Lookup a Band"),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              textInput("BAND", "Enter a Band:", width = "45%"),
              #textInput("METAL", "OR, Enter a Metal Band:", width = "45%"),
              #An error message if the inputted band is not in our master file.
              hidden(
                div(id = "nomatches_div",
                    style = "color: red",
                    "There are no birds with that band in our master file.",
                    br(),
                    br()
                    )
                ),
              actionButton("submit", "Submit", icon("paper-plane"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"), #Change color of button?
              br(),
              br(),
              img(src = "brant_band.jpg"),
              helpText("Figure 1: Atlantic brant with tarsal band"),
              "Image Credit: ", a("New Jersey DEP", href = "https://www.state.nj.us/dep/fgw/news/2018/brantstudy18-2.htm", style = "color: blue"),
              br(),
              h4("How to Read a Band"),
              "You read a band from left to right, and then add the color of the band on to the end. For example, 
               looking at Figure 1, the band would be read as E29R. E29 is what is inscribed on the band, and 
               R stands for red, which is the color of the band. Band reads should be four(4) characters long, and can be a mixture of 
               numbers, letters, and symbols.",
              br(),
              br(),
              "Follow this ", a("link", href = "https://www.pwrc.usgs.gov/BBL/bblretrv/", style = "color: blue"), " to report a band sighting",
              br(),
              br(),
              "Follow this ", a("link", href = "https://thomasvanceriecke.wixsite.com/research/research" , style = "color: blue" ), " to learn more information about the project"
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel("General Information",
                         column(11,
                                fluidRow(
                                  hidden(
                                    div(id = 'text_div',
                                        h5(textOutput("text")), 
                                        h4("Sightings:"),
                                        dataTableOutput("sightings") 
                                       )
                                  ),
                                  hidden(
                                    div(id = 'matetext_div',
                                        h5(textOutput("matetext"))
                                       )
                                  ),
                                  hidden(
                                    div(id = 'matesight_div',
                                        h4("Mate Sightings:"),
                                        dataTableOutput("mate")
                                    )
                                  )
                            ))
                       ),
                tabPanel("Map of Banding Sites",
                         br(),
                         br(),
                         leafletOutput("map", width = "97%", height = "650px"),
                         absolutePanel(bottom = 1, left = 25, 
                                       checkboxInput("allBirds", "All Bird Sightings", FALSE),  style = "color: white"
                                        )
                         )
              )
            )
        )
  ),
  
  tabPanel("General Info",
           
           titlePanel("General Information"), 
             tabsetPanel(
               tabPanel("Band",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3, 
                            h4("Band Information"),
                            "This is to provide information on bird banding as well as necessary info needed
                            in order to lookup plastic bands."
                          ),
                          
                          mainPanel(
                            column( 11, 
                            fluidRow(
                              h4("General"),
                              h5("Bands are a way to follow individual birds through their lives. There are two types
                                 of bands you will see. There are metal bands which are federal markers, as well as 
                                 plastic bands that are used by researches. These bands are designed to be read
                                 at a distance."),
                              br(),
                              br(),
                              h4("Color Codes"),
                              h5("When reporting bands it is important to know the color code. Our band codes are composed
                                 of three symbols on the band as well as the color of the band. In order to have easily
                                 readable codes we have different color codes for each band color. You can find these in 
                                 the table below."),
                              "This is where a pretty picture of color codes will go"
                          )))
                        )),
               tabPanel("Locations",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       h4("Location Information"),
                                       "This tab is to provide information on the project location and provide
                                       insight on some of the location codes that will appear in the sightings."),
                          mainPanel(
                            h4("Yukon-Kuskokwim Delta"),
                            h5("The YKD is the delta where the Yukon and Kiskokwim rivers empty into the Bering Sea. 
                               It is the nesting grounds of the Pacific Black Brant and maybe type some other stuff"),
                            "Have some action buttons and a map eventually for showing specific location code areas?"
                          )
                        )
                        ),
               tabPanel("Sighting Types",
                        br(),
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       h4("Sighting Information"),
                                       "Information on the types of bird sightings. Birds can be sighted by banding
                                       drives, nest sighting, and non-colony sights."
                                       ),
                          mainPanel(
                            h4("Banding Drives"),
                            h5("Banding drives occur when brant are molting and unable to fly. Researches herd groups
                               of brant into a general area and then band birds without bands and mark down resights
                               of birds that have been banded before"),
                            "<pic of bird drives>",
                            br(),
                            br(),
                            h4("Nest Sights"),
                            h5("Checking on female nests provides crucial information to researches to help monitor the population
                               by taking into consideration data like individual clutch size. Idk what else to say lol"),
                            "<pic of bird on nest>",
                            br(),
                            br(),
                            h4("Non-Colony Resights"),
                            h5("These resights happen when traveling in non plot areas and someone sees a bird with a band and
                               records their band. This can include people stopping boats and jumping out to go catch a bird and
                               read their band"),
                            "<pic of someone w/ a bird?>"
                          )
                        ))
             )
           )
  
))


server <- function(input, output){
  
  
  #Create a whole buncha dataframes that we'll use
  rv <- reactiveValues(df = NULL) #This is the dataframe that will take all columns in the masterfile that match our input$BAND and we can subset it further after.
  
  history <- reactiveValues(df = NULL) #This is the dataframe that we'll visualize to the user
 
  materv <- reactiveValues(df = NULL) #An equivalent to rv above but for rv's mate. It should be one row with all the columns in the master file that match the mate of the input$BAND
  matehistory <- reactiveValues(df = NULL) #This is the dataframe that we'll visualize to the user
  
  points <- reactiveValues(df = NULL) 
  locations <- reactiveValues(df = NULL) 
  locations$df <- loc_all
  
  #Allows users to input lowercase bands and just converts it to uppercase
  band <- reactive({
    band <- toupper(input$BAND)
  })
  
  #Creates a DF with all the rows that match the input band and has all the same columns as master
  formData <- reactive({
    rv$df <- master[which(master$BAND == band()), ]
  })
  
  
  #This function looks into the rv DF that we created right above, and picks out the most current mate and creates
  #   a DF for the mate (equivalent to the rv$df above)
  mateData <- reactive({
    matecols <-  rv$df[1,grep(pattern = "^MATEP", colnames(rv$df))] #Pulls all the columns that deal with the plastic mate bands
    if(!is.na(last(which(!is.na(matecols)))) ){
      currentmate <- as.character(matecols[,last(which(!is.na(matecols)))]) #Finds the most recent column where the input bird had a mate and pulls the band of the mate
      if(currentmate %in% master$BAND){ #Once there was a mate for a bird but it wasn't in our masterfile 
      materv$df <- master[which(master$BAND == currentmate),] #Creates a new DF with all the columns in the master file for the band that matches the mate.
      }else{ materv$df <- NULL } #I had to include this twice for some reason or else it wouldn't register. Not sure why. 
    }else{ materv$df <- NULL } #If the bird doesn't have a mate or the mate isn't in the masterfile we just keep the DF as null.
  })
  
  #Runs any time the input (Band in this case) changes
  observe({
    input$BAND 
    #Hide the text since they're moving on to a different band
    shinyjs::hide("text_div") 
    shinyjs::hide("matetext_div")
    shinyjs::hide("matesight_div")
    shinyjs::hide("nomatches_div")
    leafletProxy('map') %>%
      clearMarkers()
  })
  
  observeEvent(input$submit, {
    
    #We only run this code if the band the user inputs is in our masterfile. Otherwise the app will crash if the
    #   band doesn't exist.
    if(band() %in% master$BAND){
      shinyjs::show("text_div")
      shinyjs::show("matetext_div")
      
      #Calls our functions above to create rv$df and materv$df
      formData() 
      mateData()
      
      
      #**********Put history df in it's own function?********************
      #Create the input$band's dataframe that we will visualize to the user
      history$df <- rv$df[ ,masterfields] #Pulls the data from all the columns that specify the masterfields we care about.
      history$df <- gather(history$df, key = "Year", value = "Location", -c(BAND), na.rm = T) #Reshapes the data to be easier for people to read. 
      history$df <- separate(history$df, col = "Year", into = c("Type", "Year"), sep = "(?<=[A-Za-z])(?=[0-9])") #Separates the colnames from numbers and letters. For example BD09 will become two new columns (Type and Year) and it will be fileld with BD and 09 respectively.
      history$df <- history$df %>% select(-Type, Type) #Adds Type as the last column instead of in the middle
      
      #Makes things nicer to look at, changing codes into actual words and turning years from two digits to four.
      if(nrow(history$df) != 0){
        for(i in 1:nrow(history$df) ){
          if(history$df$Type[i] == "BD"){history$df$Type[i] <- "Banding Drive"}
          if(history$df$Type[i] == "N" | history$df$Type[i] == "n"){history$df$Type[i] <- "Nest"}
          if(history$df$Type[i] == "NBC"){history$df$Type[i] <- "Non-Breeding Colony"}
          
          year_ext <- str_extract(history$df$Year[i], "\\-*\\d+\\d*")
          if(year_ext < 50){ 
            history$df$Year[i] <- paste0("20",year_ext)
            next
          }else{
            history$df$Year[i] <- paste0("19",year_ext)
          }
        }
      }
      
      #Does the same as above but for mates
      if(!is.null(materv$df)){ #Checks to make sure that there is a mate. If there's not we don't run this
        shinyjs::show("matesight_div")
        matehistory$df <- materv$df[ ,masterfields] #Pulls the data from all the columns that specify the masterfields we care about.
        matehistory$df <- gather(matehistory$df, key = "Year", value = "Location", -c(BAND), na.rm = T) #Reshapes the data to be easier for people to read.
        matehistory$df <- separate(matehistory$df, col = "Year", into = c("Type", "Year"), sep = "(?<=[A-Za-z])(?=[0-9])") #Separates the colnames from numbers and letters. For example BD09 will become two new columns (Type and Year) and it will be fileld with BD and 09 respectively.
        matehistory$df <- matehistory$df %>% select(-Type, Type) #Adds Type as the last column instead of in the middle
        
        if(nrow(matehistory$df) != 0){
          for(i in 1:nrow(matehistory$df) ){
            if(matehistory$df$Type[i] == "BD"){matehistory$df$Type[i] <- "Banding Drive"}
            if(matehistory$df$Type[i] == "N" | matehistory$df$Type[i] == "n"){matehistory$df$Type[i] <- "Nest"}
            if(matehistory$df$Type[i] == "NBC"){matehistory$df$Type[i] <- "Non-Breeding Colony"}
      
            year_ext <- str_extract(matehistory$df$Year[i], "\\-*\\d+\\d*")
            if(year_ext < 50){
              matehistory$df$Year[i] <- paste0("20",year_ext)
              next
            }else{
              matehistory$df$Year[i] <- paste0("19",year_ext)
            }
          }
        }
      }
    }else{shinyjs::show("nomatches_div")}
    
  })
  
  #Pulls the first time that there is a location in the banding drive
  firstloc <- reactive({
    if(!is.na(rv$df$BANDEDYEAR[1])){
      yy <- substr(rv$df$BANDEDYEAR, 3, 4) #Pulls out the last two characters in the year. Ex: 2014 -> 14
      location <- rv$df[,paste0("BD", yy)]  #Looks for the location from the banding drive of the year it was banded. Ex: BD14 and location is whatever BD14 is.
      return(location)
    }else(return(NA))
  })
  
  #Determines sex of the banded bird
  sex <- reactive({
    if(rv$df$SEX[1] == "F"){return("female")}else{return("male")}
  })
  
  #Determines minimum age 
  age <- reactive( {
    if(is.na(rv$df$SYR[1])){
      if(rv$df$BANDEDAGE[1] == "A" & !is.na(rv$df$BANDEDAGE[1])){ #if the bird was captured as an adult its min age is the current year - when it was caught + 2 because it was at least 2 when it was first caught.
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 2) - as.integer(rv$df$BANDEDYEAR[1])
      } else if(rv$df$BANDEDAGE[1] == "SY" & !is.na(rv$df$BANDEDAGE[1])){ #if it was SY at first capture than you add one because it was one year old when it was first captured. 
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 1) - as.integer(rv$df$BANDEDYEAR[1])
      } else {
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 0) - as.integer(rv$df$BANDEDYEAR[1])
      }
    }
    else{
      if(rv$df$BANDEDAGE[1] == "A" & !is.na(rv$df$BANDEDAGE[1])){ #if the bird was captured as an adult its min age is the current year - when it was caught + 2 because it was at least 2 when it was first caught.
        calc_age <- (as.integer(rv$df$SYR[1]) + 2) - as.integer(rv$df$BANDEDYEAR[1])
      } else if(rv$df$BANDEDAGE[1] == "SY" & !is.na(rv$df$BANDEDAGE[1])){ #if it was SY at first capture than you add one because it was one year old when it was first captured.
        calc_age <- (as.integer(rv$df$SYR[1]) + 1) - as.integer(rv$df$BANDEDYEAR[1])
      } else {
        calc_age <- (as.integer(rv$df$SYR[1]) + 0) - as.integer(rv$df$BANDEDYEAR[1])
      }
    }
  })
  
  #Creates text to show for the band entered
  text <- eventReactive(input$submit, {
    paste("Bird ", rv$df$BAND[1], "is a ", sex(), "with a minimum age of ", age(), ", and its corresponding metal
          band is", rv$df$METAL[1], ". It was first caught and banded in ", rv$df$BANDEDYEAR[1], "at ", firstloc(), 
          ". The bird was shot in ", rv$df$SYR[1], " (NA means the bird is still alive according to our records).")
  })
  
  output$text <- renderText({
    text()
  })
  
  #Determines first location of mate
  matefirstloc <- reactive({
    if(!is.na(materv$df$BANDEDYEAR[1])){
      yy <- substr(materv$df$BANDEDYEAR, 3, 4) #Pulls out the last two characters in the year. Ex: 2014 -> 14
      location <- materv$df[,paste0("BD", yy)] 
      return(location)
    }
  })
  
  #Determines sex of the mate
  matesex <- reactive({
    if(materv$df$SEX[1] == "F" & !is.na(materv$df$SEX[1])){return("female")}else{return("male")}
  })
  
  #Determines minimum age of the mate
  mateage <- reactive( {
    if(is.na(materv$df$SYR[1])){
      if(materv$df$BANDEDAGE[1] == "A" & !is.na(materv$df$BANDEDAGE[1])){ 
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 2) - as.integer(materv$df$BANDEDYEAR[1])
      } else if(materv$df$BANDEDAGE[1] == "L" & !is.na(materv$df$BANDEDAGE[1])){
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 1) - as.integer(materv$df$BANDEDYEAR[1])
      } else {
        calc_age <- (as.integer(format(Sys.Date(), "%Y")) + 0) - as.integer(materv$df$BANDEDYEAR[1])
      }
    }
    else{
      if(materv$df$BANDEDAGE[1] == "A" & !is.na(materv$df$BANDEDAGE[1])){ #if the bird was captured as an adult its min age is the current year - when it was caught + 2 because it was at least 2 when it was first caught.
        calc_age <- (as.integer(materv$df$SYR[1]) + 2) - as.integer(materv$df$BANDEDYEAR[1])
      } else if(materv$df$BANDEDAGE[1] == "SY" & !is.na(materv$df$BANDEDAGE[1])){ #if it was SY at first capture than you add one because it was one year old when it was first captured.
        calc_age <- (as.integer(materv$df$SYR[1]) + 1) - as.integer(materv$df$BANDEDYEAR[1])
      } else {
        calc_age <- (as.integer(materv$df$SYR[1]) + 0) - as.integer(materv$df$BANDEDYEAR[1])
      }
    }
  })
  
  
  #Creates text to show for the mate of the bird entered
  matetext <- eventReactive(input$submit, {
    paste("The mate of bird ", rv$df$BAND[1], "is ", materv$df$BAND , "and it is a ", matesex(), "with a minimum age of ", mateage(), ", and its corresponding metal
          band is", materv$df$METAL[1], ". It was first caught and banded in ", materv$df$BANDEDYEAR[1], "at ", 
          matefirstloc(), ". The bird was shot in", materv$df$SYR[1], "(NA means the bird is still alive according
          to our records.")
  })

  
  output$matetext <- renderText({
    if(!is.null(materv$df)){ #Only create summary text if the bird has a mate
    matetext()
    }else{paste("Bird ", band(), "does not have a mate in our files.")} #If the bird doesn't have a mate say so
  })
  
  output$sightings <- DT::renderDataTable(rownames = F, {
    history$df
    #points$df
  })
  
  output$mate <- DT::renderDataTable(rownames = F, {
    if(!is.null(materv$df)){ #Only show this table if the input$BAND has a mate
    matehistory$df
    }
  })
  
  ###
  #Map Stuff
  ###
  
  formPoints <- reactive({
    points$df <- merge(history$df, locations$df, by.x = "Location", by.y = "LOC")
    points$df$lat <- jitter(points$df$lat, factor = 0.3)  #make sure the factor is ok
    points$df$long <- jitter(points$df$long, factor = 0.3)
    return(points$df)
  })
  
  popup <- reactive({
    paste("Bird:", rv$df$BAND, "<br>",
          "Where:",points$df$Location, "<br>",
          "When:", points$df$Year
          )
  })
  
  #This is gonna be wear the interactive map will live?
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% #I also like $Esri.WorldStreetMap and WorldTopoMap
      setView(lat = 61.25, lng = -165.62, zoom = 3) 
      #addMarkers(label = ~Location)  
  })
  
  observeEvent(input$submit, {
    if(band() %in% master$BAND){
      formPoints()
    
    #This observe doesn't happen if you're on the other tab for some reason when starting the app up for the first time

      if(nrow(points$df) >= 1){
        leafletProxy('map', data= points$df) %>%
          clearMarkers() %>%
          addMarkers(label = ~Location, popup = popup())
      }
    }

    #Clears the map if a user enters a band that is not in our files.
    if(!(band() %in% master$BAND)){
      leafletProxy('map') %>%
        clearMarkers()
      }

  })

  observe({
      if(input$allBirds) {
        leafletProxy('map', data = allpoints) %>%
          addCircleMarkers(label = ~Location, clusterOptions = markerClusterOptions())
        
        dat <- data.frame(x = numeric(0), y = numeric(0))
        withProgress(message = 'Adding Points', value = 0, {
          # Number of times we'll go through the loop
          n <- 75

          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste("So Many Birds"))

            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
        })
      }else{
        leafletProxy('map') %>%
          clearMarkerClusters()
      }
  })
  
}


shinyApp(ui = ui, server = server)

# test <- grep(pattern = "^BD", colnames(master), value = TRUE)
# #Find all the banding drives for some reason??
# fake <- master[,which(grepl(pattern = "^BD", colnames(master)))]
#fake1 <- master[which(master$BAND == "D/8Y"), ]
#test <- grep(pattern = "^(N|n)\\d[0-9]", colnames(master), value = TRUE)
#test <- grep(pattern = "^(NBC|nbc)\\d[0-9]", colnames(master), value = TRUE)

#Testing finding the last instance where the mate column isn't NA
# dumb <- master[which(master$BAND == "87EW"), ]
# ugh <-  dumb[,grep(pattern = "^MATEP", colnames(dumb))]
# lastmate <- ugh[,last(which(!is.na(ugh)))]
# new <- master[which(master$BAND == as.character(lastmate)),]

#Testing on reshaping data
# dumber <- dumb[ ,masterfields]
# dumber <- gather(dumber, key = "Year", value = "Location", -c(BAND), na.rm = T)
# dumber <- separate(dumber, col = "Year", into = c("Type", "Year"), sep = "(?<=[A-Za-z])(?=[0-9])")
# dumber%>%select(-Type,Type)


# tagList(
#   tags$head(
#     tags$script(type="text/javascript", src = "busy.js")
#   )
# ),
# div(class = "busy", p('your text'),img(src="loader.gif")
# ),
  
  