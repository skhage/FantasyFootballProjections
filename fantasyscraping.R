



require(shiny)
library('RCurl')
library('rjson')
require(jsonlite)
require('reshape2')
require(stringr)
require("Hmisc")
require("ifultools")
options(rpubs.upload.method = "internal")
options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

nfl.start <- as.Date("2015-09-13")
nfl.start + 7
week <- ifelse(Sys.Date()<=nfl.start,1,
               ifelse(Sys.Date()<=nfl.start+7,2,
                      ifelse(Sys.Date()<=nfl.start+14,3,
                             ifelse(Sys.Date()<=nfl.start+21,4,
                                    ifelse(Sys.Date()<=nfl.start+28,5,
                                           ifelse(Sys.Date()<=nfl.start+35,6,
                                                  ifelse(Sys.Date()<=nfl.start+42,7,
                                                         ifelse(Sys.Date()<=nfl.start+49,8,
                                                                ifelse(Sys.Date()<=nfl.start+56,9,
                                                                       ifelse(Sys.Date()<=nfl.start+63,10,
                                                                              ifelse(Sys.Date()<=nfl.start+70,11,
                                                                                     ifelse(Sys.Date()<=nfl.start+77,12,
                                                                                            ifelse(Sys.Date()<=nfl.start+84,13,
                                                                                                   ifelse(Sys.Date()<=nfl.start+91,14,
                                                                                                          ifelse(Sys.Date()<=nfl.start+98,15,
                                                                                                                 ifelse(Sys.Date()<=nfl.start+105,16,
                                                                                                                        ifelse(Sys.Date()<=nfl.start+112,17))
                                                                                                          )
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
               )
)


#Get Scout Data
scout <- getURL("https://www.kimonolabs.com/api/7oz59vj0?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
scoutdata <- fromJSON(scout,simplifyVector = TRUE)
#Get fantasypro.com data
fantasyproqb <- getURL("https://www.kimonolabs.com/api/1wwrvdou?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
fantasyprodataqb <- fromJSON(fantasyproqb,simplifyVector = TRUE)
fantasyprorb <- getURL("https://www.kimonolabs.com/api/4r3ukxte?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
fantasyprodatarb <- fromJSON(fantasyprorb,simplifyVector = TRUE)
fantasyprote <- getURL("https://www.kimonolabs.com/api/77ohqewi?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
fantasyprodatate <- fromJSON(fantasyprote,simplifyVector = TRUE)
fantasyprok <- getURL("https://www.kimonolabs.com/api/3lrfsyx4?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
fantasyprodatak <- fromJSON(fantasyprok,simplifyVector = TRUE)
fantasyprodataqb$name <- fantasyprodataqb$results$collection1$Player$text
fantasyprodataqb$name <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",fantasyprodataqb$name)))))
fantasyprodataqb$points <- fantasyprodataqb$results$collection1$Points
fpdata <- c(fantasyprodataqb$name,fantasyprodataqb$points)
fpdata <- melt(data.frame(fantasyprodataqb$name,fantasyprodataqb$points))
names(fpdata)  <- c("Name","Points")
head(fpdata)

fantasyprodatarb$name <- fantasyprodatarb$results$collection1$Player$text
fantasyprodatarb$name <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",fantasyprodatarb$name)))))
fantasyprodatarb$points <- fantasyprodatarb$results$collection1$Points
fpdatar <- c(fantasyprodatarb$name,fantasyprodatarb$points)
fpdatar <- melt(data.frame(fantasyprodatarb$name,fantasyprodatarb$points))
names(fpdatar)<- c("Name","Points")

fantasyprodatate$name <- fantasyprodatate$results$collection1$Player$text
fantasyprodatate$name <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",fantasyprodatate$name)))))
fantasyprodatate$points <- fantasyprodatate$results$collection1$Points
fantasyprodatate$points
fpdatat <- c(fantasyprodatate$name,fantasyprodatate$points)
fpdatat <- melt(data.frame(fantasyprodatate$name,fantasyprodatate$points))
names(fpdatat)<- c("Name","Points")

fantasyprodatak$name <- fantasyprodatak$results$collection1$Player$text
fantasyprodatak$name <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",fantasyprodatak$name)))))
fantasyprodatak$points <- fantasyprodatak$results$collection1$Points
fpdatak <- c(fantasyprodatak$name,fantasyprodatak$points)
fpdatak <- melt(data.frame(fantasyprodatak$name,fantasyprodatak$points))
names(fpdatak)<- c("Name","Points")

names(fpdata) == names(fpdatak)
fpdatatotal <- rbind(fpdata,fpdatar,fpdatat,fpdatak)
head(fpdatatotal)
#CBS
#scout <- getURL("https://www.kimonolabs.com/api/7oz59vj0?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
#scoutdata <- fromJSON(scout,simplifyVector = TRUE)
#Fantasy Sharks
#sharksurl <- paste("http://www.fantasysharks.com/apps/Projections/SeasonProjections.php?pos=ALL&format=json&l=",week,sep="")
#sharksurl <- gsub('"','',sharksurl)
#sharksurl
shark  <- getURL('http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=ALL&format=json')
sharks <- fromJSON(shark,simplifyVector = TRUE)
head(sharks)
#ESPN
espnn <- getURL("https://www.kimonolabs.com/api/7oz59vj0?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
espn <- fromJSON(espnn,simplifyVector = TRUE)
#FantasyCalculator
calculate <- getURL("https://www.kimonolabs.com/api/7oz59vj0?apikey=QFe3RJsKm2IwYMfsvKw3PrHhPBr3yQ8z")
calculator <- fromJSON(calculate,simplifyVector = TRUE)
calculator$name <- calculator$results$collection1$Playername$text
calculator$name <- tolower(gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",scout$results$collection1$Playername$text)))))
calculator$points <- calculator$results$collection1$ProjectedPoints
ffcalc <- c(calculator$name,calculator$points)
ffcalc <- melt(data.frame(calculator$name,calculator$points))
names(ffcalc) <- c("Name","Fantasy.Calculator.Points")

#Scout Table
scout <- fromJSON(scout)
scout$rank <- scout$results$collection1$PlayerRank
scout$player <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",scout$results$collection1$Playername$text)))))
scout$position <- scout$results$collection1$Position
scout$projection <- scout$results$collection1$ProjectedPoints

scoutdata <- c(scout$rank,scout$player,scout$position,scout$status)
scoutdata <- melt(data.frame(scout$rank,scout$player,scout$position,scout$projection))
names(scoutdata) <- c("Rank","Name","Position","Scout.Points")


#FantasyPro Table
head(sharks)
sharks$rank <- sharks$Rank
sharks$player <- strsplit(sharks$Name,", ")
sharks$player <- unlist(lapply(sharks$player,
                               function(x) paste(x[1:length(x)%%2==0],
                                                 x[1:length(x)%% 2!= 0])))
sharks$player <- (gsub("[.]","",gsub("'","",sharks$player)))
sharks$player
sharks$points <- sharks$FantasyPoints
sharks$points
sharksdata <- c(sharks$player,sharks$points)
sharksdata <- melt(data.frame(sharks$player,sharks$Pos,sharks$points,sharks$Team,sharks$Opp))
names(sharksdata) <- c("Name","Position","Team","Opponent","Disregard","Sharks.Points")
(sharksdata)

espn$player <- espn$results$collection1$Playername$text
espn$player <- (gsub("[.]","",gsub("'","",gsub(" Jr","",espn$player))))
espn$points <- espn$results$collection1$ProjectedPoints
espndata <- c(espn$player,espn$points)
espndata <- melt(data.frame(espn$player,espn$points))
espndata
names(espndata) <- c("Name","ESPN.Points")

#Merge all data into one table
calculator$name <- calculator$results$collection1$Playername$text
calculator$name <- (gsub("'","",gsub("[.]","",gsub("Jr","",gsub("/","",scout$results$collection1$Playername$text)))))
calculator$points <- calculator$results$collection1$ProjectedPoints
ffcalc <- c(calculator$name,calculator$points)
ffcalc <- melt(data.frame(calculator$name,calculator$points))
names(ffcalc) <- c("Name","Fantasy.Calculator.Points")
(espndata)
(sharksdata)
(scoutdata)
(fpdatatotal)
(ffcalc)
abc <- merge(espndata,sharksdata,by="Name")
def <- merge(abc,scoutdata,by="Name");head(def)
ghi <- merge(def,fpdatatotal,by="Name");head(ghi)
alldata <- merge(ghi,ffcalc,by="Name");head(alldata)

names(alldata) <- c("Player","ESPN","Position","Team","Opponent","Disregard","Sharks","SharksRank","Position1","Scout","FPro","FCalc")

alldata$ESPN <- as.numeric(levels(alldata$ESPN))[alldata$ESPN]
alldata$Scout <- as.numeric(levels(alldata$Scout))[alldata$Scout]
alldata$FPro <- as.numeric(levels(alldata$FPro))[alldata$FPro]
alldata$FCalc <- as.numeric(levels(alldata$FCalc))[alldata$FCalc]
alldata$Projected.Points <- (alldata$ESPN+alldata$Sharks+alldata$Scout+alldata$FPro+alldata$FCalc)/5
head(alldata)
#alldata$Name  <- properCase(alldata$Name)

alldata[c(1,3,4,5,13)]

ui <- shinyUI(fluidPage(
  title = 'Examples of DataTables',
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "diamonds"',
        selectInput("pos",
                    "Position:",
                    c("All","QB","RB","WR","TE","K"))
      ),
      conditionalPanel(
        'input.dataset === "mtcars"',
        helpText('Click the column header to sort a column.')
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText('Display 5 records by default.')
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('diamonds', dataTableOutput('mytable1')),
        tabPanel('mtcars', dataTableOutput('mytable2')),
        tabPanel('iris', dataTableOutput('mytable3'))
      )
    )
  )
))

server <- shinyServer(function(input, output) {
  
  # a large table, reative to input$show_vars
  output$mytable1 <- renderDataTable({
    library(ggplot2)
    data <- alldata[order(-alldata[,13]),][c(1,3,4,5,13)]
    if(input$pos != "All"){
      data <- data[data$Position %in% input$pos,]
    }
    data
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderDataTable({
    mtcars
  }, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- renderDataTable({
    iris
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 15))
  
})

shinyApp(ui=ui,server=server)

