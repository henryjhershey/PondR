
library(shiny)
library(devtools)
library(dplyr)
library(plyr)
library(DescTools)

add.Wr = function(x){
  Wr <- NA
  log.TL <- log10(x$tl)
  x <- cbind(x,log.TL)
  for (i in 1:nrow(x)){
    if (x$species[i] %in% c("LMB","lmb")) {
      
      exp. <- -5.528+3.273*x$log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
      
    } else if (x$species[i] %in% c("BG","BLG", "BG","bg","blue")) {
      exp. <- -5.374+3.316*x$log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
      
    } else if (x$species[i] %in% c("RE","re","RES","res","resu","RESU")) {
      exp. <- -4.968+3.119*log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
    }
    
  }
  
 x<- cbind(x,Wr)[,c("species","tl","wt","Wr")]
return(x)
}

install_github("trestletech/shinyTable")

# Define UI for data upload app ----
ui <- navbarPage(title="PondR Web Application",
  
  tabPanel("Analysis",
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
    
      # Horizontal line ----
      tags$hr(),
     
      # Input: Select length units ----
      radioButtons("lunit", "Length Units",
                   choices = c(Inches = "in",
                               Centimeters = "cm",
                               Millimeters = "mm"),
                   selected = ","),
      
      # Input: Select weight units ----
      radioButtons("wunit", "Weight Units",
                   choices = c("Grams" = "g",
                               "Kilograms" = "kg",
                               "Ounces" = "oz"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      selectInput("species", "Species",
                   choices = c("BG",
                               "LMB",
                               "RESU")
                   ),
      numericInput("bins", "Length Bins", 5,
                   5, 50, 5)
    ),
      mainPanel(
        flowLayout(
          plotOutput("lengthfreq",width="400px",height="400px")
          )
        )
      )
    ),

    tabPanel("Data",
      DT::dataTableOutput("lengthtable")
        )
)
# Define server logic to read selected file ----
server <- function(input,output,session) {
  
  data_upload <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat <- read.csv(inFile$datapath,stringsAsFactors = F)
    dat <- add.Wr(dat)
    return(dat)
  })
  
  sp_subset <- reactive({
    dat <- data_upload()
    sub <- subset(dat, dat$species == input$species)
    return(sub)
  })
  
  output$lengthtable<- DT::renderDataTable({
    dat <- data_upload()
    DT::datatable(dat)
  })
  
  
  output$lengthfreq <- renderPlot({
    dat <- sp_subset() 
    mround <- function(x,base){ 
      base*round(x/base) 
    } 
    if (is.null(dat))
        return(NULL)
    par(xaxs="i", yaxs="i")
    hist(dat$tl,
         breaks=seq(RoundTo(min(dat$tl),input$bins,floor),RoundTo(max(dat$tl),input$bins,ceiling),input$bins),
         col="darkgrey",
         xaxt="n",
         border = "white",
         main=paste("Length Frequency\nDistribution:", input$species),
         xlab="Length Bins"
    )
    axis(side=1, at=seq(RoundTo(min(dat$tl),input$bins,floor),RoundTo(max(dat$tl),input$bins,ceiling),input$bins),
         labels=seq(RoundTo(min(dat$tl),input$bins,floor),RoundTo(max(dat$tl),input$bins,ceiling),input$bins))
  })
  
  # output$PSDtable <- renderTable({
  #   if (input$species == "LMB"){ 
  #     if (input$lunit == "mm"){
  #      PSD$stock[1]= length(file1$tl[file1$sp == input$species & file1$tl >= 200])
  #      PSD$quality= length(file1$tl[file1$sp == input$species & file1$tl >= 300])
  #      PSD$preferred= length(file1$tl[file1$sp == input$species & file1$tl >= 380])
  #      PSD$memorable= length(file1$tl[file1$sp == input$species & file1$tl >= 510])
  #      PSD$trophy= length(file1$tl[file1$sp == input$species & file1$tl >= 630])
  #     } else if (input$lunit == "in"){
  #         PSD$stock[1]= length(file1$tl[file1$sp == input$species & file1$tl >= 8])
  #         PSD$quality= length(file1$tl[file1$sp == input$species & file1$tl >= 12])
  #         PSD$preferred= length(file1$tl[file1$sp == input$species & file1$tl >= 15])
  #         PSD$memorable= length(file1$tl[file1$sp == input$species & file1$tl >= 20])
  #         PSD$trophy= length(file1$tl[file1$sp == input$species & file1$tl >= 25])
  #   } else 
  #     PSD$stock[1]= length(file1$tl[file1$sp == input$species & file1$tl >= 8])
  #   PSD$quality= length(file1$tl[file1$sp == input$species & file1$tl >= 12])
  #   PSD$preferred= length(file1$tl[file1$sp == input$species & file1$tl >= 15])
  #   PSD$memorable= length(file1$tl[file1$sp == input$species & file1$tl >= 20])
  #   PSD$trophy= length(file1$tl[file1$sp == input$species & file1$tl >= 25])
  #   } else if ( input$species== "BG"){
  #     PSD$stock[1]= length(file1$tl[file1$sp == input$species & file1$tl >= 8])
  #     PSD$quality= length(file1$tl[file1$sp == input$species & file1$tl >= 12])
  #     PSD$preferred= length(file1$tl[file1$sp == input$species & file1$tl >= 15])
  #     PSD$memorable= length(file1$tl[file1$sp == input$species & file1$tl >= 20])
  #     PSD$trophy= length(file1$tl[file1$sp == input$species & file1$tl >= 25])    }else 
  #    100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 330)/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu")] >= 100)
  #   
  #   
  #   
  # })
  
}

shinyApp(ui = ui, server = server)

