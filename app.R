
library(shiny)
library(devtools)
library(dplyr)
library(plyr)
library(DescTools)
library(xtable)

add.Wr = function(x,units=c("Metric","English")){
  
  Mcoefs <- data.frame(a=c(-5.374,-5.528,-4.968),b=c(3.316,3.273,3.119),row.names=c("BG","LMB","RESU"))
  Ecoefs <- data.frame(a=c(-3.371,-3.587,-3.263),b=c(3.316,3.273,3.119),row.names=c("BG","LMB","RESU"))
  coefs <- ifelse(units=="Metric",Mcoefs,Ecoefs)
  Wr <- NA
  log.TL <- log10(x$tl)
  x <- cbind(x,log.TL)
  for (i in 1:nrow(x)){
     if (x$species[i] %in% c("BG","BLG", "BG","bg","blue")) {
      exp. <- -5.374+3.316*x$log.TL[i]
      Ws <- 10^exp.
      Wr[i] <- 100*x$wt[i]/Ws
      
    } else if (x$species[i] %in% c("LMB","lmb")) {
      
      exp. <- -5.528+3.273*x$log.TL[i]
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

x=dat
PSDval
PSDtab <- function(x,sp,units=c("Metric","English")){
  PSDval <- if(units=="Metric"){
    t(data.frame(stock=c(80,200,100),quality=c(150,300,180),preferred=c(200,380,230),
                 memorable=c(250,510,280),trophy=c(300,630,330),row.names=c("BG","LMB","RESU")))
  } else t(data.frame(stock=c(3,8,4),quality=c(6,12,7),preferred=c(8,15,9),
                      memorable=c(10,20,11),trophy=c(12,25,13),row.names=c("BG","LMB","RESU")))

  if (sp=="BG"){
    PSDtable <- data.frame(SizeClass=c("Percentage of Sample","Quantity"), quality=NA, preferred=NA, memorable=NA, trophy=NA)
    PSDtable[1,2] <- round(100*sum(x$tl[x$species=="BG"] >= PSDval[2,"BG"])/sum(x$tl[x$species=="BG"] >= PSDval[1,"BG"]))
    PSDtable[2,2] <- sum(x$tl[x$species=="BG"] >= PSDval[2,"BG"])
    PSDtable[1,3] <- round(100*sum(x$tl[x$species=="BG"] >= PSDval[3,"BG"])/sum(x$tl[x$species=="BG"] >= PSDval[1,"BG"]))
    PSDtable[2,3] <- sum(x$tl[x$species=="BG"] >= PSDval[3,"BG"])
    PSDtable[1,4] <- round(100*sum(x$tl[x$species=="BG"] >= PSDval[4,"BG"])/sum(x$tl[x$species=="BG"] >= PSDval[1,"BG"]))
    PSDtable[2,4] <- sum(x$tl[x$species=="BG"] >= PSDval[4,"BG"])
    PSDtable[1,5] <- round(100*sum(x$tl[x$species=="BG"] >= PSDval[5,"BG"])/sum(x$tl[x$species=="BG"] >= PSDval[1,"BG"]))
    PSDtable[2,5] <- sum(x$tl[x$species=="BG"] >= PSDval[5,"BG"])
  } else if (sp=="LMB") {
    PSDtable <- data.frame(SizeClass=c("Percentage of Sample","Quantity"), quality=NA, preferred=NA, memorable=NA, trophy=NA)
    PSDtable[1,2] <- round(100*sum(x$tl[x$species=="LMB"] >= PSDval[2,"LMB"])/sum(x$tl[x$species=="LMB"] >= PSDval[1,"LMB"]))
    PSDtable[2,2] <- sum(x$tl[x$species=="LMB"] >= PSDval[2,"LMB"])
    PSDtable[1,3] <- round(100*sum(x$tl[x$species=="LMB"] >= PSDval[3,"LMB"])/sum(x$tl[x$species=="LMB"] >= PSDval[1,"LMB"]))
    PSDtable[2,3] <- sum(x$tl[x$species=="LMB"] >= PSDval[3,"LMB"])
    PSDtable[1,4] <- round(100*sum(x$tl[x$species=="LMB"] >= PSDval[4,"LMB"])/sum(x$tl[x$species=="LMB"] >= PSDval[1,"LMB"]))
    PSDtable[2,4] <- sum(x$tl[x$species=="LMB"] >= PSDval[4,"LMB"])
    PSDtable[1,5] <- round(100*sum(x$tl[x$species=="LMB"] >= PSDval[5,"LMB"])/sum(x$tl[x$species=="LMB"] >= PSDval[1,"LMB"]))
    PSDtable[2,5] <- sum(x$tl[x$species=="LMB"] >= PSDval[5,"LMB"])
  } else if (sp=="RESU") {
    PSDtable <- data.frame(SizeClass=c("Percentage of Sample","Quantity"), quality=NA, preferred=NA, memorable=NA, trophy=NA)
    PSDtable[1,2] <- round(100*sum(x$tl[x$species=="RESU"] >= PSDval[2,"RESU"])/sum(x$tl[x$species=="RESU"] >= PSDval[1,"RESU"]))
    PSDtable[2,2] <- sum(x$tl[x$species=="RESU"] >= PSDval[2,"RESU"])
    PSDtable[1,3] <- round(100*sum(x$tl[x$species=="RESU"] >= PSDval[3,"RESU"])/sum(x$tl[x$species=="RESU"] >= PSDval[1,"RESU"]))
    PSDtable[2,3] <- sum(x$tl[x$species=="RESU"] >= PSDval[3,"RESU"])
    PSDtable[1,4] <- round(100*sum(x$tl[x$species=="RESU"] >= PSDval[4,"RESU"])/sum(x$tl[x$species=="RESU"] >= PSDval[1,"RESU"]))
    PSDtable[2,4] <- sum(x$tl[x$species=="RESU"] >= PSDval[4,"RESU"])
    PSDtable[1,5] <- round(100*sum(x$tl[x$species=="RESU"] >= PSDval[5,"RESU"])/sum(x$tl[x$species=="RESU"] >= PSDval[1,"RESU"]))
    PSDtable[2,5] <- sum(x$tl[x$species=="RESU"] >= PSDval[5,"RESU"])
  }
  return(PSDtable)
}
dat$tl <- dat$tl*.039
PSDtab(dat,sp="BG",units="English")

PSD = function(x, type=c("quality","preferred","memorable","trophy"),units=c("English","Metric"),do.plot=F){
  PSDval <- if(units=="Metric"){
    t(data.frame(stock=c(80,200,100),quality=c(150,300,180),preferred=c(200,380,230),
                 memorable=c(250,510,280),trophy=c(300,630,330),row.names=c("BG","LMB","RESU")))
  } else t(data.frame(stock=c(3,8,4),quality=c(6,12,7),preferred=c(8,15,9),
                      memorable=c(10,20,11),trophy=c(12,25,13),row.names=c("BG","LMB","RESU")))
  
  if (type=="quality"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RESU=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[2,"LMB"])/sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[1,"LMB"])
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[2,"BG"])/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[1,"BG"])
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[2,"RESU"])/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[1,"RESU"])
    PSDtable
  }else if (type=="preferred"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RESU=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[3,"LMB"])/sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[1,"LMB"])
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[3,"BG"])/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[1,"BG"])
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[3,"RESU"])/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[1,"RESU"])
    PSDtable
    
  } else if (type=="memorable"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RESU=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[4,"LMB"])/sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[1,"LMB"])
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[4,"BG"])/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[1,"BG"])
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[4,"RESU"])/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[1,"RESU"])
    PSDtable
    
  }else if (type=="trophy"){
    PSDtable <- data.frame(LMB=NA, BG=NA, RESU=NA, row.names = "PSD")
    PSDtable[1,1] <- 100*sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[5,"LMB"])/sum(x$tl[x$sp %in% c("LMB","lmb")] >= PSDval[1,"LMB"])
    PSDtable[1,2] <- 100*sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[5,"BG"])/sum(x$tl[x$sp %in% c("BG","BLG", "BG","bg","blue")] >= PSDval[1,"BG"])
    PSDtable[1,3] <- 100*sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[5,"RESU"])/sum(x$tl[x$sp %in% c("RE","re","RES","res","resu","RESU")] >= PSDval[1,"RESU"])
    PSDtable
  }
  
  if(do.plot==T){
    plot(PSDtable[1,1]~PSDtable[1,2], xlim=c(0,100),ylim=c(0,100), 
         xlab = "Bluegill PSD", ylab = "Largemouth Bass PSD", main = "Pond Balance", pch=17, cex=1.5, col="red")
    abline(40,0, lty = 3)
    abline(70,0, lty = 3)
    abline(v=40, lty =3)
    abline(v=20, lty=3)
    text(70,20, "Bass Crowded")
    text(8,90, "Bluegill \nCrowded")
    text(10,20, "Unfertile")
    text(30,55, "Balanced")
  }
  return(PSDtable)
}
dat$sp <- dat$species
PSD(dat,units="English")

# Define UI for data upload app ----
ui <- navbarPage(title="PondR Web Application",
  tabPanel("Analysis",
 
  # Sidebar layout with input and output definitions ----
  sidebarLayout(fluid=FALSE,
  
    # Sidebar panel for inputs ----
    sidebarPanel(      
      p("Follow these steps to get started using PondR:"),
      p("1. Open your excel spreadsheet and make sure you have three columns titled 'species', 'tl' (total length), and 'wt' (weight)."),
      p("2. Click 'Save As' and choose 'CSV comma separated values file'."),
      p("3. Now click the 'Browse...' button below and select the file you just saved."),
      
       # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      p("4. Choose the units you made your measurements in, and the species you want to look at."),
      # Input: Select length units ----
      radioButtons("units", "Units",
                   choices = c(Metric = "Metric",
                               English = "English"
                               ),
                   selected = ","),
      p("Metric Units must be millimeters and grams. English Units must be inches and pounds"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      selectInput("species", "Species",
                   choices = c("BG",
                               "LMB",
                               "RESU")
                   ),
      p("5. Explore the results of your data!"),
      width = 3),
      mainPanel(
        tabsetPanel(
          tabPanel("Length Frequency",
                   plotOutput("lengthfreq"),
                   numericInput("bins", "Length Bin Size", 1, 5, 50, 5),
                   verbatimTextOutput("CentralTendency"),align="center"
                   ),
          tabPanel("Relative Weight", plotOutput("relweight"),align="center"),
          tabPanel("PSD", 
                   flowLayout(
                   column(
                   verbatimTextOutput("thespecies"), align="center",
                   tableOutput("PSDtable")
                   )
                   )
          )
          )
        )
      )
    ),
    tabPanel("Pond Balance",plotOutput("PSDplot")),
    tabPanel("Data", DT::dataTableOutput("lengthtable"))
)
# Define server logic to read selected file ----
server <- function(input,output,session) {
  
  data_upload <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat <- read.csv(inFile$datapath,stringsAsFactors = F)
    dat <- add.Wr(dat,units=input$units)
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
  
  output$CentralTendency <- renderPrint({
    dat <- sp_subset() 
    if (is.null(dat))
      return("A summary table will appear here after a file is uploaded.")
    summary(dat$tl)
  })
  
  output$thespecies <- renderPrint({
    print(input$species)
  })
  
  output$PSDtable <- renderTable({
    dat <- data_upload()
    if (is.null(dat))
      return("A table showing the proportional size distribution will appear here after a file is uploaded.")
    PSDtab(dat,sp=input$species,units=input$units)
  })
 
  output$PSDplot <- renderPlot({
    dat <- data_upload()
    if (is.null(dat)==T)
      return("A plot showing the pond balance will appear here after a file is uploaded.")  
    PSDTABLE <- PSD(dat,units=input$units,do.plot=T)
    })
  
  output$lengthfreq <- renderPlot({
    dat <- sp_subset() 
    mround <- function(x,base){ 
      base*round(x/base) 
    } 
    if (is.null(dat))
        return(NULL)
    par(xaxs="i", yaxs="i")
    lb=RoundTo(min(dat$tl),input$bins,floor)
    ub=RoundTo(max(dat$tl),input$bins,ceiling)
    hist(dat$tl,
         breaks=seq(RoundTo(min(dat$tl),input$bins,floor),RoundTo(max(dat$tl),input$bins,ceiling),input$bins),
         col="darkgrey",
         xaxt="n",  
         border = "white",
         main=paste("Length Frequency\nDistribution:", input$species),
         xlab="Length Bins"
    )
    
    axis(side=1, at=seq(lb,ub,input$bins),
         labels=seq(lb,ub,input$bins))
  })
  
  output$relweight <- renderPlot({
    dat <- sp_subset()
    if (is.null(dat))
      return(NULL)
    plot(dat$Wr~ dat$tl, ylab = "Relative Weight", xlab = "Total Length (in)", main = paste("Relative Weight",input$species))
    abline(90,0, lty= 3, col= "green")
    abline(80,0, lty= 3, col= "red")
    abline(100,0, lty = 3, col = "blue")
    
  })
  
}

shinyApp(ui = ui, server = server)

