#rshiny_anom_detect
#cep1= read.csv("teras-cep-1_20171201-0955021.csv",header = TRUE)
#cep2= read.csv("teras-cep-2_20171201-1024441.csv",header = TRUE)
#cep3= read.csv("teras-cep-3_20171201-1049371.csv",header = TRUE)
teras32254= read.csv("filename.csv",header = TRUE)

s1=teras32254[,1:12]
q1=names(teras32254[,1:12])
library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <-dashboardPage (
  dashboardHeader(title = "Rshiny : CI"),
  dashboardSidebar(),
  dashboardBody( fluidPage(theme = shinytheme("paper"),
                
                
                # Application title
                titlePanel("IDA Fan Data points"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "vars", 
                                label = "Select Dataset", 
                                choices = c("teras32254")),
                    selectInput("x", "Select Variable",q1),
                    radioButtons("y",
                                 label="Select Confidence Interval",
                                 list("90"='p',"95"='q')),
                    print("Dimension:"),
                    br(),
                    print("No. of Rows:"),
                    nrow(s1),
                    br(),
                    print("No. of columns:"),
                    ncol(s1),
                    br(),
                    br(),
                    DTOutput(outputId = "table1")),
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    
                        plotOutput("distPlot"),
                    br(),
                    column(10,align="center",
                           DTOutput(outputId = "table",width="35%"))
                  )
                ))))
# Define server logic requi#FF6347 to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$x==q1[1]){
      i<-1
      if(input$y=='q'){
        j<-13
      }
      if(input$y=='p'){
        j<-14
      }
    }
    if(input$x==q1[2]){
      i<-2
      if(input$y=='q'){
        j<-15
      }
      if(input$y=='p'){
        j<-16
      }
    }
    if(input$x==q1[3]){
      i<-3
      if(input$y=='q'){
        j<-17
      }
      if(input$y=='p'){
        j<-18
      }
    }
    if(input$x==q1[4]){
      i<-4
      if(input$y=='q'){
        j<-19
      }
      if(input$y=='p'){
        j<-20
      }
    }
    if(input$x==q1[5]){
      i<-5
      if(input$y=='q'){
        j<-21
      }
      if(input$y=='p'){
        j<-22
      }
    }
    if(input$x==q1[6]){
      i<-6
      if(input$y=='q'){
        j<-23
      }
      if(input$y=='p'){
        j<-24
      }
    }
    if(input$x== q1[7]){
      i<-7
      if(input$y=='q'){
        j<-25
      }
      if(input$y=='p'){
        j<-26
      }
    }
    if(input$x==q1[8]){
      i<-8
      if(input$y=='q'){
        j<-27
      }
      if(input$y=='p'){
        j<-28
      }
    }
    if(input$x==q1[9]){
      i<-9
      if(input$y=='q'){
        j<-29
      }
      if(input$y=='p'){
        j<-30
      }
    }
    if(input$x== q1[10]){
      i<-10
      if(input$y=='q'){
        j<-31
      }
      if(input$y=='p'){
        j<-32
      }
    }
    if(input$x==q1[11]){
      i<-11
      if(input$y=='q'){
        j<-33
      }
      if(input$y=='p'){
        j<-34
      }
    }
    if(input$x==q1[12]){
      i<-12
      if(input$y=='q'){
        j<-35
      }
      if(input$y=='p'){
        j<-36
      }
    }
    Variable<- teras32254[,i]
    CI<- teras32254[,j]
    data=cbind(teras32254[,i],teras32254[,j])
    data=subset(data,CI==1)
    data=unique(data)
    family <- as.factor(teras32254[,j])
    plot(Variable,pch=19,cex=0.8,CI,col=family)
    output$table <- renderDT({
      datatable(data, options = list(pageLength = 10)) %>%
        formatStyle("V1", backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("V2", backgroundColor = '		#F5F5F5', fontWeight = 'bold')
      
    })
    
    output$table1 = renderDT({
      datatable(s1,options = list(scrollX =TRUE, rownames = FALSE,pageLength = 10)) %>%
        formatStyle("IDF.A.OUTLET.TEMP",backgroundColor='#F5F5F5', fontWeight = 'bold')%>%
        formatStyle("IDF.A.BRG.TEMP.2",backgroundColor='#F5F5F5', fontWeight = 'bold')%>%
        formatStyle("IDF.A.MOT.COIL.PH.A.TEMP.1",backgroundColor ='#F5F5F5',fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.B.TEMP.1",backgroundColor ='#F5F5F5',fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.C.TEMP.1",backgroundColor ='#F5F5F5',fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.DE.BRG.TEMP",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.BRG.TEMP.1",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.A.TEMP.2",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.B.TEMP.2",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.C.TEMP.2",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.NDE.BRG.TEMP",backgroundColor = '#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.HYD.CPL.IN.OIL.TEMP",backgroundColor = '#F5F5F5', fontWeight = 'bold')
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)