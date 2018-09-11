
teras32254= read.csv("newci1.csv",header = TRUE)

s1=teras32254[,1:12]
q1=names(teras32254[,1:12])
library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                
                
                # Application title
                titlePanel("IDA Fan Data points"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "vars", 
                                label = "Select Dataset", 
                                choices = c("teras32254")),
                    selectInput("x", "Select Variable",q1),
                    sliderInput("y",
                                label="Confidence Interval",
                                min=90,max=99,value=500),
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
                    plotOutput("distPlot",width="100%"),
                    br(),
                    column(10,align="center",
                           DTOutput(outputId = "table",width="35%"))
                    
                    
                  )
                ))

# Define server logic requi#FF6347 to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$x==q1[1]){
      i<-1
      if(input$y=='90'){
        j<-13
      }
      if(input$y=='91'){
        j<-14
      }
      if(input$y=='92'){
        j<-15
      }
      if(input$y=='93'){
        j<-16
      }
      if(input$y=='94'){
        j<-17
      }
      if(input$y=='95'){
        j<-18
      }
      if(input$y=='96'){
        j<-19
      }
      if(input$y=='97'){
        j<-20
      }
      if(input$y=='98'){
        j<-21
      }
      if(input$y=='99'){
        j<-22
      }
      
    }
    if(input$x==q1[2]){
      i<-2
      if(input$y=='90'){
        j<-23
      }
      if(input$y=='91'){
        j<-24
      }
      if(input$y=='92'){
        j<-25
      }
      if(input$y=='93'){
        j<-26
      }
      if(input$y=='94'){
        j<-27
      }
      if(input$y=='95'){
        j<-28
      }
      if(input$y=='96'){
        j<-29
      }
      if(input$y=='97'){
        j<-30
      }
      if(input$y=='98'){
        j<-31
      }
      if(input$y=='99'){
        j<-32
      }
    }
    if(input$x==q1[3]){
      i<-3
      if(input$y=='90'){
        j<-33
      }
      if(input$y=='91'){
        j<-34
      }
      if(input$y=='92'){
        j<-35
      }
      if(input$y=='93'){
        j<-36
      }
      if(input$y=='94'){
        j<-37
      }
      if(input$y=='95'){
        j<-38
      }
      if(input$y=='96'){
        j<-39
      }
      if(input$y=='97'){
        j<-40
      }
      if(input$y=='98'){
        j<-41
      }
      if(input$y=='99'){
        j<-42
      }
    }
    if(input$x==q1[4]){
      i<-4
      if(input$y=='90'){
        j<-43
      }
      if(input$y=='91'){
        j<-44
      }
      if(input$y=='92'){
        j<-45
      }
      if(input$y=='93'){
        j<-46
      }
      if(input$y=='94'){
        j<-47
      }
      if(input$y=='95'){
        j<-48
      }
      if(input$y=='96'){
        j<-49
      }
      if(input$y=='97'){
        j<-50
      }
      if(input$y=='98'){
        j<-51
      }
      if(input$y=='99'){
        j<-52
      }
    }
    if(input$x==q1[5]){
      i<-5
      if(input$y=='90'){
        j<-53
      }
      if(input$y=='91'){
        j<-54
      }
      if(input$y=='92'){
        j<-55
      }
      if(input$y=='93'){
        j<-56
      }
      if(input$y=='94'){
        j<-57
      }
      if(input$y=='95'){
        j<-58
      }
      if(input$y=='96'){
        j<-59
      }
      if(input$y=='97'){
        j<-60
      }
      if(input$y=='98'){
        j<-61
      }
      if(input$y=='99'){
        j<-62
      }
    }
    if(input$x==q1[6]){
      i<-6
      if(input$y=='90'){
        j<-63
      }
      if(input$y=='91'){
        j<-64
      }
      if(input$y=='92'){
        j<-65
      }
      if(input$y=='93'){
        j<-66
      }
      if(input$y=='94'){
        j<-67
      }
      if(input$y=='95'){
        j<-68
      }
      if(input$y=='96'){
        j<-69
      }
      if(input$y=='97'){
        j<-70
      }
      if(input$y=='98'){
        j<-71
      }
      if(input$y=='99'){
        j<-72
      }
    }
    if(input$x== q1[7]){
      i<-7
      if(input$y=='90'){
        j<-73
      }
      if(input$y=='91'){
        j<-74
      }
      if(input$y=='92'){
        j<-75
      }
      if(input$y=='93'){
        j<-76
      }
      if(input$y=='94'){
        j<-77
      }
      if(input$y=='95'){
        j<-78
      }
      if(input$y=='96'){
        j<-79
      }
      if(input$y=='97'){
        j<-80
      }
      if(input$y=='98'){
        j<-81
      }
      if(input$y=='99'){
        j<-82
      }
    }
    if(input$x==q1[8]){
      i<-8
      if(input$y=='90'){
        j<-83
      }
      if(input$y=='91'){
        j<-84
      }
      if(input$y=='92'){
        j<-85
      }
      if(input$y=='93'){
        j<-86
      }
      if(input$y=='94'){
        j<-87
      }
      if(input$y=='95'){
        j<-88
      }
      if(input$y=='96'){
        j<-89
      }
      if(input$y=='97'){
        j<-90
      }
      if(input$y=='98'){
        j<-91
      }
      if(input$y=='99'){
        j<-92
      }
    }
    if(input$x==q1[9]){
      i<-9
      if(input$y=='90'){
        j<-93
      }
      if(input$y=='91'){
        j<-94
      }
      if(input$y=='92'){
        j<-95
      }
      if(input$y=='93'){
        j<-96
      }
      if(input$y=='94'){
        j<-97
      }
      if(input$y=='95'){
        j<-98
      }
      if(input$y=='96'){
        j<-99
      }
      if(input$y=='97'){
        j<-100
      }
      if(input$y=='98'){
        j<-101
      }
      if(input$y=='99'){
        j<-102
      }
    }
    if(input$x== q1[10]){
      i<-10
      if(input$y=='90'){
        j<-103
      }
      if(input$y=='91'){
        j<-104
      }
      if(input$y=='92'){
        j<-105
      }
      if(input$y=='93'){
        j<-106
      }
      if(input$y=='94'){
        j<-107
      }
      if(input$y=='95'){
        j<-108
      }
      if(input$y=='96'){
        j<-109
      }
      if(input$y=='97'){
        j<-110
      }
      if(input$y=='98'){
        j<-111
      }
      if(input$y=='99'){
        j<-112
      }
    }
    if(input$x==q1[11]){
      i<-11
      if(input$y=='90'){
        j<-113
      }
      if(input$y=='91'){
        j<-114
      }
      if(input$y=='92'){
        j<-115
      }
      if(input$y=='93'){
        j<-116
      }
      if(input$y=='94'){
        j<-117
      }
      if(input$y=='95'){
        j<-118
      }
      if(input$y=='96'){
        j<-119
      }
      if(input$y=='97'){
        j<-120
      }
      if(input$y=='98'){
        j<-121
      }
      if(input$y=='99'){
        j<-122
      }
    }
    if(input$x==q1[12]){
      i<-12
      if(input$y=='90'){
        j<-123
      }
      if(input$y=='91'){
        j<-124
      }
      if(input$y=='92'){
        j<-125
      }
      if(input$y=='93'){
        j<-126
      }
      if(input$y=='94'){
        j<-127
      }
      if(input$y=='95'){
        j<-128
      }
      if(input$y=='96'){
        j<-129
      }
      if(input$y=='97'){
        j<-130
      }
      if(input$y=='98'){
        j<-131
      }
      if(input$y=='99'){
        j<-132
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
        formatStyle("IDF.A.OUTLET.TEMP", backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.BRG.TEMP.2",  backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.A.TEMP.1", backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.B.TEMP.1",  backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.C.TEMP.1",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.DE.BRG.TEMP",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.BRG.TEMP.1",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.A.TEMP.2",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.B.TEMP.2",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.COIL.PH.C.TEMP.2",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.MOT.NDE.BRG.TEMP",   backgroundColor = '		#F5F5F5', fontWeight = 'bold') %>%
        formatStyle("IDF.A.HYD.CPL.IN.OIL.TEMP",   backgroundColor = '		#F5F5F5', fontWeight = 'bold')
      
      
    })
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

