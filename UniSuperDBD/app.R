#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
lsf_dat <- data.table(age = 40:65,
                      lsf = seq(from = 18,
                                to = 23,
                                by = 0.2))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculating UniSuper Defined Benefit division"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("income",
                        "Five year average annual income (before tax):",
                        min = 10000,
                        max = 1000000,
                        value = 60000),
            numericInput("years",
                         "Years of service (year spent contributing to DBD):",
                         min = 1,
                         max = 100,
                         value = 5),
            numericInput("fulltime",
                         "Percentage of service working full-time",
                         min = 1,
                         max = 100,
                         value = 100),
            numericInput("age",
                         "Age in Years:",
                         min = 1,
                         max = 100,
                         value = 35),
            p("This determines your lump sum factor."),
            numericInput("acf",
                         "Average contribution factor/percentage:",
                         min = 75,
                         max = 100,
                         value = 74.5),
            p("Average contribution factor is dependent on what percentage of your
              salary is committed to your super, you will need to get this number
              from UniSuper or from your profile on the website.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Insert plot here"),
           textOutput("maturity")
           #plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   lsf <- reactive({
      if(input$age < 40){
         return(18)
      }else{
      if(input$age > 65){
         return(23)
      }else{
         return(lsf_dat[age == input$age,lsf])
      }
      }
      return(NA)
      })

   output$maturity <- reactive(input$income * input$years * (input$fulltime/100) * (lsf()/100) * (input$acf/100))
}

# Run the application
shinyApp(ui = ui, server = server)

