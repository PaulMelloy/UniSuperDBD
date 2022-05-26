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
library(ggplot2)
lsf_dat <- data.table(age = 40:65,
                      lsf = seq(from = 18,
                                to = 23,
                                by = 0.2))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculating UniSuper Defined Benefit division"),
    a(href="https://paulmelloy.github.io/",
      "This app was designed and coded by Dr Paul Melloy"),
    p(),
    a(href="https://github.com/PaulMelloy/UniSuperDBD",
      "Find the source code, make a pull request, or lodge an issue on GitHub PaulMelloy/UniSuperDBD"),
    h4("This is a guide only!! any users should talk to an independant financial advisor, not just a UniSuper rep."),
    p("By using this tool you recognise the author accepts no reponsibility in any financial actions you take or give to others."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("income",
                        "Five year average annual income (before tax):",
                        min = 10000,
                        max = 1000000,
                        value = 60000),
            numericInput("years",
                         "Years of service (year/s spent contributing to DBD):",
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
              from UniSuper or from your profile on the website."),
            numericInput("contrib",
                         "Total employer and employee percent of salary contributed to super",
                         min = 5,
                         max = 25,
                         value = 17),
            numericInput("acc_return",
                         "Average annual return on investment for an accumulation fund",
                         min = 0,
                         max = 100,
                         value = 2),
            p("This is assumed an average over the whole Years of service and"),
            a(href= "https://www.unisuper.com.au/investments/investment-performance",
              "can be estimated from this link")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("The Maturity of your DBD"),
           textOutput("maturity"),
           p(),
           h3("The approximate total contributions after tax"),
           textOutput("contributions"),
           p("Super contributions attract a 15% tax"),
           p("If greater than 5 years service, this app assumes your income was 2% lower for each year prior to the last five years according to average inflation"),
           p(),
           p(),
           h3("Percent return on investment"),
           textOutput("percent_return"),
           p(""),
           p(""),
           h3("Estimated maturity if accumulation was chosen"),
          textOutput("accumulation"),
          plotOutput("acc_plot"),
          p(""),
           p(""),
           p(""),
           p(""),
           p("")
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

   Maturity <-
      reactive({
         input$income *
            input$years *
            (input$fulltime / 100) *
            (lsf() / 100) *
            (input$acf / 100)})


   Contributions <- reactive({

      if(input$years > 5){
         income_last_5 <-
            input$income * # Five year annual income average
            5 * # last five years
            (input$fulltime/100)


         for(i in seq_len(input$years - 5)){
            if(i == 1){
               income_legacy <- 0
               salary_legacy <- input$income
            }

            salary_legacy <-
               salary_legacy *
               0.98 * # reduce according to average inflation
               (input$fulltime / 100)

            income_legacy <-
               income_legacy +
               salary_legacy
         }

         Income_adj <- income_last_5 + income_legacy

      }else{
         Income_adj <-
            input$income * # Five year annual income average
            input$years *
            (input$fulltime/100)
      }

      Income_adj *
         (input$contrib / 100) *
         0.85 # apply 15% tax

   })


   output$maturity <- reactive(format(Maturity(),
                                      big.mark = ",",
                                      scientific = FALSE))
   output$contributions <- reactive(format(Contributions(),big.mark = ",",
                                    scientific = FALSE))

   output$percent_return <- reactive({
      round((Maturity() - Contributions())/
         Contributions(),5)*100
   })

   Accumulation <- reactive({

      if(input$years > 5){
         contrib_last_5 <-
            rep(input$income * # Five year annual income average
            (input$fulltime/100)*
            (input$contrib / 100) *
            0.85, # apply 15% tax
            5)


         for(i in seq_len(input$years - 5)){
            if(i == 1){
               salary_legacy <- contrib_legacy <- vector(mode = "numeric", length = input$years - 5)
               salary_legacy[1] <- input$income
            }else{

            salary_legacy[i] <- salary_legacy[i] *
               0.98 * # reduce according to average inflation
               (input$fulltime/100)
            }

            contrib_legacy[i] <-
               salary_legacy *
               (input$contrib / 100) *
               0.85
         }

         contrib_adj_v <- c(rev(contrib_legacy),contrib_last_5)

         for(i in seq_along(contrib_adj_v)){
            if(i == 1){
               accu_return <- vector(mode = "numeric", length = input$years)
               accu_return[i] <- contrib_adj_v[i]
            }else{
               accu_return[i] <- contrib_adj_v[i-1] * (1 + (input$acc_return/100))
            }
         }

         accum_dat <- data.table(salary = c(rev(salary_legacy), rep(input$income,5)),
                                 contrib = c(contrib_adj_v),
                                 accumulation1 = accu_return)

      }else{
         contrib_last <-
            rep(input$income * # Five year annual income average
                   (input$fulltime/100)*
                   (input$contrib / 100) *
                   0.85, # apply 15% tax
                input$years)

         for(i in seq_along(contrib_last)){
            if(i == 1){
               accu_return <- vector(mode = "numeric", length = input$years)
               accu_return[i] <- contrib_last[i]
            }else{
               accu_return[i] <- contrib_last[i-1] * (1 + (input$acc_return/100))
            }
         }

         accum_dat <- data.table(salary = rep(input$income,input$years),
                                 contrib = c(contrib_last),
                                 accumulation1 = accu_return)
      }

      return(accum_dat)

   })
   output$accumulation <- renderText(format(Accumulation()[.N,accumulation1],big.mark = ",",
                                            scientific = FALSE))

   output$acc_plot <- renderPlot({
      dat <- Accumulation()
      dat[, years := input$years]
      dat |>
         ggplot(aes(x = years, y = accumulation1))+
         geom_line()+
         theme_minimal()
   })

}

# Run the application
shinyApp(ui = ui, server = server)

