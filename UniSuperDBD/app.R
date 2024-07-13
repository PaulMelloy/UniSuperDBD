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
source("R/calc_lsf.R")
source("R/calc_abm.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculating UniSuper Defined Benefit division"),
    h2("UniSuper DBD"),

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
                         "Age at maturity in Years:",
                         min = 1,
                         max = 100,
                         value = 35),
            p("This determines your lump sum factor."),
            numericInput("acf",
                         "Average contribution factor/percentage:",
                         min = 75,
                         max = 100,
                         value = 75.4),
            p("Average contribution factor is dependent on what percentage of your
              salary is committed to your super, you will need to get this number
              from UniSuper or from your online account profile."),
            a(href= "https://www.unisuper.com.au/-/media/files/pds/dbd-and-accumulation-2/pds.pdf",
              "You can also see the range of contributions factors on page 54 of this booklet"),
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
              "can be estimated from this link"),
            numericInput(inputId = "PSS_con",
                         label = "PSS annual benefit",
                         min = 0,
                         max = 10,
                         value = 5)
            # p("This is assumed an average over the whole Years of service and"),
            # a(href= "https://www.unisuper.com.au/investments/investment-performance",
            #   "can be estimated from this link")
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
           h3("DBD Percent return on investment"),
           textOutput("percent_return"),
           p(""),
           p(""),
           h3("Estimated maturity if accumulation was chosen"),
          textOutput("accumulation"),
          plotOutput("acc_plot"),
          p("The blue line is the growth of the Defined benefit division;
            The black line is the growth of an estimated Accumulation super product"),
           p(""),
           p(""),
           p(""),
           p("This tool assumes fees and insurance between the DBD and Accumulation products would be the same over time")
           #plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   Maturity <-
      reactive({
         input$income *
            input$years *
            (input$fulltime / 100) *
            (calc_lsf(input$age) / 100) *
            (input$acf / 100)})


    Salary <- reactive({

      if(input$years > 5){
         income_last_5 <-
            rep(input$income,5)  # Five year annual income average


         for(i in seq_len(input$years - 5)){
            if(i == 1){
               salary_legacy <- income_legacy <-
                  vector(mode = "numeric", length = input$years - 5)
               salary_legacy[i] <-
                  input$income * 0.98 # reduce according to average inflation


            }else{

            salary_legacy[i] <-
               salary_legacy[i-1] *
               0.98 # reduce according to average inflation

}
         }

         Income_adj <- c(rev(salary_legacy),income_last_5)

      }else{

         Income_adj <-
            rep(input$income * # Five year annual income average
                   (input$fulltime / 100),
                input$years)
      }

      Income_adj

   })

   Contributions <-
      reactive({
         Salary() *
            (input$fulltime/100)*
            (input$contrib / 100) *
            0.85 # apply 15% tax
      })


   output$maturity <- reactive(format(Maturity(),
                                      big.mark = ",",
                                      scientific = FALSE))
   output$contributions <- reactive(format(sum(Contributions()),big.mark = ",",
                                    scientific = FALSE))

   output$percent_return <- reactive({
      round((Maturity() - sum(Contributions()))/
         sum(Contributions()),5)*100
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
               salary_legacy[1] <- input$income *
                  0.98 * # reduce according to average inflation
                  (input$fulltime/100)
            }else{

            salary_legacy[i] <- salary_legacy[i-1] *
               0.98 * # reduce according to average inflation
               (input$fulltime/100)
            }

            contrib_legacy[i] <-
               salary_legacy[i] *
               (input$contrib / 100) *
               0.85
         }

         contrib_adj_v <- c(rev(contrib_legacy),contrib_last_5)

         for(i in seq_along(contrib_adj_v)){
            if(i == 1){
               accu_return <- vector(mode = "numeric", length = input$years)
               accu_return[i] <- contrib_adj_v[i]
            }else{
               accu_return[i] <- (accu_return[i-1] * (1 + (input$acc_return/100))) +
                  contrib_adj_v[i]
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
               accu_return[i] <- (accu_return[i-1] * (1 + (input$acc_return/100)))+
                  contrib_last[i]
            }
         }

         accum_dat <- data.table(salary = rep(input$income,input$years),
                                 contrib = c(contrib_last),
                                 accumulation1 = accu_return)
      }

      return(accum_dat)

   })

   pss_dat <- reactive({
      pss <-
         data.table(
            years = 1:input$years,
            income = input$income,
            abm = input$PSS_con/100)

      pss$abm <- sapply(1:nrow(pss),function(x){
         calc_abm(pss$abm[x],ten_year = x>9)
         })
      pss[, acc_abm := cumsum(abm)]
      pss[, benefit := acc_abm * income]
      return(pss)
   })

   output$accumulation <- renderText(format(Accumulation()[.N,accumulation1],big.mark = ",",
                                            scientific = FALSE))

   output$acc_plot <- renderPlot({
      dat <- Accumulation()
      dat[, c("age","years") := list((input$age - input$years+1):input$age,
                                     1:input$years)]
      dat[, DBD :=
             .(salary *
             years *
             (calc_lsf(age)/100)*
             (input$acf/100) *
             (input$fulltime/100))
             ]
      dat[ ,pss := pss_dat()$benefit]

      dat |>
         ggplot(aes(x = years, y = accumulation1/1000))+
         geom_line(size = 1)+
         geom_line(aes(x = years, y = DBD/1000), colour = "blue", size =1)+
         geom_line(aes(x = years, y = pss/1000), colour = "cyan", size =1)+
         theme_minimal()+
         ylab("Account value in $1,000")+
         ggtitle("Comparison of return over time for Accumulation and DBD products")
   })





}

# Run the application
shinyApp(ui = ui, server = server)

