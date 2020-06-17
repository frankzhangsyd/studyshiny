library(here)
library(shiny)
library(data.table)
library(ggplot2)
library(vroom)

injuries <- vroom(here('neiss/injuries.tsv.gz'))
population <- vroom(here('neiss/population.tsv'))
products <- vroom(here('neiss/products.tsv'))

lapply(list(injuries,population,products),setDT)

ui <- fluidPage(
    fluidRow(
        column(6,
               selectInput("code",label ="Produce", setNames(products$prod_code,products$title)))
    ),
    fluidRow(
        column(4,tableOutput("diag")),
        column(4,tableOutput("body_part")),
        column(4,tableOutput("location"))
    ),
    fluidRow(
        column(12,plotOutput("age_sex"))
    )
)

server <- function(input, output) {
    selected <- reactive(injuries[prod_code == input$code])
    
    output$diag <- renderTable(
        selected()[,.(n=sum(weight)),by=.(age,sex)][order(-n)]
    )
    output$body_part <- renderTable(
        selected()[,.(n=sum(weight)),by=.(body_part)][order(-n)]
    )
    output$location <- renderTable(
        selected()[,.(n=sum(weight)),by=.(location)][order(-n)]
    )
    
    summary <- reactive({
        population[selected()[,.(n=sum(weight)),by=.(age,sex)][order(-n)],
                   on=.(age,sex)][
                       c(.SD,rate = n/population*10000)
                   ]
    })
    
    output$age_sex <- renderPlot({
        summary() %>% 
            ggplot(aes(x = age,y = n, colour = sex))+
            geom_line()+
            labs(y="Estimated Injutied")
    },res = 96)
}

shinyApp(ui = ui, server = server)


