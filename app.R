## This is my shiny app application

library(shiny)
library(tidyverse)
library(shinyjs)
library(Stat2Data)



######################################## DATA PREPARATION
data(Clothing)
Clothing <- Clothing[-60,] # Missing Value
Clothing$Card <- as.factor(Clothing$Card) # Categorical variable


card=matrix(0,nrow=nrow(Clothing),ncol=1)
for (i in 1:nrow(card)){
  if(Clothing[i,8]==1){
    card[i]="Private label"
  }
  if(Clothing[i,8]==0){
    card[i]="Public label"
  }
}

Clothing$Card <- as.factor(card)

########################################


list_choices <-  unique(Clothing$Card)


########################################

ui <- navbarPage("Shiny app",tabPanel("Clothing Summary",
                                      fluidPage(
                                        sidebarLayout(sidebarPanel(
                                          selectInput("select", label = h3("Plot by type of card"), 
                                                      choices = list_choices,
                                                      selected = 1)
                                        ), 
                                        mainPanel(
                                          plotOutput(outputId = "hello", click = "plot_click"),
                                          tableOutput("info")
                                        )
                                        )))
)

########################################

col_scale <- scale_colour_discrete(limits = list_choices)


########################################


server <- function(input, output) {
  output$hello <- renderPlot({
    ggplot(Clothing %>% filter(Card == input$select)
           , aes(Dollar12,Dollar24, colour = Card)) +
      scale_x_log10() +
      col_scale +
      geom_point()
  })
  output$info <- renderTable({
    nearPoints(Clothing
               %>% filter(Card == input$select) 
               %>% select(Card, Dollar12,  Dollar24, Amount, Recency), 
               input$plot_click, threshold = 10, maxpoints = 1,
               addDist = TRUE)
  })
  samples <- reactive({
    input$goButton;
    dist <- eval(parse(text=paste(input$dist)))
    dist(isolate(input$n_sample))
  });
}









################################# Run the application 
shinyApp(ui = ui, server = server)