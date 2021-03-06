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


######################################## ui panel 

ui <- navbarPage("Shiny app",tabPanel(" Plot Clothing",
                                      fluidPage(
                                        sidebarLayout(sidebarPanel(
                                          selectInput("select", label = h3("Plot by type of card"), 
                                                      choices = list_choices,
                                                      selected = 1)
                                        ), 
                                        mainPanel(
                                          plotOutput(outputId = "button", click = "plot_click"),
                                          tableOutput("info")
                                        )
                                        ))),
                 tabPanel("Summary Data",
                          fluidPage(
                            sidebarLayout(
                              selectInput(inputId = "dataset",
                                          label = "Choose a variable:",
                                          choices = c("Amount", "Recency","Freq12","Dollar12","Freq24","Dollar24")),
                              numericInput(inputId = "obs",
                                           label = "Number of observations to view:",
                                           value = 10)
                              ),
                            mainPanel(
                              
                              # Output: Verbatim text for data summary ----
                              verbatimTextOutput("summary"),
                              
                              # Output: HTML table with requested number of observations ----
                              tableOutput("view"))
                              
                              
                            )
                          ))
                 
                 

########################################

col_scale <- scale_colour_discrete(limits = list_choices)


########################################


server <- function(input, output) {
  output$button <- renderPlot({
    ggplot(Clothing %>% filter(Card == input$select)
           , aes(Dollar12,Dollar24, colour = Card)) +
      scale_x_log10() +
      col_scale +
      theme_bw()+
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
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Amount" = Clothing$Amount,
           "Recency" = Clothing$Recency,
           "Freq12"=Clothing$Freq12,
           "Dollar12"=Clothing$Dollar12,
           "Freq24"=Clothing$Freq24,
           "Dollar24"=Clothing$Dollar24)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
}








################################# Run the application 
shinyApp(ui = ui, server = server)