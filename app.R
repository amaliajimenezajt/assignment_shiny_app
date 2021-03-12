## This is my shiny app application, click on Run App to show 
library(shiny)
library(tidyverse)
library(shinyjs)
library(Stat2Data)
library(ggplot2)
library(shinythemes)


######################################## DATA PREPARATION
data(Clothing)
Clothing <- Clothing[-60,-1] # Missing Value
Clothing$Card <- as.factor(Clothing$Card) # Categorical variable
card=matrix(0,nrow=nrow(Clothing),ncol=1)
for (i in 1:nrow(card)){
  if(Clothing[i,7]==1){
    card[i]="Private label"
  }
  if(Clothing[i,7]==0){
    card[i]="Public label"
  }
}
Clothing$Card <- as.factor(card)


########################################
list_choices <-  unique(Clothing$Card)
variable_num_choices <- colnames(Clothing[,-7])
######################################## ui panel 

ui <- navbarPage("Shiny app",
                 theme = shinytheme("superhero"),
                 tabPanel("Summary Data",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel (h3(strong("Data Summary")),
                                            p("This shiny app is inspired in the HTML Presentation about the data set",
                                              code("Clothing"),"whish in the first Panel we are able to see a summary of each variable,
                                               in the second a Plot about the Numeric variable and finally a boxplot set."),
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
                              ))),
                 tabPanel(" Plot Clothing",
         fluidPage(
           sidebarLayout(sidebarPanel(
             selectInput("select", label = h3("Plot by type of credit-card:"), 
                         choices = list_choices,
                         selected = 1)
           ), 
           mainPanel(
             plotOutput(outputId = "ploty", click = "plot_click"),
             tableOutput("info")
           )
           ))),
         tabPanel("Box-Plot",
         fluidPage(
           sidebarLayout(sidebarPanel(
             selectInput("select2", label = h3("Plot by variable:"), 
                         choices = variable_num_choices,
                         selected = 1)
           ), 
           mainPanel(
             plotOutput(outputId = "box")
           )
           )))
)





########################################
col_scale <- scale_colour_discrete(limits = list_choices)
########################################
server <- function(input, output) {
  
  output$ploty <- renderPlot({
    ggplot(Clothing %>% filter(Card == input$select)
           , aes(Dollar12,Dollar24, colour = Card)) +
      ggtitle("Point plot my Shiny app")+
      scale_x_log10() +
      col_scale +
      theme_bw()+
      geom_point()
  })
  
  
  output$box <- renderPlot({
    ggplot(data=Clothing, aes(Clothing$Card, Clothing[,input$select2])) + 
      geom_boxplot(color="black",fill=c("cyan3",'darkgoldenrod1'),alpha=0.2,
                   notchwidth = 0.8,outlier.colour="red",outlier.fill="red",
                   outlier.size=3)+
      stat_summary(fun.y=mean, geom="point", shape=18,
                   size=3, color="red")+
      theme_bw() +
      scale_fill_manual(values=c('lightcyan1'))
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