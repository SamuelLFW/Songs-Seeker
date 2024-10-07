

library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)


df_spotify = read_csv( 'spotify_top_200_sample.csv')


type_choices = c('Popularity', 
                 'Danceability', 
                 'Energy', 'Loudness', 'Speechiness',
                 'Acousticness','Liveness', 'Highest_Charting_Position',
                 'Number_of_Times_Charted', "Valence")


# UI Design ---------------------------------------------------------------

ui = navbarPage(
  #  theme = bs_theme(bootswatch = "superhero"),
  title = "LFW",
  # Sidebar with a slider input for number of bins 
  tabPanel( 
    title = 'Input / Visualization',
    titlePanel(title = "Top 200 music in Spotify"),  
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "type", 
                    label = "Measure Type", 
                    choices = type_choices, 
                    selected = 'Popularity'),
        selectInput(inputId = "Genre",
                    label = "Genre", 
                    choices = sort( unique( df_spotify$Genre_1 ) ),
                    selected = "pop"), 
        selectInput(inputId = "Singer",
                    label = "Singer: ",
                    choices = sort (unique( df_spotify$Artist ) ), 
                    selected = "Taylor Swift"), 
        
      ),
      mainPanel(plotOutput("popular_pop_song") )
    )
    
    
  ),
  
  tabPanel(title = 'Table',
           dataTableOutput("table")
           
  ),
  tabPanel(title = 'About', 
           includeMarkdown("about.Rmd"))
  
)

# Server -------------------------------------------------------------------


server = function(input, output) {
  
  the_singer = reactive({
    df_spotify %>% 
      filter(Genre_1 == input$Genre)
    
    
  })
  
  observeEvent(eventExpr = input$Genre, 
               handlerExpr = {
                 updateSelectInput(
                   inputId = "Singer",
                   choices = sort (unique(the_singer()$Artist)),
                   selected = check_singer(the_singer()$Artist)
                 )
               })
  
  choiced_singer = reactive({
    the_singer() %>% 
      filter(Artist == input$Singer ) %>% 
      arrange(desc(!!as.symbol(input$type)))
    
  })
  
  output$popular_pop_song = renderPlot({
    ggplot( choiced_singer()  ) + 
      geom_col(mapping = aes(x=Song_Name, y= !!as.symbol(input$type) )) + 
      labs(title = "Check Songs and their Ranking!") + 
      theme(axis.text.x = element_text(face="bold", size=6, angle=45))
    
  })
  
  output$table = renderDataTable({
    
    choiced_singer() 
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




