library(shiny)
library(tidyverse)
library(forcats)
library(ggthemes)
library(DT)
library(stringr)

data <- read.csv("data.csv")

genres_list <- c("Action & Adventure", "Animation" , "Anime", "Biography", "Children", "Comedy",
                 "Crime", "Cult", "Documentary", "Drama", "Family", "Fantasy", "Food", "Game Show",
                 "History", "Home & Garden", "Horror", "LGBTQ", "Musical", "Mystery", "Reality",
                 "Romance", "Science-Fiction", "Sport", "Stand-up & Talk", "Thriller", "Travel")

shinyUI <- fluidPage(
  titlePanel(h4("Final Data Science Project: Find a Show")),
  sidebarLayout(
    sidebarPanel(
      selectInput("service", "Choose a service to look through (or all of them)",
                  choices = c("All Services", "Netflix", "Hulu", "Amazon Prime", "Disney+"),
                  selected = "All Services"),
      selectInput("genre", "Choose one or more genres", 
                  multiple = TRUE,
                  selected = "Crime",
                  choices= genres_list),
      sliderInput("rating", "IMdB Rating range",
                  min = 1, max = 10,
                  value = c(2,8)),
      selectInput("age", "Choose one or more Age ratings",
                  selected = "TV-14",
                  choices = unique(data$age), 
                  multiple = TRUE),
      sliderInput("year", "Year range",
                  min = 1901, max = 2020,
                  value = c(2000,2016), sep = "")
      
    ),
    mainPanel(
      h5("Stuck at home and having trouble finding a show to watch? This app will recommend 10 shows from the service you select
         based on what genres, imdb scores, age ratings, and years you input! Because 
         streaming platforms have such huge catalogs, once you input your 
         preferences the selector will pick 10 shows at random from that pool.
         Depending on how broad your selection pool is, you might never see the same
         set of shows twice!"),
      dataTableOutput("mytable")
      
    )
  )
)
  



server <- function(input, output) {
  # v<- reactiveValues(data = NULL)
  # observeEvent(input$genre, input$rating, input$age, input$year)
  
  
  output$mytable <- renderDataTable({
    if(identical(input$service, "All Services")) {
      data[which(str_detect(data$genres, input$genre) == TRUE),] %>% 
        filter(between(imdb, input$rating[1], input$rating[2]) & 
                 between(year, input$year[1], input$year[2]) & 
                 age == input$age) %>% 
        sample_n(10)
    } else if(identical(input$service, "Netflix")) {
      data[which(str_detect(data$genres, input$genre) == TRUE),] %>% 
        filter(between(imdb, input$rating[1], input$rating[2]) & 
                 between(year, input$year[1], input$year[2]) & 
                 age == input$age &
                 netflix == 1) %>% 
        sample_n(10)
    } else if(identical(input$service, "Hulu")) {
      data[which(str_detect(data$genres, input$genre) == TRUE),] %>% 
        filter(between(imdb, input$rating[1], input$rating[2]) & 
                 between(year, input$year[1], input$year[2]) & 
                 age == input$age &
                 hulu == 1) %>% 
        sample_n(10)
    } else if(identical(input$service, "Amazon Prime")) {
      data[which(str_detect(data$genres, input$genre) == TRUE),] %>% 
        filter(between(imdb, input$rating[1], input$rating[2]) & 
                 between(year, input$year[1], input$year[2]) & 
                 age == input$age &
                 prime == 1) %>% 
        sample_n(10)
    } else if(identical(input$service, "Disney+")) {
      data[which(str_detect(data$genres, input$genre) == TRUE),] %>% 
        filter(between(imdb, input$rating[1], input$rating[2]) & 
                 between(year, input$year[1], input$year[2]) & 
                 age == input$age &
                 disney == 1) %>% 
        sample_n(10)
    }
  })

}


shinyApp(ui = shinyUI, server = server)
