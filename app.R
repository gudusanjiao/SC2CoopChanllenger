#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


mutator.list <- read.delim("mutator.list", header = TRUE, stringsAsFactors = FALSE)
map.list <- read.delim("map.list", header = TRUE, stringsAsFactors = FALSE)
commander.list <- read.delim("commander.list", header = TRUE, stringsAsFactors = FALSE)


ui <- fluidPage(
   
   # Application title
   titlePanel("StarCraft2 Co-op Players VS Amon Ban-Pick Tool v0.0.3"),
   
   sidebarLayout(
      sidebarPanel(
         h4("1. Amon chooses one fixed mutator"),
         actionButton("Amon_mutator_fix", "Amon First Mutator"),
         h4("2. Players ban 4 mutators"),
         selectInput("ban1", "Choose First Banned Mutator",
                     choices = mutator.list$mutator),
         selectInput("ban2", "Choose Second Banned Mutator",
                     choices = mutator.list$mutator),
         selectInput("ban3", "Choose Third Banned Mutator",
                     choices = mutator.list$mutator),
         selectInput("ban4", "Choose Fourth Banned Mutator",
                     choices = mutator.list$mutator),
         sliderInput("Amon_mutator_num", "Number of Additional Mutators:",
                     min = 2, max = 4, value = 2, step = 1
         ),
         sliderInput("Amon_mutator_max", "Maximum of Additional Difficulties:",
                     min = 5, max = 25, value = 10, step = 1
         ),
         actionButton("players_ban_mutators", "Ban Mutators/Amon chooses two"),
         h4("3. Player ban 2 maps"),
         selectInput("ban5", "Choose First Banned Map",
                     choices = map.list$map),
         selectInput("ban6", "Choose Second Banned Map",
                     choices = map.list$map),
         actionButton("select_map", "Random Map Candidates"),
         h4("4. Amon ban N commanders"),
         sliderInput("ban_commanders_num", "Number of Banned Commanders:",
                     min = 2, max = 6, value = 2, step = 1
         ),
         actionButton("ban_commanders", "Amon Ban Commanders")
      ),
      
      mainPanel(
         h3("Amon First Picked: "),
         textOutput("Amon_first"),
         h3("Players Banned: "),
         textOutput("players_banned_mutator"),
         h3("Amon Picked Mutators: "),
         textOutput("Amon_second"),
         h3("Players Banned Maps: "),
         textOutput("players_banned_map"),
         h3("Amon Picked Maps: "),
         textOutput("Amon_third"),
         h3("Amon Banned Commanders: "),
         textOutput("Amon_fourth")
      )
   )
)

# Define server logic 
server <- function(input, output) {
   # Amon fix one mutator
   Amon_first_mutator <- eventReactive(input$Amon_mutator_fix, {
     sample(mutator.list$mutator, 1)
     
   })
   output$Amon_first <- renderText({ Amon_first_mutator() })
   
   # players ban 4 mutators/Amon select rest 2
   mutation_ban_list <- eventReactive(input$players_ban_mutators, {
     c(input$ban1, input$ban2, input$ban3, input$ban4)
   })
   mutation_pick <- eventReactive(input$players_ban_mutators, {
     x <- sample(mutator.list$mutator[!mutator.list$mutator %in% c(input$ban1, input$ban2, input$ban3, input$ban4)], input$Amon_mutator_num)
     while (sum(mutator.list$point[match(x, mutator.list$mutator)]) > input$Amon_mutator_max ){
       x <- sample(mutator.list$mutator[!mutator.list$mutator %in% c(input$ban1, input$ban2, input$ban3, input$ban4)], input$Amon_mutator_num)
     }
     mutator.list$mutator[match(x, mutator.list$mutator)]
   })
   output$players_banned_mutator <- renderText({ mutation_ban_list() })
   output$Amon_second <- renderText({ mutation_pick() })
   
   # players ban 2 maps/Amon select 2 candidate
   map_ban_list <- eventReactive(input$select_map, {
     c(input$ban5, input$ban6)
   })
   map_pick <- eventReactive(input$select_map, {
     sample(map.list$map[!map.list$map %in% c(input$ban5, input$ban6)], 2)
   })
   output$players_banned_map <- renderText({ map_ban_list() })
   output$Amon_third <- renderText({ map_pick() })
   
   # Amon ban N commanders
   Amon_ban_commander <- eventReactive(input$ban_commanders, {
     sample(commander.list$commander, as.numeric(input$ban_commanders_num))
   })
   output$Amon_fourth<- renderText({ Amon_ban_commander() })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

