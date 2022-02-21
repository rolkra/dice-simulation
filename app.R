library(tidyverse)
library(tidydice)
library(shiny)
library(colourpicker)
library(shinybusy)

# Helper variables ----

colors <- c(c("White" = "white",
              "Black" = "black",
              "Grey" = "grey"))

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Dice Simulation"),
  use_busy_spinner(spin = "fading-circle"),
  
#  mainPanel(
    tabsetPanel(
      
      tabPanel("Design",
              
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            
            colourInput("color_fill", "Fill:", value = "grey"),
            colourInput("color_success", "Fill Success:", value = "#123456"),
            colourInput("color_point", "Points:", value = "white"),
            selectInput("color_line", "Line:", colors, selected = "white"),
            
            sliderInput("cheat", "Cheat?", min = 0, max = 10, value = 0)
            
            #sliderInput("line_size", "Line Size:", min = 0, max = 5, value = 1)
         ),
                 
         mainPanel(
           plotOutput("designPlot")
         )
      ) # sidebarLayout
    ), # tabsetPanel Design
      
    tabPanel("Roll", 
            plotOutput("rollPlot")
    ),

    tabPanel("Check",
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          sliderInput("six_check", "Check success:", min = 10, max = 24, value = 12),
        ),
        mainPanel(
          plotOutput("checkPlot")
        )
      )
    )
    
    #,tabPanel("Help", tableOutput("help"))
      
  ) # tabsetPanel

) # fluidPage

# Define server logic ----
server <- function(input, output, session) {
  
  output$designPlot <- renderPlot({

    data <- force_dice(1) %>% 
      force_dice(2) %>% 
      force_dice(3) %>% 
      force_dice(4) %>% 
      force_dice(5) %>% 
      force_dice(6)
    
   data %>% 
      plot_dice(
        fill = input$color_fill,
        fill_success = input$color_success,
        line_color = input$color_line,
        point_color = input$color_point
      )
    
  })
  
  output$rollPlot <- renderPlot({

    #show_modal_spinner(text = "Rolling 60 dice...")
    show_spinner()
        
    cheat <- input$cheat

    if (input$cheat > 0) {
      cheat <- input$cheat
      prob_six <- 1/6 + 1/6*cheat/10
      prob <- c(rep((1 - prob_six)/5, 5), prob_six)
      data <- roll_dice(times = 6, rounds = 10, prob = prob)
    } else {
      data <- roll_dice(times = 6, rounds = 10)
    }
    
    roll_cnt <- nrow(data)
    six_cnt <- sum(data$success)
    
    p <- data %>%  
      plot_dice(
        fill = input$color_fill,
        fill_success = input$color_success,
        line_color = input$color_line,
        point_color = input$color_point
        #line_size = input$line_size
      ) +
      ggtitle(paste("Success:", six_cnt, "of", roll_cnt))
    
    #remove_modal_spinner()
    hide_spinner()
    
    p
    
  })

  observeEvent(input$button_roll, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Roll It!')
  })  
  
  output$checkPlot <- renderPlot({
    
    color_success <- "red"
    if (input$color_success != "grey") {
      color_success <- input$color_success
    }
    
    binom_dice(times = 60) %>% 
    plot_binom(
      highlight = seq(input$six_check, 60),
      title = "Binomial Distribution of 10 x 6 fair dice",
      color_highlight = color_success)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)