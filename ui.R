# Load Shiny library
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Who is the best at flipping coins?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("n_players",
                        "Number of players on each team",
                        min = 0,
                        max = 100,
                        value = 10,
                        step = 1),
            
            sliderInput("n_flips_big",
                        "Number of flips per player for Team Big",
                        min = 0,
                        max = 100,
                        value = 100,
                        step = 5),
            
            sliderInput("n_flips_small",
                        "Number of flips per player for Team Small",
                        min = 0,
                        max = 50,
                        value = 10,
                        step = 5),
            
            sliderInput("prob_heads",
                        "Probability of flipping a heads",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.01),
            
            actionButton("run", "Let it flip!"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Results",
                                 textOutput("game_results"),
                                 hr(),
                                 tableOutput("summary_table"),
                                 fluidRow(
                                     column(6,
                                            h4("Team Big Results"),
                                            tableOutput("big_row_sum")
                                     ),
                                     column(6,
                                            h4("Team Small Results"),
                                            tableOutput("small_row_sum")
                                     )
                                 ),
                                 hr(),
                                 h2("Instructions"),
                                 p("The goal of the game is to flip more heads
                                   than the other team. Team big has more flips 
                                   per player so it won't be fair to strictly
                                   count the total number of heads per player. 
                                   Instead, we're going to look at the highest 
                                   proportion of heads flipped per player. The
                                   team with the player who has the greatest 
                                   proportion wins."),
                                 p(strong("Which team do you team will win more
                                          often?")),
                                 p("Hit the 'Let it flip!' button to flip the 
                                   coins and see who wins. You can also run a 
                                   simulation in the 'Head-to-Head' tab 
                                   (pun intended) to see who wins more often
                                   in the long run.")
                                 ),
                        tabPanel("Plot", 
                                 plotOutput("distributions")
                        ),
                        tabPanel("Raw Flips",
                                 h1("Team Big Flips"),
                                 tableOutput("big_raw"),
                                 h1("Team Small Flips"),
                                 tableOutput("small_raw")
                        ),
                        tabPanel("Head-to-Head Simulation",
                                 actionButton("sim", "Run the simulation"),
                                 sliderInput("replications",
                                             "Number of replications",
                                             min = 0,
                                             max = 500,
                                             value = 10,
                                             step = 10),
                                 textOutput("sim_results"),
                                 tableOutput("sim_table")
                        )
                        
            ) # Close tabset panel
        ) # Close main panel
    ) # Close sidebar panel
)) # Close fluid page & Shiny UI
