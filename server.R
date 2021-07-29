# Load Libraries
library(shiny)
library(tidyverse) #dplyr and ggplot would suffice

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Only flip the coins when the "Let if flip!" button is pressed
    big_sample <- eventReactive(input$run, {
        
        # Create an empty dataframe to be filled by the for loop
        df <- data.frame(
            matrix(
                data = NA, nrow = input$n_players, ncol = input$n_flips_big,
                dimnames = list(
                    paste("player_", 1:input$n_players, sep = ""),
                    paste("flip_", 1:input$n_flips_big, sep = "")
                    )
        ))
        
        # Flip coins for the big team
        for(i in 1:input$n_players){
            
            df[i, ] <- sample(
                x = c(1, 0), # Head == 1, Tails == 0
                size = input$n_flips_big,
                replace = TRUE,
                prob = c(input$prob_heads, 1 - input$prob_heads)
                )
        }
        
        df
    })
    
    # Only flip the coins when the "Let if flip!" button is pressed
    small_sample <- eventReactive(input$run, {
        
        # Create an empty dataframe to be filled by the for loop
        df <- data.frame(
            matrix(
                data = NA, nrow = input$n_players, ncol = input$n_flips_small,
                dimnames = list(
                    paste("player_", 1:input$n_players, sep = ""),
                    paste("flip_", 1:input$n_flips_small, sep = "")
                )
            ))
        
        # Flip coins for the small team
        for(i in 1:input$n_players){
            
            df[i, ] <- sample(
                x = c(1, 0), # Head == 1, Tails == 0
                size = input$n_flips_small,
                replace = TRUE,
                prob = c(input$prob_heads, 1 - input$prob_heads)
            )
        }
        
        df
    })
    
    # Create output table objects that will be displayed in the "Raw Flips" Tab
    output$big_raw <- renderTable(
        big_sample()
    )
    
    output$small_raw <- renderTable(
        small_sample()
    )
    
    # Summary Tables
    big_summary <- reactive({
        big_sample() %>%
            rowwise() %>% # Get stats for each player
            summarise(
                n_heads = sum(c_across()), 
                prop_heads = n_heads/input$n_flips_big)
    })
    
    small_summary <- reactive({
        small_sample() %>%
            rowwise() %>%
            summarise(
                n_heads = sum(c_across()),
                prop_heads = n_heads/input$n_flips_small)
    })
    
    game_summary <- reactive({
        rbind(
            big_summary() %>% summarise(
                team = "Big",
                flips_per_player = input$n_flips_big,
                max_n_heads = max(n_heads),
                max_prop_heads = max(prop_heads),
                total_prop = sum(n_heads) / (input$n_players * input$n_flips_big)
            ),
            small_summary() %>% summarise(
                team = "Small",
                flips_per_player = input$n_flips_small,
                max_n_heads = max(n_heads),
                max_prop_heads = max(prop_heads),
                total_prop = sum(n_heads) / (input$n_players * input$n_flips_small)
            )
        )
    })
    
    
    
    # Create output table objects that will be displayed in the "Results" Tab
    output$big_row_sum <- renderTable(
        big_summary()
    )
    
    output$small_row_sum <- renderTable(
        small_summary()
    )
    
    
    
    # Create a summary table that will be displayed in the "Results" Tab
    output$summary_table <- renderTable(
        game_summary()
    )
    
    # Create a reactive text object to display who wins.
    winner <- eventReactive(input$run, {
        
        if(game_summary()$max_prop_heads[1] >= game_summary()$max_prop_heads[2]){
            index <- 1 
        } else{
            index <- 2
        }
        
        paste(
            "Team",
            game_summary()$team[index],
            "won! Their best flipper flipped a head",
            (game_summary()$max_prop_heads[index])*100, 
            "% of the time."
        )
    })
    
    # Output simulation result sentence.
    output$game_results <- renderText(
        winner()
    )
    
    # Reactive dataframes with theoretical probabilities for each proportion for both teams
    prob_dist <- reactive({
        rbind(
            
            # Team Big
            data.frame(matrix(data = NA, nrow = input$n_flips_big + 1, ncol = 1)) %>%
                transmute(
                    team = "Big",
                    n_heads = 0:input$n_flips_big,
                    proportion = n_heads / input$n_flips_big,
                    probability = dbinom(
                        x = 0:input$n_flips_big,
                        size = input$n_flips_big,
                        prob = input$prob_heads
                        )
                ),
            # Team Small
            data.frame(matrix(data = NA, nrow = input$n_flips_small + 1, ncol = 1)) %>%
                transmute(
                    team = "Small",
                    n_heads = 0:input$n_flips_small,
                    proportion = n_heads / input$n_flips_small,
                    probability = dbinom(
                        x = 0:input$n_flips_small ,
                        size = input$n_flips_small,
                        prob = input$prob_heads
                        )
                )
        )
    })
    
    # Plot theoretical probabilities for both teams using ggplot
    output$distributions <- renderPlot({
        
        prob_dist() %>%
            ggplot(aes(x = proportion, y = probability, group = team, colour = team)) +
            geom_point(alpha = 0.8) +
            geom_vline(xintercept = input$prob_heads, linetype = "dashed", alpha = 0.2) +
            scale_x_continuous(labels = scales::percent) +
            scale_y_continuous(labels = scales::percent) +
            theme_classic() +
            labs(
                title = "Theoretical Probability of Observing a Proportion of Heads",
                x = "Proportion of Heads",
                y = " Probability"
            )

    })
    
    
    # Max Number of heads simulation
    # "Head-to-Head" tab
    
    simulation <- eventReactive(input$sim, { # Only run when the button is pressed
        
        # Empty dataframe to store results of simulation
        df <- data.frame(matrix(data = NA, nrow = input$replications, ncol = 2))
        
        for(i in 1:input$replications){
            
            # Empty dataframe for flips of Team Big
            df_big <- data.frame(
                matrix(
                    data = NA,
                    nrow = input$n_players,
                    ncol = input$n_flips_big
                    )
                )
            
            # Flip the coins for Team Big
            for(j in 1:input$n_players){
                df_big[j, ] <- sample(
                    x = c(1, 0),
                    size = input$n_flips_big,
                    replace = TRUE,
                    prob = c(input$prob_heads, 1 - input$prob_heads)
                    )
            }
            
            # Count proportion of heads for each player
            big_row_sum <- df_big %>%
                rowwise() %>%
                summarise(
                    n_heads = sum(c_across()),
                    prop_heads = n_heads/input$n_flips_big)
            
            # Empty dataframe for flips of Team Small
            df_small <- data.frame(
                matrix(
                    data = NA,
                    nrow = input$n_players,
                    ncol = input$n_flips_small
                    )
                )
            # Flip the coins for Team Small
            for(k in 1:input$n_players){
                df_small[k, ] <- sample(
                    x = c(1, 0),
                    size = input$n_flips_small,
                    replace = TRUE,
                    prob = c(input$prob_heads, 1 - input$prob_heads)
                    )
            }
            
            # Count proportion of heads for each player
            small_row_sum <- df_small %>%
                rowwise() %>%
                summarise(
                    n_heads = sum(c_across()),
                    prop_heads = n_heads/input$n_flips_small)
            
            # Create new columns in results df with the highest proportions
            df$max_big_prop[i] <- max(big_row_sum$prop_heads)
            df$max_small_prop[i] <- max(small_row_sum$prop_heads)
            
        }
        
        # Add a third column with a logical that test whether Team Small won
        df <- df %>%
            select(-1, -2) %>%
            mutate(
                small_win = case_when(
                    max_big_prop >= max_small_prop ~ FALSE,
                    max_big_prop < max_small_prop ~ TRUE
                )
            )
        
        df
    })
    
    # Output simulation table
    output$sim_table <- renderTable(
        simulation()
    )
    
    # Create a reactive text object
    text_results <- eventReactive(input$sim, {
        paste(
            "The small team had a higher proportion of heads",
            sum(simulation()$small_win),
            "of the", input$replications, "replications (",
            (sum(simulation()$small_win)/input$replications)*100, "%)."
            )
    })
    
    # Output simulation result sentence.
    output$sim_results <- renderText(
        text_results()
    )

}) # Close Server
