function(input, output, session) {
  
  library(magrittr)
  library(gluesless)
  
  # load data
  load("/home/clemens/neomod/neomod_datapool/model_data/hex_graph.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_df.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_hex_df.RData")

  output$state_nr_control <- renderUI({
    sliderInput(
      "state_nr", "Floooeeet!",
      min = 1, max = length(states()), value = 1
    )
  })
  
  #run model
  states <- reactive({

    new(
      "model_builder",
      networkland_env = graphwrite(hex_graph),
      number_iterations = input$iterations
    ) %>%
      run() %$%
      idea_exp %>%
      gluesless::link_ideas_world(hex_graph)

  })
  
  worldplot <- gluesless::plot_world(
    graph = hex_graph, 
    world = research_area_df, 
    hex = research_area_hex_df
  )
    
  output$plot1 <- renderPlot({
    if(input$state_nr %>% is.null %>% `!`) {
      worldplot %>%
        plot_state(states = states(), input$state_nr)
    }
  })
}