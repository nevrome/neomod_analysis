function(input, output, session) {
  
  library(magrittr)
  library(gluesless)
  
  # load data
  load("/home/clemens/neomod/neomod_datapool/model_data/hex_graph.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_df.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_hex_df.RData")

  output$state_nr_control <- renderUI({
    sliderInput(
      "state_nr", "state / timestep!",
      min = 0, max = length(states()), value = 0, step = 5
    )
  })
  
  graphstring <- graphwrite(hex_graph)
  
  #run model
  states <- reactive({

    new(
      "model_builder",
      networkland_env = graphstring,
      number_iterations = input$iterations
    ) %>%
      run() %$%
      idea_exp %>%
      gluesless::link_ideas_world(hex_graph)

  })
  
  # prepare base plot
  worldplot <- gluesless::plot_world(
    graph = hex_graph, 
    world = research_area_df, 
    hex = research_area_hex_df
  )
  
  # add ideas to base plot  
  output$map <- renderPlot({
    if(
      input$state_nr %>% is.null %>% `!` &&
      input$state_nr > 0
    ) {
      worldplot %>%
        plot_state(states = states(), input$state_nr)
    } else {
      worldplot
    }
  }, height = 700, width = 1000)
}