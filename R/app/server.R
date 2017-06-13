function(input, output, session) {
  
  library(magrittr)
  
  # clusters <- reactive({
  #   kmeans(selectedData(), input$clusters)
  # })
  
  # load data
  load("/home/clemens/neomod/neomod_datapool/model_data/hex_graph.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_df.RData")
  load("/home/clemens/neomod/neomod_datapool/model_data/research_area_hex_df.RData")

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
    worldplot %>%
      plot_state(states = states(), input$state_nr)
  })
}