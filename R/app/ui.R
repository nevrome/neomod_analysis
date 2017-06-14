pageWithSidebar(
  headerPanel('Simulation'),
  sidebarPanel(
    uiOutput("state_nr_control"),
    numericInput("iterations", "number of model iteration", 50)
  ),
  mainPanel(
    plotOutput("map", width = "100%")
  )
)