pageWithSidebar(
  headerPanel('Simulation'),
  sidebarPanel(
    actionButton("do", "Click Me"),
    uiOutput("state_nr_control"),
    numericInput('iterations', 'iter', 30,
                 min = 10, max = 100)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)