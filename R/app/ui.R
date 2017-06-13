pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    actionButton("do", "Click Me"),
    sliderInput(
      "state_nr", "Floooeeet!",
      #min = 1, max = length(states), value = 3
      min = 1, max = 20, value = 3
    ),
    numericInput('iterations', 'iter', 30,
                 min = 10, max = 100)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)