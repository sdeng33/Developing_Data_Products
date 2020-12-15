# Libraries required for the Shiny Dashboard Application #
library(shiny)
library(shinydashboard)
library(plotly)

# Formatting for the text generated on the dashboard sidebar. #
vel_lab <- HTML(paste("v",tags$sub(0)," (m/s):", sep = ""))
ang_lab <- HTML(paste("\U1D6FC", tags$sub(0)," (\u00B0):", sep = ""))

# Give the dashboard header a title. #
header <- dashboardHeader(title = "2-D Projectile Motion")

sidebar <- dashboardSidebar(title = h4("Controls", align = "center"),
                            
           # Define the sidebar with two input slider widgets for control. #
                            sliderInput(inputId = "v0", label = vel_lab,
                                        min = 5, max = 100,
                                        value = 50, step = 5),
                            sliderInput(inputId = "alpha0", label = ang_lab,
                                        min = 5, max = 85,
                                        value = 45, step = 5),
                            h4("Instructions", align = "center"),
                            h5("Please select an initial velocity with a lower
                                limit of 5 m/s to an upper limit of 100 m/s that 
                                is incremented by 5 m/s using the first slider 
                                above."),
                            h5("Also, please select an initial angle that is 
                                limited to the 1st quadrant of the Cartesian
                                coordinate system with the exceptions of 0\u00B0 
                                and 90\u00B0 that is incremented by 5\u00B0 
                                using the 2nd slider.")
                             )

body <- dashboardBody(fluidRow(
                      # Configure the aesthetics of the boxes and create spots # 
                      # for the animated plots. #
                      box(title = "Trajectory", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE,
                          plotlyOutput(outputId = "traj", height = 750)),
                      box(title = "Speed", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE,
                          plotlyOutput(outputId = "spd", height = 750))
                      ))

ui <- dashboardPage(header, sidebar, body)
