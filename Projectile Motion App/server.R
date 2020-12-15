g <- 9.8 # (m/s^2) Acceleration of gravity in SI units #

server <- function(input, output, session){
          
          # This reactive expression converts the degrees inputted by the user #
          # and converts it to radians. #
          rad <- reactive({init_ang <- input$alpha0
                           conv <- pi * init_ang/180})
          
          # This reactive expression is where the projectile lands in terms of #
          # the initial velocity and initial launch angle. #
          displ_root <- reactive({init_vel <- input$v0
                                  gnd <- (init_vel)^2 * sin(2 * rad())/g})
          
          # This reactive expression is the total flight time of the projectile #
          # in terms of the initial velocity and initial launch angle. #
          tot_time <- reactive({init_vel <- input$v0
                                t_d <- (2 * init_vel * sin(rad()))/g})
          
          # This reactive expression are the 10 equidistant x-coordinates of # 
          # the projectile along its horizontal range. # 
          x <- reactive({horiz_posns <- 
                         seq(from = 0, to = displ_root(), length.out = 10)})
          
          # This reactive expression are the 5 equidistant y-coordinates along # 
          # its maximum height while ascending and descending. #
          y <- reactive({init_vel <- input$v0
                         first_term <- tan(rad()) * x()
                         numerator <- g * (x())^2
                         denominator <- 2 * (init_vel)^2 * (cos(rad()))^2
                         second_term <- numerator/denominator
                         vert_posns <- first_term - second_term
                         vert_posns[length(vert_posns)] <- 0
                         vert_posns})
          
          # This reactive expression are 10 equally spaced measures of time #
          # from the start to the end. #
          t <- reactive({T <- seq(from = 0, to = tot_time(), length.out = 10)})
          
          # This reactive expression are 10 equally spaced measures of speed #
          # including the projectile's initial speed, and the projectile final #
          # speed just before it crashes into the ground. #
          speed <- reactive({init_vel <- input$v0
                            v_y <- init_vel * sin(rad()) - (g * t())
                            v_x <- init_vel * cos(rad())
                            v <- sqrt((v_x)^2 + (v_y)^2)
                            })
          
          # This reactive expression are the specific number of frames in the #
          # animated plot. #
          f <- reactive({frame_num <- 1:length(x())})
          
          # This reactive expression is a data frame of the trajectory of the #
          # projectile consisting of some of the reactive expressions specified #
          # above. #
          traj_df <- reactive({df <- data.frame(x = x(), 
                                                y = y(),
                                                f = f())})

          # This reactive expression is a data frame of the speed of the #
          # projectile consisting of some of the reactive expressions specified #
          # above. #
          spd_df <- reactive({df <- data.frame(x = t(),
                                               y = speed(),
                                               f = f())})
          
          # Render an animated trajectory plot of the projectile in Plotly with #
          # all the necessary formatting. #
          output$traj <- renderPlotly({fig <- traj_df() %>% plot_ly(x = ~x,
                                                          y = ~y,
                                                          frame = ~f,
                                                          type = "scatter",
                                                          mode = "markers",
                                                          showlegend = F)
          
                                       fig <- fig %>% 
                                              layout(title = "Trajectory In Vacuum",
                                                     xaxis = list(title = "x (m)"),
                                                     yaxis = list(title = "y (m)"))
                                       
                                       fig
                                      })
          # Render an animated speed plot of the projectile in Plotly with #
          # all the necessary formatting. #
          output$spd <- renderPlotly({fig <- spd_df() %>% plot_ly(x = ~x,
                                                                  y = ~y,
                                                                  frame = ~f,
                                                                  type = "scatter",
                                                                  mode = "markers",
                                                                  showlegend = F)
          
                                      fig <- fig %>% 
                                      layout(title = "Speed In Vacuum",
                                      xaxis = list(title = "t (s)"),
                                      yaxis = list(title = "speed (m/s)"))
          
                                      fig
                                     })
          
          }