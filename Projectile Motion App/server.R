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
          
          # This reactive expression is the calculation of the horizontal #
          # displacement at the time the projectile reaches the maximum height #
          x_h_max <- reactive({init_vel <- input$v0
                               X_y_max <- ((init_vel)^2 * sin(2 * rad()))/(2 *g)})
          
          # This reactive expression adds the reactive expression above and sorts 
          # to create a new x-coordinates of the projectile along its horizontal 
          # range #
          x_add <- reactive({x <- c(x(), x_h_max())
                             x <- sort(x)})
          
          # This reactive expression are the 5 equidistant y-coordinates along # 
          # the projectile's maximum height while ascending and descending. #
          y <- reactive({init_vel <- input$v0
                         first_term <- tan(rad()) * x_add()
                         numerator <- g * (x_add())^2
                         denominator <- 2 * (init_vel)^2 * (cos(rad()))^2
                         second_term <- numerator/denominator
                         vert_posns <- first_term - second_term
                        })
          
          # This reactive expression calculates the index of the horizontal #
          # displacement reactive expression as the projectile crashes into the #
          # ground. #
          marker <- reactive({if (any(x_add() == displ_root())){
                  
                                        mrkr <- which(x_add() == displ_root())
                                        
                                      }

                             })
           
          # This reactive expression makes sure that the vertical displacement #
          # when the projectile crashes into the ground is zero. #
           y_alt <- reactive({y_star <- y()
                              bkmrk <- marker()        
                              y_star[bkmrk] <- 0 
                              y_edit <- y_star})
           
          # This reactive expression are 10 equally spaced measures of time #
          # from the start to the end. #
          t <- reactive({T <- seq(from = 0, to = tot_time(), length.out = 10)})
          
          # This reactive expression is the calculation of the time when the #
          # speed of the projectile is at a minimum #
          t_spd_min <- reactive({init_vel <- input$v0
                                 t_v_min <- (init_vel * sin(rad()))/g})

          # This reactive expression are 10 equally spaced measures of speed #
          # including the projectile's initial speed, and the projectile final #
          # speed just before it crashes into the ground. #
          speed <- reactive({init_vel <- input$v0
                            v_y <- init_vel * sin(rad()) - (g * t())
                            v_x <- init_vel * cos(rad())
                            v <- sqrt((v_x)^2 + (v_y)^2)
                            })
          
          # This reactive expression is the calculation of the minimum speed of#
          # the projectile. #
          spd_min <- reactive({init_vel <- input$v0
                               v_min <- init_vel * cos(rad())})

          # This reactive expression are the specific number of frames in the #
          # trajectory animated plot. #
          f <- reactive({frame_num <- 1:length(x_add())})
          
          # This reactive expression is a data frame of the trajectory of the #
          # projectile consisting of some of the reactive expressions specified #
          # above. #
          traj_df <- reactive({df <- data.frame(x = x_add(),
                                                y = y_alt(),
                                                f = f())})

          # This reactive expression is a data frame of the speed of the #
          # projectile consisting of some of the reactive expressions specified #
          # above. #
          spd_df <- reactive({df <- data.frame(x = c(t()[1:5], t_spd_min(),
                                                     t()[6:10]),
                                               y = c(speed()[1:5], spd_min(),
                                                     speed()[6:10]),
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