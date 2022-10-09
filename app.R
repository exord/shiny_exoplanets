#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(forcats)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exoplanet Detections in Time"),

    sliderInput("yr",
                "Año:",
                min = 1992,
                max = 2022,
                value = 1992, sep='',
                ),
  
    plotOutput("distPlot"), 
    plotOutput("cumPlot")
)

# # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("yr",
#                         "Year:",
#                         min = 1995,
#                         max = 2022,
#                         value = 1992)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df_all <- reactive({
      read_csv('/Users/rodrigo/ExP/_tables/exoplanet_archive.csv',
               skip = 316) %>% 
        mutate(disc_method = fct_reorder(discoverymethod, disc_year))
               })
      
    df <- reactive({
        df_all() %>%
        filter(disc_year != 1989 & disc_year <= input$yr)
      })
      
    df_filter <- reactive({
          df_all() %>% 
          mutate(disc_method = fct_reorder(discoverymethod, disc_year),
                 sig_masse = 0.5 * (pl_bmasseerr1 + pl_bmasseerr2)) %>% 
          filter(disc_year != 1989 & disc_year <= input$yr) %>% 
          filter( sig_masse / pl_bmasse < 0.5)
      })

    df_count1 <- reactive({
      df_filter() %>%  
        group_by(disc_year) %>% 
        tally()
    })
    
    df_count_all <- reactive({
      df() %>%  
        group_by(disc_year) %>% 
        tally()
    })
      
    output$distPlot <- renderPlot({
    
        # filter according to year
         
        ggplot(df_filter()) + geom_point(aes(x=pl_orbper, y=pl_bmasse, 
                                        color=disc_method), alpha=0.5, size=2) +
        
        scale_color_discrete(drop=TRUE, 
                             limits=levels(df_all()$disc_method)) +
        
        coord_cartesian(xlim = c(1e-2, 1e7), ylim = c(1e-2, 1e+5)) +
        
        scale_y_continuous('Masa (mínima) [Mtierra]',
                           labels = scales::comma_format(big.mark='.',
                                                         decimal.mark = ','),
                           trans = 'log10',
                           sec.axis = sec_axis(~ . * 0.001, 
                                               name='Masa (mínima) [Mjup]',
                                               labels = comma_format(big.mark='.', 
                                                                     decimal.mark =',',
                                                                     ),
                                               )
                           ) +
        
        scale_x_continuous('Período Orbita [días]',
                           labels = scales::comma_format(big.mark='.',
                                                         decimal.mark = ','),
                           trans = 'log10',
                           breaks = 10**(seq(-1, 8, by=2)),
                           sec.axis = sec_axis(~ . / 365.25, 
                                               name='Período Orbita [años]',
                                               breaks = 10**(seq(-1, 4, by=1)),
                                               labels = comma_format(big.mark='.', 
                                                                     decimal.mark =','),
                                               
                           )
        ) +
        
        labs(title=paste('Descubrimientos hasta', input$yr, 
                         '(masa medida con error menor al 50%)')) +
        
        theme(text = element_text(size=18), 
              axis.text = element_text(size=12))
    })
    
    output$cumPlot <- renderPlot({
        # Filtered
        ggplot(df_count1(), aes(x=disc_year, y=cumsum(n))) + 
        geom_line(color='red', size=1) +
        geom_point() +
        
        # All
        geom_line(data=df_count_all(), 
                  aes(x=disc_year, y=cumsum(n)),
                  color='black', size=1) +
        
        coord_cartesian(xlim = c(1992, 2022), ylim = c(1, 5250)) +
        scale_y_continuous('Número de planetas', n.breaks=5) +
        scale_x_continuous('Año', n.breaks = 10,
                           minor_breaks = seq(1995, 2022, by=1)) +
        labs(x = 'Año', caption='Rodrigo F. Díaz ©2022') +
        
        theme(text = element_text(size=18), 
              axis.text = element_text(size=12))
      
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
