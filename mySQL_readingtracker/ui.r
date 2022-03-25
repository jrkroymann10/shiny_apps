navbarPage(title = "Reading Tracker",
           tabPanel(title = "Database Tables",
                    column(3,
                           wellPanel(uiOutput(outputId = "table_input"))
                           ),
                    column(9,
                           reactableOutput(outputId = "table_output")
                           )
                    )
           )