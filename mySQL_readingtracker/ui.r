navbarPage(title = "Reading Tracker",
           tabPanel(title = "Database Tables",
                    column(2,
                           wellPanel(uiOutput(outputId = "table_input"))
                           ),
                    column(10,
                           reactableOutput(outputId = "table_output")
                           )
                    )
           )