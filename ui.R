body = dashboardBody(

   wellPanel(
      id = 'ControlPanel',
      fluidRow(
         width = 12,
         column(
            width = 1,
            h4('Monte Carlo Simulator')
         ),
         column(
            width = 1
         ),
         column(
            width = 2,
            fluidRow(
               width = 12,
               actionButton(
                  inputId = "actionButtonAddVariable", 
                  label = "Add variable"
               )
            ),
            fluidRow(
               width = 12,
               actionButton(
                  inputId = "actionButtonUploadEmpiricalDistributions", 
                  label = "Empirical distributions"
               )
            )
         ),
         column(
            width = 1
         ),
         column(
            width = 2,
            fluidRow(
               width = 12,
               numericInput(
                  inputId = 'Iterations', 
                  label = 'Iterations', 
                  value = 100000, 
                  min = 0, 
                  step = 1
               )
            ),
            fluidRow(
               width = 12,
               actionButton(
                  inputId = "actionButtonRun", 
                  label = "Run"
               )
            ),
            fluidRow(
               width = 12,
               actionButton(
                  inputId = "actionButtonViewSummary", 
                  label = "Summary"
               )
            )
         ),
         column(
            width = 1
         ),
         column(
            width = 2,
            downloadButton(
               outputId = 'downloadScenario',
               label = "Download Scenario"
            ),
            uiOutput('uiResettableFileInput')
         ),
         column(
            width = 1
         ),
         column(
            width = 1,
            actionButton(
               inputId = "actionButtonHelp", 
               label = "Help"
            )
         )
      ),
      htmlOutput('vcNotification')
   )

   # this is now being added through server.R itself
   # this is to ensure that on new file upload, the same
   # UI gets used as when the app is initialised
   # wellPanel(
   #    style = "overflow-y:scroll; max-height: 750px",
   #    id = 'PanelToAddVariables'
   # )

)

# Complete app with UI and server components
dashboardPage(
   dashboardHeader(disable = T),
   dashboardSidebar(width = 0),
   body
)