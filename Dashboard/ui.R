body = dashboardBody(

   fluidRow(
      id = 'ControlPanel',
      width = 12, 
      column(
         width = 12,
         fluidRow(
            box(
               width = 12,
               collapsible = T,
               column(
                  width = 2,
                  HTML('<h4>Rhyhorn</h4>'),
                  HTML('R-Shiny Based Monte Carlo Simulations'),
                  HTML('Developed by <a href="https://atherenergy.com", target="_blank">Ather Energy</a>; code <a href="https://github.com/AtherEnergy/Rhyhorn", target="_blank">here</a>')
               ),
               column(
                  width = 2,
                  actionButton(
                     inputId = "actionButtonAddVariable", 
                     label = "Add variable"
                  )
               ),
               column(
                  width = 2,
                  actionButton(
                     inputId = "actionButtonUploadEmpiricalDistributions", 
                     label = "Empirical distributions"
                  )
               ),
               column(
                  width = 2,
                  numericInput(
                     inputId = 'Iterations', 
                     label = 'Iterations', 
                     value = 10000, 
                     min = 0, 
                     step = 1
                  )
               ),
               column(
                  width = 2,
                  actionButton(
                     inputId = "actionButtonRun", 
                     label = "Run"
                  )
               ),
               column(
                  width = 2,
                  actionButton(
                     inputId = "actionButtonViewSummary", 
                     label = "Summary"
                  ),
                  downloadButton(
                     outputId = 'downloadScenarioResults',
                     label = "Download Results"
                  )
               )
            )
         ),
         fluidRow(
            box(
               width = 12, 
               collapsible = T,
               column(
                  width = 4,
                  box(
                     title = 'Local Machine',
                     width = 12,
                     uiOutput('uiResettableFileInput'),
                     hr(),
                     downloadButton(
                        outputId = 'downloadScenario',
                        label = "Download Scenario"
                     )
                  )
               ),
               # column(
               #    width = 4,
               #    box(
               #       title = 'Cloud',
               #       width = 12,
               #       selectizeInput(
               #          inputId = 'selectizeInputLoadScenarioFromCloud', 
               #          label = 'Load', 
               #          choices = c(), 
               #          selected = c(), 
               #          multiple = F
               #       ),
               #       hr(),
               #       textInput(
               #          inputId = 'textInputSaveScenarioOnCloud',
               #          value = 'Scenario.json',
               #          label = NULL,
               #          placeholder = 'Scenario name on cloud'
               #       ),
               #       actionButton(
               #          inputId = "actionButtonSaveScenarioOnCloud", 
               #          label = "Save"
               #       )
               #    )
               # ),
               column(
                  width = 4,
                  box(
                     title = 'App Settings',
                     width = 12,
                     checkboxInput("checkboxLoadGUI", "Load variable GUI?", TRUE),
                     checkboxInput("checkboxLoadCorrelations", "Calculate correlations?", TRUE),
                     checkboxInput("checkboxValidations", "Validate before run?", TRUE),
                     checkboxInput("checkboxStoreLastResult", "Store last run values?", TRUE)
                  )
               ),
               column(
                  width = 4,
                  box(
                     title = 'Help',
                     width = 12,
                     actionButton(
                        inputId = "actionButtonAppFunctionality", 
                        label = "App Functionality"
                     ),
                     actionButton(
                        inputId = "actionButtonAppInfo", 
                        label = "App Information"
                     ),
                     actionButton(
                        inputId = "actionButtonConfiguring", 
                        label = "Configuring"
                     )
                  )
               )
            )
         )
      )
   ),
   htmlOutput('vcNotification')

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