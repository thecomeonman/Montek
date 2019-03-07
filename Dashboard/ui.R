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
                  h4('Monte Carlo Tool')
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
               column(
                  width = 4,
                  box(
                     title = 'Cloud',
                     width = 12,
                     selectizeInput(
                        inputId = 'selectizeInputLoadScenarioFromCloud', 
                        label = 'Load', 
                        choices = c(), 
                        selected = c(), 
                        multiple = F
                     ),
                     hr(),
                     textInput(
                        inputId = 'textInputSaveScenarioOnCloud',
                        value = 'blahblah.json',
                        label = NULL,
                        placeholder = 'Scenario name on cloud'
                     ),
                     actionButton(
                        inputId = "actionButtonSaveScenarioOnCloud", 
                        label = "Save"
                     )
                  )
               ),
               column(
                  width = 4,
                  box(
                     title = 'Help',
                     width = 12,
                     actionButton(
                        inputId = "actionButtonUploadEmpiricalDistributions", 
                        label = "Empirical distributions"
                     ),
                     hr(),
                     actionButton(
                        inputId = "actionButtonHelp", 
                        label = "Help"
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