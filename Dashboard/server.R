
function(input, output, session) {

   #' Function to generate uniform distribution
   #' @param iIterations The number of values to generate
   #' @param nLowerBound The lower limit of values to look at
   #' @param The upper limit of values to look at
   fUniformDistribution = function(
      iIterations = 2,
      nLowerBound = 0,
      nUpperBound = 1
   ) {

      vnDistribution = runif(iIterations)

      vnDistribution = vnDistribution * abs(nUpperBound-nLowerBound)

      vnDistribution = vnDistribution + min(nUpperBound, nLowerBound)

      return ( vnDistribution )

   }


   #' https://github.com/rstudio/DT/issues/93#issuecomment-111001538
   #' Function to retrieve multiple UI elements values from basic argument constructs
   #' @example 
   #' # To retrieve inputs 'Variable1', 'Variable2', and 'Variable3' -
   #' fRetrieveShinyValue(
   #'    input = input,
   #'    id = 'Variable',
   #'    vcSuffixes = 1:3
   #' )
   fRetrieveShinyValue = function(
      input,
      id,
      vcSuffixes
   ) {

      unlist(
         lapply(
            seq(length(vcSuffixes)),
            function( i ) {

               value = input[[
                  paste0(id, vcSuffixes[i])
               ]]

               if (is.null(value)) NA else value
            }
         )
      )

   }


   #' Create UI which accepts variables input
   #' @param lVariablesMetadata Is generated on clicking the Run button
   #' @return A data.table with one row for each variable with its front-end
   #' name and its back-end name
   fMapVariableNames = function (
      lVariablesMetadata
   ) {

      rbindlist(
         lapply(
            lVariablesMetadata,
            function ( lVariableMetadata ) {
               data.table(
                  VariableNumber = lVariableMetadata$iVariableNumber,
                  VariableBackEndName = paste0(cTempVariableString, lVariableMetadata$iVariableNumber),
                  # VariableBackEndName = paste0('Variable',lVariableMetadata$iVariableNumber),
                  VariableName = lVariableMetadata$cVariableName
               )
            }
         )
      )

   }

   #' Create UI which accepts variables input
   #' All the arguments are the initialisation values for each UI element
   #' @param iVariableNumber Integer which is unique for every new variable added
   #' @param cVariableName Character for user facing name of the variable
   #' @param cVariableDescription Character for some comments that the user wants to record
   #' @param bIsInput Boolean to dictate whether input variable or output variable
   #' @param cDistribution Character for the type of distribution to select, eg. Normal, Beta, etc.
   #' @param lParameters List of parameters needed to generate distribution
   #' @param nDistributionUpperBound Number upper cap for the distribution
   #' @param nDistributionLowerBound Number lower cap for the distribution
   #' @param cEquation Character the equation to evaluate an output variable
   #' @return The entire UI element for an the variable
   fCreateVariableAdditionUI = function(
      iVariableNumber = 1,
      cVariableName = NULL,
      cVariableDescription = NULL,
      bIsInput = T,
      cDistribution = 'Normal',
      lParameters = list(),
      # nNormalMean = 0,
      # nNormalSD = 1,
      # nBetaShapeAlpha = NULL,
      # nBetaShapeBeta = NULL,
      # nConstant = NULL,
      vcEmpiricalDistributionNames = c(),
      nDistributionUpperBound = 0,
      nDistributionLowerBound = 0,
      cEquation = NULL
   ) {

      fluidRow(
         width = 12,

         # Some border to divide. It isn't very effective, to be honest.
         hr(size=30),
         # column(
         #    width = 2,

            # The box with the variable naming, description, etc.
            box(
               # width = 12,
               width = 2,

               # to provide alternating colours to the rows
               status = ifelse(iVariableNumber%%2, 'primary','info'),

               title = iVariableNumber,
               checkboxInput( 
                  inputId = paste0(
                     "IsInput", 
                     iVariableNumber
                  ), 
                  label = 'Input variable?', 
                  value = bIsInput
               ),
               textInput(
                  inputId = paste0(
                     "VariableName", 
                     iVariableNumber
                  ),
                  label = NULL,
                  placeholder = "Variable name",
                  value = cVariableName
               ),
               textInput(
                  inputId = paste0(
                     "VariableDescription", 
                     iVariableNumber
                  ),
                  label = NULL,
                  placeholder = "Comments ( for your reference )",
                  value = cVariableDescription
               )
            ),
         # ),

         # The box with the deatils of calculating the variable, etc.
         box(
            width = 10,
            collapsible = T,
            # background = ifelse(iVariableNumber%%2, 'blue','light-blue'),
            solidHeader = T,

            # to provide alternating colours to the rows
            status = ifelse(iVariableNumber%%2, 'primary','info'),
            title = '',
            fluidRow(
               width = 12,
               column(
                  width = 2,

                  # Input variable conditional panel
                  conditionalPanel(
                     condition = paste0(
                        'input.IsInput',
                        iVariableNumber,
                        '== 1'
                     ),
                     selectizeInput(
                        inputId = paste0(
                           "Distribution", 
                           iVariableNumber
                        ), 
                        label = 'Distribution',
                        choices = sort(c('Constant','Normal','Uniform','Beta','Empirical')), 
                        selected = cDistribution
                     ),

                     # normal distribution conditional panel
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '== "Normal"'
                        ), 
                        numericInput(
                           inputId = paste0('NormalMean', iVariableNumber),
                           label = 'Mean',
                           value = lParameters$nNormalMean
                        ),
                        numericInput(
                           inputId = paste0('NormalSD', iVariableNumber),
                           label = 'SD',
                           value = lParameters$nNormalSD
                        )
                     ),

                     # BEta distribution conditional panel
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '== "Beta"'
                        ), 
                        numericInput(
                           inputId = paste0('BetaShapeAlpha', iVariableNumber),
                           label = 'Beta Shape Alpha',
                           min = 0,
                           value = lParameters$nBetaShapeAlpha
                        ),
                        numericInput(
                           inputId = paste0('BetaShapeBeta', iVariableNumber),
                           label = 'Beta Shape Beta',
                           min = 0,
                           value = lParameters$nBetaShapeBeta
                        )
                     ),

                     # Constant conditional panel
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '== "Constant"'
                        ), 
                        numericInput(
                           inputId = paste0('ConstantValue', iVariableNumber),
                           label = 'Value',
                           value = lParameters$nConstant
                        )

                     ),

                     # Uniform distribution conditional panel
                     # needs only bounds so this is just a placeholder really
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '== "Uniform"'
                        ),
                        fluidRow()
                     ),

                     # Empirical distribution conditional panel
                     # needs only bounds so this is just a placeholder really
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '== "Empirical"'
                        ),
                        selectizeInput(
                           inputId = paste0(
                              "ChosenEmpiricalDistribution", 
                              iVariableNumber
                           ), 
                           label = 'Distribution',
                           choices = vcEmpiricalDistributionNames, 
                           selected = lParameters$cEmpiricalDistributionName
                        )
                     ),

                     # Add conditional panels for new distributions here

                     # Panel to input upper and lower caps
                     conditionalPanel(
                        condition = paste0(
                           'input.Distribution',
                           iVariableNumber,
                           '!= "Constant"'
                        ),
                        numericInput(
                           inputId = paste0('DistributionUpperBound', iVariableNumber),
                           label = 'Upper cap',
                           value = nDistributionUpperBound
                        ),
                        numericInput(
                           inputId = paste0('DistributionLowerBound', iVariableNumber),
                           label = 'Lower cap',
                           value = nDistributionLowerBound
                        )
                     )
                  ),

                  # Output variable conditional panel
                  conditionalPanel(
                     condition = paste0(
                        'input.IsInput',
                        iVariableNumber,
                        '== 0'
                     ),
                     textInput(
                        inputId = paste0('textInputEquation', iVariableNumber), 
                        label = NULL, 
                        placeholder = 'Equation', 
                        width = '100%',
                        value = cEquation
                     )
                  )
               ),

               # Details of the eventual distribution / calculation
               column(
                  width = 4,
                  box(
                     width = 12,
                     collapsible = T,
                     collapsed = F,
                     title = 'Summary',
                     status = 'warning',

                     # Median, mean, etc. and quantiles tables
                     dataTableOutput(
                        paste0('DistributionSummary', iVariableNumber)
                     )
                  ),
                  box(
                     width = 12,
                     collapsible = T,
                     collapsed = T,
                     title = 'PDF and CDF',
                     status = 'warning',

                     # The 100 rows of CDFs and PDFs
                     dataTableOutput(
                        paste0('DistributionDetails', iVariableNumber)
                     )
                  )
               ),

               # Plots of the eventual distribution / calculation
               column(
                  width = 6,
                  box(
                     width = 12,
                     collapsible = T,
                     collapsed = T,
                     title = 'Plots',
                     status = 'warning',
                     plotOutput(
                        paste0('VariableDistributionChartPDF', iVariableNumber)
                     ),
                     plotOutput(
                        paste0('DistributionChartCDF', iVariableNumber)
                     )
                  )
               )
            )
         )
      )
   }


   #' Sample an empirical distribution
   #' @param lReactiveValues
   #' @param cEmpiricalDistributionName User facing name of the empirical distribution
   #' @param iIterations The number of samples to generate
   #' @return The entire UI element for an the variable
   fSampleFromEmpirical = function(
      lReactiveValues,
      cEmpiricalDistributionName,
      iIterations = 1
   ) {

      # Retrieve the empirical distribution names
      # We'll need to match the selected name to this list to
      # figure out which empirical distribution to use
      vcEmpiricalDistributionNames = fRetrieveShinyValue(
         input = lReactiveValues,
         id = 'EmpiricalDataName',
         vcSuffixes = 1:iRandomlyLargeNumberForEmpirical
      )

      vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
         !is.na(vcEmpiricalDistributionNames)
      ]

      vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
         !is.null(vcEmpiricalDistributionNames)
      ]

      iEmpiricalDistributionNbr = which(
         vcEmpiricalDistributionNames == cEmpiricalDistributionName
      )

      # Retrieve the empirical distribution
      vnRawDistribution = lReactiveValues[[
         paste0(
            'EmpiricalData',
            iEmpiricalDistributionNbr
         )
      ]]

      # Sample the distribution
      vnDistribution = sample(
         x = vnRawDistribution,
         size = iIterations,
         replace = T
      )

      return ( vnDistribution )

   }


   #' Discretise a set of numbers into a certain number of buckets
   #' Also gives a quantised PDF, CDF, etc
   #' @param vnDistribution All the numbers
   #' @param iBuckets The number of buckets to break it into
   #' @return A data.table with The bucket, the median value of each bucket,
   #' the PDF, and the CDF of that bucket.
   fBucketDistributions = function(
      vnDistribution,
      iBuckets = 100
   ) {

      dtDistribution = data.table(
         Values = vnDistribution
      )

      # Fitting the values into the buckets
      dtDistribution[,
         Bucket := as.integer(ceiling(
            ( iBuckets * (Values - min(Values) ) ) /
            (max(Values) - min(Values))
         ))
      ]

      # The min value would be 0 so clubbing that with 1
      # This will cause an artificial spike for bucket 1
      dtDistribution[
         Bucket == 0L,
         Bucket := 1L
      ]

      # Calculated PDFs and CDFs
      dtDistribution = dtDistribution[,
         list(
            PDF = .N / length(vnDistribution),
            Values = as.numeric(median(Values))
         ),
         Bucket
      ]

      setkey(dtDistribution, Values)
      dtDistribution[, CDF := cumsum(PDF)]


      # Return
      return ( dtDistribution )

   }


   #' Function which does basic checks on the variables
   #' There is lots to be added to this. All in good time.
   #' @param input
   #' @param lVariablesMetadata Is generated on clicking the Run button
   #' @param dtAllowedOperations This is created in Global.R and has the operations
   #' and details about the operations
   fValidateVariablesMetadata = function (
      input,
      lVariablesMetadata,
      dtAllowedOperations
   ) {


      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'fValidateVariablesMetadata'
         )
      )


      # Getting a list of all the variable names
      vcVariableNames = sapply(
         lVariablesMetadata,
         function (lVariableMetadata) {
            lVariableMetadata$cVariableName
         }
      )
      dtVariableNameMapping = fMapVariableNames(lVariablesMetadata)  

      # Step 1 of this function is validating the structure

      # check for missing variable names
      if (
         any( vcVariableNames == '' | is.null(vcVariableNames) )
      )  {

         lMainNotification = list(
            cMessage = paste0(
               "Missing variable names: ",
               paste(
                  which( vcVariableNames == '' | is.null(vcVariableNames) ), 
                  collapse = '<br/>'
               )
            ),
            cType = 'error'
         )

         return ( lMainNotification )

      }
      
      names(lVariablesMetadata) = vcVariableNames

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfValidateVariablesMetadata: Operators in variable names'
         )

      )

      # check for operators in variable names
      # @todo we should probably not need to this if we can change 
      # the equation parsing to recognise variables first and then split
      # on operators
      if ( F ) {
         dtOperatorInVariableName = data.table(
            VariableName = 'q',
            Operator = 'q'
         )[0]

         for ( cPattern in dtAllowedOperations[, c(' ', ',', OperatorString)] ) {

            vcInvalidVariableNames = grep(
               x = vcVariableNames, 
               pattern = cPattern,
               value = T
            )

            if ( length(vcInvalidVariableNames) > 0 ) {

               dtOperatorInVariableName = rbind(
                  dtOperatorInVariableName,
                  data.table(
                     VariableName = vcInvalidVariableNames,
                     Operator = cPattern
                  )
               )

            }

         }

         setDT(dtOperatorInVariableName)

         if (
            nrow(dtOperatorInVariableName) > 0 
         )  {

            dtOperatorInVariableName = dtOperatorInVariableName[, 
               list(
                  Operators = paste(Operator, collapse = ', ')
               ),
               VariableName
            ]

            dtOperatorInVariableName[, ErrorString := paste0(VariableName,': ',Operators)]

            lMainNotification = list(
               cMessage = paste0(
                  "Variable names can't have operators:<br/>",
                  paste(
                     dtOperatorInVariableName[, ErrorString],
                     collapse = '<br/>'
                  )
               ),
               cType = 'error'
            )

            return ( lMainNotification )

         }

      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfValidateVariablesMetadata: duplicate varibable names'
         )

      )


      # check for duplicate variable names
      viVariableNameCount = table(vcVariableNames)

      if (
         max(viVariableNameCount) > 1
      )  {

         lMainNotification = list(
            cMessage = paste0(
               "Duplicate variable names:<br/>",
               paste(
                  names(viVariableNameCount)[
                     viVariableNameCount > 1
                  ], 
                  collapse = '<br/>'
               )
            ),
            cType = 'error'
         )

         return ( lMainNotification )

      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfValidateVariablesMetadata: invalid equations'
         )

      )

      # Check for invalid equations
      dtVariableNameMapping[, NameLength := -nchar(VariableName)]
      setkey(dtVariableNameMapping, NameLength)
      

      vcNotification = sapply(
         lVariablesMetadata,
         function(lVariableMetadata) {

            cReturn = NULL

            if ( lVariableMetadata$bIsInput == T ) {

               return ( cReturn )

            }

            # Figure out what the upstream variables are

            # Removing operators
            cEquation = lVariableMetadata$cEquation
            vcUpstreamVariables = c()

            for ( iRow in seq(nrow(dtVariableNameMapping)) ) {

               if ( !dtVariableNameMapping[iRow, VariableName] == lVariableMetadata$cVariableName ) {

                  cEquation = unlist(strsplit(
                     x = as.character(cEquation),
                     split = dtVariableNameMapping[iRow, VariableName]
                  ))

               }

            }

            for ( cPattern in c(' ', ',') ) {

               cEquation = unlist(strsplit(
                  x = as.character(cEquation),
                  split = cPattern
               ))


            }

            cEquation = paste(
               cEquation,
               collapse = ''
            )

            # check if functions have the mandatory arguments or not
            for ( iOperatorNumber in seq(nrow(dtAllowedOperations)) ) {

               cOperatorString = dtAllowedOperations[iOperatorNumber, OperatorString]

               if ( !cOperatorString %in% c('(',')') ) {

                  cMandatoryArguments = dtAllowedOperations[iOperatorNumber, MandatoryArguments]

                  if ( is.na(cMandatoryArguments) | cMandatoryArguments == '' ) {

                     cEquation = gsub(
                        x = cEquation,
                        pattern = cOperatorString,
                        replacement = ''
                     )

                  }

                  gregexprOperationsMatches = gregexpr(
                     pattern = paste0(
                        cOperatorString, 
                        '\\(.*?\\)'
                     ),
                     text = cEquation
                  )

                  if ( gregexprOperationsMatches[[1]][1] != -1 ) {

                     vcOperationsInThisEquation = regmatches(
                        x = cEquation,
                        m = gregexprOperationsMatches, 
                     )

                     vcNotOperationsInThisEquation = regmatches(
                        x = cEquation,
                        m = gregexprOperationsMatches, 
                        invert = T
                     )

                     cMandatoryArguments = gsub(
                        cMandatoryArguments,
                        pattern = ' ',
                        replacement = ''
                     )

                     vcArgumentsInThisFunction = unlist(strsplit(cMandatoryArguments, ','))

                     for ( cMandatoryArgument in vcArgumentsInThisFunction ) {
         
                        vcOriginalOperationsInThisEquation = vcOperationsInThisEquation
                           
                        vcOperationsInThisEquation = gsub(
                           x = vcOperationsInThisEquation,
                           pattern = paste0(cMandatoryArgument,'='),
                           replacement = ''
                        )
                        
                        if ( all(vcOriginalOperationsInThisEquation == vcOperationsInThisEquation) ) {
                        
                           cReturn = paste(cReturn, ' ', cOperatorString, 'must explicitly specify', cMandatoryArguments)
                           
                        }

                     }

                     vcOperationsInThisEquation = gsub(
                        x = vcOperationsInThisEquation,
                        pattern = cOperatorString,
                        replacement = ''
                     )
                     
                     cEquation = c()

                     if ( gregexprOperationsMatches[[1]][1] == 1 ) {

                        cEquation[seq(length(vcOperationsInThisEquation))-2] = vcOperationsInThisEquation
                        cEquation[seq(length(vcOperationsInThisEquation))-1] = vcNotOperationsInThisEquation

                     } else {

                        cEquation[seq(length(vcOperationsInThisEquation))-1] = vcOperationsInThisEquation
                        cEquation[seq(length(vcOperationsInThisEquation))-2] = vcNotOperationsInThisEquation

                     }

                     cEquation = paste(
                        cEquation,
                        collapse = ''
                     )
        
                  }  

               }

            }

            if ( nchar(cEquation) > 0 ) {            

               cEquation = strsplit(
                  cEquation,
                  '\\)'
               )

               cEquation = strsplit(
                  unlist(cEquation),
                  '\\('
               )

               cEquation = strsplit(
                  unlist(cEquation),
                  ' '
               )

               cEquation = strsplit(
                  unlist(cEquation),
                  '\t'
               )

            }

            if ( length(cEquation) > 0) {

               if ( any(nchar(cEquation) > 0) ) {

                  if ( any(is.na(as.numeric(unlist(cEquation[sapply(cEquation, nchar)>0])))) ) {

                     cReturn = paste0(
                        lVariableMetadata$cVariableName,
                        ': ',
                        paste(
                           cEquation,
                           collapse = ', '
                        ),
                        cReturn
                     )

                  }

               }
            }

            return ( cReturn ) 

         }
      )

      vcNotification = unlist(vcNotification)

      if ( any(!is.null(vcNotification)) )  {

         lMainNotification = list(
            cMessage = paste0(
               "Unable to evaluate some terms:<br/>",
               paste(
                  vcNotification,
                  collapse = '<br/>'
               )
            ),
            cType = 'error'
         )

         return ( lMainNotification )

      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfValidateVariablesMetadata: missing equations'
         )

      )

      # Check for missing equations
      vcNotification = sapply(
         lVariablesMetadata,
         function( lVariableMetadata ) {

            cReturn = NULL

            if (!lVariableMetadata$bIsInput) {

               if ( 
                  nchar(
                     gsub(
                        x = lVariableMetadata$cEquation,
                        pattern = ' ',
                        replacement = ''
                     )
                  ) == 0
               ) {

                  cReturn = lVariableMetadata$cVariableName

               }

            }

            cReturn

         }

      )

      vcNotification = unlist(vcNotification)

      if ( any ( !is.null(vcNotification) ) ) {

         lMainNotification = list(
            cMessage = paste0(
               "Equations missing for:<br/>",
               paste(
                  vcNotification,
                  collapse = '<br/>'
               )
            ),
            cType = 'error'
         )

         return ( lMainNotification )

      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfValidateVariablesMetadata: what is this exactly?'
         )

      )

      # Step 1 of this function is calculating order of evaluation
      # The logic is basically to find the currently assigned sequence of evaluation
      # of all upstream variables, say x, and then assign x + 1 as the sequence of 
      # evaluation to the variable itself. Iterate till no changes.
      i = 0

      lVariablesMetadata3 = lapply(
         lVariablesMetadata,
         function( lVariableMetadata ) {

            list(
               # VariableNumber = lVariableMetadata[[x]]$VariableNumber,
               cVariableName = lVariableMetadata$cVariableName,
               # IsInput = lVariableMetadata[[x]]$IsInput,
               # Equation = lVariableMetadata[[x]]$Equation,
               vcUpstreamVariables = lVariableMetadata$vcUpstreamVariables,
               iSequence = lVariableMetadata$iSequence
            )

         }

      )
      names(lVariablesMetadata3) = names(lVariablesMetadata)

      repeat {
            
         lVariablesMetadata2 = lapply(
            names(lVariablesMetadata3),
            function(x) {
               
               vcUpstreamVariables = unlist(lVariablesMetadata3[[x]]$vcUpstreamVariables)

               if ( length(vcUpstreamVariables) == 0 ) {

                  iHighestSequence = 0

               } else {

                  iHighestSequence = max(
                     unlist(
                        sapply(
                           lVariablesMetadata3[vcUpstreamVariables], 
                           function(y) {
                              y$iSequence
                           }
                        )
                     ),
                     na.rm = T
                  )

               }

               iSequence = 1 + iHighestSequence
            
               list(
                  # VariableNumber = lVariablesMetadata3[[x]]$VariableNumber,
                  cVariableName = lVariablesMetadata3[[x]]$cVariableName,
                  # IsInput = lVariablesMetadata3[[x]]$IsInput,
                  # Equation = lVariablesMetadata3[[x]]$Equation,
                  vcUpstreamVariables = lVariablesMetadata3[[x]]$vcUpstreamVariables,
                  iSequence = max(iSequence, lVariablesMetadata3[[x]]$iSequence)
               )
            }
         )

         names(lVariablesMetadata2) = names(lVariablesMetadata3)

         if ( 
            identical(lVariablesMetadata3, lVariablesMetadata2)
         ) {
            
            rm(lVariablesMetadata2)
            break
         }

         # If there are cyclic conditions between variables then 
         # the loop counter should go beyond the number of variables
         # that are there.
         if ( i >  length(lVariablesMetadata2) + 5 ) {

            vcNotification = sapply(

               lVariablesMetadata3,

               function(lVariableMetadata) {

                  if ( lVariableMetadata$iSequence > length(lVariablesMetadata2) ) {

                     return ( lVariableMetadata$cVariableName )

                  } else {
                     return ( NULL )
                  }

               }

            )

            lMainNotification = list(
               cMessage = paste0(
                  'Possible cyclic relation between: ',
                  paste(vcNotification[!is.null(vcNotification)], collapse = ',')
               ),
               cType = 'error'
            )

            return ( lMainNotification )
            
         }

         i = i + 1
         
         lVariablesMetadata3 = lVariablesMetadata2
         gc()
         
      }

      # Assigning sequence number 
      lVariablesMetadata = lapply(
         names(lVariablesMetadata),
         function ( cVariableListName ) {

            lVariablesMetadata[[cVariableListName]]$iSequence = lVariablesMetadata3[[cVariableListName]]$iSequence
            lVariablesMetadata[[cVariableListName]]

         }
      )
      rm(lVariablesMetadata3)

      return ( lVariablesMetadata )

   }



   #' Function that evaluates all the variables
   #' @param iIterations Integer for the number of iteration to be run
   #' @param lVariablesMetadata Uhm... the list of variables and all the metadata
   fEvaluateVariables = function(
      iIterations,
      lVariablesMetadata,
      dtAllowedOperations
   ) {

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            '\nfEvaluateVariables:'
         )
      )


      #' @todo The utility of some variables is not beyond certain other variables
      #' so try and implement logic to get rid of these variables to save memory

      # We will convert the variable names to some safe names in the back end
      # to prevent mischief through sudo rm -rf level variable names
      dtVariableNameMapping = fMapVariableNames(lVariablesMetadata) 

      # Getting the order of evaluation
      vcEvaluationOrder = c(1:length(lVariablesMetadata))[
         order(
            sapply(
               lVariablesMetadata,
               function(x) {
                  x$iSequence
               }
            )
         )
      ]

      # Flag to flip at the end of evaluation to check if evaluation went fine
      bEverythingEvaluated = F

      # order variable names in decreasing order of length.
      # this is to avoid tragedies whereone variable name containts another
      dtVariableNameMapping[, NameLength := -nchar(VariableName)]
      setkey(dtVariableNameMapping, NameLength)

      lReactiveValuesPlaceholder = list()

      tryCatch(
         {

            vcVariablesWhichAreConstant = c()

            for ( iVariableNumber in vcEvaluationOrder ) {


               lVariableMetadata = lVariablesMetadata[[iVariableNumber]]

               cat(
                  file = stderr(),
                  paste(
                     Sys.time(),
                     '\nfEvaluateVariables:',
                     lVariableMetadata$iVariableNumber,
                     lVariableMetadata$cVariableName
                  )
               )

               bIsInput = lVariableMetadata$bIsInput

               # input variable evaluation logic is simple. 
               if ( bIsInput ) {

                  cDistribution = lVariableMetadata$cDistribution
                  nDistributionLowerBound = lVariableMetadata$nDistributionLowerBound
                  nDistributionUpperBound = lVariableMetadata$nDistributionUpperBound
                  lParameters = lVariableMetadata$lParameters

                  vnDistribution = c()

                  repeat {

                     # Add logic for new distributions here
                     vnIntermediateDistribution = switch(
                        cDistribution,
                        Normal = rnorm(
                           n = iIterations,
                           mean = lParameters$nNormalMean,
                           sd = lParameters$nNormalSD
                        ),
                        Beta = rbeta(
                           n = iIterations,
                           shape1 = lParameters$nBetaShapeAlpha,
                           shape2 = lParameters$nBetaShapeBeta
                        ),
                        Uniform = fUniformDistribution(
                           iIterations = iIterations,
                           nLowerBound = nDistributionLowerBound,
                           nUpperBound = nDistributionUpperBound
                        ),
                        Constant = rep(
                           lParameters$nConstant,
                           iIterations
                        ),
                        Empirical = fSampleFromEmpirical(
                           lReactiveValues = lReactiveValues,
                           cEmpiricalDistributionName = lParameters$cEmpiricalDistributionName,
                           iIterations = iIterations
                        )
                     )

                     if ( !cDistribution %in% c('Constant', 'Uniform') ) {

                        if ( nDistributionUpperBound != nDistributionLowerBound ) {

                           # This was wrong. You don't have to cap the
                           # the distribution, you have to generate the 
                           # samples from within the bounds
                           # vnDistribution = pmax(
                           #    nDistributionLowerBound,
                           #    pmin(
                           #       vnDistribution,
                           #       nDistributionUpperBound
                           #    )
                           # )

                           vnIntermediateDistribution = vnIntermediateDistribution[
                              vnIntermediateDistribution <= nDistributionUpperBound &
                              nDistributionLowerBound <= vnIntermediateDistribution 
                           ]

                        }

                        vnDistribution = c(
                           vnDistribution,
                           vnIntermediateDistribution
                        )

                     } else {
                        vnDistribution = vnIntermediateDistribution
                     }

                     if ( length(vnDistribution) >=  iIterations ) {

                        vnDistribution = vnDistribution[1:iIterations]
                        break

                     }

                  }

               # output variable evaluation logic
               } else {

                  cEquation = lVariableMetadata$cEquation

                  bNeedsLoop = F

                  # Get all the variable names replaced with backend names
                  # To avoid some smart alec doing things like system('rm -rf')
                  for ( iRow in seq(nrow(dtVariableNameMapping)) ) {

                     cOriginalEquation = cEquation
                     cEquation = gsub(
                        x = cEquation,
                        pattern = dtVariableNameMapping[iRow, VariableName],
                        replacement = dtVariableNameMapping[iRow, paste0(VariableBackEndName,'[iIndex]')]
                     )

                     if ( cOriginalEquation != cEquation ) {

                        if ( !dtVariableNameMapping[iRow, VariableNumber] %in% vcVariablesWhichAreConstant ) {

                           bNeedsLoop = T

                        }

                     }
                  }

                  cEquation = gsub(
                     x = cEquation,
                     pattern = cTempVariableString,
                     replacement = 'Variable'
                  )

                  # @todo Should ensure that no function ever has an overlap with any of the variable names
                  # otherwise some equations will be unnecessarily be looped and not vectorised

                  bNeedsLoop = grepl(
                     x = cEquation, 
                     pattern = dtAllowedOperations[Vectorised == F, paste(OperatorString, collapse = '|')]
                  ) & bNeedsLoop

                  # Adding default arguments

                  for ( iAddDefaultArgumentsFor in dtAllowedOperations[, which(is.na(DefaultArguments) | DefaultArguments != '' )] ) {

                     cEquation = gsub(
                        x = cEquation,
                        pattern = paste0(
                           dtAllowedOperations[iAddDefaultArgumentsFor, OperatorString],
                           '\\('
                        ),
                        replacement = paste0(
                           dtAllowedOperations[iAddDefaultArgumentsFor, OperatorString],
                           '(', 
                           dtAllowedOperations[iAddDefaultArgumentsFor, DefaultArguments],
                           ','
                        )
                     )

                  }


                  if ( bNeedsLoop ) {

                     vnDistribution = sapply(
                        1:iIterations,
                        function(iIndex) {
                            eval(parse(text = cEquation))
                        }
                     )

                  } else {

                     iIndex = 1:iIterations
                     vnDistribution = eval(parse(text = cEquation))

                     if ( length(vnDistribution) == 1 ) {
                        vnDistribution = rep(
                           vnDistribution,
                           iIterations
                        )
                     }

                  }

               }

               assign(
                  paste0('Variable', iVariableNumber),
                  vnDistribution
               )

               # Don't need to store this. Saves lots of memory.
               # lReactiveValuesPlaceholder[[paste0('Distribution', iVariableNumber)]] = vnDistribution

               # PDF and CDF broken into 100
               dtDistribution = fBucketDistributions(
                  vnDistribution
               )

               lReactiveValuesPlaceholder[[paste0('dtDistribution', iVariableNumber)]] = dtDistribution

               # Qunatiles, means, etc.
               vnQuantiles = c(0.01, 0.05, 0.50, 0.95, 0.99)

               dtVariableSummary = data.table(
                  Statistic = c(
                     'Mean',
                     'Std Dev',
                     'Worst (min)',
                     'Worst (max)',
                     paste(vnQuantiles*100, '%ile')
                  ),
                  Value = c(
                     mean(vnDistribution), 
                     sd(vnDistribution),
                     min(vnDistribution),
                     max(vnDistribution),
                     quantile(
                        vnDistribution, 
                     ) 
                  )
               )

               lReactiveValues[[paste0('dtVariableSummary', iVariableNumber)]] = dtVariableSummary

               if ( 
                  dtVariableSummary[Statistic == 'Worst (min)', Value] == dtVariableSummary[Statistic == 'Worst (max)', Value]
               ) {

                  vcVariablesWhichAreConstant = c(
                     vcVariablesWhichAreConstant,
                     lVariableMetadata$iVariableNumber
                  )

               }

               save(
                  list = paste0('Variable', iVariableNumber), 
                  file = paste0('/tmp/',paste0('Variable', iVariableNumber),'.Rdata')
               )

            }

            # Setting the flag to T so that subsequent code knows evaluation is done
            bEverythingEvaluated = T

         },
         error = function(e) {

            # bEverythingEvaluated = F
            save(list = ls(), file = '/tmp/MCFailed.Rdata')

         }

      )

      # Depending on whether the evaluation finished, return an error message or the result
      # iVariableNumber will be the loop on which it errored out
      if ( bEverythingEvaluated ) {

         return ( lReactiveValuesPlaceholder )

      } else {

         lMainNotification = list(
            cMessage = paste(
               'Problem in calculating<br/>', 
               lVariablesMetadata[[iVariableNumber]]$cVariableName,
               ifelse(
                  lVariablesMetadata[[iVariableNumber]]$bIsInput,
                  paste(
                     unlist(
                        c(
                           'Distribution:', lVariablesMetadata[[iVariableNumber]]$cDistribution,
                           '<br/>Lower Bound: ', lVariablesMetadata[[iVariableNumber]]$nDistributionLowerBound,
                           '<br/>Upper Bound: ', lVariablesMetadata[[iVariableNumber]]$nDistributionUpperBound,
                           '<br/>Parameters: ', paste(
                              names(lVariablesMetadata[[iVariableNumber]]$lParameters),
                              unlist(
                                 lVariablesMetadata[[iVariableNumber]]$lParameters
                              ),
                              collapse = ' '
                           )
                        )
                     ),
                     collapse = ''
                  ),
                  paste(
                     c(
                        'Your equation,',
                        lVariableMetadata$cEquation,
                        ', is being internally evaluated as, ',
                        cEquation,
                        '. Maybe you used special characters in variable names? Or missed an operator?'
                     ),
                     collapse = ''
                  )
               )
            ),
            cType = 'error'
         )

         return ( lMainNotification )

      }


   }

   #' Calculate order of evaluation of variables
   #' Looks at dependencies in the eqations, etc.
   #' and sequences each ariable accordingly
   fOrderOfEvaluation = function(
      lVariablesMetadata
   ) {

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'fOrderOfEvaluation',
            '\n'

         )

      )      
         
      # Trying to figure out what variables are dependent on what other variables
      dtVariableNameMapping = fMapVariableNames(lVariablesMetadata)  

      # sorting the list of variables in order of name so that if one variable
      # name is used inside another variable name then that doesn't cause problems
      #' @todo this seems to be duplicated with the validation. maybe try and reuse.   
      dtVariableNameMapping[, NameLength := -nchar(VariableName)]
      setkey(dtVariableNameMapping, NameLength)
      

      lVariablesMetadata = lapply(
         lVariablesMetadata,
         function(lVariableMetadata) {

            cat(
               file = stderr(),
               paste(
                  Sys.time(),
                  '\nfOrderOfEvaluation:',
                  lVariableMetadata$cVariableName
               )

            )

            # Removing operators and variable names
            cEquation = lVariableMetadata$cEquation

            vcUpstreamVariables = c()

            if ( !is.null(cEquation) ) {

               for ( iRow in seq(nrow(dtVariableNameMapping)) ) {

                  if ( !dtVariableNameMapping[iRow, VariableName] == lVariableMetadata$cVariableName ) {

                     cNewEquation = gsub(
                        x = cEquation,
                        pattern = dtVariableNameMapping[iRow, VariableName],
                        replacement = dtVariableNameMapping[iRow, VariableBackEndName]
                     )

                     if ( cNewEquation != cEquation ) {

                        cEquation = cNewEquation
                        vcUpstreamVariables = c(
                           vcUpstreamVariables,
                           dtVariableNameMapping[iRow, VariableName]
                        )

                     }

                  }

               }


            }

            # Artifact of strsplit
            lVariableMetadata$vcUpstreamVariables = vcUpstreamVariables

            lVariableMetadata

         }
      )

      return (lVariablesMetadata)

   }

   #' Function to write out variable metadata list as json
   #' This adds line breaks, etc. and makes it easier to read.
   #' Default writing is everything in one line and painful to read.
   fMetaDataToJSONFile = function(
      lVariablesMetadata,
      cFileName
   ) {

      write(
         "[",
         file = cFileName
      )

      for ( y in seq(length(lVariablesMetadata)) ) {

         write(
            gsub(
               toJSON(lVariablesMetadata[[y]]), 
               pattern = ',', 
               replacement = ',\n\t'
            ),
            file = cFileName,
            append = T
         )
         
         if ( y != length(lVariablesMetadata) ) {
            write(
               ',',
               file = cFileName,
               append = T
            )  
         }
         
      }

      write(
         "]",
         file = cFileName,
         append = T
      )

   }


   # UI Panel which contains the rows for the variables
   # On uploading a template, this thing is deleted and reinsterted
   uiPanelToAddVariables = wellPanel(
      style = "overflow-y:scroll; max-height: 750px",
      id = 'PanelToAddVariables'
   )

   # Having this here to allow for easy reset of the file input
   # box for the template uploads
   uiResettableFileInput = fileInput(
      inputId = 'fileInputUploadScenario',
      label = 'Upload Scenario'
   )        

   # Initialising the reactive values
   # @todo, need to add some more here which are getting initialised
   # inside the code somewhere
   lReactiveValues = reactiveValues(
      iTotalVariables = 0,
      iTotalEmpiricals = 0,
      dtvcNotification = data.table()
   )

   # Initial addition of the UI which contains the variable rows
   insertUI(
      selector = "#ControlPanel",
      where = "afterEnd",
      uiPanelToAddVariables
   )

   # Help button work
   output[['dtAllowedOperations']] = renderDataTable(
      {
         dtAllowedOperations[,
            list(
               Operator,
               Usage,
               Description
            )
         ]
      },
      options = list(
         bLengthChange = F,
         pageLength = 10
      )
   )

   observeEvent(
      input$actionButtonHelp, {

         showModal(
            modalDialog(
               title = "Help Section",
               dataTableOutput('dtAllowedOperations'),
               easyClose = TRUE,
               footer = NULL
            )
         )

      }
   )
   

   observeEvent(
      input$actionButtonHelp, {

         showModal(
            modalDialog(
               title = "Help Section",
               dataTableOutput('dtAllowedOperations'),
               easyClose = TRUE,
               footer = NULL
            )
         )

      }
   )
   


   # Where it all happens
   observeEvent(
      input$actionButtonRun, {

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'actionButtonRun initiated'
         )

      )

      progress = Progress$new(session)
      on.exit(progress$close())
      progress$set(
         message = 'Run initiated'
      )

      # Get the total number of variables that are there
      iTotalVariables = lReactiveValues$iTotalVariables

      if ( is.null(iTotalVariables) ) {
         return ( NULL )
      }

      if ( iTotalVariables == 0 ) {
         return ( NULL )
      }

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'actionButtonRun: Retrieving metadata'
         )

      )

      progress$set(
         message = 'Run: Retrieving metadata'
      )

      # Prepare the variable meta data list
      lVariablesMetadata = lapply(
         1:iTotalVariables,
         function( iVariableNumber ) {

            lVariableMetadata = list(
               iVariableNumber = iVariableNumber,
               bIsInput = fRetrieveShinyValue(
                  input = input,
                  id = "IsInput",
                  vcSuffixes = iVariableNumber
               ),
               cVariableName = fRetrieveShinyValue(
                  input = input,
                  id = "VariableName",
                  vcSuffixes = iVariableNumber
               ),
               cVariableDescription = fRetrieveShinyValue(
                  input = input,
                  id = 'VariableDescription',
                  vcSuffixes = iVariableNumber
               )
            )

            if ( lVariableMetadata$bIsInput ) {

               lVariableMetadata = append(
                  lVariableMetadata,
                  list(
                     cDistribution = fRetrieveShinyValue(
                        input = input,
                        id = 'Distribution',
                        vcSuffixes = iVariableNumber
                     ),
                     nDistributionUpperBound = fRetrieveShinyValue(
                        input = input,
                        id = 'DistributionUpperBound',
                        vcSuffixes = iVariableNumber
                     ),
                     nDistributionLowerBound = fRetrieveShinyValue(
                        input = input,
                        id = 'DistributionLowerBound',
                        vcSuffixes = iVariableNumber
                     ),
                     iSequence = 1
                  )
               )


               # Add only the relevant distribution parameters.
               if ( lVariableMetadata$cDistribution == 'Normal' ) {

                  lVariableMetadata$lParameters = list(
                     nNormalMean = fRetrieveShinyValue(
                        input = input,
                        id = 'NormalMean',
                        vcSuffixes = iVariableNumber
                     ),
                     nNormalSD = fRetrieveShinyValue(
                        input = input,
                        id = 'NormalSD',
                        vcSuffixes = iVariableNumber
                     )
                  )

               } else if ( lVariableMetadata$cDistribution == 'Beta' ) {

                  lVariableMetadata$lParameters = list(
                     nBetaShapeAlpha = fRetrieveShinyValue(
                        input = input,
                        id = 'BetaShapeAlpha',
                        vcSuffixes = iVariableNumber
                     ),
                     nBetaShapeBeta = fRetrieveShinyValue(
                        input = input,
                        id = 'BetaShapeBeta',
                        vcSuffixes = iVariableNumber
                     )
                  )

               } else if ( lVariableMetadata$cDistribution == 'Constant' ) {

                  lVariableMetadata$lParameters = list(
                     nConstant = fRetrieveShinyValue(
                        input = input,
                        id = 'ConstantValue',
                        vcSuffixes = iVariableNumber
                     )
                  )

               } else if ( lVariableMetadata$cDistribution == 'Empirical' ) {

                  lVariableMetadata$lParameters = list(
                     cEmpiricalDistributionName = fRetrieveShinyValue(
                        input = input,
                        id = 'ChosenEmpiricalDistribution',
                        vcSuffixes = iVariableNumber
                     )
                  )

               }

            } else {


               lVariableMetadata$cEquation = fRetrieveShinyValue(
                  input = input,
                  id = "textInputEquation",
                  vcSuffixes = iVariableNumber
               )

            }

            # Add the parameters needed for new distribution here

            lVariableMetadata

         }
      )

      # Trying to figure out what variables are dependent on what other variables
      dtVariableNameMapping = fMapVariableNames(lVariablesMetadata)  

      lVariablesMetadata = fOrderOfEvaluation(
         lVariablesMetadata
      )

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'actionButtonRun: Basic checks'
         )

      )

      progress$set(
         message = 'Run: Basic checks'
      )

      # Validations
      lVariablesMetadata = fValidateVariablesMetadata(
         input = input,
         lVariablesMetadata = lVariablesMetadata,
         dtAllowedOperations = dtAllowedOperations
      )

      if ( !is.null(lVariablesMetadata$cType) ) {

         lReactiveValues$lMainNotification = lVariablesMetadata
         return ( NULL )

      }

      lReactiveValues$lVariablesMetadata = lVariablesMetadata

      cat(
         file = stderr(),
         paste(
            Sys.time(),
            'actionButtonRun: Evaluating'
         )
      )

      progress$set(
         message = 'Run: Evaluating'
      )

      # Evaluation
      lReactiveValuesPlaceholder = fEvaluateVariables(
         iIterations = input$Iterations,
         lVariablesMetadata,
         dtAllowedOperations
      )

      if ( !is.null(lReactiveValuesPlaceholder$cType) ) {

         lReactiveValues$lMainNotification = lReactiveValuesPlaceholder
         return ( NULL )

      }

      for ( cName in names(lReactiveValuesPlaceholder) ) {
         lReactiveValues[[cName]] = lReactiveValuesPlaceholder[[cName]]
      }

      lReactiveValues$lMainNotification = list(
         cMessage = 'Run successful!',
         cType = 'message'
      )


   })


   # Since variable are being added, we need to dynamically handle them
   # https://gist.github.com/wch/5436415/
   for  ( iVariableNumber in seq(iRandomlyLargeNumberForVariables) ) {

      local({

         iLocalVariableNumber = iVariableNumber


         # VariableDistributionChartPDF
         output[[paste0('VariableDistributionChartPDF', iLocalVariableNumber)]] = renderPlot({

            dtDistribution = lReactiveValues[[paste0('dtDistribution', iLocalVariableNumber)]]

            if ( is.null(dtDistribution) ) {

               return ( NULL )

            }

            ggplot() +
               geom_line(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = PDF
                  )
               ) +
               geom_point(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = PDF
                  )
               ) +
               scale_y_continuous(label = percent, name = 'PDF') +
               xlab(NULL) +
               ggtitle(
                  fRetrieveShinyValue(
                     input = input,
                     id = 'VariableName',
                     vcSuffixes = iLocalVariableNumber
                  )
               )

         })



         # DistributionChartCDF
         output[[paste0('DistributionChartCDF', iLocalVariableNumber)]] = renderPlot({
            
            dtDistribution = lReactiveValues[[paste0('dtDistribution', iLocalVariableNumber)]]

            if ( is.null(dtDistribution) ) {

               return ( NULL )
               
            }

            ggplot() +
               geom_point(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = CDF
                  )
               ) +
               geom_line(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = CDF
                  )
               ) +
               scale_y_continuous(label = percent, name = 'CDF') +
               xlab(NULL) +
               ggtitle(
                  fRetrieveShinyValue(
                     input = input,
                     id = 'VariableName',
                     vcSuffixes = iLocalVariableNumber
                  )
               )

         })


         # DistributionDetails
         output[[paste0('DistributionDetails', iLocalVariableNumber)]] = renderDataTable(
            {

               dtDistribution = lReactiveValues[[paste0('dtDistribution', iLocalVariableNumber)]]

               if ( is.null(dtDistribution) ) return ( NULL )

               dtDistribution[, list(Values, PDF, CDF)]


            },
            options = list(
               bLengthChange = F,
               pageLength = 10,
               searching = F
            )
         )
           

         # DistributionSummary
         output[[paste0('DistributionSummary', iLocalVariableNumber)]] = renderDataTable(
            {

               dtVariableSummary = lReactiveValues[[paste0('dtVariableSummary', iLocalVariableNumber)]]

               dtVariableSummary


            },
            options = list(
               bLengthChange = F,
               paging = FALSE
            )
         )
         
      })

   }



   # To show notifications, I basically show a pop up
   # and also retain the message on screen for the user
   # to refer to again
   output[['vcNotification']] = renderUI({

      lMainNotification = lReactiveValues$lMainNotification

      if(is.null(lMainNotification)) {
         return(NULL)
      }

      vcHTMLMessage = HTML(
         paste(
            lMainNotification$cMessage,
            collapse = '<br/>'
         )
      )

      # Customising the appearance depending on the type of message
      if ( lMainNotification$cType == 'error') { 

         showModal(
            modalDialog(
               title = "Error!",
               vcHTMLMessage,
               easyClose = TRUE,
               footer = NULL
            )
         )

      } else if ( lMainNotification$cType == 'message') { 

         showModal(
            modalDialog(
               title = NULL,
               HTML(
                  paste(
                     lMainNotification$cMessage,
                     collapse = '<br/>'
                  )
               ),
               easyClose = TRUE,
               footer = NULL
            )
         )

      }

      lReactiveValues$lMainNotification = NULL

      return (
         vcHTMLMessage
      )

   })


   # adding UI for new variable
   observeEvent(
      input$actionButtonAddVariable, {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'actionButtonAddVariable'
            )
         )

         iVariableNumber = lReactiveValues$iTotalVariables + 1

         vcEmpiricalDistributionNames = isolate(
            {
               fRetrieveShinyValue(
                  input = lReactiveValues,
                  id = 'EmpiricalDataName',
                  vcSuffixes = 1:iRandomlyLargeNumberForEmpirical
               )
            }
         )

         vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
            !is.null(vcEmpiricalDistributionNames)
         ]

         vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
            !is.na(vcEmpiricalDistributionNames)
         ]

         insertUI(
            selector = "#PanelToAddVariables",
            where = "afterBegin",
            # ui = 
            ui = fCreateVariableAdditionUI(
               iVariableNumber = iVariableNumber,
               vcEmpiricalDistributionNames = vcEmpiricalDistributionNames
            )
         )

         lReactiveValues$iTotalVariables = iVariableNumber
         
      }
   )



   # Compiling the variable summaries to prepare a run summary
   output[['dtRunLayout']] = renderDataTable(
      {

         lVariablesMetadata = isolate(lReactiveValues$lVariablesMetadata)

         dtLaidoutSummary = rbindlist(
            lapply(
               lVariablesMetadata,
               function( lVariableMetadata ) {
                  
                  dtReturn = data.table(
                     VariableName = lVariableMetadata$cVariableName
                  )
                  
               
                  if ( !is.null(lVariableMetadata$cEquation) ) {
                     
                     cContent = lVariableMetadata$cEquation
                     
                  } else {
                     
                     cContent = paste0(
                        c(
                           lVariableMetadata$cDistribution,
                           paste0('[',lVariableMetadata$nDistributionUpperBound,',',lVariableMetadata$nDistributionLowerBound,']'),
                           sapply(
                              seq(length(lVariableMetadata$lParameters)),
                              function( iParmIndex ) {
                                 paste0(names(lVariableMetadata$lParameters)[iParmIndex], ':', lVariableMetadata$lParameters[iParmIndex] ) 
                              }
                           )
                        ),
                        collapse = '<br>'
                     )
                     
                     lVariableMetadata$cEquation
                     
                  }
                  
                  dtReturn[, Content := cContent]
                  
               }
            )
         )

         dtLaidoutSummary[,
           Variable := VariableName
         ]

         dtLaidoutSummary[,
           Type := ''
         ]

         dtLaidoutSummary[
            grepl(x = VariableName, pattern = '_'), 
            Variable := paste(rev(rev(unlist(strsplit(VariableName, '_')))[-1]), collapse = '_'), 
            VariableName
         ]

         dtLaidoutSummary[
            grepl(x = VariableName, pattern = '_'), 
            Type := rev(unlist(strsplit(VariableName, '_')))[1], 
            VariableName
         ]

         dtLaidoutSummary = dcast(
            dtLaidoutSummary, 
            Type ~ Variable, 
            value.var = 'Content'
         )

         dtLaidoutSummary

      },         
      extensions = c('FixedColumns',"FixedHeader"),
      escape = F,
      options = list(
         bLengthChange = F,
         pageLength = 10,
         scrollX = T,
         fixedColumns = list(leftColumns = 2)
      )
   )


   # Compiling the variable summaries to prepare a run summary
   output[['dtRunSummary']] = renderDataTable(
      {

         # just to trigger reactivty
         lVariablesMetadata = lReactiveValues$lVariablesMetadata

         if ( is.null(lVariablesMetadata) ) {
            return ( NULL )
         }

         # Getting each variable summary
         dtRunSummary = rbindlist(
            lapply(
               lVariablesMetadata,
               function( lVariableMetadata ) {

                  dtVariableSummary = lReactiveValues[[paste0('dtVariableSummary',lVariableMetadata$iVariableNumber)]]
                  dtVariableSummary[, Equation := lVariableMetadata$cEquation]
                  dtVariableSummary[, VariableName := lVariableMetadata$cVariableName]

                  dtVariableSummary

               }
            ),
            fill = T
         )

         # If there are no output variables then placeholder equation column
         if ( !'Equation' %in% colnames(dtRunSummary)) {

            dtRunSummary[, Equation := '']

         }

         # reshaping
         dtRunSummary = dcast(
            dtRunSummary,
            VariableName+Equation~Statistic,
            value.var = 'Value'
         )


         # ... and reodering
         vcCols = colnames(dtRunSummary)

         setcolorder(
            dtRunSummary,
            c(
               'VariableName',
               'Equation',
               vcCols[
                  grepl(
                     x = vcCols,
                     pattern = 'Mean|Std|Worst'
                  )
               ],
               vcCols[
                  grepl(
                     x = vcCols,
                     pattern = '%ile'
                  )
               ]
            )
         )

         lReactiveValues$dtRunSummary = dtRunSummary

         dtRunSummary
         
      },
      extensions = c('FixedColumns',"FixedHeader"),
      options = list(
         bLengthChange = F,
         pageLength = 10,
         scrollX = T,
         fixedColumns = list(leftColumns = 2)
      )
   )

   # Button that puts the run summary up
   observeEvent(
      input$actionButtonViewSummary, {


         showModal(
            modalDialog(
               title = "Run summary",
               h4('Formulation summary'),
               dataTableOutput('dtRunLayout'),
               hr(),
               downloadButton(
                  outputId = 'downloadSummary',
                  label = "Download Summary"
               ),
               h4('Results summary'),
               dataTableOutput('dtRunSummary'),
               easyClose = TRUE,
               footer = NULL,
               size = 'l'
            )
         )


      }
   )


   # Downloading a summary for sharing, etc.
   output$downloadSummary = downloadHandler(

      filename = function() {

         paste0(
            "RunSummary-",
            strftime(Sys.time(), '%Y%m%d%H%M'),
            ".csv"
         )

      },
      content = function( file ) {

         dtRunSummary = lReactiveValues$dtRunSummary
         if ( is.null(dtRunSummary) ) {
            return ( NULL )
         }

         write.csv(
            dtRunSummary,
            file = file,
            row.names = F,
            na = ''
         )

      }

   )


   # Downloading a scenario for later use
   output$downloadScenario = downloadHandler(

      filename = function() {

         paste0(
            "Scenario-",
            strftime(Sys.time(), '%Y%m%d%H%M'),
            ".json"
         )

      },
      content = function( file ) {

         lVariablesMetadata = lReactiveValues$lVariablesMetadata

         fMetaDataToJSONFile(
            lVariablesMetadata = lVariablesMetadata,
            cFileName = file
         )


      }

   )

   # Upon uploading a scenario -
   # Code is very similar to the logic against actionButtonRun
   observe(
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Uploading a scenario'
            )
         )

         fileInputUploadScenario = input$fileInputUploadScenario

         if ( is.null(fileInputUploadScenario) ) {

            return ( NULL )

         }

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Upload initiated'
         )

         lVariablesMetadata = readLines(
            fileInputUploadScenario$datapath
         )

         progress$set(
            message = 'Upload: Basic checks'
         )

         bUploadValid = F

         # Validate the uploaded scenario
         tryCatch(
            {

               cat(
                  file = stderr(),
                  paste(
                     Sys.time(),
                     'fileInputUploadScenario: Basic checks'
                  )

               )

               lVariablesMetadata = fromJSON(paste(lVariablesMetadata, collapse = ''))

               # initialising the other metadata ( which is optional )
               lVariablesMetadata = lapply(
                  seq(length(lVariablesMetadata)),
                  function( iIndex ) {

                     lVariableMetadata = lVariablesMetadata[[iIndex]]

                     lVariableMetadata = append(
                        lVariableMetadata,
                        list(
                           iSequence = 1, 
                           bIsInput = is.null(lVariableMetadata$cEquation), 
                           iVariableNumber = iIndex
                        )
                     )

                     return ( lVariableMetadata )

                  }
               )

               cat(
                  file = stderr(),
                  paste(
                     Sys.time(),
                     'fileInputUploadScenario: Order of evaluation'
                  )

               )

               lVariablesMetadata = fOrderOfEvaluation(
                  lVariablesMetadata
               )

               cat(
                  file = stderr(),
                  paste(
                     Sys.time(),
                     'fileInputUploadScenario: Validating variable metadata'
                  )

               )

               lVariablesMetadata = fValidateVariablesMetadata(
                  input = input,
                  lVariablesMetadata = lVariablesMetadata,
                  dtAllowedOperations = dtAllowedOperations
               )

               if ( !is.null(lVariablesMetadata$cType) ) {

                  lReactiveValues$lMainNotification = lVariablesMetadata
                  return ( NULL )

               }

               bUploadValid = T


            },
            error = function(e) {


               showModal(
                  modalDialog(
                     title = "Error!",
                     paste0(
                        "There is something wrong with the uploaded file. Contact the author."
                     ),
                     easyClose = TRUE,
                     footer = NULL
                  )
               )

               lVariablesMetadata = NULL

               return ( NULL )

            }

         )


         if ( !bUploadValid ) {
            return ( NULL )
         }

         lReactiveValues$iTotalVariables = length(lVariablesMetadata)


         # removing all the variable rows and adding rows for the new variables
         removeUI(
            selector = paste0(
               "#PanelToAddVariables"
            )
         )

         insertUI(
            selector = "#ControlPanel",
            where = "afterEnd",
            uiPanelToAddVariables
         )

         for ( iVariableNumber in seq(isolate(lReactiveValues$iTotalVariables)) ) {

            removeUI(
               selector = paste0(
                  "#",
                  paste0(
                     'PanelFor', 
                     iVariableNumber
                  )
               )
            )

         }

         # Getting the empirical distributions
         vcEmpiricalDistributionNames = isolate(
            {
               fRetrieveShinyValue(
                  input = isolate(lReactiveValues),
                  id = 'EmpiricalDataName',
                  vcSuffixes = 1:iRandomlyLargeNumberForEmpirical
               )
            }
         )

         vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
            !is.null(vcEmpiricalDistributionNames)
         ]

         vcEmpiricalDistributionNames = vcEmpiricalDistributionNames[
            !is.na(vcEmpiricalDistributionNames)
         ]


         # Adding UI for each new variable
         for ( iVariableNumber in seq(length(lVariablesMetadata))) {

            insertUI(
               selector = "#PanelToAddVariables",
               where = "afterBegin",
               # ui = 
               ui = fCreateVariableAdditionUI(
                  iVariableNumber = iVariableNumber,
                  bIsInput = lVariablesMetadata[[iVariableNumber]]$bIsInput,
                  cVariableName = lVariablesMetadata[[iVariableNumber]]$cVariableName,
                  cVariableDescription = lVariablesMetadata[[iVariableNumber]]$cVariableDescription,
                  cDistribution = lVariablesMetadata[[iVariableNumber]]$cDistribution,
                  lParameters = lVariablesMetadata[[iVariableNumber]]$lParameters,
                  # nNormalMean = lVariablesMetadata[[iVariableNumber]]$lParameters$nNormalMean,
                  # nNormalSD = lVariablesMetadata[[iVariableNumber]]$lParameters$nNormalSD,
                  # nBetaShapeAlpha = lVariablesMetadata[[iVariableNumber]]$lParameters$nBetaShapeAlpha,
                  # nBetaShapeBeta = lVariablesMetadata[[iVariableNumber]]$lParameters$nBetaShapeBeta,
                  # nConstant = lVariablesMetadata[[iVariableNumber]]$lParameters$nConstant,
                  vcEmpiricalDistributionNames = vcEmpiricalDistributionNames,
                  nDistributionUpperBound = lVariablesMetadata[[iVariableNumber]]$nDistributionUpperBound,
                  nDistributionLowerBound = lVariablesMetadata[[iVariableNumber]]$nDistributionLowerBound,
                  cEquation = lVariablesMetadata[[iVariableNumber]]$cEquation
               )
            )

         }

         # Committing
         lReactiveValues$lVariablesMetadata = lVariablesMetadata

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'fileInputUploadScenario: Evaluating'
            )
         )

         progress$set(
            message = 'Upload: Evaluating'
         )

         # Evaluating the scenario
         lReactiveValuesPlaceholder = fEvaluateVariables(
            iIterations = input$Iterations,
            lVariablesMetadata,
            dtAllowedOperations
         )

         # save(
         #    list = 'lVariablesMetadata',
         #    file = '/tmp/lVariablesMetadata.Rdata'
         # )

         if ( !is.null(lReactiveValuesPlaceholder$cType) ) {

            lReactiveValues$lMainNotification = lReactiveValuesPlaceholder
            return ( NULL )

         }

         for ( cName in names(lReactiveValuesPlaceholder) ) {
            lReactiveValues[[cName]] = lReactiveValuesPlaceholder[[cName]]
         }

         # Updating other details
         lReactiveValues$iTotalVariables = length(lVariablesMetadata)

         lReactiveValues$lMainNotification = list(
            cMessage = 'Run successful!',
            cType = 'message'
         )

         lReactiveValues$posixctResetFileInput = Sys.time()

      }
   )

   # The file input thing doesn't reset and it's a pain
   output[['uiResettableFileInput']] = renderUI({

      lReactiveValues$posixctResetFileInput
      uiResettableFileInput

   })



   # Adding empirical distributions
   observe(
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'Adding empirical distributions'
            )
         )

         fileInputEmpiricalData = input$fileInputEmpiricalData
         iTotalEmpiricals = isolate(lReactiveValues$iTotalEmpiricals)

         if ( is.null(fileInputEmpiricalData) ) {

            return ( NULL )

         }

         progress = Progress$new(session)
         on.exit(progress$close())
         progress$set(
            message = 'Upload initiated'
         )

         # Reading the CSV
         #' @todo trycatch this
         dtEmpiricalData = fread(fileInputEmpiricalData$datapath)

         # Keep only numeric columns
         #' @todo there must be cases were categorical / ordinal columns
         #' are required, no?
         vcNumericColumns = colnames(
            dtEmpiricalData
         )[
            sapply(
               dtEmpiricalData, 
               class
            ) %in% 
            c('numeric', 'integer')
         ]

         # Loop through each column, add UI element, distribution, etc.
         for ( iNumericColumn in seq(length(vcNumericColumns)) ) {

            # Ensure naming clashes don't happen
            cEmpiricalDataName = vcNumericColumns[iNumericColumn]

            if ( iNumericColumn + iTotalEmpiricals > 1) {

               vcEmpiricalDataNames = sapply(
                  1:(iNumericColumn + iTotalEmpiricals - 1),
                  function(x) {
                     lReactiveValues[[paste0('EmpiricalDataName',x)]]
                  }
               )

               if ( cEmpiricalDataName %in% vcEmpiricalDataNames) {

                  cEmpiricalDataName = paste0(
                     cEmpiricalDataName,
                     '_',
                     iNumericColumn + iTotalEmpiricals
                  )

               }

            }

            # Commit
            lReactiveValues[[paste0('EmpiricalDataName', iNumericColumn + iTotalEmpiricals)]] = cEmpiricalDataName

            #' @todo try and abstract details away and store a much smaller object?
            lReactiveValues[[paste0('EmpiricalData', iNumericColumn + iTotalEmpiricals)]] = unlist(

               dtEmpiricalData[,
                  vcNumericColumns[iNumericColumn],
                  with = F
               ]

            )

            # Insert UI element
            insertUI(
               selector = "#PanelToAddEmpiricalDistributions",
               where = "afterBegin",
               column(
                  width = 12,
                  h5(cEmpiricalDataName),
                  plotOutput(
                     paste0('EmpiricalDistributionChartPDF', iNumericColumn + iTotalEmpiricals)
                  )
               )
            )


         }

         lReactiveValues$iTotalEmpiricals = iTotalEmpiricals + length(vcNumericColumns)

      }
   )


   # Generate charts for the empirircal distributions
   for  ( iVariableNumber in seq(iRandomlyLargeNumberForEmpirical) ) {

      local({

         iLocalVariableNumber = iVariableNumber

         # VariableDistributionChartPDF
         output[[paste0('EmpiricalDistributionChartPDF', iLocalVariableNumber)]] = renderPlot({

            # Retrieve distribution details
            vnDistribution = lReactiveValues[[paste0('EmpiricalData', iLocalVariableNumber)]]


            if ( is.null(vnDistribution) ) {

               return ( NULL )

            }

            cEmpiricalDistributionName = lReactiveValues[[paste0('EmpiricalDataName', iLocalVariableNumber)]]

            # Calculate PDF, CDF, etc.
            dtDistribution = fBucketDistributions(
               vnDistribution
            )

            # Sned plot
            ggplot() +
               geom_line(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = PDF
                  )
               ) +
               geom_point(
                  data = dtDistribution,
                  aes(
                     x = Values,
                     y = PDF
                  )
               ) +
               scale_y_continuous(label = percent, name = 'PDF') +
               xlab(NULL) +
               ggtitle(
                  cEmpiricalDistributionName
               )

         })

      })

   }


   # Displaying the empirical distributions box
   observeEvent(
      input$actionButtonUploadEmpiricalDistributions,
      {

         cat(
            file = stderr(),
            paste(
               Sys.time(),
               'actionButtonUploadEmpiricalDistributions'
            )
         )

         # Show modal dialog which, at this point, has no distributions
         showModal(
            modalDialog(
               title = "Empirical Distributions",
               fileInput(
                  inputId = 'fileInputEmpiricalData',
                  label = 'Upload data here'
               ),
               # textOutput('empdist'),
               wellPanel(
                  style = "overflow-y:scroll; max-height: 750px",
                  id = 'PanelToAddEmpiricalDistributions'
               ),
               easyClose = TRUE,
               footer = NULL
            )
         )

         # Loop through all distributions and add plot to modal box one by one
         iTotalEmpiricals = lReactiveValues$iTotalEmpiricals

         if ( is.null(iTotalEmpiricals) ) {
            
            return ( NULL )

         }

         if ( iTotalEmpiricals == 0 ) {
            
            return ( NULL )

         }

         for ( iNumericColumn in seq(iTotalEmpiricals) ) {

            insertUI(
               selector = "#PanelToAddEmpiricalDistributions",
               where = "afterBegin",
               column(
                  width = 12,
                  h5(lReactiveValues[[paste0('EmpiricalDataName', iNumericColumn)]]),
                  plotOutput(
                     paste0('EmpiricalDistributionChartPDF', iNumericColumn)
                  )
               )
            )

         }

      }
   )
   
   # updating the empirical distributions in the options
   # of each variable as and when new ones get added
   observe(
      {

         vcEmpiricalDistributionNames = fRetrieveShinyValue(
            input = lReactiveValues,
            id = 'EmpiricalDataName',
            vcSuffixes = 1:iRandomlyLargeNumberForEmpirical
         )

         for  ( iVariableNumber in seq(iRandomlyLargeNumberForVariables) ) {

            updateSelectizeInput(
               session = session,
               inputId = paste0(
                  "ChosenEmpiricalDistribution", 
                  iVariableNumber
               ),
               choices = vcEmpiricalDistributionNames
            )

         }


      }
   )

   
   # populating the list of available scenarios
   observe({

      # if new scenario has been saved then
      # it needs to be made available
      input$actionButtonSaveScenarioOnCloud

      vcScenariosOnCloud = list.files(
         paste0(
            lArchitectureParms$cAtherDataLocation,
            '/Processed/MonteCarloScenarios'
         ),
         full.names = T
      )

      names(vcScenariosOnCloud) = gsub(
         vcScenariosOnCloud,
         pattern = '.*/',
         replacement = ''
      )

      updateSelectizeInput(
         session = session,
         inputId = 'selectizeInputLoadScenarioFromCloud',
         choices = vcScenariosOnCloud
      )

   })
    

    observeEvent(
      input$actionButtonSaveScenarioOnCloud, {

         lVariablesMetadata = lReactiveValues[['lVariablesMetadata']]
         textInputSaveScenarioOnCloud = input[['textInputSaveScenarioOnCloud']]

         cFileName = paste0(
            lArchitectureParms$cAtherDataLocation,
            '/Processed/MonteCarloScenarios/',
            textInputSaveScenarioOnCloud
         )

         if ( file.exists(cFileName) ) {

            showModal(
               modalDialog(
                  title = "Warning",
                  h6('File overwrites not allowed. Please rename.'),
                  easyClose = TRUE,
                  footer = NULL
               )
            )

         } else {

            fMetaDataToJSONFile(
               lVariablesMetadata = lVariablesMetadata,
               cFileName = cFileName
            )

            showModal(
               modalDialog(
                  h6('Saved.'),
                  easyClose = TRUE,
                  footer = NULL
               )
            )

         }

      }
   )


}