library(data.table)
library(rjson)
library(sensitivity)

# There is a bit of code that saves the the distribution from each variable
# into /tmp. Just copy all those files into tbe below folder
cDirectoryWithData = '/home/ask/Desktop/MCRun/'
# Saved scenario from the tool
cScenarioFileName = '/home/ask/Downloads/Scenario-201903061358.json'
# The variable we want sensitivites computed for
cOutputName = 'Time'
   
dtDataset = data.table()

for(
   
   cFileName in list.files(
      cDirectoryWithData,
      full.names = T,
      recursive = T
   )
   
) {
   
   load(cFileName)
   
   cObjectName = gsub(
      x = cFileName, 
      pattern = '.*/|\\..*', 
      replacement = ''
   )
   
   if ( nrow(dtDataset) == 0 ) {
      
      dtDataset = data.table(
         Temp = get(cObjectName)
      )
      
   } else {
      
      dtDataset[,
         Temp := get(cObjectName)
      ]
      
   }
   
   setnames(
      dtDataset,
      'Temp',
      cObjectName
   )
   
   rm(
      list = cObjectName
   )
   
}

lScenario <- fromJSON(file=cScenarioFileName)

dtVariableDetails = data.table(
    VariableNumber = unlist(sapply(lScenario, function(x) {x$iVariableNumber})),
    VariableName = unlist(sapply(lScenario, function(x) {x$cVariableName})),
    Input = unlist(sapply(lScenario, function(x) {x$bIsInput}))
)

dtVariableDetails[
   Input == T,
   Distribution := unlist(
      sapply(
         VariableNumber, 
         function(x) {
            
            lScenario[[x]]$cDistribution
            
         }
      )
   )
]

dtVariableDetails[
   Input == F,
   Equation := unlist(
      sapply(
         VariableNumber, 
         function(x) {
            
            lScenario[[x]]$cEquation
            
         }
      )
   )
]

dtVariableDetails[, 
   Equation := gsub(
      x = Equation,
      pattern = '\t',
      replacement = ''
   )
]

setnames(
   dtDataset,
   dtVariableDetails[, paste0('Variable', VariableNumber)],
   dtVariableDetails[, VariableName]
)

setcolorder(
   dtDataset,
   dtVariableDetails[, VariableName]
)





# Spearman's correlation
vnCorrelations = sapply(
   # dtVariableDetails[Input == T, VariableName],
   lScenario[[dtVariableDetails[VariableName == cOutputName, VariableNumber]]]$vcUpstreamVariables,
   function ( cInputVariableName ) {
      
      cor(
         dtDataset[, cInputVariableName, with = F],
         dtDataset[, cOutputName, with = F]
      )
      
   }
)

# This is what Crystal Ball is doing as contribution to variance :|
# https://docs.oracle.com/cd/E12825_01/epm.111/cb_user/frameset.htm?ch07s04s03.html
vnCorrelations^2 / sum(vnCorrelations^2, na.rm = T)

if ( F ) {
   
   # Need to generate this. Hard coded for now.
   
   
   dtInputVariables = dtVariableDetails[
      VariableName %in% lScenario[[
         dtVariableDetails[
            VariableName == cOutputName,
            VariableNumber
         ]
      ]]$vcUpstreamVariables
   ][
      order(VariableNumber)
   ]
   
   cEquation = 'Variable10*Variable11*log( (Variable6+Variable10*Variable14)/ ( Variable10*Variable14+Variable6* Variable3 / (Variable3+Variable8) - Variable13 ) )'
   
   cEquationForFunction = gsub(
       x = cEquation,
       pattern = 'Variable([[:digit:]]*)',
       replacement = 'X[,\\1]'
   )
   
   for ( cVariableName in dtInputVariables[Distribution == 'Constant', VariableName] ) {
   
      cEquationForFunction = gsub(
         x = cEquationForFunction,
         pattern = paste0(
            'X\\[,',
            dtInputVariables[
               VariableName == cVariableName, 
               VariableNumber
            ], 
            '\\]'
         ),
         replacement = lScenario[[
            dtInputVariables[
               VariableName == cVariableName, 
               VariableNumber
            ]
         ]]$lParameters$nConstant
      )
      
   }
   
   dtInputVariables = dtInputVariables[is.na(Distribution) | Distribution != 'Constant']
   
   for ( cVariableName in dtInputVariables[, VariableName] ) {
      
      print(cVariableName)
      
      cEquationForFunction = gsub(
         x = cEquationForFunction,
         pattern = paste0(
            '\\[,',
            dtInputVariables[
               VariableName == cVariableName, 
               VariableNumber
            ], 
            '\\]'
         ),
         replacement = paste0(
            '[,',
            dtInputVariables[, 
               which(VariableName == cVariableName)
            ], 
            ']'
         )
      )
      
      print(cEquationForFunction)
      
   }
   
      
   eval(
      parse(
         text = paste0(
            "fEquationForSobol = function ( X ) {",
            cEquationForFunction,
            "}"
         )
      )
   )
   
   
   sobol(
      model = fEquationForSobol, 
      X1 = as.matrix(
         dtDataset[
            1:1000, 
            dtInputVariables[, VariableName], 
            with = F
         ]
      ), 
      X2 = as.matrix(
         dtDataset[
            1001:2000, 
            dtInputVariables[, VariableName], 
            with = F
         ]
      ), 
      order = 2, 
      nboot = 100, 
      conf = 0.95
   )
   
   
   fConvertEquation = function (
      bFromVariableNames = T,
      cEquation
   ) {
      
      # gsub(
      #    cEquation,
      #    
      # )
      
   }
   
   dtVariableDetails[VariableName == cOutputName, Equation]

}