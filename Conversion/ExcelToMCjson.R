# t15 points to 16
# bc13 has two columns why

rm(list = ls())








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













library(data.table)
library(rjson)

dtStackFormulae = fread('/home/adityakothari/angular offset pulley 1 to pulley 2 mid frame.csv')





fTranslateFormula = function( vcFormulae ) {

   # vcFormulae = dtStackFormulae[, Formula]




   # normal dist
   vcFormulae = gsub(
      x = vcFormulae,
      pattern = 'norm.inv\\(rand\\(\\),',
      replacement = 'rnorm\\('
   )

   vcFormulae = gsub(
      x = vcFormulae,
      pattern = 'rnorm\\((.*),(.*)\\)',
      replacement = 'rnorm(mean = \\1, sd = \\2)'
   )





   # Accomodates only one range right now
   viFormulaeWithRange = grepl(
      x = vcFormulae,
      pattern = '\\:'
   )

   vcFormulaeWitHrange = vcFormulae[viFormulaeWithRange]


   vcFormulaeWitHrange = sapply(
      vcFormulaeWitHrange,
      function( cFormulaWithRange ) {
         
         vcFormulaWithRange = unlist(strsplit(cFormulaWithRange, '\\('))
         
         viFormulaWithRangeWithRange = grep(
            vcFormulaWithRange,
            pattern = ':'
         )
         
         vcFormulaWithRange[viFormulaWithRangeWithRange] = sapply(
            vcFormulaWithRange[viFormulaWithRangeWithRange],
            function(cFormulaWithRangeWithRange) {
               
            
               vcRange = gsub(
                  x = cFormulaWithRangeWithRange,
                  pattern = '(.*):(.*)\\).*',
                  replacement = '\\1 \\2'
               )
            
               viSameCellRanges = sapply(
                  lapply(
                     strsplit(vcRange, ' '),
                     unique
                  ), 
                  length
               ) == 1
            
               if ( any( !viSameCellRanges ) ) {
            
                  stop(
                     paste0(
                        'Cannot able to ranges:',
                        paste(
                           cFormulaWithRangeWithRange[!viSameCellRanges],
                           collapse = ','
                        )
                     )
                  )
            
                  return ( NULL )
            
               }
            
               cFormulaWithRangeWithRange = gsub(
                  cFormulaWithRangeWithRange,
                  pattern = '(.*):(.*)\\)(.*)',
                  replacement = '\\1\\)\\3'
               )
               
               
               
            }
         )
         
         vcFormulaWithRange = paste(
            vcFormulaWithRange,
            collapse = '('
         )
         
         vcFormulaWithRange
         
      }
   )

   names(vcFormulaeWitHrange) = NULL


   vcFormulae[viFormulaeWithRange] = vcFormulaeWitHrange





   # if condition
   vcFormulae = gsub(
      x = vcFormulae,
      pattern = 'if\\(',
      replacement = 'ifelse\\('
   )




   return ( vcFormulae )


}


fCleanNames = function( vcNames ) {

   vcNames = make.names(vcNames)
   vcNames = gsub(x = vcNames, pattern = '\\.+', replacement = '\\.')
   vcNames = gsub(x = vcNames, pattern = '\\.$', replacement = '')

   return ( vcNames )
}















setnames(
   dtStackFormulae,
   fCleanNames(colnames(dtStackFormulae))
)

if ( dtStackFormulae[, length(unique(Variable)) != .N] ) {

   stop('Variable is being repeated')

}

dtStackFormulae[, Variable := fCleanNames(Variable)]

dtVariableMapping1 = data.table(
   Variable = colnames(dtStackFormulae)[-1]
)

vcLabels = letters

repeat {
   
   if ( nrow(dtVariableMapping1) <= (length(vcLabels) + 1) ) {
      break
   }
      
   vcLabels = apply(
      expand.grid(
         vcLabels,
         c('', letters)
      ),
      1,
      function(x) {
         paste0(x[2], x[1])
      }
   )
   
}

dtVariableMapping1[, Labels := vcLabels[2:(.N+1)]]







dtVariableMapping2 = data.table(
   Variable = dtStackFormulae[, Variable],
   Labels = seq(nrow(dtStackFormulae)) + 1
)

dtVariableMapping1[, k := 'k']
dtVariableMapping2[, k := 'k']

dtVariableMapping = merge(
   dtVariableMapping1,
   dtVariableMapping2,
   'k',
   allow.cartesian = T
)

dtVariableMapping[, Variable := paste0(Variable.x, '_', Variable.y)]
dtVariableMapping[, Labels := paste0(Labels.x, Labels.y)]

dtVariableMapping[, Variable.x := NULL]
dtVariableMapping[, Variable.y := NULL]
dtVariableMapping[, Labels.x := NULL]
dtVariableMapping[, Labels.y := NULL]
dtVariableMapping[, k := NULL]


dtVariableMapping[, Variable := fCleanNames(vcNames = Variable)]










dtStackFormulae = rbindlist(
   lapply(
      seq(nrow(dtStackFormulae)),
      function( iType ) {
         rbindlist(
            lapply(
               seq(ncol(dtStackFormulae) - 1) + 1,
               function( iHeading ) {

                  data.table(
                     Variable = paste0(
                        colnames(dtStackFormulae)[iHeading],
                        '_', 
                        dtStackFormulae[iType, Variable]
                     ),
                     Formula = tolower(unlist(dtStackFormulae[iType])[iHeading])
                  )

               }
            )
         )
      }
   )
)

dtStackFormulae = dtStackFormulae[!is.na(Formula)]
dtStackFormulae = dtStackFormulae[Formula != '']

dtStackFormulae[, Formula := gsub(x = Formula, pattern = '\\$', replacement = '')]

dtStackFormulae[, Formula := fTranslateFormula(Formula)]














dtVariableMapping[, LabelLength := nchar(Labels)]
setkey(
   dtVariableMapping,
   LabelLength
)
dtVariableMapping = dtVariableMapping[nrow(dtVariableMapping):1]
dtVariableMapping[, LabelLength := NULL]
for ( iVariableNumber in seq(nrow(dtVariableMapping)) ) {

   dtStackFormulae[,
      Formula := gsub(
         x = Formula,
         pattern = dtVariableMapping[iVariableNumber, Labels],
         replacement = dtVariableMapping[iVariableNumber, Variable]
      )
   ]

}













setkey(
   dtStackFormulae,
   Variable
)

lVariablesMetadata = lapply(
   seq(nrow(dtStackFormulae)),
   function ( iVariableNumber ) {

      dtStackFormula = dtStackFormulae[iVariableNumber]

      lElement = list()

      if ( !is.na(as.numeric(dtStackFormula[,Formula])) ) {

         lElement = list(
            cVariableName = dtStackFormula[, Variable],
            cVariableDescription = '',
            cDistribution = "Constant",
            nDistributionUpperBound = 0,
            nDistributionLowerBound = 0,
            lParameters = list(nConstant = as.numeric(dtStackFormula[, Formula]))
         )

      } else {


         lElement = list(
            cVariableName = dtStackFormula[, Variable],
            cVariableDescription = '',
            cEquation = dtStackFormula[, Formula]
         )

      }

      return ( lElement ) 

   }
)

fMetaDataToJSONFile(
   lVariablesMetadata = lVariablesMetadata,
   cFileName = '/home/adityakothari/something.json'
)