An R-Shiny app to run quick Monte Carlo simulations -

A blank input variable -
![Adding a variable](/Screenshots/010_Add_Variable.png?raw=true)

Adding empirical distributions -
![Adding empirical distributions](/Screenshots/020_Empirical_Distributions.png?raw=true)

A very simple run -
![Simple run](/Screenshots/030_Simple_Run.png?raw=true)

Result summary of the simple run - 
![Summary](/Screenshots/040_Summary.png?raw=true)

Another input variable - 
![Simple input variable](/Screenshots/050_Input_Variable_Simple.png?raw=true)

An output variable -
![Output variable examples](/Screenshots/060_Output_Variables.png?raw=true)


### Getting Started

#### Installing R
https://cran.r-project.org/

#### Installing RStudio
https://www.rstudio.com/products/RStudio/#Desktop

#### Installing the tool
Open Rstudio and run the following command in the console - 
`install.packages(c('shiny','shinydashboard','DT','ggplot2','scales','data.table','rjson'))`

The file Dashboard/Global.R  has a bunch of parameters. The only one you NEED to modify to get started is `lArchitectureParms$cRootDirectory`. The others can be left as they are. This should be enough
to let you run this on your local machine or on a VM. More instructions can be found in the help
and explanations button in the tool.

#### Running the tool
Opening Dashboard/Global.R in RStudio should cause a Run App button to show up towards the top left. Click that to, well, run the app.

### Advanced

I recommend using the application so you have some idea of how it works. Some of the terms used in these instructions will be much easier to understand that way.

There are some configurations that can be traced from Global.R.

#### Operation Details

`/Metadata/Operations.csv` has four columns: Operator, OperatorString, Usage, and Description. Except for OperatorString, the other three are self explanatory and also get used in the Help button entry. OperatorString is the regex pattern that should be searched for in an output equation to parse it and calculate it correctly.

#### Adding distributions

This section needs better instructions and a simplified process. For now, ctrl+F for `normal` in server.r and add corresponding bits of code for any new distribution you'd like to add.