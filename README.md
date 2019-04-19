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

You can add your own functions, distributions, and configure various other things.

I recommend using the application first so you have some idea of how it works. Some of the terms used in these instructions will be much easier to understand that way.

Instructions to change configurations can be found in Global.R or the Configuring button in the UI.