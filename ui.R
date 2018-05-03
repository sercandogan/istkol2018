library(shiny)
library(shinydashboard)



# Define UI for dashboard
shinyUI(
  dashboardPage(skin = "blue",
    dashboardHeader(
      title = "istkol2018"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Statistical Distributions", tabName = "dist", icon = icon("bar-chart")),
        menuItem("Central Limit Theorem ", tabName = "clt", icon = icon("area-chart")),
        menuItem("Simple Linear Regression", tabName = "regression", icon = icon("line-chart")),
        HTML("<a style='position:fixed;bottom:10px;margin-left:10px;' href='http://twitter.com/sergiostats'>@sergiostats</a>")
      )
    ),
    dashboardBody(
      tabItems(
        # Statistical Distributions -----------------------------------------------------
        tabItem(tabName = "dist",
                fluidRow(
                  box(width = 8,
                      plotOutput("distPlot")
                  ),
                  box(width = 4, 
                      title = "Parameters", 
                      status = "primary", 
                      solidHeader = TRUE, # Default is TRUE (Solid Color of Header)
                      # Distribution Name
                      selectInput(inputId = "distName",
                                  label = "Choose a distribution:",
                                  choices = c("Normal" = 'normal',
                                              "Binomial" = 'binom',
                                              "Poisson" = 'poisson',
                                              "Exponential" = "exponential",
                                              "Uniform" = 'uniform',
                                              "Geometric" = 'geometric',
                                              "Negative Binomial" = "nbinom"
                                  )),
                      # Number of obvervations
                      sliderInput(inputId = 'nObs',
                                  label = 'Number of Observations',
                                  min = 1,
                                  max = 100,
                                  value = 10,
                                  animate = TRUE),
                      # Specific parameters for each distribution
                      conditionalPanel(
                        condition = "input.distName == 'normal'",
                        sliderInput("normalM", "Mean", min=0, max=75, value=0),
                        sliderInput("normalSd", "Standard Deviation", min=0, max=10, value=1.5)
                      ),
                      conditionalPanel(
                        condition = "input.distName == 'binom'",
                        sliderInput("binomProb", "Probability of Success on each trail", min=0, max=1, value=0.4)
                      ),
                      conditionalPanel(
                        condition = "input.distName == 'exponential'",
                        sliderInput("expRate", "Rate", min=0, max=1, value=0.4)
                      ),
                      conditionalPanel(
                        condition = "input.distName == 'poisson'",
                        sliderInput("poisLambda", "Lambda", min=0.00, max=1.00, value=0.05)
                      ),
                      conditionalPanel(
                        condition = "input.distName == 'geometric'",
                        sliderInput("geomProb", "Probability of Success in each trial", min=0, max=1, value=0.15)
                      ),
                      conditionalPanel(
                        condition = "input.distName == 'nbinom'",
                        sliderInput("nbinomProb", "Probability of Success in each trail", min=0, max=1, value=0.7)
                      )
                      
                      
                  )
                )),
        
        # CENTRAL LIMIT THEOREM --------------------------------------------
        tabItem(tabName = "clt",
                fluidRow(
                  box(width = 8,
                      plotOutput("cltPlot")
                  ),
                  box(width = 4, 
                      title = "", 
                      status = "primary", 
                      solidHeader = TRUE, # Default is TRUE (Solid Color of Header)
                      helpText("Assume random variables as identically distributed"),
                      selectInput(inputId = "cltDistName",
                                  label = "Choose a distribution:",
                                  choices = c("Normal" = 'normal',
                                              "Binomial" = 'binom',
                                              "Poisson" = 'poisson',
                                              "Exponential" = "exponential",
                                              "Uniform" = 'uniform',
                                              "Geometric" = 'geometric',
                                              "Negative Binomial" = "nbinom"
                                  )),
                      sliderInput("samplesize", "Sample Size:", min=1, max=100, value=0.05, animate = TRUE),
                      conditionalPanel(
                        condition = "input.cltDistName == 'normal'",
                        sliderInput("cltNormalM", "Mean", min=0, max=100, value=0.4),
                        sliderInput("cltNormalSd", "Standard Deviation", min=1, max = 3, value = 1.2, step = 0.01)
                        
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'binom'",
                        sliderInput("cltBinomP", "Probability of Success on each trail", min=0, max=1, value=0.4)
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'uniform'",
                        sliderInput("cltUnifMin", "Min", min=0, max=100, value=50),
                        sliderInput("cltUnifMax", "Standard Deviation", min=0, max = 100, value = 75)
                        
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'exponential'",
                        sliderInput("cltExpRate", "Rate", min=0, max=1, value=0.4)
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'poisson'",
                        sliderInput("cltPoisLambda", "Lambda", min=0.00, max=1.00, value=0.05)
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'geometric'",
                        sliderInput("cltGeomProb", "Probability of Success in each trial", min=0, max=1, value=0.15)
                      ),
                      conditionalPanel(
                        condition = "input.cltDistName == 'nbinom'",
                        sliderInput("cltNbinomProb", "Probability of Success in each trail", min=0, max=1, value=0.7)
                      )
                      
                      
                      
                      
                  )
                )),
          # Simple Linear Regression --------------------------------
        tabItem(tabName = "regression",
                fluidRow(
                  box(width = 8,
                      plotOutput("regressionPlot")
                  ),
                  box(width = 4, 
                      title = "", 
                      status = "primary", 
                      solidHeader = TRUE, # Default is TRUE (Solid Color of Header)
                      helpText("Simple Linear Regression"),
                      sliderInput("regressionN", "Number Of Observations", min=25, max=100, value=50),
                      sliderInput("regressionB0", "Beta0", min=-10.00, max=10.00, value=5.00, step = 0.001),
                      sliderInput("regressionB1", "Beta1", min=-5.00, max=5.00, value=2.22, step = 0.001),
                      checkboxInput("regressionShowModel","Show model with OLS method"),
                      checkboxInput("regressionHelpNotification", "Help me to fit model")
                    )
                ),
                fluidRow(
                     tabBox(   
                        tabPanel(
                            title = "Dataset (First 10 rows)",
                            tableOutput("regressionTable") # Regression Table
                            
                        ),
                        tabPanel(
                            title = "Summary of Dataset",
                            tableOutput("regressionSummaryDataset") # Regression Table
                        )
                     ),
                     tabBox(
                       tabPanel(
                         title = "Information",
                         htmlOutput("regressionInformationModel")
                       ),
                       tabPanel(
                         title = "Model",
                         conditionalPanel(
                           condition = "input.regressionShowModel == true",
                           htmlOutput("regressionRealInformation")
                         )
                        )
                       )
                     
                )
        )
        
    ))

))

