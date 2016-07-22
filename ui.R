#################################################
#               Factor Analysis                 #
#################################################

library("shiny")
#library("foreign")

shinyUI(fluidPage(
  tags$head(includeScript("google_analytics.js")),
  
  # Header:
  titlePanel("Factor analysis"),
  # Input in sidepanel:
  sidebarPanel(
    # Upload data:
    fileInput("file", "Upload input data (csv file with header))"),  
    
    htmlOutput("fselect"),
    
    sliderInput("cutoff", "Cutoff for factor loadings(for Plotting only)", min = 0,  max = 1, value = 0.25),
    htmlOutput("xaxis"),
    htmlOutput("yaxis"),
    
    sliderInput("cutoffcorr", "Cutoff for Correlation in Factors vs Variable", min = 0,  max = 1, value = 0.25),
    br()
     # submitButton(text = "Apply Changes", icon("refresh"))
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #

tabPanel("Overview",h5(p("Factor Analysis")), 
p("Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of a potentially lower 
number of unobserved variables called factors. For example, it is possible that variations in four observed variables mainly reflect the variations 
in two unobserved variables. Factor analysis searches for such joint variations in response to unobserved latent variables. The observed variables 
are modelled as linear combinations of the potential factors, plus error terms. The information gained about the interdependencies between 
observed variables can be used later to reduce the set of variables in a dataset. Computationally this technique is equivalent to low rank 
approximation of the matrix of observed variables. Factor analysis originated in psychometrics, and is used in behavioral sciences, social sciences,
marketing, product management, operations research, and other applied sciences that deal with large quantities of data.",align="justify"),
a(href="http://en.wikipedia.org/wiki/Factor_analysis","- Wikipedia"),
h5(p("How to use this shiny application")),
p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
  Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
  and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file",align="justify"),
p("Once csv file is uploaded successfully, application will fit a factor model with optimal factors from parallel Analysis and various 
results will be showed in the above tabs. In left-side bar panel you can change the parameters value and correspondingly new results 
  will be showed",align="justify"),
br()
),
    
                tabPanel("Summary",
                         (h6(p("Correlation"))),
                         (tableOutput("table22")),
                         (h6(p("Test Summary"))),(textOutput("text1")),(textOutput("text2")),(textOutput("text3")),
                         (h6(p("Factors Loadings Summary"))),
                          (verbatimTextOutput("mat")),
                         
                         (h6(p("Uniqueness"))),
                         (tableOutput("uni")),
#                          (textOutput("text4")),
                         plotOutput("plot1",height = 600, width = 850)),
                tabPanel("Loadings",tableOutput("loadings")),
                tabPanel("Scores",tableOutput("scores")),
                tabPanel("Factor vs Variables",plotOutput("plot20",height = 600, width = 850)),
                tabPanel("Factor vs Variables 2",plotOutput("plot2",height = 600, width = 850)),
                tabPanel("Factor vs Users",plotOutput("plot3",height = 600, width = 850)),
                tabPanel("Data",tableOutput("table")) 
    )
  ) 
) 
)