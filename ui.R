
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Wells-Barkerville Community Forest SCP"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    #    strong(textOutput("clickcoordsnmds")),
    #submitButton("Run Marxan")
    actionButton("mrun",HTML("<h4>Run Optimization</h4>")), 
    helpText("================================="), 
    checkboxInput("MultiScen", "Run multiple scenarios"),
    conditionalPanel(
      condition = "input.MultiScen == true",  
      helpText(HTML("You can either set the scenario parameters directly in the 'Scenario List' table on the right, or upload your scenario file below.<br>
                    Make sure that your file follows the same structure as the table on the right.<br>
                    For details about the columns please refer to the tool manual.")),
      fileInput('scen_file', 'Choose scenario file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'))
      ),
    conditionalPanel(
      condition = "input.MultiScen == false",
      #actionButton("tree.update",HTML("Update tree community input")), 
      helpText("================================="),
      helpText(HTML("<h4><strong>Global parameters:</strong></h4>")),
      #selectInput("time", "What time period/scenario do you want to use:",
      #            c("present" = "curr")),
      #    selectInput("scale", "What planning unit size do you want to use:",
      #                c("100m" = "100",
      #                  "250m" = "250",
      #                  "500m" = "500")),
      #sliderInput("FTcutoff", label = "Cutoff value for high quality features", 
      #            min=0, max=100, value=0, step = 1),    
      #    selectInput("connectivity", "Include connectivity in the analysis:",
      #                c("No" = "no",
      #                  "Yes" = "yes")),
      selectizeInput("cost", "What cost metric should be used:", 
                     choices = names(cost),
                     selected = names(cost)[1], multiple = FALSE),
      selectInput("protected", "How to deal with protected areas:",
                  c("Locked in" = "locked",
                    "Available" = "avail")),
      # numericInput("portfolio", "How many solutions should be generated:", 1, min = 1, max = 100),
      helpText("--------------------------------------------"),
      helpText(HTML("<h5><strong>Boundary Length Modfier settings:</strong></h5>")),
      helpText(HTML("<p>Below you can set the Boundary Length Modifier settings. The first parameter is equivalent to BLM from Marxan, the 
                    second parameter represents the proportion to scale edges that do not have any neighboring planning units.
                    For more details please see here: 
                    <a href='https://prioritizr.github.io/prioritizr/reference/add_boundary_penalties.html' target='_blank'>Boundary penalty documentation</a> </p>")),
      numericInput("blm", "Boundary Length Modifier:", 0, min = 0, max = 100),
      numericInput("edge", "Edge factor:", 0.5, min = 0, max = 1)
      )
    
      ),
  
  # Outputs
  mainPanel(
    #Setup in Server.R
    uiOutput("tabsets")
  )
    ))

