
function.header <- function() {
  dashboardHeader(title = "Naive Bayes")
}

function.sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Initialisation", tabName = "initialisation"),
      menuItem("Target Config",tabName = "targetconfig"),
      menuItem("Data Quality Config", tabName = "dqconfig"),
      menuItem("Costs Config", tabName = "costsconfig"),
      menuItem("Results", tabName = "results"),
      menuItem("Optional", tabName = "opt", startExpanded = TRUE, 
               menuSubItem("Fixed Data", tabName = "fixingData"),
               menuSubItem("Results fixed Data", tabName = "resultsfixing")),
      menuItem("Website", icon = icon("send",lib='glyphicon'), 
               href = "https://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29")
    )
  )
}

function.body <- function(){
  dashboardBody(
    tabItems(
      
      #__________________________________________________ Initialisation _______________________________________________________________________________________#
      
      tabItem(
        tabName = "initialisation",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Initialisation"),
            
            tabsetPanel(
              id = "tabsetInitialisation",
              
              tabPanel(
                "Load your file",
                value = "load",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfile"),
                      uiOutput("parametersbox"),
                      fluidRow(
                        column(6, uiOutput("uploadbutton")),
                        column(6, uiOutput("demobutton"))
                      ),
                      tags$br()
                  )
                ),
                uiOutput("fromLoadToNextTab")
              ),
              
              tabPanel(
                "Define NAs",
                value = "defineNas",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      status = "primary",
                      title = "Define NAs",
                      solidHeader = TRUE,
                      uiOutput("checkBoxInterogation"),
                      uiOutput("checkBoxEmpty"),
                      uiOutput("checkBoxNa"),
                      uiOutput("confirmNAs")
                  ),
                  uiOutput("fromInitToNextButton")
                )
              )
              
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedInitialisation")
          )
        )
      ),
      
      
      #____________________________________________________ Target Config _________________________________________________________________________________________#
      
      tabItem(
        tabName = "targetconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Target Config"),
            
            tabsetPanel(
              id = "tabSetTarget",
              
              tabPanel(
                "Target",
                value = "column",
                tags$br(),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("selectcolumn"),
                    tags$hr(),
                    uiOutput("foldselection"),
                    uiOutput("nextButton")
                  )
                )
              ),
              
              tabPanel(
                "Remove other targets",
                value = "removecolumn",
                tags$br(),
                box(width = 12,
                    uiOutput("checkBox"),
                    uiOutput("ValidCheckBox")
                ),
                uiOutput("fromTargetToNextButton")
              )
              
            )
          ),
          mainPanel(
            dataTableOutput("tabLoadedTargetConfig")
          )
        )
      ),
      
      
      #__________________________________________________ DataQuality Config _______________________________________________________________________________________#
      
      
      
      tabItem(
        tabName = "dqconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Data Quality Config"),
            tags$hr(),
            
            tabsetPanel(
              id = "tabsetdqconfig",
              
              tabPanel(
                "Revome columns",
                value = "removecolumnsMV",
                tags$br(),
                box(width = 12,
                    h4("Do you want to remove columns with too many missing values ?"),
                    uiOutput("pourcentageSelection"),
                    uiOutput("removecolumnbutton")
                ),
                uiOutput("fromRemoveColToNext")
              ),
              
              tabPanel(
                "Types/Ranges",
                value = "typesranges",
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileTypes"),
                      uiOutput("parametersboxTypes"),
                      uiOutput("typesButton")
                  ),
                  box(width = 12,
                      uiOutput("selectionfileRanges"),
                      uiOutput("parametersboxRanges"),
                      uiOutput("rangesButton")
                  ),
                  uiOutput("fromRangesToNextButton")
                )
              ),
              
              tabPanel(
                "Result",
                value = "filter",
                tags$br(),
                box(width = 12,
                    uiOutput("infosRowRemoved"),
                    uiOutput("downloadButtonFixing")
                ),
                uiOutput("fromDQConfigToNextButton")
              )
              
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "tabset",
              
              tabPanel(
                "Bar Chart",
                value = "barchart",
                h3("Pourcentage of missing values in each column"),
                plotlyOutput("NAsBarChart")
              ),
              
              tabPanel(
                "Types and Ranges",
                value = "typesranges",
                h3("Types and Ranges"),
                dataTableOutput("typesFile"),
                dataTableOutput("rangesFile")
              ),
              
              tabPanel(
                "DataBase",
                value = "database",
                dataTableOutput("tabLoadedDQconfig")
              )
            )
          )
        )
      ),
      
      
      
      #____________________________________________________ Costs Config _________________________________________________________________________________________#
      
      tabItem(
        tabName = "costsconfig",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Costs Config"),
            tags$br(),
            
            tabsetPanel(
              id = "tabsetcosts",
              
              tabPanel(
                "Prediction",
                value = "prediction",
                fluidRow(
                  box(width = 12,
                      helpText("Editable table : Choose costs and validate"),
                      rHandsontableOutput("costsTab"),
                      tags$br(),
                      uiOutput("validate"),
                      uiOutput("downloadButton")
                  ),
                  tags$hr(),
                  uiOutput("fromPredictionTabToNext")
                )
              ),
              
              tabPanel(
                "Fixing",
                value = "fixing",
                fluidRow(
                  box(width = 12,
                      uiOutput("costFixingSelection")
                    )
                ),
                uiOutput("fromCostsToNextButton")
              )
            )
          ),
          
          mainPanel(
            dataTableOutput("tabLoadedCostsConfig")
          )
          
        )
      ),
      
      #_______________________________________________________ Results ___________________________________________________________________________________________#
      
      tabItem(
        tabName = "results",
        
          fluidRow(
            
            column(6,
                   h1("Results - Initial"),
                   tags$hr(),
                   uiOutput("accuracyvalueSaved"),
                   tags$hr(),
                   uiOutput("boxBarChartSaved"),
                   tags$hr(),
                   uiOutput("costResultsValueSaved"),
                   tags$hr()
                   ,
                   uiOutput("infodataSaved"),
                   dataTableOutput("tabLoadedResultsSaved")
            ),
            
            column(6,
                   h1("Results - According Data Quality Config"),
                   tags$hr(),
                   uiOutput("accuracyvalue"),
                   tags$hr(),
                   uiOutput("boxBarChar"),
                   tags$hr(),
                   uiOutput("costresultsvalue"),
                   tags$hr()
                   ,
                   uiOutput("infodata"),
                   dataTableOutput("tabLoadedResults")
            )
            
          )
        ),
      
      #____________________________________________________ Fixing Data _________________________________________________________________________________________#
      
      tabItem(
        tabName = "fixingData",
        
        sidebarLayout(
          
          sidebarPanel(
            h1("Fixing Data"),
            
            tabsetPanel(
              id = "tabsetfixing",
              
              tabPanel(
                
                title = "Load File",
                value = "loadfixing",
                tags$br(),
                fluidRow(
                  box(width = 12,
                      uiOutput("selectionfileFixing"),
                      uiOutput("parametersboxFixing"),
                      fluidRow(
                        column(6, uiOutput("uploadbuttonFixing"))
                      ),
                      tags$br()
                  )
                ),
                uiOutput("fromLoadfixingToNextTab")
                
              )
              
            )
            
          ),
          
          mainPanel(
            
            dataTableOutput("tabfixing")
            
          )
          
        )
      ),
      
      tabItem(
        tabName = "resultsfixing",
        
        fluidRow(
          
          column(12,
                 h1("Results - Fixed data"),
                 tags$hr(),
                 uiOutput("accuracyvalueFixed"),
                 tags$hr(),
                 uiOutput("boxBarChartFixed"),
                 tags$hr(),
                 uiOutput("costResultsValueFixed"),
                 tags$hr()
                 ,
                 uiOutput("infodataFixed"),
                 dataTableOutput("tabLoadedResultsFixed")
          )
        )
      )
      
    )
      
  )
}