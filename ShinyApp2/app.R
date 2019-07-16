library(shiny)
require(shinydashboard)
library(e1071) # Naive Bayes
library(mlr)
library(caret) 
library(dplyr)
library(plotly) # Plots
library(rhandsontable) # Edit table

source("funct_1UI.R")
source("funct_2reactivevalues.R")
source("funct_3initStep.R")
source("funct_4dataquality.R")
source("funct_5CVNaiveBayes.R")
source("funct_6costs.R")
source("funct_7results.R")


ui <- dashboardPage(title = 'Costs test - Week 6 (shinyApp2)', function.header(), function.sidebar(), function.body(), skin='black')


server <- function(input, output, session) {
    
    v <- function_reactiveValues()
    
    #__________________________________________________ Initialisation _____________________________________________________________________________________________________________________________________________#
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Upload File Button
    
    output$uploadbutton <- renderUI({
        actionButton("uploadbutton","Upload")
    })
    observeEvent(input$uploadbutton,{
        infile <- input$fileCSV
        if (is.null(infile)) return (NULL)
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile(infile$datapath, input$header , input$sep , input$quote)
    })
    
    
### Upload a demo button
    
    output$demobutton <- renderUI({
        actionButton("demobutton","Upload a Demo")
    })
    observeEvent(input$demobutton,{
        v$dataframe_initialisationBis <- v$dataframe_initialisation <- function.loadFile("risk_factors_cervical_cancer_Original.csv", input$header , input$sep , input$quote)
    })
    
    
### Next tab button
    
    output$fromLoadToNextTab <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromLoadToNextTab", "Next")
    })
    observeEvent(input$fromLoadToNextTab, {
        updateTabsetPanel(session, "tabsetInitialisation", "defineNas")
    })
    
    
### Next Panel button
    
    output$fromInitToNextButton <- renderUI({
        if (is.null(v$dataframe_initialisation)) return (NULL)
        actionButton("fromInitToNextButton","Next step")
    })
    observeEvent(input$fromInitToNextButton,{
        v$dataframe_targetconfig <- v$dataframe_initialisation
        updateTabItems(session,"sidebarmenu", "targetconfig")
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
### Selection file
    
    output$selectionfile <- renderUI(
        function.fileInput()
    )
   
     
### Parameters box + ?/""/NA
    
    output$parametersbox <- function_parametersBox()
    
    output$checkBoxInterogation <- renderUI({
        checkboxInput("interrogation", "?")
    })
    
    output$checkBoxEmpty <- renderUI({
        checkboxInput("empty", "\" \"")
    })
    
    output$checkBoxNa <- renderUI({
        checkboxInput("na", "NA")
    })
    
    
### Validate parameters button
    
    output$confirmNAs <- renderUI({
        actionButton("confirmNAs", "OK")
    })
    observeEvent(input$confirmNAs, {
        v$dataframe_initialisation <- v$dataframe_initialisationBis
        if (input$interrogation){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == "?"), col] <- NA
            }
        }
        if (input$empty){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == ""), col] <- NA
            }
        }
        if (input$na){
            for (col in names(v$dataframe_initialisation )) {
                v$dataframe_initialisation [which(v$dataframe_initialisation [,col] == "NA"), col] <- NA
            }
        }
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### DataBase initial
    
    output$tabLoadedInitialisation <- renderDataTable(
        v$dataframe_initialisation,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    #____________________________________________________ Target Config __________________________________________________________________________________________________________________________________________#
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$selectcolumn <- renderUI(
        function.selectionColumn(v$dataframe_initialisation)
    )
    observeEvent(input$selectcolumn,{
        v$columnSelected <- input$selectcolumn
    })
    
    
    output$foldselection <- renderUI({
        sliderInput("foldselection","Number of fold for Cross Validation (Naive Bayes)", 1,50,10)
    })
    
    output$checkBox <- renderUI({ 
        
        v$dataframe_withoutcolselected <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%v$columnSelected]
        newList <- rev(names(v$dataframe_withoutcolselected))
        checkboxGroupInput("targets",label = "Select target(s)", choices = newList)
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$nextButton <- renderUI({
        actionButton("nextButton","Next")
    })
    observeEvent(input$nextButton,{
        updateTabsetPanel(session, "tabSetTarget", "removecolumn")
    })
    
    
    output$ValidCheckBox <- renderUI({
        actionButton("OK","Remove")
    })
    observeEvent(input$OK,{
        if (!is.null(input$targets)){
            
            list <- data.frame(Column = input$targets)
            v$dataframe_targetconfig <- v$dataframe_targetconfig[,!names(v$dataframe_targetconfig)%in%list$Column]
            
        }
    })
    
    
    output$fromTargetToNextButton <- renderUI({
        actionButton("fromTargetToNextButton","Next Step")
    })
    observeEvent(input$fromTargetToNextButton,{
        v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfigBis <- v$dataframe_targetconfig
        updateTabItems(session,"sidebarmenu", "dqconfig")
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    output$tabLoadedTargetConfig <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    
    #__________________________________________________ DataQuality Config _________________________________________________________________________________________________________________________________________#
    
    
### Selection pourcentage of missing or inconsisting values    

    output$pourcentageSelection <- renderUI(
        sliderInput("pourcentageSelection","Pourcentage of missing values max", 0,100,15)
    )
    
    
### Remove columns button
    
    output$removecolumnbutton <- renderUI({
        if(is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("removecolumnbutton","Remove")
    })
    observeEvent(input$removecolumnbutton,{
        v$dataframe_dataqualityconfig <- function.removeColumns(v$resNAsBarChart, v$dataframe_dataqualityconfigBis, input$pourcentageSelection, v$columnSelected)
    })
    
    
### Next tab button : Remove columns -> Types Ranges config  
    
    output$fromRemoveColToNext <- renderUI({
        actionButton("fromRemoveColToNext","Next")
    })
    observeEvent(input$fromRemoveColToNext,{
        updateTabsetPanel(session, "tabsetdqconfig", selected = "typesranges")
        updateTabsetPanel(session, "tabset", selected = "typesranges")
    })
    

    
### TYPES selection file
    
    output$selectionfileTypes <- renderUI({
        function.fileInputTypes()
    })
    
    
### TYPES upload file
    
    output$typesButton <- renderUI({
        actionButton("typesButton", "Upload Types")
    })
    observeEvent(input$typesButton,{
        infileTypes <- input$fileCSVTypes
        if (is.null(infileTypes)) return (NULL)
        v$df_types <- function.loadFile(infileTypes$datapath, input$headerTypes , input$sepTypes , input$quoteTypes)
        v$df_types <- v$df_types[,names(v$dataframe_dataqualityconfig)]
    })
    
    
    
### RANGES selection file
    
    output$selectionfileRanges <- renderUI({
        function.fileInputRanges()
    })
    
    
### RANGES upload file
    
    output$rangesButton <- renderUI({
        infileRanges <- input$fileCSVRanges
        if (is.null(infileRanges)) return (NULL)
        actionButton("rangesButton", "Upload Ranges")
    })
    observeEvent(input$rangesButton,{
        infileRanges <- input$fileCSVRanges
        if (is.null(infileRanges)) return (NULL)
        v$df_ranges <- function.loadFile(infileRanges$datapath, input$headerRanges , input$sepRanges , input$quoteRanges)
        v$df_ranges <- v$df_ranges[,names(v$dataframe_dataqualityconfig)]
    })
    
    
### RANGES next tab button
    
    output$fromRangesToNextButton <- renderUI({
        if (is.null(v$df_types) || is.null(v$df_ranges)) return(NULL)
        actionButton("fromRangesToNextButton","Next")
    })
    observeEvent(input$fromRangesToNextButton,{
        
        #v$matrixBooloeanMissingValues_Consistency <- function.matrixBooleanConsistency(v$dataframe_dataqualityconfig, v$df_types, v$df_ranges)
        #v$dataframe_dataqualityconfig <- function.removeConsistency(v$dataframe_dataqualityconfig, v$matrixBooloeanMissingValues_Consistency)
        
        rowRemove <- function.removeMVandConsistency(function.df_prepareRemove(v$dataframe_dataqualityconfig,v$df_types), v$df_ranges)
        v$nbRowRemovedConsistency <- length(rowRemove)
        v$dataframe_dataqualityconfig <- v$dataframe_dataqualityconfig[! rownames(v$dataframe_dataqualityconfig)%in%rowRemove, ]
        
        updateTabsetPanel(session,"tabsetdqconfig", "filter")
        updateTabsetPanel(session, "tabset", selected = "database")
    })
    
    
### Infos nb Row removed
    
    output$infosRowRemoved <- renderUI({
        h2("Results of DQ config")
        h4("Number of rows removed : ",v$nbRowRemovedConsistency)
    })
    
    
    
### Next Step button
    
    output$fromDQConfigToNextButton <- renderUI({
        if (is.null(v$dataframe_dataqualityconfig)) return (NULL)
        actionButton("fromDQConfigToNextButton","Next Step")
    })
    observeEvent(input$fromDQConfigToNextButton,{
        v$dataframe_costsconfig <- function.as_factor(v$dataframe_dataqualityconfig)
        v$tabCosts <- function.tabNaiveBayes(v$dataframe_costsconfig, v$columnSelected)
        updateTabItems(session,"sidebarmenu", "costsconfig")
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
### Bar chart missing and inconsistency values
    
    output$NAsBarChart <- renderPlotly({
        v$resNAsBarChart <-res <- function.barChartMissingValues(v$dataframe_dataqualityconfig)
        res <- sort(res, decreasing = TRUE)
        col_names <- names(res)
        
        plot_ly(x = factor(col_names, levels = col_names), 
                y = res, 
                type = "bar",
                color = res > input$pourcentageSelection, colors = c("#132B43", "#56B1F7")
        ) %>% 
            layout(xaxis = list(title = "Column's name"),
                   yaxis = list(title = "Pourcentage of missing values"))
        
        
    })
    
    
### Data table
    
    output$tabLoadedDQconfig <- renderDataTable(
        v$dataframe_dataqualityconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )

    
### Types table
    
    output$typesFile <- renderDataTable(
        v$df_types,
        options = list(scrollX = TRUE,pageLength = 5, searching = FALSE)
    )
    
    
### Ranges table
    
    output$rangesFile <- renderDataTable(
        v$df_ranges,
        options = list(scrollX = TRUE,pageLength = 10, searching = FALSE)
    )
    
    
### Tab 0/1 Consistency
    
    output$tabmatrix <- renderDataTable(
        v$matrixBooloeanMissingValues_Consistency,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
### Boxes Types and Ranges
    
    output$parametersboxTypes <- function_parametersBoxTypes()
    
    output$parametersboxRanges <- function_parametersBoxRanges()
    
    
    
    
    #____________________________________________________ Costs Config __________________________________________________________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Renders ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$tabLoadedCostsConfig <- renderDataTable(
        v$dataframe_costsconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    output$costsTab <- renderRHandsontable({
        rhandsontable(v$tabCosts)
    })
    
    
    output$downloadData <- function.downloadFile(v$tabCosts)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Buttons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$downloadButton <- renderUI({
        if (v$validate == FALSE) return(NULL)
        downloadButton('downloadData', 'Download Costs Tab')
    })
    
    output$validate <- renderUI(
        actionButton("validate","Validate"),
    )
    observeEvent(input$validate,{
        v$tabCosts <- function.saveDataInFile(input$costsTab, "MyData.csv")
        v$validate <- TRUE
    })
    
    
### Naive Bayes
    
    output$fromCostsToNextButton <- renderUI({
        if (is.null(v$dataframe_costsconfig) || v$validate == FALSE) return (NULL)
        actionButton("fromCostsToNextButton","Results")
    })
    observeEvent(input$fromCostsToNextButton,{
        
        #As factor to run naive Bayes
        v$dataframe_results <- v$dataframe_costsconfig
        v$dataframe_targetconfig <- function.as_factor(v$dataframe_targetconfig)
        
        
        # Naive Bayes INITIAL 
        resultats <- function.CVNaiveBayes(v$dataframe_targetconfig,input$selectcolumn,v$tabCosts,input$foldselection)
        v$resultDataSaved = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
        v$accuracySaved <<- mean(resultats$moy)
        v$accuracyTabSaved <<- resultats$moy
        
        
        # Naive Bayes according DQ config #
        resultats <- function.CVNaiveBayes(v$dataframe_results,input$selectcolumn,v$tabCosts,input$foldselection)
        v$resultData = sum(resultats$restab$cost * v$tabCosts$cost) * 5 
        v$accuracy <<- mean(resultats$moy)
        v$accuracyTab <<- resultats$moy
        
        
        updateTabItems(session,"sidebarmenu", "results")
    })
    
    
    
### Selection Cost Fixing
    
    
    output$costFixingSelection <- renderUI({
        numericInput("costFixingSelection", label = "Choose a cost of fixing one value", value = 3,min = 0,max = 100,step = 1)
    })
    
    output$fromPredictionTabToNext <- renderUI({
        if (is.null(v$dataframe_costsconfig) || v$validate == FALSE) return (NULL)
        actionButton("fromPredictionTabToNext","Next")
    })
    observeEvent(input$fromPredictionTabToNext,{
        updateTabsetPanel(session,"tabsetcosts","fixing")
    })
    
    
    #_______________________________________________________ Compare Results INITIAL / DQ config ____________________________________________________________________________________________#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results initial ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$accuracyvalueSaved <- renderValueBox(
        function.accuracyBoxWithConfInterval(v$accuracyTabSaved, v$accuracySaved)
    )
    
    
    
    output$accuracyCVBarSaved <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTabSaved, v$accuracySaved, input$foldselection)
    )
    
    
    
    output$boxBarChartSaved <- renderUI(
        function.BarChartBox(v$accuracySaved, "accuracyCVBarSaved")
    )
    
    
    
    output$costResultsValueSaved <- renderValueBox(
        function.costsResultsVaue(v$resultDataSaved)
    )
    
    output$infodataSaved <- renderUI({
        comp <- function.nbMissingValues(v$dataframe_targetconfig)
        fluidRow(
            h4("Initial table : ", ncol(v$dataframe_targetconfig), " x ", nrow(v$dataframe_targetconfig), "  (columns x rows)"),
            h4("Missing Values : ", comp)
        )
    })
    
    
    output$tabLoadedResultsSaved <- renderDataTable(
        v$dataframe_targetconfig,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results with DATA QUALITY config ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    output$accuracyvalue <- renderValueBox(
        function.accuracyBoxWithConfInterval(v$accuracyTab, v$accuracy)
    )
    
    
    
    output$accuracyCVbar <- renderPlotly (
        function.accuracyCVBarChart(v$accuracyTab, v$accuracy, input$foldselection)
    )
    output$boxBarChar <- renderUI(
        function.BarChartBox(v$accuracy, "accuracyCVbar")
    )
    
    
    
    output$costresultsvalue <- renderValueBox(
        function.costsResultsVaue(v$resultData)
    )
    
    
    output$infodata <- renderUI({
        comp <- function.nbMissingValues(v$dataframe_results)
        fluidRow(
            h4("New table : ", ncol(v$dataframe_results), " x ", nrow(v$dataframe_results), "  (columns x rows)"),
            h4("Missing Values : ", comp)
        )
    })
    
    
    output$tabLoadedResults <- renderDataTable(
        v$dataframe_results,
        options = list(scrollX = TRUE,pageLength = 14, searching = FALSE)
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
