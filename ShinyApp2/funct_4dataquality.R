
#--- Missing Values Bar Chart -----------------------------------------------------------------------------------------#

### Remove columns with too many missing values

function.removeColumns <- function(resNas, df, pourcent, columnSelected){
  
  resColo <- 0
  
  for (i in names(resNas)){
    if (i == columnSelected){
      resColo[i] = i
    }
    else if (resNas[i] < pourcent){
      resColo[i] = i
    }
  }
  resColo <- resColo[-1]
  df <- df[,resColo]
  
  return(df)
  
}


### Bar chart Missing values

function.barChartMissingValues <- function(df){
  res <- 0
  for (i in names(df)) {
    col <- df[,i]
    
    a <- 0
    for (j in col) {
      if(is.na(j) || j == "" || j == "?") a = a + 1
    }
    res[i] = round(a / length(col) * 100,digits = 2)
  }
  res <- res[-1]
  return(res)
}


#--- Consistency --------------------------------------------------------------------------------------------------------#


### Prepare 

function.df_prepareRemove <- function(df,types){
  
  for (col in names(df)) {
    
    if (types[,col] == "string"){
      df[,col] <- as.character(df[,col])
    }
    
    else if (types[,col] == "numeric") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
    }
    
    else if (types[,col] == "integer") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.integer(df[,col])
    }
  }
  return(df)
}


### Remove MV and inconsistencies

function.removeMVandConsistency <- function(df,ranges){
  
  rowRemove <- 0
  
  for (ligne in row.names(df)){
    
    for (col in names(df)) {
      val <- df[ligne,col]
      
      if (class(val) == "numeric" || class(val) == "integer") {
        
        if (is.na(val)){
          rowRemove[ligne] <- ligne
          break
        }
        else if (val < ranges[1,col] || val > ranges[2,col]) {
          rowRemove[ligne] <- ligne
          break
        }
      }
      
      else if (class(val) == "string") {
        
        if (val %in% ranges[,col] && val != ""){}
        else {
          rowRemove[ligne] <- ligne
          break
        }
      }
    }
  }
  
  return(rowRemove[-1])
}


#--- Types and Ranges ------------------------------------------------------------------------------------------------------#


### Types

function.fileInputTypes <- function(){
  fileInput("fileCSVTypes", "CSV File Types",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}



function_parametersBoxTypes <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters Types (CSV)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        column(6,
               checkboxInput("headerTypes", "Header", TRUE),
               radioButtons("sepTypes", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ';')
        ),
        column(6,
               radioButtons("quoteTypes", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}


### Ranges

function.fileInputRanges <- function(){
  fileInput("fileCSVRanges", "CSV File Ranges",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}



function_parametersBoxRanges <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters Ranges (CSV)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        column(6,
               checkboxInput("headerRanges", "Header", TRUE),
               radioButtons("sepRanges", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ';')
        ),
        column(6,
               radioButtons("quoteRanges", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}