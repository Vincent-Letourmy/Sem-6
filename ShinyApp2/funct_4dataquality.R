
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


### Bar chart inconsistency

function.barChartInconsistency <- function(matrixBool){
  res <- 0
  for (col in names(matrixBool)){
    column <- matrixBool[,col]
    res[col] <- round ( sum(column == 1) / length(column) * 100 , digits = 2 )
  }
  res <- res[-1]
  return(res)
}


#--- Consistency --------------------------------------------------------------------------------------------------------#


### 0/1 file inconsistency

function.matrixBooleanConsistency <- function(df,types,ranges){
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- data.frame(matrix (rep(0, n1*n2), n1, n2))
  names(a) <- names(df)
  rownames(a) <- rownames(df)
  
  for (col in names(df)) {
    typ <- types[,col]
    
    if (typ == "string"){
      df[,col] <- as.character(df[,col])
      rang <- ranges[,col]
    }
    else if (typ == "numeric" || typ == "integer") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
      rangMin <- ranges[1,col]
      rangMax <- ranges[2,col]
    }
    
    for (ligne in row.names(df)){
      
      val <- df[ligne,col]
      
      if (typ == "numeric" || typ == "integer") {
        if (! is.na(val)){
          if (val < rangMin || val > rangMax) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
      }
      else if (typ == "string") {
        if (! is.na(val)){
          if (val %in% rang && val != ""){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
    }
  }
  return(a)
}

function.removeConsistency <- function(df, a){
  rem <- 0
  for (row in row.names(a)) {
    if (1 %in% a[row,]) rem[row] = row
  }
  return(rem[-1])
}

function.nbInconsistenciesValues <- function(matrixBool){
  res <- 0
  for (col in names(matrixBool)){
    column <- matrixBool[,col]
    res[col] <- sum(column == 1)
  }
  res <- res[-1]
  return(sum(res))
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