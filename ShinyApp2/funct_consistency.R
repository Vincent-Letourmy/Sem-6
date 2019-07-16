
function.removeConsistency <- function(df, a){
  rem <- 0
  for (row in row.names(a)) {
    if (!1 %in% a[row,]) rem[row] = row
  }
  rem <- rem[-1]
  return(df[rem,])
}

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


### TEST AVEC SWITCH

function.removeMVandConsistencySwitch <- function(df,ranges){
  
  rowRemove <- 0
  
  for (ligne in row.names(df)){
    
    for (col in names(df)) {
      
      val <- df[ligne,col]
      
      switch (class(val),
        numeric = {
          if (is.na(val)){
            rowRemove[ligne] <- ligne
            break
          }
          else if (val < ranges[1,col] || val > ranges[2,col]) {
            rowRemove[ligne] <- ligne
            break
          }
        },
        integer = {
          if (is.na(val)){
            rowRemove[ligne] <- ligne
            break
          }
          else if (val < ranges[1,col] || val > ranges[2,col]) {
            rowRemove[ligne] <- ligne
            break
          }
        },
        character = {
          if (val %in% ranges[,col] && val != ""){}
          else {
            rowRemove[ligne] <- ligne
            break
          }
        }
      )
    }
  }
  
  return(rowRemove[-1])
}

### Remove without any 0/1 matrix

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




### Without logical

function.matrixBooleanConsistency <- function(df,types,ranges){
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  rownames(a) <- rownames(df)
  
  for (col in names(df)) {

    if (types[,col] == "string"){
      df[,col] <- as.character(df[,col])
    }
    
    else if (types[,col] == "numeric" || types[,col] == "integer") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
    }
    
    for (ligne in row.names(df)){
      
      val <- df[ligne,col]
      
      if (class(val) == "numeric" || class(val) == "integer") {
        if (! is.na(val)){
          if (val < ranges[1,col] || val > ranges[2,col]) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
        
      }
      else if (class(val) == "string") {
        if (! is.na(val)){
          if (val %in% ranges[,col] && val != ""){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
    }
  }
  
  return(a)
  
}

### With logical

function.matrixBooleanConsistencyBis <- function(df,types,ranges){
  
  #types <- read.csv("TypesData.csv", header = TRUE, sep = ";")
  #ranges <- read.csv("RangesData.csv", header = TRUE, sep = ";")
  
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  rownames(a) <- rownames(df)
  
  for (col in names(df)) {
    
    if (types[,col] == "numeric") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
    }
    
    else if (types[,col] == "string") {
      df[,col] <- as.character(df[,col])
    }
    
    else if (types[,col] == "logical") {
      df[,col] <- as.character(df[,col])
      df[,col] <- as.numeric(df[,col])
    }
    
    
    for (ligne in row.names(df)){
      if (types[,col] == "numeric") {
        if (! is.na(df[ligne,col])){
          if (df[ligne,col] < ranges[1,col] || df[ligne,col] > ranges[2,col]) {
            a[ligne,col] <- 1
          }
        }
        else a[ligne,col] <- 1
        
      }
      else if (types[,col] == "string") {
        if (! is.na(df[ligne,col])){
          if (df[ligne,col] %in% levels(ranges[,col]) && df[ligne,col] != ""){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
      else if (types[,col] == "logical") {
        if (!is.na(df[ligne,col])){
          if (df[ligne,col] == 1 || df[ligne,col] == 0){}
          else a[ligne,col] <- 1
        }
        else a[ligne,col] <- 1
      }
    }
  }
  
  return(a)
  
}


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


