
function.downloadFile <- function(tabCosts) {
  downloadHandler(
    
    filename = function() {
      paste("MydataDownload", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(tabCosts, file, sep = ",",
                  row.names = FALSE)
    }
  )
}


function.saveDataInFile <- function(costsTab, file){
  write.csv(hot_to_r(costsTab), file,row.names = FALSE)
  return(as.data.frame(read.csv(file)))
}


function.nbMV <- function(df){
  comp <- 0
  for (col in df) {
    for(val in col){
      if (val == 1) comp = comp + 1
    }
  }
  return(comp)
}


function.nbRowInconsistency <- function(df){
  comp <- 0
  for (row in rownames(df)) {
    if (1 %in% df[row,]) comp = comp + 1
  }
  return(comp)
}


### Uplaod file fixing

function.fileInputFixing <- function(){
  fileInput("fileCSVFixing", "CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
}


### Parameters box fixing

function_parametersBoxFixing <- function(){
  
  renderUI({
    box(width = 12,
        title = "Parameters (CSV)",
        status = "primary",
        solidHeader = TRUE,
        column(6,
               checkboxInput("headerFixing", "Header", TRUE),
               radioButtons("sepFixing", "Separator",
                            choices = c("Comma" = ",",
                                        "Semicolon" = ';',
                                        "Tab" = "\t"),
                            selected = ',')
        ),
        column(6,
               radioButtons("quoteFixing", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = "")
        )
    )
  })
  
}

### Download after DQ config

function.downloadFileDQconfig <- function(df) {
  downloadHandler(
    
    filename = function() {
      paste("MydataDQconfig", "csv", sep = ".")
    },
    
    content = function(file) {
      write.table(df, file, sep = ",",
                  row.names = FALSE)
    }
  )
}


































