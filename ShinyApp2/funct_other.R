
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

















