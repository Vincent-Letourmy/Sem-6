
function.nbMV <- function(df){
  comp <- 0
  for (col in df) {
    for(val in col){
      if (val == 1) comp = comp + 1
    }
  }
  return(comp)
}

function.nbMissingValues <- function(df){
  comp <- 0
  for (i in df){
    for (j in i){
      if (j == "" || is.na(j)) comp = comp + 1
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


function.matrixBoolean <- function(df){
  n1 <- nrow(df)
  n2 <- ncol(df)
  a <- matrix (rep(0, n1*n2), n1, n2)
  a <- data.frame(a)
  names(a) <- names(df)
  
  for (col in names(df)) {
    ligne <- 1
    for (val in df[,col]) {
      if (is.na(val) || val == "" || val == "?"){
        a[ligne,col] <- 1
      }
      ligne <- ligne + 1
    }
  }
  return(a)
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
































