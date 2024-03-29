GetYahooData <- function(stock = "%5ENSEI",
                         start.date = "2010-01-01",
                         freq = "d",
                         intraday = FALSE,
                         intraLength = "5d") {
  
  # Set Options ---------------------------------------------------------------
  options(show.error.messages = F)
  options(warn = -1)
  
  errorFlag = 0
  
  # Dates ---------------------------------------------------------------------
  start.date = as.Date(start.date)   #  Format should be "YYYY-mm-dd"
  end.date = Sys.Date()              #  Auto current date
  
  # If not Intraday -----------------------------------------------------------
  if (intraday == FALSE) {
    
    # Create URL
    a = as.numeric(format(start.date, "%d"))
    b = as.numeric(format(start.date, "%m"))
    c = as.numeric(format(start.date, "%Y"))
    
    d = as.numeric(format(end.date, "%d"))
    e = as.numeric(format(end.date, "%m"))
    f = as.numeric(format(end.date, "%Y"))
    
    part1 = 'http://real-chart.finance.yahoo.com/table.csv?s='
    
    part2 = paste0('&a=',a,"&",
                   'b=',b,"&",
                   'c=',c,"&",
                   'd=',d,"&",
                   'e=',e,"&",
                   'f=',f,"&",
                   'g=',freq,"&",
                   "ignore=.csv")
    
    URL = paste0(part1, stock, part2)
    
    # Simply read as csv from URL
    dat = try(read.csv(URL, stringsAsFactors = F), silent = T)
    
    # If Error, flag it
    if (class(dat) == "try-error") {
      errorFlag = 1
    }
  }
  
  # If Intraday ---------------------------------------------------------------
  if (intraday == TRUE) {
    
    # Create URL
    #part1 = "http://chartapi.finance.yahoo.com/instrument/1.0/"
    #part2 = paste0("/chartdata;type=quote;range=", intraLength, "/csv/")
    
    #URL = paste0(part1, stock, part2)
    URL <- paste0("eodhistoricaldata.com/api/table.csv?s=", symbols[i])
    
    # Read each line separately as text
    dat = readLines(URL)
    
    # If no error, run code
    if (length(dat) > 4) {
      
      # Get the column names
      n = as.numeric(unlist(strsplit(intraLength, "d")))
      n = ifelse(n == 1, 12, 12 + n)
      col.names = (unlist(strsplit(dat[n], ":"))[2])
      col.names = unlist(strsplit(col.names, ","))
      
      # Remove the first few unnecessary rows
      dat = dat[(n + 6):length(dat)]
      
      # Convert the vector of strings into a vector of numbers
      dat = as.numeric(unlist(strsplit(dat, ",")))
      
      # Create a matrix and add column names
      dat = matrix(dat, ncol = 6, byrow = T)
      colnames(dat) = col.names
      
      # Add Row Names
      dat = data.frame(Date = dat[,1], dat[,-1])
      
    }
    
    else {
      
      # If error flag it
      errorFlag = 1
    }
  }
  
  # Print errors etc ----------------------------------------------------------
  if (errorFlag == 1) {
    
    print("Data pull unsuccessful. Check Stock Code cde...")
    dat = NULL
  }
  
  else {
    
    print("Data pull successful...")
  }
  
  # Fix options back to original ----------------------------------------------
  options(show.error.messages = T)
  options(warn = 1)
  
  # Return data ---------------------------------------------------------------
  return(dat)
  
}

