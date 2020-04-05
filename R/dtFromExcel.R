dtFromExcel <- function(v){
  as.POSIXct(v*(24*60*60), # convert to seconds
             origin="1899-12-30", # beginning date for Excel values
             tz="GMT") # this is not true, but we don't have correct timezone information for each instructor
}

# # Convert Excel's date values to a useable form
# for (i in time.cols){
#   dat[,i] <- dtFromExcel(dat[,i])
# }