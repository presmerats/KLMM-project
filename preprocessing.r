setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Reading all .csv files in a given path and returning the merge dataframe
# parameters: path (str)
read_files <- function(path) {
  files <- list.files(path = path)
  total_size <- length(files) - 2
  df <- data.frame()
  group_number <- 1
  for(f in files) {
    #skipping sampleMaxError.csv and sampleMinError.csv
    if(nchar(f) < 10) {
      file_path <- paste(path, f, sep = '/')
      print(paste("Reading:", file_path, total_size - group_number, "files left", sep = ' '))
      
      df_temp <- read.csv(file = file_path, header = TRUE, sep=",")
      df_temp['group'] <- group_number
      
      df <- rbind(df, df_temp)
      group_number <- group_number + 1
    }
  }
  
  keeps <- c("V2","V3","V4","V5","V7",'group')
  df_final <- df[keeps]
  names(df_final) <- c('time','x1','x2','x3','error','group')
  
  print("Saving merge dataframe into data.Rdata file...")
  save(df_final, file = 'data.Rdata')
  return(df_final)
}