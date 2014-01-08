# obtain data from the PRISM ftp site (prism.nacse.org)
# see http://prism.oregonstate.edu/documents/PRISM_downloads.pdf
# 2013 data is still not all "stable" and so not available in one zip file
# download data for 2013 separately

# folder where all the monthly precip data resides
base_url <- "ftp://prism.nacse.org/monthly/ppt/"

# data for 1895 to 2012
for (eachYear in c(1895:2012)) {
    
  cat("reading...", eachYear, "\n")
  
  # construct final url and output file name
  year_url <- paste0("PRISM_ppt_stable_4kmM2_", eachYear, "_all_bil.zip")
  prism_url <- paste0(base_url, eachYear, "/", year_url)
  outFile   <- paste0("data/", year_url)
  
  # internet connection could be "intermittent" and files are not sometimes downloaded
  # hence, the quieted while loop below; max tries limited to 20 to avoid infinite loops
  fileError <- TRUE
  max_tries <- 0
  while (fileError & max_tries < 20) {
    max_tries <- max_tries + 1
    fileStatus <- try(download.file(url = prism_url, 
                                    destfile = outFile, 
                                    mode = "wb", 
                                    quiet = TRUE), 
                      silent = TRUE)
    fileError  <- ifelse(class(fileStatus) == "try-error", TRUE, FALSE)
  }
  
  # only extract the files corresponding to the entire year
  file_names <- unzip(outFile, list = TRUE)
  find_str <- paste0("PRISM_ppt_stable_4kmM2_", eachYear, "_")
  file_names <- file_names$Name[grep(find_str, file_names$Name)]  
  unzip(outFile, files = file_names, exdir = "data")
  
  # delete raw file
  file.remove(outFile)
}

# data for 2013
for (eachYear in c(2013)) {
  
  cat("reading...", eachYear, "\n")
  
  # construct final url and output file name
  for (eachMonth in c(1:12)) {
    if (eachMonth <= 6) {
      year_url <- paste0("PRISM_ppt_stable_4kmM2_", 
                         eachYear, 
                         sprintf("%.2d", eachMonth), 
                         "_bil.zip")
    } else {
      year_url <- paste0("PRISM_ppt_provisional_4kmM2_", 
                         eachYear, 
                         sprintf("%.2d", eachMonth), 
                         "_bil.zip")      
    }
    prism_url <- paste0(base_url, eachYear, "/", year_url)
    outFile   <- paste0("data/", year_url)
    
    # internet connection could be "intermittent" and files are not sometimes downloaded
    # hence, the quieted while loop below; max tries limited to 20 to avoid infinite loops
    fileError <- TRUE
    max_tries <- 0
    while (fileError & max_tries < 20) {
      max_tries <- max_tries + 1
      fileStatus <- try(download.file(url = prism_url, 
                                      destfile = outFile, 
                                      mode = "wb", 
                                      quiet = TRUE), 
                        silent = TRUE)
      fileError  <- ifelse(class(fileStatus) == "try-error", TRUE, FALSE)
    }
    
    # only extract the files corresponding to the entire year
    unzip(outFile, exdir = "data")
    
    # delete raw file
    file.remove(outFile)
  }
}
