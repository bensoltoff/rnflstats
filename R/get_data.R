load_data <- function(){
  # create db
  my_db <- src_sqlite(path = "./data/armchair.sqlite", create = TRUE)
  
  # create tables
  ## get names of tables from csv files
  files <- list.files("./data/armchair_analysis/")
  files <- sub("^([^.]*).*", "\\1", files)
  files <- tolower(files)
  
  ## create and fill tables
  for(name in files){
    path <- paste0("./data/armchair_analysis/", toupper(name), ".csv")
    data <- copy_to(my_db, read_csv(path), name = name)
  }
}



