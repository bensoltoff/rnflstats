download_data <- function(){
  dbUrl <- "http://burntsushi.net/stuff/nfldb/nfldb.sql.zip"
  dbDir = "./data"
  dbZipPath <- "./data/nfldb.zip.sql"
  dbPath <- "./data/nfldb.sql"
  dateDownloadedFile <- "./data/dateDownloaded.txt"
  
  if (!file.exists(dbDir)) {
    dir.create(dbDir)
  }
  
  if (!file.exists(dbPath)) {
    download.file(dbUrl, destfile = dbZipPath)
    unzip(dbZipPath, exdir = dbDir)
    dateDownloaded <- date()
    dateDownloaded
    cat(dateDownloaded, file = dateDownloadedFile)
    print(paste("Downloaded database", dbPath, "at"))
    cat(readChar(dateDownloadedFile, 1e5))
  } else if (file.exists(dateDownloadedFile)) {
    print(paste("Database", dbPath, "downloaded on"))
    cat(readChar(dateDownloadedFile, 1e5))
  }
}

load_data <- function(update = FALSE){
  download_data()
  
  # call system command to load db into postgreSQL server
  system("psql -h localhost -d soltofbc -U soltofbc -f data/nfldb.sql")
  my_db <- src_postgres(dbname = "nfldb", host = "soltofbc",
                        port = 5432)
}



