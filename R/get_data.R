load_data <- function(){
  # if db exists, delete and start fresh
  db_path <- "./data/armchair.sqlite"
  if(file.exists(db_path)){
    file.remove(db_path)
  }
  
  # create db
  my_db <- src_sqlite(path = "./data/armchair.sqlite", create = TRUE)
  
  # create tables
  ## get names of tables from csv files
  files <- list.files("./data/armchair_analysis/")
  files <- sub("^([^.]*).*", "\\1", files)
  files <- tolower(files)
  
  ## get indexes
  indexes <- list(block = list("pid"),
                  chart = list("pid"),
                  conv = list("pid"),
                  defense = list("uid", "gid", "player"),
                  drive = list("uid", "gid", "fpid", "tname"),
                  fgxp = list("pid", "fgxp"),
                  fumble = list("pid", "fum"),
                  game = list("gid", "seas"),
                  intercpt = list("pid", "psr", "ints"),
                  kicker = list("uid", "gid", "player"),
                  koff = list("uid", "kicker"),
                  offense = list("uid", "gid", "player"),
                  oline = list("olid"),
                  pass = list("pid", "psr", "trg"),
                  penalty = list("uid", "pid"),
                  play = list("pid", "gid"),
                  player = list("player", "fname", "lname"),
                  punt = list("pid", "punter"),
                  redzone = list("uid", "gid", "player"),
                  rush = list("pid", "bc"),
                  sack = list("uid", "pid", "qb", "sk"),
                  safety = list("pid", "saf"),
                  tackle = list("uid", "pid", "tck"),
                  team = list("tid", "gid", "tname"))
  
  ## create and fill tables
  for(name in files){
    path <- paste0("./data/armchair_analysis/", toupper(name), ".csv")
    copy_to(my_db, read_csv(path), name = name, temporary = FALSE, indexes = indexes$`name`)
  }
  
  return(my_db)
}




