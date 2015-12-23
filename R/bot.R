load_data <- function(){
  cat("Loading data and setting up model.", fill = TRUE)
  
  data <- list()
  data$fgs <- readr::read_csv("data/fgs_grouped.csv")
  data$punts <- readr::read_csv("data/punts_grouped.csv")
  data$fd_open_field <- readr::read_csv("data/fd_open_field.csv")
  data$fd_inside_10 <- readr::read_csv("data/fd_inside_10.csv")
  data$final_drives <- readr::read_csv("data/final_drives.csv")
  data$decisions <- readr::read_csv("data/coaches_decisions.csv")
  data$scaler <- readRDS("models/scaler.RDs")
  data$features <- c("dwn", "yfog", "secs_left",
                     "score_diff", "timo", "timd", "spread",
                     "kneel_down", "qtr", "qtr_scorediff")
  
  win_model <- readRDS("models/win_probability.RDs")
  fg_model <- readRDS("models/fg_probability.RDs")
  
  return(list(data = data,
              win_model = win_model,
              fg_model = fg_model))
}

fg_make_prob <- function(situation, model){
  # Convert situation to data frame for prediction
  situation <- as.data.frame(situation) %>%
    tbl_df
  
  pred_prob <- predict(model, newdata = situation, type = "response")[1]
  return(pred_prob)
}

run_bot <- function(data, win_model, fg_model){
  situation <- vector("list", length = length(data$features))
  names(situation) <- data$features
  
  situation$dwn <- as.numeric(readline("Down: "))
  situation$ytg <- as.numeric(readline("Yards to go: "))
  situation$yfog <- as.numeric(readline("Yards from own goal: "))
  situation$secs_left <- as.numeric(readline("Seconds remaining in game: "))
  situation$score_diff <- as.numeric(readline("Offense's lead (can be negative): "))
  situation$timo <- as.numeric(readline("Timeouts remaining, offense: "))
  situation$timd <- as.numeric(readline("Timeouts remaining, defense: "))
  situation$spread <- as.numeric(readline("Spread in terms of offensive team (can be negative, enter 0 if you don't know): "))
  situation$dome <- as.numeric(readline("Is game in dome? 1 for yes, 0 for no: "))
  situation$offense <- readline("Team on offense (e.g. PHI): ")
  situation$home <- readline("Home team: ")
  situation$temp <- as.numeric(readline("Temperature: "))
  situation$humd <- as.numeric(readline("Humidity (as a percentage): "))
  situation$wspd <- as.numeric(readline("Windspeed (mph): "))
  situation$precip <- as.numeric(readline("Precipitation? 1 for yes, 0 for no: "))
  situation$grass <- as.numeric(readline("Played on grass? 1 for yes, 0 for no: "))
  situation$highalt <- ifelse(situation$home == "DEN", 1, 0)
  situation$seas <- 2015
  situation$fg_make_prob <- fg_make_prob(situation[!sapply(situation, is.null)], fg_model)
  
  response <- generate_response(situation, data, win_model)
  
  return(response)
}







