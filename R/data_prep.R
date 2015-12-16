load_games <- function(game_data_fname, remove_ties = FALSE){
  games <- readr::read_csv(game_data_fname)
  
  # Data from 2000 import is less reliable, omit this season
  # and use regular season games only.
  games <- games %>%
    filter(seas >= 2001 & wk <= 17) %>%
    select(-stad, -temp, -humd, -wspd, -wdir, -cond, -surf)
  
  # calculate winner
  games <- games %>%
    mutate(winner = ifelse(ptsv > ptsh, v,
                           ifelse(ptsh > ptsv, h, "TIE")))
  
  if(remove_ties){
    games <- games %>%
      filter(winner != "TIE")
  }
  
  return(games)
}

