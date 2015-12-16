load_games <- function(game_data_fname, remove_ties = FALSE){
  games <- readr::read_csv(game_data_fname)
  
  # data from 2000 import is less reliable, omit this season
  # and use regular season games only.
  games <- games %>%
    dplyr::filter(seas >= 2001 & wk <= 17) %>%
    dplyr::select(-stad, -temp, -humd, -wspd, -wdir, -cond, -surf)
  
  # calculate winner
  games <- dplyr::mutate(games, winner = ifelse(ptsv > ptsh, v,
                           ifelse(ptsh > ptsv, h, "TIE")))
  
  if(remove_ties){
    games <- dplyr::filter(games, winner != "TIE")
  }
  
  return(games)
}

load_pbp <- function(pbp_data_fname, games, remove_knees = FALSE){
  pbp <- readr::read_csv(pbp_data_fname) %>%
    dplyr::select(gid, pid, off, def, type, qtr, min, sec,
           kne, ptso, ptsd, timo, timd, dwn, ytg, yfog,
           yds, fd, fgxp, good, pnet, pts, detail)
  
  # remove overtime
  pbp <- dplyr::filter(pbp, qtr <= 4)
  
  # restrict to regular season games after 2000
  pbp <- dplyr::filter(pbp, gid %in% games$gid)
  
  if(remove_knees){
    pbp <- dplyr::filter(pbp, is.na(kne))
  }
  
  return(pbp)
}
