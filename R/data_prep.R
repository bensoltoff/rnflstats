load_games <- function(game_data_fname, remove_ties = FALSE){
  games <- readr::read_csv(game_data_fname)
  
  # Data from 2000 import is less reliable, omit this season
  # and use regular season games only.
  games %<>%
    dplyr::filter(seas >= 2001 & wk <= 17) %>%
    dplyr::select(-stad, -temp, -humd, -wspd, -wdir, -cond, -surf)
  
  # Calculate winner
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
  
  # Remove overtime
  pbp <- dplyr::filter(pbp, qtr <= 4)
  
  # Restrict to regular season games after 2000
  pbp <- dplyr::filter(pbp, gid %in% games$gid)
  
  if(remove_knees){
    pbp <- dplyr::filter(pbp, is.na(kne))
  }
  
  return(pbp)
}

switch_offense <- function(df){
  # used solution from http://stackoverflow.com/questions/7746567/how-to-swap-values-between-2-columns
  df[df$type == "PUNT" | df$type == "KOFF",
     c("off", "def", "ptso", "ptsd", "timo", "timd")] <- df[df$type == "PUNT" | df$type == "KOFF",
                                                            c("def", "off", "ptsd",
                                                              "ptso", "timd", "timo")]

  # if any points are scored on a PUNT/KOFF, they are given in terms
  # of the receiving team - switch this
  dplyr::mutate(df, pts = ifelse(type == "PUNT" | type == "KOFF", -1 * pts, pts))
  
  return(df)
}

code_fourth_downs <- function(df){
  fourths <- dplyr::filter(df, dwn == 4) %>%
    dplyr::mutate(goforit = 0,
                  punt = 0,
                  kick = 0)
  
  # Omit false start, delay of game, encroachment, neutral zone infraction
  # We cannot infer from these plays if the offense was going to
  # go for it or not.
  
  omitstring <- "encroachment|false start|delay of game|neutral zone infraction"
  fourths <- dplyr::filter(fourths, !grepl(pattern = omitstring, x = detail, ignore.case = TRUE))
  
  # Ran a play
  fourths %<>%
    dplyr::mutate(goforit = ifelse(type == "RUSH" | type == "PASS", 1, goforit),
           punt = ifelse(type == "RUSH" | type == "PASS", 0, punt),
           kick = ifelse(type == "RUSH" | type == "PASS", 0, kick))
  
  # Field goal attempts and punts
  fourths %<>%
    dplyr::mutate(goforit = ifelse(type == "FGXP" | type == "PUNT", 0, goforit),
           kick = ifelse(type == "FGXP", 1, kick),
           punt = ifelse(type == "PUNT", 1, punt))
  
  # Punted, but penalty on play
  puntstring <- "punts|out of bounds"
  fourths %<>%
    dplyr::mutate(punt = ifelse(type == "NOPL" &
                           grepl(pattern = puntstring, x = detail, ignore.case = TRUE), 1, punt))
  
  # Kicked, but penalty on play
  kickstring <- "field goal is|field goal attempt"
  fourths %<>%
    dplyr::mutate(kick = ifelse(type == "NOPL" &
                           grepl(pattern = kickstring, x = detail, ignore.case = TRUE), 1, kick))
  
  # Went for it, but penalty on play
  gostring <- paste0("pass to|incomplete|sacked|left end|up the middle|pass interference|",
                     "right tackle|right guard|right end|pass intended|left tackle|left guard|",
                     "pass deep|pass short|up the middle")
  fourths %<>%
    dplyr::mutate(goforit = ifelse(type == "NOPL" & 
                                     grepl(pattern = gostring, x = detail, ignore.case = TRUE) &
                                     -grepl(pattern = puntstring, x = detail, ignore.case = TRUE) &
                                     -grepl(pattern = kickstring, x = detail, ignore.case = TRUE),
                                   1, goforit))
  
  fourths %<>%
    dplyr::mutate(sum = goforit + punt + kick) %>%
    dplyr::filter(sum == 1) %>%
    dplyr::select(-sum)
  
  return(fourths)
}

fg_success_rate <- function(fg_data_fname, out_fname, min_pid = 473957){
  fgs <- readr::read_csv(fg_data_fname)
  
  fgs <- dplyr::filter(fgs, fgxp == "FG" & pid > min_pid)
  
  fgs_grouped <- fgs %>%
    dplyr::group_by(dist) %>%
    dplyr::summarize(N = n(),
              average = mean(good))
  
  fgs_grouped <- dplyr::mutate(fgs_grouped, yfog = 100 - (dist - 17))
  fgs_grouped %>%
    dplyr::select(yfog, average) %>%
    readr::write_csv(out_fname)
  
  return(fgs_grouped)
}

nyt_fg_model <- function(fname, outname){
  fgs <- readr::read_csv(fname) %>%
    mutate(yfog = 100 - (fg_distance - 17))
  
  write_csv(fgs, outname)
  
  return(fgs)
}


