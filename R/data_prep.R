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
    dplyr::mutate(yfog = 100 - (fg_distance - 17))
  
  write_csv(fgs, outname)
  
  return(fgs)
}

punt_averages <- function(punt_data_fname, out_fname, joined){
  punts <- readr::read_csv(punt_data_fname) %>%
    dplyr::left_join(select(joined, pid, yfog))
  
  punts_dist <- punts %>%
    dplyr::group_by(yfog) %>%
    dplyr::summarize(pnet = mean(pnet))
  
  readr::write_csv(punts_dist, out_fname)
  
  return(punts_dist)
}

group_coaches_decisions <- function(fourths, out_fname){
  df <- fourths %>%
    dplyr::mutate(down_by_td = score_diff <= -4,
           up_by_td = score_diff >= 4,
           yfog_bin = floor(yfog / 20),
           short = ytg <= 3,
           med = ytg >= 4 & ytg <= 7,
           long = ytg > 7) %>%
    dplyr::mutate_each(funs(as.numeric), down_by_td, up_by_td, short:long)
  
  grouped <- df %>%
    dplyr::group_by(down_by_td, up_by_td, yfog_bin,
                    short, med, long)
  
  goforit <- grouped %>%
    dplyr::summarize(proportion_went = mean(goforit),
              sample_size = n())
  punt <- grouped %>%
    dplyr::summarize(proportion_punted = mean(punt),
                     sample_size = n())
  kick <- grouped %>%
    dplyr::summarize(proportion_kicked = mean(kick),
                     sample_size = n())
  
  decisions <- grouped %>%
    dplyr::summarize(proportion_went = mean(goforit),
              sample_size = n(),
              proportion_punted = mean(punt),
              sample_size_punt = n(),
              proportion_kicked = mean(kick),
              sample_size_kick = n())
  
  readr::write_csv(decisions, out_fname)
  return(decisions)
}

first_down_rates <- function(df_plays, yfog){
  downs <- df_plays
  if(yfog == "yfog_bin") {
    # Break down the field into deciles
    downs[[yfog]] <- with(downs, floor(yfog / 10))
    downs <- dplyr::filter(downs, yfog < 90)
  } else {
    downs <- dplyr::filter(downs, yfog >= 90)
  }

  # For each segment, find the average first down rate by dwn and ytg
  if(yfog == "yfog_bin") {
    grouped <- downs %>%
      dplyr::group_by(yfog_bin, dwn, ytg) %>%
      dplyr::summarize(fdr = mean(first_down),
                       N = n())
  } else {
    grouped <- downs %>%
      dplyr::group_by(yfog, dwn, ytg) %>%
      dplyr::summarize(fdr = mean(first_down),
                       N = n())
  }

  # Just keep 3rd and 4th downs
  grouped <- dplyr::filter(grouped, dwn >= 3)
  if(yfog == "yfog_bin") {
    merged <- grouped %>%
      dplyr::left_join(grouped, by = c("yfog_bin", "ytg")) %>%
      ungroup
  } else {
    merged <- grouped %>%
      dplyr::left_join(grouped, by = c("yfog", "ytg")) %>%
      ungroup
  }

  # Note this will lose scenarios that have *only* ever seen a 4th down
  # This matches to one play since 2001.
  merged <- dplyr::filter(merged, dwn.x == 4 & dwn.y == 3)
  
  # Compute a weighted mean of FDR on 3rd & 4th down to deal with sparsity
  merged %<>%
    dplyr::mutate(weighted_N.x = fdr.x * N.x,
                  weighted_N.y = fdr.y * N.y,
                  weighted_total = weighted_N.x + weighted_N.y,
                  total_N = N.x + N.y,
                  weighted_fdr = weighted_total / total_N) %>%
    dplyr::select(-weighted_N.x, -weighted_N.y, -weighted_total, -total_N) %>%
    dplyr::rename(dwn = dwn.x)
  
  # Need to fill in any missing combinations where possible
  if(yfog == "yfog_bin"){
    merged <- tidyr::complete(merged, c(yfog_bin, dwn, ytg))
  } else {
    merged <- tidyr::complete(merged, c(yfog, dwn, ytg))
  }
  merged %<>%
    dplyr::rename(fdr = weighted_fdr)
  
  # Eliminate impossible combinations
  if(yfog == "yfog_bin"){
    # Sparse situations, just set to p(success) = .1
    merged %<>%
      dplyr::mutate(fdr = ifelse(ytg > 13, 0.10, fdr))
    
    # Missing values inside -10 because no one goes for it here
    merged %<>%
      dplyr::mutate(fdr = ifelse(is.na(fdr.x) & ytg <= 3, .2, fdr),
                    fdr = ifelse(is.na(fdr.x) & ytg > 3, .1, fdr))
    
    # Fill in missing values
#     merged %<>%
#       dplyr::mutate(fdr = approxfun(fdr))
    readr::write_csv(merged, "data/fd_open_field.csv")
  } else {
    merged %<>%
      dplyr::filter(yfog + ytg <= 100) %>%
      dplyr::mutate(fdr = ifelse(yfog == 99 & ytg == 1, fdr.x, fdr))
    #     merged %<>%
    #       dplyr::mutate(fdr = approxfun(fdr))
    readr::write_csv(merged, "data/fd_inside_10.csv")
  }
  return(merged)
}

data_prep <- function(pbp_data_location){
  if(!dir.exists(file.path(getwd(), "data"))){
    print("Making data directory.")
    dir.create(file.path(getwd(), "data"), showWarnings = FALSE)
  }
  
  print("Loading game data.")
  games <- load_games(file.path(pbp_data_location, "GAME.csv"))
  print("Loading play by play data")
  pbp <- load_pbp(file.path(pbp_data_location, "PBP.csv"), games, remove_knees = FALSE)
  
  print("Joining game and play by play data.")
  joined <- dplyr::left_join(pbp, games)
  
  # Switch offensive and defensive stats on PUNT/KOFF
  print("Munging data...")
  joined <- switch_offense(joined)
  
  # Modify the spread so that the sign is negative when the offense
  # is favored and positive otherwise
  joined %<>%
    dplyr::mutate(spread = sprv,
                  spread = ifelse(off != v, spread,
                                  -1 * spread))
  
  # For model purposes, touchdowns are "first downs" (successful conversion)
  joined %<>%
    dplyr::mutate(first_down = ifelse((fd == "Y" & !is.na(fd)) |
                                        (pts >= 6 & !is.na(pts)), 1, 0))
  
  # Add winners for classification task
  joined %<>%
    dplyr::mutate(win = as.numeric(off == winner))
  
  # Features needed for the win probability model
  joined %<>%
    dplyr::mutate(score_diff = ptso - ptsd,
                  secs_left = (((4 - qtr) * 15.0) * 60 +
                                 (min * 60) + sec))
  
  # Group all fourth downs that indicate if the team went for it or not
  # by down, yards to go, and yards from own goal
  print("Processing fourth downs.")
  fourths <- code_fourth_downs(joined)
  
  # Merge the goforit column back into all plays, not just fourth downs
  joined %<>%
    left_join(dplyr::select(fourths, gid, pid, goforit))
  
  print("Grouping and saving historical 4th down decisions.")
  decisions <- group_coaches_decisions(fourths, "data/coaches_decisions.csv")
  fourths_grouped <- fourths %>%
    dplyr::group_by(dwn, ytg, yfog) %>%
    dplyr::mutate(mean = mean(goforit),
                  N = n())
  readr::write_csv(fourths_grouped, "data/fourths_grouped.csv")
  
  # Remove kickoffs and extra points, retain FGs
  joined %<>%
    dplyr::filter(type != "KOFF") %>%
    dplyr::filter(fgxp != "XP" | is.na(fgxp))
  
  print("Grouping and saving field goal attempts and punts.")
  fgs_grouped <- fg_success_rate(file.path(pbp_data_location, "FGXP.csv"),
                                 "data/fgs_grouped.csv")
  punt_dist <- punt_averages(file.path(pbp_data_location, "PUNT.csv"),
                             "data/punts_grouped.csv", joined)
  
  # Code situations where the offense can take a knee(s) to win
  print("Coding kneel downs.")
  # joined <- kneel_down(joined)
  
  print("Computing first down rates.")
  
  # Only rush and pass plays that were actually executed are eligible
  # for computing first down success rates
  df_plays <- dplyr::filter(joined, type == "PASS" | type == "RUSH")
  fd_open_field <- first_down_rates(df_plays, "yfog_bin")
  fd_inside_10 <- first_down_rates(df_plays, "yfog")
}


