#' Parent function called by the bot to make decisions on 4th downs.
#'
#' @param situation List
#' @param data List, contains historical data frames
#' @param model Logistic regression model, results of \code{logit}
#'
#' @return List
generate_response <- function(situation, data, model){
  situation <- calculate_features(situation, data)
  
  # Generate the game state of possible outcomes
  scenarios <- simulate_scenarios(situation, data)
  
  # Calculate the win probability for each scenario
  probs <- generate_win_probabilities(situation, scenarios, model, data)
  
  # Calculate breakeven points, make decision on optimal decision
  optimal <- generate_decision(situation, data, probs)
  
  payload <- list(decision = optimal$decision,
                  probs = optimal$probs,
                  situation = situation)
  
  return(payload)
}

#' Generate features needed for the win probability model that are
#' not contained in the general game state information passed via API.
#'
#' @param situation List
#' @param data The same list, with new keys and values
#'
#' @return
calculate_features <- function(situation, data){
  situation$kneel_down <- kneel_down(situation$score_diff,
                                     situation$timd,
                                     situation$secs_left,
                                     situation$dwn)
  
  situation$qtr <- qtr(situation$secs_left)
  situation$qtr_scorediff <- situation$qtr * situation$score_diff
  
  situation$spread <- situation$spread * (situation$secs_left / 3600)
  
  cum_pct <- which.min(abs(situation$secs_left - data$final_drives$secs))
  
  situation$poss_prob <- data$final_drives$cum_pct[cum_pct]
  
  return(situation)
}

#' Simulate game state after each possible outcome.
#' Possible scenarios are: touchdown, first down, turnover on downs,
#' field goal attempt (success or failure), and punt.
#'
#' @param situation 
#' @param data 
#'
#' @return scenarios
simulate_scenarios <- function(situation, data){
  features <- data$features
  scenarios <- list()
  
  # If it's 4th and goal, success is a touchdown, otherwise a 1st down.
  if(situation$ytg + situation$yfog >= 100){
    scenarios$touchdown <- change_poss(situation, touchdown, features)
  } else {
    scenarios$first_down <- first_down(situation)
  }
  
  scenarios$fail <- change_poss(situation, turnover_downs, features)
  
  scenarios$punt <- change_poss(situation, punt, features, data = data$punts)
  
  scenarios$fg <- change_poss(situation, field_goal, features)
  scenarios$missed_fg <- change_poss(situation, missed_field_goal,
                                     features)
  
  return(scenarios)
}

#' For each of the possible scenarios, estimate the win probability
#' for that game state.
#'
#' @param situation 
#' @param scenarios 
#' @param model 
#' @param data 
#'
#' @return
generate_win_probabilities <- function(situation, scenarios, model, data){
  probs <- vector("list", length = length(scenarios))
  names(probs) <- paste0(names(scenarios), "_wp")
  probs <- lapply(probs, replace_null)
  
  features <- data$features
  
  # Pre-play win probability calculation
  # Note there is more information in situation than just model features
  feature_vec <- situation[features]
  feature_vec <- do.call(cbind.data.frame, feature_vec) %>%
    tbl_df %>%
    scale(center = attr(data$scaler, "scaled:center"),
          scale = attr(data$scaler, "scaled:scale")) %>%
    as.data.frame %>%
    tbl_df
  
  probs$pre_play_wp <- predict(model, newdata = feature_vec, type = "response")[1]
  
  for(i in 1:length(scenarios)){
    feature_vec <- scenarios[[i]][features]
    feature_vec <- do.call(cbind.data.frame, feature_vec) %>%
      tbl_df %>%
      scale(center = attr(data$scaler, "scaled:center"),
            scale = attr(data$scaler, "scaled:scale")) %>%
      as.data.frame %>%
      tbl_df
    
    pred_prob <- predict(model, newdata = feature_vec, type = "response")[1]
    
    # Change of possessions require 1 - WP
    if(names(scenarios)[i] %in% c("fg", "fail", "punt", "missed_fg", "touchdown")){
      pred_prob <- 1 - pred_prob
    }
    
    probs[[paste0(names(scenarios)[i], "_wp")]] <- pred_prob
  }
  
  # Account for situations in which an opponent's field goal can end
  # the game, driving win probability down to 0
  
  if(situation$secs_left < 40 & (situation$score_diff >= 0 & situation$score_diff <= 2) &
     situation$timo == 0){
    # Estimate probability of successful field goal and
    # set the win probability of failing to convert a 4th down
    # to that win probability.
    if(situation$dome > 0){
      prob_opp_fg <- with(data, dome_rate[fgs == scenarios$fail$yfog])[1]
    } else {
      prob_opp_fg <- with(data, open_rate[fgs == scenarios$fail$yfog])[1]
    }
    
    probs$fail_wp <- ((1 - prob_opp_fg) * probs$fail_wp)
  }
  
  # Teams may not get the ball back during the 4th quarter
  if(situation$qtr == 4){
    probs$fail_wp <- probs$fail_wp * situation$poss_prob
    probs$punt_wp <- probs$punt_wp * situation$poss_prob
  }
  
  # Always have a "success_wp" field, regardless of TD or 1st down
  if("touchdown_wp" %in% names(probs)){
    probs$success_wp <- probs$touchdown_wp
  } else {
    probs$success_wp <- probs$first_down_wp
  }
  
  return(probs)
}

#' Decide on optimal play based on game states and their associated
#' win probabilities.
#' 
#' Note the currently 'best play' is based purely
#' on the outcome with the highest expected win probability. This
#' does not account for uncertainty of these estimates.
#' For example, the win probabilty added by a certain play may be
#' very small (0.0001), but that may be the 'best play.'
#'
#' @param situation 
#' @param data 
#' @param probs 
#'
#' @return
generate_decision <- function(situation, data, probs){
  
  decision <- list()
  
  decision$prob_success <- calc_prob_success(situation, data)
  
  # Expected value of win probability of going for it
  wp_ev_goforit <- expected_win_prob(decision$prob_success,
                                     probs$success_wp,
                                     probs$fail_wp)
  probs$wp_ev_goforit <- wp_ev_goforit
  
  # Expected value of kick factors in probability of FG
  probs$prob_success_fg <- expected_wp_fg(situation, probs, data)$prob_success_fg
  probs$fg_ev_wp <- expected_wp_fg(situation, probs, data)$fg_ev_wp
  
  # If the offense can end the game with a field goal, set the
  # expected win probability for a field goal attempt to the
  # probability of a successful field goal kick
  if(situation$secs_left < 40 & (situation$score_diff >= -2 & situation$score_diff <= 0) &
     situation$timd == 0){
    probs$fg_wp <- probs$prob_success_fg
    probs$fg_ev_wp <- probs$prob_success_fg
  }
  
  # If down by more than a field goal in the 4th quarter, need to
  # incorporate the probability that you will get the ball back
  if(situation$qtr == 4 & situation$score_diff < -3){
    probs$fg_ev_wp <- probs$fg_ev_wp * situation$poss_prob
  }
  
  # Breakeven success probabilities
  decision$breakeven_punt <- breakeven(probs)$breakeven_punt
  decision$breakeven_fg <- breakeven(probs)$breakeven_fg
  
  # Of the kicking options, pick the one with the highest E(WP)
  decision$kicking_option <- best_kicking_option(probs, wp_ev_goforit)$decision
  decision$wpa_going_for_it <- best_kicking_option(probs, wp_ev_goforit)$win_prob_added
  
  # Make the final call on kick / punt / go for it
  # If a win is unlikely in any circumstance, favor going for it.
#   if(probs$pre_play_wp < .05){
#     decision$best_play <- "go for it"
#   } else {
  decision$best_play <- decide_best_play(decision)
  # }
  
  # Only provide historical data outside of two-minute warning
  decision <- get_historical_decision(situation, data, decision)
  
  return(list(decision = decision,
              probs = probs))
}

#' Compare current game situation to historically similar situations.
#' 
#' Currently uses score difference and field position to provide
#' rough guides to what coaches have done in the past.
#'
#' @param situation 
#' @param data 
#' @param decision 
#'
#' @return
get_historical_decision <- function(situation, data, decision){
  historical_data <- data$decisions
  
  down_by_td <- situation$score_diff <= -4
  up_by_td <- situation$score_diff >= 4
  yfog_bin <- floor(situation$yfog / 20)
  short_tg <- as.numeric(situation$ytg <= 3)
  med_tg <- as.numeric(situation$ytg >= 4 & situation$ytg <= 7)
  long_tg <- as.numeric(situation$ytg > 7)
  
  history <- historical_data %>%
    dplyr::filter(down_by_td == down_by_td &
                    up_by_td == up_by_td &
                    yfog_bin == yfog_bin &
                    short == short_tg &
                    med == med_tg &
                    long == long_tg)
  
  if(nrow(historical_data) == 0){
    decision$historical_goforit_pct <- "None"
    decision$historical_punt_pct <- "None"
    decision$historical_kick_pct <- "None"
    decision$historical_N <- "None"
  } else {
    decision$historical_goforit_pct <- history$proportion_punted[1]
    decision$historical_punt_pct <- history$proportion_kicked[1]
    decision$historical_kick_pct <- history$proportion_went[1]
    decision$historical_N <- history$sample_size[1]
  }
  
  return(decision)
}

#' Expected value of win probability, factoring in p(success).
#'
#' @param pos_prob 
#' @param pos_win_prob 
#' @param neg_win_prob 
#'
#' @return
expected_win_prob <- function(pos_prob, pos_win_prob, neg_win_prob){
  return((pos_prob * pos_win_prob) + ((1 - pos_prob) * neg_win_prob))
}

#' Expected WP from kicking, factoring in p(FG made).
#'
#' @param situation 
#' @param probs 
#' @param data 
#'
#' @return
expected_wp_fg <- function(situation, probs, data){
  if("fg_make_prob" %in% names(situation) & is.numeric(situation$fg_make_prob)){
    pos <- situation$fg_make_prob
  } else {
    fgs <- data$fgs
    
    # Set the probability of success of implausibly long kicks to 0
    if(situation$yfog < 42){
      pos <- 0
    } else {
      # Account for indoor vs. outdoor kicking
      if(situation$dome > 0){
        pos <- with(fgs, dome_rate[fgs == situation$yfog])[1]
      } else {
        pos <- with(fgs, open_rate[fgs == situation$yfog])[1]
      }
    }
  }
  
  return(list(prob_success_fg = pos,
              fg_ev_wp = expected_win_prob(pos, probs$fg_wp, probs$missed_fg_wp)))
}

#' Calculates the breakeven point for making the decision.
#' 
#' The breakeven is the point at which a coach should be indifferent
#' between two options. We compare the expected win probability
#' of going for it on 4th down to the next best kicking option
#' and determine what the probability of converting the 4th down
#' needs to be in order to make the coach indifferent to going for it
#' or kicking.
#'
#' @param probs 
#'
#' @return
breakeven <- function(probs){
  denom <- probs$success_wp - probs$fail_wp
  
  breakeven_punt <- (probs$punt_wp - probs$fail_wp) / denom
  breakeven_fg <- (probs$fg_ev_wp - probs$fail_wp) / denom
  
  # Coerce breakevens to be in the range [0, 1]
  breakeven_punt <- max(min(1, breakeven_punt), 0)
  breakeven_fg <- max(min(1, breakeven_fg), 0)
  
  return(list(breakeven_punt = breakeven_punt,
              breakeven_fg = breakeven_fg))
}

#' Use historical first down rates. When inside the opponent's 10,
#' use dwn, ytg, yfog specific rates. Otherwise, use binned yfog where
#' field is broken into 10 segments
#'
#' @param situation 
#' @param data 
#'
#' @return
calc_prob_success <- function(situation, data){
  fd_open <- data$fd_open_field
  fd_inside <- data$fd_inside_10
  
  if(situation$yfog < 90){
    tryCatch({
      yfog_bin <- floor(situation$yfog / 10)
      p_success <- with(fd_open, fdr[dwn == situation$dwn &
                                       ytg == situation$ytg &
                                       yfog_bin == yfog_bin])[1]
    },
    error = function(e) {
      # Arbitrary, set the probability of success for very long
      # 4th downs to be 0.1
      p_success <- 0.1
    })
  } else {
    p_success <- with(fd_inside, fdr[dwn == situation$dwn &
                                     ytg == situation$ytg &
                                     yfog_bin == yfog_bin])[1]
  }
  
  return(p_success)
}

#' Use the expected win probabilities to determine best kicking option
#'
#' @param probs 
#' @param wp_ev_goforit 
#'
#' @return
best_kicking_option <- function(probs, wp_ev_goforit){
  # Account for end of game situations where FG WP is higher
  if(probs$fg_ev_wp > probs$punt_wp &
     probs$prob_success_fg > .3){
    decision <- "kick"
    win_prob_added <- wp_ev_goforit - probs$fg_ev_wp
  } else {
    decision <- "punt"
    win_prob_added <- wp_ev_goforit - probs$punt_wp
  }
  
  return(list(decision = decision,
              win_prob_added = win_prob_added))
}

decide_best_play <- function(decision){
  if(decision$kicking_option == "punt" & decision$prob_success < decision$breakeven_punt){
    return("punt")
  } else if(decision$kicking_option == "kick" & decision$prob_success < decision$breakeven_fg){
    return("kick")
  } else {
    return("go for it")
  }
}

#' Generate a random play with plausible values for debugging purposes.
#'
#' @param data 
#'
#' @return
random_play <- function(data){
  features <- data$features
  situation <- vector("list", length = length(features))
  names(situation) <- features
  situation <- lapply(situation, replace_null)
  
  situation$dwn <- 4
  situation$ytg <- floor(runif(1, 1, 10))
  situation$yfog <- floor(runif(1, 1, (100 - situation$ytg)))
  situation$secs_left <- floor(runif(1, 1, 3600))
  situation$score_diff <- floor(runif(1, -20, 20))
  situation$timeo <- round(runif(1, 1, 3), digits = 0)
  situation$timed <- round(runif(1, 1, 3), digits = 0)
  situation$spread <- 0
  
  situation <- calculate_features(situation, data)
  
  situation$dome <- round(runif(1, 0, 1), digits = 0)
  return(situation)
}

