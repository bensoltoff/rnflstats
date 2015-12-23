#' kneel_down
#'
#' @param score_diff 
#' @param timd 
#' @param secs_left 
#' @param dwn 
#'
#' @return 0/1
kneel_down <- function(score_diff, timd, secs_left, dwn){
  if(score_diff <= 0 | dwn == 4){
    return(0)
  }
  
  if(timd == 0 & secs_left <= 120 & dwn == 1){
    return(1)
  }
  if(timd == 1 & secs_left <= 87 & dwn == 1){
    return(1)
  }
  if(timd == 2 & secs_left <= 48 & dwn == 1){
    return(1)
  }
  
  if(timd == 0 & secs_left <= 84 & dwn == 2){
    return(1)
  }
  if(timd == 1 & secs_left <= 45 & dwn == 2){
    return(1)
  }
  
  if(timd == 0 & secs_left <= 42 & dwn == 3){
    return(1)
  }
  
  return(0)
}

replace_null <- function(x){
  if(is.null(x)){
    x <- NA
  }
  return(x)
}

#' Handles situation updating for all plays that involve
#' a change of possession, including punts, field goals,
#' missed field goals, touchdowns, turnover on downs.
#'
#' @param situation List
#' @param play_type function
#' @param features List of existing features
#' @param ... Additional parameters
#'
#' @return List
change_poss <- function(situation, play_type, features, ...){
  new_situation <- vector("list", length = length(features))
  names(new_situation) <- features
  new_situation <- lapply(new_situation, replace_null)
  
  # Nearly all changes of possession result in a 1st & 10
  # Doesn't cover the edge case of a turnover within own 10 yardline.
  new_situation$dwn <- 1
  new_situation$ytg <- 10
  
  # Assumes 10 seconds of game clock have elapsed per play
  # Could tune this.
  new_situation$secs_left <- max(c(situation$secs_left - 10, 0))
  new_situation$qtr <- qtr(new_situation$secs_left)
  
  # Assign timeouts to the correct teams
  new_situation$timo <- situation$timd
  new_situation$timd <- situation$timo
  
  # Valid types are turnover_downs, punt, field_goal,
  # missed_field_goal, touchdown
  
  # Any score changes are handled here
  new_situation <- play_type(situation, new_situation, ...)
  
  # Change sign on spread, recompter over-under
  new_situation$spread <- -1 * situation$spread + 0
  
  # Avoid negative zeros
  if(is.na(new_situation$score_diff)){
    new_situation$score_diff <- trunc(-1 * situation$score_diff)
  } else {
    new_situation$score_diff <- trunc(-1 * new_situation$score_diff)
  }
  
  new_situation$kneel_down <- kneel_down(new_situation$score_diff,
                                         new_situation$timd,
                                         new_situation$secs_left,
                                         new_situation$dwn)
  
  new_situation$qtr_scorediff <- new_situation$qtr * new_situation$score_diff
  
  return(new_situation)
}

field_goal <- function(situation, new_situation){
  new_situation$score_diff <- situation$score_diff + 3
  
  # Assume the starting field position will be own 25, accounts
  # for touchbacks and some run backs.
  new_situation$yfog <- 25
  
  return(new_situation)
}

#' Opponent takes over from the spot of the kick.
#'
#' @param situation 
#' @param new_situation 
#'
#' @return
missed_field_goal <- function(situation, new_situation){
  new_situation$yfog <- 100 - (situation$yfog - 8)
  return(new_situation)
}

touchdown <- function(situation, new_situation){
  # Assumes successful XP and no 2PC -- revisit this for 2015?
  new_situation$score_diff <- situation$score_diff + 7
  new_situation$yfog <- 25
  
  return(new_situation)
}

turnover_downs <- function(situation, new_situation){
  new_situation$yfog <- 100 - situation$yfog
  return(new_situation)
}

#' Use the average net punt distance (punt distance - return yards).
#' 
#' Not all situations have historical data, especially very
#' close to opponent's end zone. Use a net punt distance of
#' 5 yards here.
#'
#' @param situation 
#' @param new_situation 
#' @param ... 
#'
#' @return
punt <- function(situation, new_situation, ...){
  default_punt <- 5
  
  pnet <- tryCatch({
    i <- which(data$yfog == situation$yfog)
    return(data$pnet[i])
  },
  error = function(e) {
    return(default_punt)
  })
  
  new_yfog <- floor(100 - (situation$yfog + pnet))
  
  # Touchback
  if(new_yfog > 0){
    new_situation$yfog <- new_yfog
  } else {
    new_situation$yfog <- 25
  }
  
  return(new_situation)
}

first_down <- function(situation){
  new_situation <- list()
  new_situation$dwn <- 1
  
  yfog <- situation$yfog + situation$ytg
  new_situation$ytg <- min(c(10, yfog))
  new_situation$yfog <- yfog
  
  # 10 seconds of clock time elapsed, or game over
  new_situation$secs_left <- max(c(situation$secs_left - 10, 0))
  
  # These values don't change
  new_situation$score_diff <- situation$score_diff
  new_situation$timo <- situation$timo
  new_situation$timd <- situation$timd
  new_situation$spread <- situation$spread
  
  new_situation$kneel_down <- kneel_down(new_situation$score_diff,
                                         new_situation$timd,
                                         new_situation$secs_left,
                                         new_situation$dwn)
  
  new_situation$qtr <- qtr(new_situation$secs_left)
  new_situation$qtr_scorediff <- new_situation$qtr * new_situation$score_diff
  
  return(new_situation)
}

#' Given the seconds left in the game, determine the current quarter.
#'
#' @param secs_left 
#'
#' @return
qtr <- function(secs_left){
  if(secs_left <= 900) return(4)
  if(secs_left <= 1800) return(3)
  if(secs_left <= 2700) return(2)
  return(1)
}
