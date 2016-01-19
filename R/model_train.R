#' Calculate classification accuracy statistics.
#'
#' @param response Vector of actual wins.
#' @param predicted Vector of predicted win probabilities.
#' @param threshold Probability level that determines whether the
#' prediction is a win or a loss.
#'
#' @return Object of class \code{accuracy.meas} containing precision,
#' recall, and F-score statistics.
accuracy.meas <- function(response, predicted, threshold = 0.5) 
{
  if (length(response) != length(predicted)) 
    stop("Response and predicted must have the same length.\n")
  if (length(labels <- levels(factor(response))) != 2) 
    stop("Response must have two levels.\n")
  if (cl <- class(predicted) == "factor" | class(predicted) == 
      "character") {
    if (lev <- length(levels(factor(predicted))) != 2) 
      stop("predicted must have two levels.\n")
    predicted <- as.numeric(predicted)
  }
  splitted <- split(predicted, response)
  negatives <- splitted[[as.character(labels[1])]]
  n.negatives <- length(negatives)
  positives <- splitted[[as.character(labels[2])]]
  n.positives <- length(positives)
  TP <- sum(positives >= threshold)
  FP <- sum(negatives >= threshold)
  TN <- sum(negatives < threshold)
  FN <- sum(positives < threshold)
  PRECISION <- TP/(TP + FP)
  RECALL <- TP/(TP + FN)
  F <- 2 * (RECALL * PRECISION) / (RECALL + PRECISION)
  out <- list(Call = match.call(), threshold = threshold, precision = PRECISION, 
              recall = RECALL, F = F)
  class(out) <- "accuracy.meas"
  return(out)
}

print.accuracy.meas <- function(x, ...)
{
  cat("\n")
  cat("Call: \n")
  print(x$Call)
  cat("\n")
  cat("Examples are labelled as positive when predicted is greater than", x$threshold,"\n")
  cat("\n")
  cat(paste("precision: ", sprintf("%.3f", x$precision), "\n", sep = ""))
  cat(paste("recall: ", sprintf("%.3f", x$recall), "\n", sep = ""))
  cat(paste("F: ", sprintf("%.3f", x$F), "\n", sep = ""))
}

#' ROC Plot
#'
#' @param roc_data Data frame containing ROCR \code{performance} results.
#' @param roc_auc AUC value.
#'
#' @return Plot of ROC curve.
#' @export
plot_roc <- function(roc_data, roc_auc){
  ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = tpr, ymin = 0, ymax = tpr)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_ribbon(alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::annotate("text", x = 1, y = 0, hjust = 1.2, vjust = -1.2,
                      label = paste("Area Under the Curve =", round(roc_auc, digits = 2))) +
    ggplot2::labs(title = "Receiver Operating Characteristic",
         x = "False Positive Rate",
         y = "True Positive Rate") +
    ggplot2::theme_bw()
}

read_key <- function()
{
  cat("Press [enter] to continue", fill = TRUE)
  line <- readline()
}

#' Produces a calibration plot for the win probability model.
#' 
#' Splits the predictions into percentiles and calculates the percentage
#' of predictions per percentile that were wins. A perfectly calibrated model
#' means that plays with a win probability of n% win about n% of the time.
#'
#' @param preds Vector of predicted win probabilities.
#' @param truth Vector of actual wins.
#'
#' @return \code{ggplot2}
#' @export
calibration_plot <- function(preds, truth, fg = FALSE){
  cal_df <- dplyr::data_frame(pred = preds, win = truth,
                       pred_bin = dplyr::ntile(pred, 100))
  
  win_means <- cal_df %>%
    dplyr::group_by(pred_bin) %>%
    dplyr::summarize(pred = mean(pred),
                     win = mean(win),
                     win_sd = sqrt(win * (1 - win) / n()),
                     win_min = win - 1.96 * win_sd,
                     win_max = win + 1.96 * win_sd)

  plot <- ggplot2::ggplot(win_means, ggplot2::aes(pred, win, ymin = win_min, ymax = win_max)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_ribbon(fill = "blue", alpha = .3) +
    ggplot2::geom_line(color = "black") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_bw()
  
  if(fg == FALSE){
    plot +
      ggplot2::labs(title = "Win probability calculation, binned by percent",
                    x = "Estimated win probability",
                    y = "True win percentage")
  } else {
    plot +
      ggplot2::labs(title = "FG success probability calculation, binned by percent",
                    x = "Estimated FG success probability",
                    y = "True FG success percentage")
  }
}

#' Estimate win probability model using Armchair Analysis play-by-play data.
#' 
#' @param plot Set to 'TRUE' if you wish to view the calibration plots
#' and ROC curves for the model.
#' @return None
#' @importFrom dplyr tbl_df
#' @export
model_train <- function(plot = FALSE){
  # Only train on actual plays, remove 2pt conversion attempts
  cat("Reading play by play data.", fill = TRUE)
  df <- readr::read_csv("data/pbp_cleaned.csv")
  df_plays <- df %>%
    dplyr::filter(type != "CONV")
  
  # Custom features
  df_plays %<>%
    # Interaction between qtr & score difference -- late score differences
    # are more important than early ones.
    dplyr::mutate(qtr_scorediff = qtr * score_diff,
                  # Decay effect of spread over course of game
                  spread = spread * (secs_left / 3600))
  
  # Features to use in the model
  features <- c("dwn", "yfog", "secs_left",
                "score_diff", "timo", "timd", "spread",
                "kneel_down", "qtr",
                "qtr_scorediff")
  target <- "win"
  
  cat("Splitting data into train/test sets", fill = TRUE)
  df_plays %<>%
    dplyr::select(one_of(c(features, target))) %>%
    # remove plays with missing data
    na.omit %>%
    dplyr::mutate(train = rbinom(n(), 1, .9))
  
  train <- df_plays %>%
    dplyr::filter(train == 1) %>%
    dplyr::select(-train)
  test <- df_plays %>%
    dplyr::filter(train == 0) %>%
    dplyr::select(-train)
  
  cat("Scaling features.", fill = TRUE)
  scaled_features <- train %>%
    dplyr::select(-win) %>%
    scale
  
  train_scaled <- train %>%
    dplyr::select(-win) %>%
    scale(center = attr(scaled_features, "scaled:center"),
          scale = attr(scaled_features, "scaled:scale")) %>%
    as.data.frame %>%
    cbind(dplyr::select(train, win)) %>%
    tbl_df
  
  cat("Training model.", fill = TRUE)
  logit <- glm(win ~ ., data = train_scaled, family = binomial(link = "logit"))
  summary(logit)
  
  cat("Making predictions on test set.", fill = TRUE)
  test_scaled <- test %>%
    dplyr::select(-win) %>%
    scale(center = attr(scaled_features, "scaled:center"),
          scale = attr(scaled_features, "scaled:scale")) %>%
    as.data.frame %>%
    tbl_df %>%
    dplyr::bind_cols(dplyr::select(test, win))
  preds <- predict(logit, newdata = test_scaled, type = "response")
  
  cat("Evaluating model performance.", fill = TRUE)
  log_loss <- function(actual, predicted, eps = 0.00001) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    -1 / length(actual) * (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted)))
  }
  
  pred <- ROCR::prediction(preds, test_scaled$win)
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
  roc_data <- dplyr::data_frame(fpr = unlist(perf@x.values),
                                tpr = unlist(perf@y.values),
                                model = "GLM")
  
  roc_auc <- ROCR::performance(pred, measure = "auc")
  roc_auc <- roc_auc@y.values[[1]]
  cat(paste("AUC:", roc_auc), fill = TRUE)
  cat(paste("Log loss:", log_loss(test_scaled$win, preds)), fill = TRUE)
  
  print(accuracy.meas(test_scaled$win, preds))
  cat(paste("F1 score:", accuracy.meas(test_scaled$win, preds)$F), fill = TRUE)
  
  if(plot){
    print(plot_roc(roc_data, roc_auc))
    read_key()
    
    print(calibration_plot(preds, test_scaled$win))
  }
  
  cat("Saving model and scaler.", fill = TRUE)
  if(!(dir.exists(file.path(getwd(), "models")))){
    dir.create(file.path(getwd(), "models"))
  }
  
  saveRDS(logit, file = "models/win_probability.RDs")
  saveRDS(scaled_features, file = "models/scaler.RDs")
}

#' Estimate field goal success probability model using Armchair Analysis play-by-play data.
#' 
#' @param plot Set to 'TRUE' if you wish to view the calibration plots
#' and ROC curves for the model.
#' @return None
#' @importFrom dplyr tbl_df
#' @export
fg_model_train <- function(plot = FALSE){
  # Only train on field goal attempts
  cat("Reading play by play data.", fill = TRUE)
  df <- readr::read_csv("data/pbp_cleaned.csv")
  df_plays <- df %>%
    dplyr::filter(fgxp == "FG" &
                    !is.na(fgxp))
  
  # Custom features
  df_plays %<>%
    # Check if game is played in a closed dome
    dplyr::mutate(dome = ifelse(cond == "Closed Roof" | cond == "Dome", 1, 0),
                  wspd = ifelse(dome == 1, 0, wspd),
                  humd = ifelse(dome == 1, 0, humd)) %>%
    # Check if game is played on grass
    dplyr::mutate(grass = ifelse(surf == "Grass", 1, 0)) %>%
    # Check if there is precipiation
    dplyr::mutate(precip = ifelse(cond == "Flurries" | cond == "Light Snow" |
                                    cond == "Snow" | cond == "Light Rain" |
                                    cond == "Rain", 1, 0),
                  precip = ifelse(dome == 1, 0, precip)) %>%
    # Check if game is played at high altitude
    dplyr::mutate(highalt = ifelse(h == "DEN", 1, 0))
  
  # Features to use in the model
  features <- c("yfog", "temp", "humd",	"wspd",
                "dome", "grass", "precip", "highalt",
                "fkicker", "stad", "seas")
  target <- "good"
  
  cat("Splitting data into train/test sets", fill = TRUE)
  df_plays %<>%
    dplyr::select(one_of(c(features, target))) %>%
    # remove plays with missing data
    na.omit %>%
    dplyr::mutate(train = rbinom(n(), 1, .9))
  
  train <- df_plays %>%
    dplyr::filter(train == 1) %>%
    dplyr::select(-train)
  test <- df_plays %>%
    dplyr::filter(train == 0) %>%
    dplyr::select(-train)
  
#   cat("Scaling features.", fill = TRUE)
#   scaled_features <- train %>%
#     dplyr::select(-good) %>%
#     scale
#   
#   train_scaled <- train %>%
#     dplyr::select(-good) %>%
#     scale(center = attr(scaled_features, "scaled:center"),
#           scale = attr(scaled_features, "scaled:scale")) %>%
#     as.data.frame %>%
#     tbl_df %>%
#     dplyr::bind_cols(dplyr::select(train, good))
  
  cat("Training model.", fill = TRUE)
  logit <- glm(good ~ yfog +
                 (poly(temp, 2) + poly(humd, 2) +
                    poly(wspd, 2) + precip) * dome +
                 grass + highalt + poly(seas, 3),
               data = train, family = binomial(link = "logit"))
  summary(logit)
  
  cat("Making predictions on test set.", fill = TRUE)
#   test_scaled <- test %>%
#     dplyr::select(-good) %>%
#     scale(center = attr(scaled_features, "scaled:center"),
#           scale = attr(scaled_features, "scaled:scale")) %>%
#     as.data.frame %>%
#     tbl_df %>%
#     dplyr::bind_cols(dplyr::select(test, good))
  preds <- predict(logit, newdata = test, type = "response")
  
  cat("Evaluating model performance.", fill = TRUE)
  log_loss <- function(actual, predicted, eps = 0.00001) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    -1 / length(actual) * (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted)))
  }
  
  pred <- ROCR::prediction(preds, test$good)
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
  roc_data <- dplyr::data_frame(fpr = unlist(perf@x.values),
                                tpr = unlist(perf@y.values),
                                model = "GLM")
  
  roc_auc <- ROCR::performance(pred, measure = "auc")
  roc_auc <- roc_auc@y.values[[1]]
  cat(paste("AUC:", roc_auc), fill = TRUE)
  cat(paste("Log loss:", log_loss(test$good, preds)), fill = TRUE)
  
  print(accuracy.meas(test$good, preds))
  cat(paste("F1 score:", accuracy.meas(test$good, preds)$F), fill = TRUE)
  
  if(plot){
    print(plot_roc(roc_data, roc_auc))
    read_key()
    
    print(calibration_plot(preds, test$good, fg = TRUE))
  }
  
  cat("Saving model and scaler.", fill = TRUE)
  if(!(dir.exists(file.path(getwd(), "models")))){
    dir.create(file.path(getwd(), "models"))
  }
  
  saveRDS(logit, file = "models/fg_probability.RDs")
  # saveRDS(scaled_features, file = "models/fg_scaler.RDs")
}





