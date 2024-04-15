library(Boruta)
library(tidymodels)
library(dplyr)
library(ggplot2)

boruta_ranking <- function(data, voutcome, folder_str, predictor_labs,
                           k_outer = 5, seed = 10, verbose = F) {
  # folder_str name of the folder inside "Model" to save all the intermediate results

  # create the folder if not already exists
  ifelse(!dir.exists(file.path(folder_str)), dir.create(file.path(folder_str)), FALSE)

  # outer folds
  set.seed(seed)
  outer_cv <- rsample::vfold_cv(data, v = k_outer, strata = voutcome)

  # formula
  formula <- reformulate(".", voutcome)

  # -------- Feature selection within each fold --------
  for (i_outer in 1:k_outer) {
    print(i_outer)
    outer_i <- outer_cv$splits[[i_outer]]

    # retrieve train and test data
    train_data <- training(outer_i)
    if (verbose) print(dim(train_data))
    test_data <- testing(outer_i)
    if (verbose) print(dim(train_data))

    boruta_file <- paste0(folder_str, "/boruta.", i_outer, ".rds")

    if (file.exists(boruta_file)) {
      boruta.cv <- readRDS(boruta_file)
    } else {
      set.seed(seed)

      ## feature importance using boruta
      boruta.cv_train <- Boruta(formula, data = train_data, doTrace = 0)
      if (verbose) print(boruta.cv_train)

      ## take a call on tentative features
      boruta.cv <- TentativeRoughFix(boruta.cv_train)
      if (verbose) print(boruta.cv)

      ## save result
      saveRDS(boruta.cv, boruta_file)
    }
  }

  # -------- Have a look at the feature importances in each fold --------
  npredictor <- nrow(predictor_labs)
  boruta.res <- data.frame()
  for (i_outer in 1:k_outer) {
    boruta_file <- paste0(folder_str, "/boruta.", i_outer, ".rds")
    boruta.cv.i <- readRDS(boruta_file)

    lz<-lapply(1:ncol(boruta.cv.i$ImpHistory),function(i)
      boruta.cv.i$ImpHistory[is.finite(boruta.cv.i$ImpHistory[,i]),i])
    names(lz) <- colnames(boruta.cv.i$ImpHistory)

    median <- sapply(lz, median)[1:npredictor] # remove the last three "shadowMax", "shadowMean", "shadowMin"
    lower  <- sapply(lz, quantile, probs = 0.25)[1:npredictor]
    names(lower) <- gsub('.npredictor%','',names(lower))
    upper  <- sapply(lz, quantile, probs = 0.75)[1:npredictor]
    names(upper) <- gsub('.75%','',names(upper))

    decisions <- as.character(boruta.cv.i$finalDecision)
    names(decisions) <- attr(boruta.cv.i$finalDecision, "names")

    boruta.res.i <- data.frame(fold = rep(i_outer, length(decisions)),
                               var = names(decisions),
                               median = median,
                               lower = lower,
                               upper = upper,
                               decisions = decisions) %>%
      arrange(median)
    rownames(boruta.res.i) <- NULL

    boruta.res <- bind_rows(boruta.res, boruta.res.i)
  }

  # -------- Ranking variables based on mean importance values across 5 folds --------
  boruta.res.avg <- boruta.res %>%
    group_by(var) %>%
    summarise(importance = mean(median),
              nconfirmed = sum(decisions == "Confirmed")) %>%
    arrange(nconfirmed, importance)
  boruta.res.avg$nconfirmed <- factor(boruta.res.avg$nconfirmed,
                                      levels = sort(unique(boruta.res.avg$nconfirmed), decreasing = TRUE))
  boruta.res.avg$var <- factor(boruta.res.avg$var, levels = boruta.res.avg$var)

  boruta.res <- boruta.res %>% left_join(boruta.res.avg, by = "var")
  boruta.res <- boruta.res %>% left_join(predictor_labs, by = "var")
  boruta.res$var <- factor(boruta.res$var, levels = rev(boruta.res.avg$var))
  boruta.res <- boruta.res %>% arrange(var)
  boruta.res$label <- factor(boruta.res$label, levels = rev(unique(boruta.res$label)))

  # -------- Plot --------
  n_counts <- table(boruta.res.avg$nconfirmed)
  intercepts <- rep(0, length(n_counts) - 1)
  for(i in 1:(length(n_counts) - 1)) {
    intercepts[i] <- npredictor - (sum(n_counts[1:i]) - 0.5)
  }

  figure <- ggplot(boruta.res, aes(label, median, fill = nconfirmed, color = nconfirmed)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5)  +
    labs(x = "Variables",
         y = paste("Variable importance across", k_outer ,"folds"),
         fill = "# selected",
         color = "# selected")+
    geom_vline(xintercept = intercepts, color = "red", linetype = "dashed")+
    coord_flip()+
    theme_bw()+
    theme(text = element_text(size = 12),
          legend.position = c(0.75, 0.25),
          axis.title.y = element_blank())

  return(list(boruta.res = boruta.res,
              boruta.res.avg = boruta.res.avg,
              figure = figure))
}
