# The outer layer of subsetAUC that is visible to users
#' Calculate the single-item AUC values and rank the items accordingly.
#'
#' @param group            Vector of group labels
#' @param response         Data frame of responses: using the binary (0/1) for multiple choice test and option weight for scaled test
#' @param indList          Indices of items in each subset
#' @param nfold            Number of fold for cross validation (CV), default as 1: no CV
#' @param seed             Random seed for CV
#' @param is_train         Flag of if training or testing
#' @param group_name       A string vector with labels of the two groups of interest
#' @param ind_subset       A vector of the indices of subsets to be highlighted
#' @param stratifier       Vector of stratifier, e.g. study visits
#' @param stratifier_label For plotting, labels of the stratifier
#' @param type_vec         For plotting, labels of each subset
#' @param rank_vec         For plotting, labels of different rank
#' @param rank_order       If there are multiple item rankings, the order of rankings for plotting
#' @param rank_cols        If there are multiple item rankings, the color of each ranking for plotting
#' @param level1           For plotting, ranks to be highlighted
#' @param level2           For plotting, ranks to be highlighted, type 2
#' @param titlestr         For plotting, plot title
#' @param ci               For plotting, flag for if plotting confidence interval
#' @param legend.posistion For plotting, position of the color legend.
#' @param ymin             For plotting, minimum value of the y-axis
#' @param ymax             For plotting, maximum value of the y-axis
#' @param new_line         For plotting, An additional reference line
#' @return A list of below components:
#'         subAUC Data frame of the results
#'         maxInd Data frame, indices of the subsets with highest AUC values
#'         p      Plot
#' @export
subsetAUC <- function(group, response, indList,
                      nfold = 1, seed = 10, is_train = TRUE,
                      group_name = NULL, ind_subset = NULL,
                      stratifier = NULL, stratifier_label = NULL,
                      type_vec = NULL,
                      rank_vec = NULL, rank_order = NULL, rank_cols = NULL,
                      level1 = NULL, level2 = NULL,
                      titlestr = "Title", ci = TRUE,
                      legend.posistion = "None", ymin = 0.5, ymax = 1.0,
                      new_line = NULL,
                      ylab = "Sub-test AUC value",
                      xlab = "Number of items in each subset") {
  # to work with the "no visible binding" note
  nitem <- NULL

  # =========================================
  n <- ncol(response) # number of items
  data_itmvec <- names(response)

  # Checking inputs
  if (length(group) != nrow(response)) {
    stop("length(group) != nrow(response)")
  }

  if (is.null(group_name)) group_name <- unique(group)
  if (length(group_name) != 2) {
    stop("The current function can only handle two groups.")
  }

  if (!is.null(stratifier)) {
    if (length(group) != length(stratifier)) {
      stop("length(group) != length(stratifier)")
    }

    if (is.null(stratifier_label)) stratifier_label <- unique(stratifier[!is.na(stratifier)])
    if (length(stratifier_label) != length(unique(stratifier[!is.na(stratifier)]))) {
      stop("Length of stratifier_label and levels of stratifier do not match.")
    }
  } else {
    stratifier <- rep("all", length(group))
    stratifier_label <- c("all")
  }

  if (!is.null(rank_vec)) {
    if (is.null(rank_order)) rank_order <- unique(rank_vec)
  } else {
    rank_vec <- rep("This", length(indList))
  }

  # Create data frame
  data_sub <- bind_cols(response, group = group, stratifier = stratifier)
  data_sub <- data_sub %>% filter(group %in% group_name)

  # Number of item in each subtest
  n_subtest <- length(indList)
  nitem_vec <- rep(0, n_subtest)
  for (i in 1:n_subtest) {
    nitem_vec[i] <- length(indList[[i]])
  }

  # Name of first rank if compare multiple item rankings
  if (is.null(rank_vec)) {
    rank_vec <- rep("None", n_subtest)
  }

  if (is.null(level1)) {
    level1 <- "None"
  }

  subAUC <- data.frame()
  maxInd <- data.frame()

  for (j in seq_len(length(stratifier_label))) {
    subAUC_j <- data.frame()
    maxInd_j <- data.frame()

    strfj <- stratifier_label[j]
    df_j <- data_sub %>% filter(stratifier == strfj)

    # Create k folds
    set.seed(seed)
    if (nfold > 1) {
      trainInds <- caret::createFolds(df_j$group, k = nfold, returnTrain=T)
    } else {
      nfold <- 1
      trainInds <- list()
      trainInds[[1]] <- 1:nrow(df_j)
    }

    # core
    for (ifold in 1:nfold) {
      if (nfold > 1) {
        if (is_train) {
          dfi <- df_j[trainInds[[ifold]],]
        } else {
          dfi <- df_j[-trainInds[[ifold]],]
        }
      } else {
        dfi <- df_j
      }

      # Iterate through top-1 to top-n items and calculate the subset AUC
      df_auc <- subsetAUC.inner(dfi, data_itmvec, indList,
                                type_vec, group_name)

      df_auc$nitem <- nitem_vec
      df_auc$rank <- rank_vec
      df_auc$fold <- rep(ifold, nrow(df_auc))
      df_auc$stratifier <- rep(strfj, nrow(df_auc))

      # save it
      subAUC_j <- bind_rows(subAUC_j, df_auc)

      # ------------------------------------------
      # Find the index of the best-perform subtest
      maxInd_i <- df_auc %>%
        group_by(rank) %>%
        summarise(max_ind = which(auc == max(auc)))
      maxInd_i$fold <- rep(ifold, nrow(maxInd_i))
      maxInd_i$stratifier <- rep(strfj, nrow(maxInd_i))

      # save it
      maxInd_j <-  bind_rows(maxInd_j, maxInd_i)
    }

    subAUC <- bind_rows(subAUC, subAUC_j)
    maxInd <-  bind_rows(maxInd, maxInd_j)
  }

  # plot
  if (nfold > 1) {
    df_ave <- subAUC %>%
      group_by(rank, nitem) %>%
      summarise(mean = mean(auc),
                min = min(auc),
                max = max(auc))
    df_ave_1 <- df_ave %>% filter(rank %in% level1)
    if (!is.null(level2)) {
      df_ave_2 <- df_ave %>% filter(rank %in% level2)
    }
    if (!is.null(ind_subset)) {
      df_subset <- data.frame()
      for (ii in seq_len(length(ind_subset))) {
        if (!is.na(ind_subset[ii])) {
          df_subset <- bind_rows(df_subset,
                                 df_ave %>% filter(rank == rank_order[ii],
                                                   nitem == ind_subset[ii]))
        }
      }
    }
    df_max <- df_ave_1 %>% filter(mean == max(df_ave_1$mean))
    auc_n <- df_ave_1 %>% filter(nitem == n)
    names(auc_n)[names(auc_n) == "mean"] <- "auc_n"
  } else {
    df_ave <- subAUC
    names(df_ave)[names(df_ave) == "auc"] <- "mean"
    df_ave_1 <- df_ave %>% filter(rank %in% level1)
    if (!is.null(level2)) {
      df_ave_2 <- df_ave %>% filter(rank %in% level2)
    }
    if (!is.null(ind_subset)) {
      df_subset <- data.frame()
      for (ii in seq_len(length(ind_subset))) {
        if (!is.na(ind_subset[ii])) {
          df_subset <- bind_rows(df_subset,
                                 df_ave %>% filter(rank == rank_order[ii],
                                                   nitem == ind_subset[ii]))
        }
      }
    }
    auc_n <- df_ave_1 %>% group_by(stratifier) %>%
      summarise(auc_n = last(mean))
    df_max <- df_ave_1 %>% group_by(stratifier) %>%
      summarise(mean = max(mean))
    df_max$nitem <- df_ave_1[df_ave_1$mean %in% df_max$mean,]$nitem
  }

  p <- ggplot(df_ave, aes(nitem, mean, color = rank))+
    geom_point()+
    geom_line(linetype = "dotted")

  if (!is.null(ind_subset)) {
    p <- p +
      geom_point(data = df_subset, aes(nitem, mean, color = rank), size = 4) +
      scale_size(guide = 'none')
  }

  if (!is.null(level2)) {
    p <- p +
      geom_point(data = df_ave_2, aes(nitem, mean, color = rank), size = 2)+
      geom_line(data = df_ave_2, aes(nitem, mean, color = rank), size=1, linetype = "dashed")
  }
  p <- p +
    geom_point(data = df_ave_1, aes(nitem, mean, color = rank), size = 2)+
    geom_line(data = df_ave_1, aes(nitem, mean, color = rank), size=1)

  if (ci) {
    p <- p+
      geom_linerange(aes(ymin = min, ymax = max), show.legend = F)
    if (!is.null(level2)) {
      p <- p +
        geom_linerange(data = df_ave_2, aes(x = nitem, ymin = min, ymax = max), show.legend = F)
    }
    p <- p +
      geom_linerange(data = df_ave_1, aes(x = nitem, ymin = min, ymax = max), show.legend = F)
  }

  p <- p +
    #geom_point(data = df_max, aes(nitem, mean), color = "black", size = 4, shape = 17)+
    geom_hline(data = auc_n, aes(yintercept = auc_n), color = "black", linetype = "dashed")+
    scale_x_continuous(breaks = c(1:n))+
    labs(y = ylab,
         x = xlab,
         title = titlestr)+
    ylim(ymin,ymax)+
    theme_bw()+
    theme(text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = legend.posistion)

  if (!is.null(new_line)) p <- p + geom_hline(yintercept = new_line, color = "red", linetype = "dashed")
  if (!is.null(rank_cols)) p <- p + scale_color_manual(breaks = rank_order, values = rank_cols)
  if (length(stratifier_label) > 1) p <- p + facet_wrap(~stratifier)

  return(list(subAUC = subAUC, maxInd = maxInd, p = p))
}

# --------------------------
# The core/inner layer of subsetAUC
subsetAUC.inner <- function(dfi, data_itmvec, indList,
                            type_vec, group_name) {

  # set up the data frame
  ntype <- length(indList)
  df_auc <- data.frame(auc = rep(0, ntype),
                       ci1 = rep(0, ntype),
                       ci2 = rep(0, ntype),
                       type = type_vec)

  # get sum score of the the subtests
  score_vec <- rep("", ntype)
  for (i in seq_len(ntype)) {
    cols <- data_itmvec[indList[[i]]]

    df <- dfi %>% dplyr::select(all_of(cols))
    sumscr <- rowSums(df)

    dfi$new <- sumscr
    score_name <- paste("scrsub_", i, sep = "")
    names(dfi)[ncol(dfi)] <- score_name
    score_vec[i] <- score_name
  }

  # update auc values in df_auc
  for (i in seq_len(ntype)) {
    col <- score_vec[i]

    score <- unlist(dfi %>% dplyr::select(all_of(col)))
    roc_1 <- roc(dfi$group, score, levels=group_name, na.rm=TRUE, ci=TRUE)

    if (roc_1$auc < 0.5) {
      roc_1 <- roc(dfi$group, score, levels=group_name, na.rm=TRUE, ci=TRUE,
                   direction = ">")
    }
    df_auc$auc[i] <- roc_1$auc
    df_auc$ci1[i] <- roc_1$ci[1]
    df_auc$ci2[i] <- roc_1$ci[3]
  }

  return(df_auc)
}
