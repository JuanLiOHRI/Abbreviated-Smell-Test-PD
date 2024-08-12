#' Calculate the single-item AUC values and rank the items accordingly.
#'
#' @param group            Vector of group labels
#' @param response         Data frame of responses: using the binary (0/1) for multiple choice test and option weight for scaled test
#' @param nfold            Number of fold for cross validation (CV), default as 1: no CV
#' @param seed             Random seed for CV
#' @param group_name       A string vector with labels of the two groups of interest
#' @param item_label       For plotting, labels of the items
#' @param stratifier       Vector of stratifier, e.g. study visits
#' @param stratifier_name  For plotting, name of the stratifier
#' @param stratifier_label For plotting, labels of the stratifier
#' @param ntop             For plotting, number of top items to be highlighted
#' @param nbottom          For plotting, number of bottom items to be highlighted
#' @return A list of below components:
#'         df_ave Data frame of the results averaging across all folds
#'         itmAUC Data frame of the results in each fold
#'         rankList List of final item ranking
#'         topList List of top items
#'         bottomList List of bottom items
#'         pList List of plot objects
#' @export
itemAUC <- function(group, response,
                    nfold = 1, seed = 10,
                    group_name = NULL,
                    item_label = NULL,
                    stratifier = NULL, stratifier_name = NULL, stratifier_label = NULL,
                    ntop = 5, nbottom = 5) {

  # to work with the "no visible binding" note
  item <- NULL
  rank_mean <- NULL
  fold <- NULL
  item_ind <- NULL

  # =========================================
  n <- ncol(response) # number of items

  # Checking inputs
  if (length(group) != nrow(response)) {
    stop("length(group) != nrow(response)")
  }

  if (is.null(group_name)) group_name <- unique(group)
  if (length(group_name) != 2) {
    stop("The current function can only handle two groups.")
  }

  if (is.null(item_label)) item_label <- names(response)
  if (length(item_label) != ncol(response)) {
    stop("length(item_label) != ncol(response)")
  }

  if (!is.null(stratifier)) {
    if (length(group) != length(stratifier)) {
      stop("length(group) != length(stratifier)")
    }

    if (is.null(stratifier_name)) {
      if (is.null(names(stratifier))) {
        stratifier_name <- "stratifier"
      } else {
        stratifier_name <- names(stratifier)
      }
    }

    if (is.null(stratifier_label)) stratifier_label <- unique(stratifier[!is.na(stratifier)])
    if (length(stratifier_label) != length(unique(stratifier[!is.na(stratifier)]))) {
      stop("Length of stratifier_label and levels of stratifier do not match.")
    }
  } else {
    stratifier <- rep("all", length(group))
    stratifier_name <- "stratifier"
    stratifier_label <- c("all")
  }

  # Create data frame
  data_item <- bind_cols(response, group = group, stratifier = stratifier)
  data_item <- data_item %>% filter(group %in% group_name)

  # ===============================================
  # Single item AUC: ranking items in each fold
  itmAUC <- data.frame()
  df_ave <- data.frame()
  rankList <- list()
  topList <- list()
  bottomList <- list()
  pList <- list()

  for (j in seq_len(length(stratifier_label))) {
    itmAUC_j <- data.frame()
    strfj <- stratifier_label[j]
    df_j <- data_item %>% filter(stratifier == strfj)

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
      data_itm <- df_j[trainInds[[ifold]],]

      df_order <- itemAUC.inner(data_itm, group_name)
      df_order$fold <- rep(ifold, nrow(df_order))
      df_order$stratifier <- rep(strfj, nrow(df_order))

      df_order <- df_order %>% arrange(auc)
      df_order$rank <- c(1:n)

      itmAUC_j <- bind_rows(itmAUC_j, df_order)
    }

    vec <- rep("", nrow(itmAUC_j))
    for (i in seq_len(nrow(itmAUC_j))) {
      vec[i] <- item_label[itmAUC_j$item_ind[i]]
    }
    itmAUC_j$item <- vec

    itmAUC <- bind_rows(itmAUC, itmAUC_j)

    # Calculate the average ranking for all items across fold
    df <- itmAUC_j %>%
      group_by(item) %>%
      summarise(auc = mean(auc),
                rank_mean = mean(rank)) %>%
      arrange(rank_mean)
    df$rank <- c(1:n)
    df$stratifier <- rep(strfj, n)

    df_ave <- bind_rows(df_ave, df)

    item_rank <- df$item
    top <- as.vector(tail(item_rank, ntop))
    bottom <- as.vector(head(item_rank, nbottom))
    itmAUC_j$item <- factor(itmAUC_j$item, levels = item_rank)

    rankList[[j]] <- item_rank
    topList[[j]] <- top
    bottomList[[j]] <- bottom

    # plot of each fold
    p1 <- ggplot(itmAUC_j, aes(fold, auc, color = item, group = item, label = item_ind))+
      geom_text(alpha = 0.5)+
      geom_line(linetype = "dotted", alpha = 0.5)+
      geom_text(data = itmAUC_j %>% filter(item %in% bottom),
                aes(fold, auc, color = item, group = item, label = item_ind))+
      geom_line(data = itmAUC_j %>% filter(item %in% bottom),
                aes(fold, auc, color = item, group = item), linetype = "dashed")+
      geom_text(data = itmAUC_j %>% filter(item %in% top),
                aes(fold, auc, color = item, group = item, label = item_ind))+
      geom_line(data = itmAUC_j %>% filter(item %in% top),
                aes(fold, auc, color = item, group = item))+
      scale_color_discrete(breaks=rev(levels(itmAUC_j$item)))+
      scale_x_continuous(breaks = c(1:10))+
      theme(legend.title = element_blank(),
            text = element_text(size=14))+
      labs(y = "Single item AUC")

    p2 <- ggplot(itmAUC_j, aes(fold, rank, color = item, group = item, label = item_ind))+
      geom_text(alpha = 0.5)+
      geom_line(linetype = "dotted", alpha = 0.5)+
      geom_text(data = itmAUC_j %>% filter(item %in% bottom),
                aes(fold, rank, color = item, group = item, label = item_ind))+
      geom_line(data = itmAUC_j %>% filter(item %in% bottom),
                aes(fold, rank, color = item, group = item), linetype = "dashed")+
      geom_text(data = itmAUC_j %>% filter(item %in% top),
                aes(fold, rank, color = item, group = item, label = item_ind))+
      geom_line(data = itmAUC_j %>% filter(item %in% top),
                aes(fold, rank, color = item, group = item))+
      scale_color_discrete(breaks=rev(levels(itmAUC_j$item)))+
      scale_x_continuous(breaks = c(1:10))+
      theme(legend.title = element_blank(),
            text = element_text(size=14))+
      labs(y = "Rank")

    p <- ggarrange(p2, p1, ncol = 2, common.legend = T, legend = "right")

    pList[[j]] <- p
  }

  return(list(df_ave = df_ave,
              itmAUC = itmAUC,
              rankList = rankList,
              topList = topList,
              bottomList = bottomList,
              pList = pList))
}

# --------------------------
# The core/inner layer of itemAUC
itemAUC.inner <- function(data_itm, group_name) {

  # number of items
  n <- ncol(data_itm) - 2

  aucvec <- rep(0,n)
  for (i in 1:n) {
    score <- as.vector(unlist(data_itm[,i]))
    roc_1 <- roc(data_itm$group, score, levels=group_name, na.rm=TRUE, ci=TRUE)

    if (roc_1$auc < 0.5) {
      roc_1 <- roc(data_itm$group, score, levels=group_name, na.rm=TRUE, ci=TRUE,
                   direction = ">")
    }
    aucvec[i] <- roc_1$auc
  }
  df_auc <- data.frame(item_ind = c(1:n),
                       auc = aucvec)

  return(df_auc)
}
