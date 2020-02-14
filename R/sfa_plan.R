# Compute WOE Table
#' @title woe_cal
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export woe_cal
#' @param x The categorical variable stored as factor for which WOE Table is to be computed.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' woe_cal(x = hmeq$reason, y = hmeq$bad)
woe_cal <- function(x, y, valueOfBad = 1) {
  # check format of y
  if (is.numeric(y) == FALSE) {
    stop("y should be a numeric variable (0,1)", "\n")
  }
  # check length of y
  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  } else {
    y <- ifelse(y == valueOfBad, 1, 0)
  }

  # check type variable store of x
  if(is.numeric(x)){
    warning("x should be a factor variable")
  }

  if(length(unique(x)) > 20){
    stop("too many unique value of x")
  }

  x <- as.factor(x)
  if (sum(is.na(x)) > 0) {
    levels(x) <- c(levels(x), "missing")
    x[is.na(x)] <- "missing"
    warning("x contains missing value")
  }
  x <- droplevels(x)
  woe_table <- data.frame(matrix(nrow = nlevels(x), ncol = 10))
  names(woe_table) <- c("group", "total", "good", "bad", "pert", "pertg", "pertb", "br", "woe", "iv")
  woe_table$group <- levels(x)
  for (i in levels(x)) {
    woe_table$total[woe_table$group == i] <- sum(x == i, na.rm = TRUE)
    woe_table$good[woe_table$group == i] <- sum(x == i & y == 0, na.rm = TRUE)
    woe_table$bad[woe_table$group == i] <- sum(x == i & y == 1, na.rm = TRUE)
    if (woe_table$good[woe_table$group == i] == 0 | woe_table$bad[woe_table$group == i] == 0) {
      cat("Group:", i, "having 0 obs good/bad", "\n")
    }
  }
  woe_table$pert <- woe_table$total/sum(woe_table$total, na.rm = TRUE)
  woe_table$pertg <- woe_table$good/sum(woe_table$good, na.rm = TRUE)
  woe_table$pertb <- woe_table$bad/sum(woe_table$bad, na.rm = TRUE)
  woe_table$br <- woe_table$bad/woe_table$total
  woe_table$woe <- log(woe_table$pertg/woe_table$pertb)
  woe_table$woe[is.finite(woe_table$woe) == FALSE] <- NA
  woe_table$iv <- (woe_table$pertg - woe_table$pertb) * woe_table$woe
  return(woe_table)
}

# Compute WOE Table
#' @title woe_cal_quantiles
#' @description Compute the woe_cal_quantiles that shows the Weights Of Evidence (WOE) for each group and respective Information Values (IVs). The group X is devided by quantile
#' @details For a given actual for a Binary Y variable and a numeric X variable, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom stats quantile
#' @export woe_cal_quantiles
#' @param x The categorical variable stored as factor for which WOE Table is to be computed.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @param nbr_bin default = 20
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' woe_cal_quantiles(x = hmeq$loan, y = hmeq$bad)

woe_cal_quantiles <- function(x, y, valueOfBad = 1, nbr_bin = 15) {
  if (is.numeric(y) == FALSE) {
    stop("y should be a numeric variable (0,1)", "\n")
  }
  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  } else {
    y <- ifelse(y == valueOfBad, 1, 0)
  }

  break_x = quantile(x, probs = seq(0, 1, length.out = nbr_bin + 1), na.rm = TRUE)
  break1 = break_x[2:(length(break_x) - 1)]
  break_x = c(-Inf, break1, +Inf)
  break_x = unique(break_x)
  bin_x = cut(x, breaks = break_x, include.lowest = TRUE, right = FALSE, dig.lab = 10)

  woe_cal(bin_x, y)
}

# Compute WOE Table
#' @title woe_cal_table
#' @description Compute the woe_cal_table that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a data.frame, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export woe_cal_table
#' @importFrom dplyr select mutate_if
#' @importFrom purrr map
#' @param df data.frame.
#' @param y The actual 1/0 flags for the binary response variable. It can take values of either 1 or 0, where 1 represents the 'Bad' or 'Events' while 0 represents 'Good' or 'Non-Events'.
#' @param valueOfBad The value in y that is used to represent 'Bad' or the occurence of the event of interest. Defaults to 1.
#' @return The WOE table with the respective weights of evidence for each group and the IV's.
#' \itemize{
#'   \item group The groups (levels) of the categorical X variable for which WOE is to be calculated.
#'   \item total. The total number of observations in respective group.
#'   \item good. The total number of 'Goods' or 'Events' in respective group.
#'   \item bad. The total number of 'Bads' or 'Non-Events' in respective group.
#'   \item pert. The Percentage of observations accounted for by respective group.
#'   \item pertg. The Percentage of 'Goods' or 'Events' accounted for by respective group.
#'   \item pertb. The Percentage of 'Bads' or 'Non-Events' accounted for by respective group.
#'   \item br. The Percentage of 'Bads' or 'Non-Events' in respective group.
#'   \item woe. The computed weights of evidence(WOE) for respective group. The WOE values can be used in place of the actual group itself, thereby producing a 'continuous' alternative.
#'   \item iv. The information value contributed by each group in the X. The sum of IVs is the total information value of the categorical X variable.
#' }
#' @examples
#' data('hmeq')
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' woe_table <- woe_cal_table(df = hmeq1, y = "bad")
woe_cal_table <- function(df, y, valueOfBad = 1){
  explanatory <- df[[y]]

  if (is.numeric(explanatory) == FALSE) {
    stop("y should be a numeric variable (0,1)", "\n")
  }
  if (length(unique(explanatory)) != 2) {
    stop("y is not a binary variable", "\n")
  } else {
    explanatory <- ifelse(explanatory == valueOfBad, 1, 0)
  }

  woe_table <- df %>% select(-y) %>%
    mutate_if(is.character, as.factor) %>%
    map(woe_cal, y = explanatory)

  return(woe_table)

}


# Compute WOE for data.frame
#' @title woe_cal_df
#' @description Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
#' @details For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom purrr map map2 reduce transpose
#' @importFrom magrittr %>% set_names
#' @importFrom dplyr select inner_join mutate_if
#' @export woe_cal_df
#' @param data The data is to be computed.
#' @param woe_table The table is calculated woe
#' @return The woe data
#' \itemize{
#'   \item woe_data. The data is computed woe.
#' }
#' @examples
#' data('hmeq')
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' woe_table <- woe_cal_table(df = hmeq1, y = "bad")
#' df_woe <- woe_cal_df(data = hmeq, woe_table = woe_table)
woe_cal_df <- function(data, woe_table) {
  name_list <- list(names(woe_table), paste0(names(woe_table), "_woe")) %>% transpose()
  woe_table2 <- woe_table %>%
    map(select, c("group", "woe")) %>%
    map(mutate_if, is.character, as.factor) %>%
    map2(name_list, set_names)
  woe_table2[["data"]] <- data
  woe_data <- reduce(woe_table2, inner_join, .dir = "backward")
  return(woe_data)
}

# Compute AUC
#' @title get_auc
#' @description Compute the auc
#' @details With a Binary Y variable and a categorical X variable stored as factor. AUC value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom pROC auc
#' @importFrom stats glm binomial
#' @importFrom magrittr %>%
#' @export get_auc
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' auc1 <- get_auc(x = hmeq$reason, y = hmeq$bad)

get_auc <- function(x, y) {

  if (length(x) != length(y)) {
    stop("length(x) != length(y)")
  }

  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  }

  logit_fit <- glm(y ~ x, family = binomial(link = "logit"))
  logit_auc <- auc(logit_fit$y, logit_fit$fitted.values, levels = c(0, 1), direction = "<") %>% as.numeric()
  return(logit_auc)

}


# Compute information value
#' @title Compute information value
#' @description Compute the iv
#' @details For a given actual for a Binary Y variable and a category or continous X variable. Information value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom magrittr %>%
#' @export get_iv
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' iv1 <- get_iv(x = hmeq$reason, y = hmeq$bad)
#' iv2 <- get_iv(x = hmeq$loan, y = hmeq$bad)

get_iv <- function(x, y) {
  if (is.numeric(x)) {
    iv <- sum(woe_cal_quantiles(x, y)$iv, na.rm = TRUE)
  } else if (is.factor(x)) {
    iv <- sum(woe_cal(x, y)$iv, na.rm = TRUE)
  } else {
    stop("x is not a factor or numeric variable")
  }
  return(iv)
}


# Compute missing rate
#' @title Compute missing rate
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export get_missing
#' @param x input variable
#' @examples
#' data('hmeq')
#' get_missing(hmeq$loan)

get_missing <- function(x) {
  rate <- sum(is.na(x))/length(x)
  return(rate)
}

# Compute identical rate
#' @title Compute identical rate
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @export get_identical
#' @param x input variable
#' @examples
#' data('hmeq')
#' get_identical(hmeq$loan)

get_identical <- function(x) {
  pt = prop.table(table(x))
  max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
  return(max_rate)
}


# Compute Kolmogorov Smirnov statistic
#' @title Compute Kolmogorov Smirnov statistic
#' @description Compute the ks
#' @details With a Binary Y variable and a categorical X variable stored as factor. KS value will be calculated
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom ROCR prediction performance
#' @importFrom stats glm binomial
#' @export get_ks
#' @param y binary variable
#' @param x categorical variable
#' @return AUC value
#' @examples
#' data('hmeq')
#' ks1 <- get_ks(x = hmeq$reason, y = hmeq$bad)
get_ks <- function(x, y) {
  if (length(x) != length(y)) {
    stop("length(x) != length(y)")
  }

  if (length(unique(y)) != 2) {
    stop("y is not a binary variable", "\n")
  }
  logit_fit <- glm(y ~ x, family = binomial(link = "logit"))
  logit_scores <- prediction(predictions = logit_fit$fitted.values, labels = logit_fit$y)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  logit_ks <- max(logit_perf@y.values[[1]] - logit_perf@x.values[[1]])
  logit_ks
}


# Single factor analysis
#' @title Filter variable
#' @description This function filter variables base on specified conditions, such as information value, missing rate, identical value rate, Kolmogorov Smirnov statistic.
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @importFrom dplyr select select_if mutate mutate_if group_by setdiff case_when
#' @importFrom magrittr set_names
#' @importFrom purrr map_df
#' @importFrom tidyr gather nest
#' @importFrom tibble rownames_to_column
#' @export filter_sfa
#' @param df data.frame contains y and x_list variables
#' @param y binary variable
#' @param x_list list predictor variables
#' @param missing_limit The missing rate of kept variables should <= missing_limit. The default is 0.95.
#' @param iv_limit The information value of kept variables should >= iv_limit. The default is 0.02.
#' @param auc_limit The AUC of kept variables should >= auc_limit The default is 0.5.
#' @param ks_limit The Kolmogorov Smirnov of kept variables should >= ks_limit The default is 0.1.
#' @param identical_limit The identical value rate (excluding NAs) of kept variables should <= identical_limit. The default is 0.95.
#' @param var_skip Name of force kept variables, default is NULL.
#' @param return_rm_reason Logical, default is FALSE.
#' @return data.frame include pass variables
#' @examples
#' data('hmeq')
#' hmeq1 <- hmeq[,c("bad", "reason", "job")]
#' x <- filter_sfa(hmeq1,
#'            y = 'bad',
#'            missing_limit = 0.95,
#'            iv_limit = 0.3,
#'            auc_limit = 0.5,
#'            ks_limit = 0.4,
#'            identical_limit = 0.95,
#'            var_skip = 'loan',
#'            return_rm_reason = TRUE )

filter_sfa <- function(df, y, x_list = NULL, missing_limit = 0.95, iv_limit = 0.01, auc_limit = 0.5, ks_limit = 0.08,
                       identical_limit = 0.95, var_skip = NULL, return_rm_reason = TRUE) {
  # Check x_list status
  if (is.null(x_list)) {
    predictor <- names(df) %>% setdiff(c(y, var_skip))
  } else {
    predictor <- x_list %>% setdiff(c(y, var_skip))
  }

  dat <- select(df, predictor)
  explanatory <- df[[y]]

  tbl_missing <- dat %>% map_df(get_missing)
  tbl_identical <- dat %>% map_df(get_identical)
  tbl_auc <- dat %>% map_df(get_auc, y = explanatory)
  tbl_iv <- dat %>% map_df(get_iv, y = explanatory)
  tbl_ks <- dat %>% map_df(get_ks, y = explanatory)

  tbl_sfa <- tbl_missing %>%
    rbind(tbl_identical) %>%
    rbind(tbl_auc) %>%
    rbind(tbl_iv) %>%
    rbind(tbl_ks) %>%
    t() %>%
    as.data.frame() %>%
    set_names(c("missing_rate", "identical_rate", "auc", "iv", "ks")) %>%
    rownames_to_column() %>%
    mutate(rm_reason = case_when(missing_rate > missing_limit ~ paste("missing >", missing_limit),
                                 identical_rate > identical_limit ~ paste("identical >", identical_limit),
                                 auc < auc_limit ~ paste("auc <", auc_limit),
                                 iv < iv_limit ~ paste("iv <", iv_limit),
                                 ks < ks_limit ~ paste("ks <", ks_limit), TRUE ~ "pass"))

  df_pass <- df %>% select(c(y, # select y and pass variables
                             tbl_sfa[tbl_sfa$rm_reason == "pass", ]$rowname))

  return(list(tbl_sfa = tbl_sfa, df_pass = df_pass))
}

#=============================================================================

sfa_plan = drake_plan(
  df_gb = dt_final %>% filter(id  %in% 0:29999),
  df_pred = dt_final %>% filter(id  %in% 30000:49999),
  df_split = df_gb %>% initial_split(prop = 2/3, strata = "label"),
  df_train = training(df_split),
  df_test = testing(df_split),
  table_bins = bins %>% map_dfr(rbind),
  # vars_sfa_1 = df_train %>% names() %>% setdiff(c('id', 'label')) %>% sort(),
  table_single_analysis = df_train %>% filter_sfa("label", var_skip = c("id", "new_id")),
  df_sfa = table_single_analysis$df_pass,
  vars_sfa_remain = df_sfa %>% names()
)
