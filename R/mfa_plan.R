
#==============================================================================
# Hàm lọc các mô hình đa biến
# First date: 12/04/2019
# Hàm này dùng để lọc mô hình đa biến cuối cùng
# Đầu vào là file data đã được tính woe
# Đầu ra là coef, pvalue, auc, ks, vif, trọng số
#=============================================================================
#=============================================================================

#=============================================================================
f_mfa_metrics <- function(fit){
  library(ROCR)
  library(magrittr)
  # KS
  logit_scores <- prediction(fit$fitted.values, fit$y)
  logit_perf <- performance(logit_scores, "tpr", "fpr")
  logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]]) %>% set_names("ks")
  # AUC
  logit_perf_auc <- performance(logit_scores, "auc")
  logit_auc <- logit_perf_auc@y.values[[1]] %>% set_names("auc")
  # Acurracy
  pred <- ifelse(fit$fitted.values > 0.5, 1, 0) %>% as.factor()
  truth <- fit$y %>% as.factor()
  conf <- caret::confusionMatrix(pred, truth, mode="everything")
  # Output
  metric <- c(logit_auc, logit_ks, conf$overall, conf$byClass) %>%
    t() %>%
    as.data.frame()

  metric
}
#=============================================================================
# Hàm chuẩn hóa hệ số beta
f_mfa_sd_beta <- function(fit){
  b <- summary(fit)$coef[-1, 1]
  sx <- sapply(fit$model[-1], sd)
  sy <- sd(fit$y)
  beta <- b * sx/sy
  return(beta)
}
#=============================================================================
# Hàm tính toán các đầu ra liên quan đến hệ số beta
f_mfa_estimate <- function(fit){
  # VIF
  mvif <- car::vif(fit) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_colnames(c("term","vif"))
  # Coef
  standard_estimate <- f_mfa_sd_beta(fit) %>%
    as.data.frame() %>%
    rownames_to_column()%>%
    set_colnames(c("term","standard_estimate"))

  coef_summary <- fit %>%
    broom::tidy() %>%
    left_join(standard_estimate, by = "term") %>%
    left_join(mvif, by = "term") %>%
    mutate(weight_estimate = abs(standard_estimate)/ sum(abs(standard_estimate), na.rm = TRUE)* 100 )

  coef_summary %>% return()

}

#=============================================================================
f_mfa_combn_predictors <- function(predictors, y = "good_bad", min = NULL, max = NULL){
  # Dạng mô  hình
  f_model_form <- function(x){paste(y,paste(x, collapse = '+'), sep = "~")}
  # Kết hợp biến
  f_combn <- function(i) {combn(predictors, i, f_model_form)}
  # Tất cả các kết hợp
  formulas <- map(min:max, f_combn) %>%
    unlist()
  # Output
  formulas %>% return()
}

#=============================================================================
f_perf <- function(fit){
  logit_performance <- list(fitted.values = fit$fitted.values, y = fit$y)
}

f_check_est <- function(est){
  est <- est[-1,]
  check_beta <- all(est$estimate < 0)
  check_pvalue <- all(est$p.value <= 0.05)
  check_vif <- all(est$vif < 10)
  check_weight <- all(est$weight_estimate < 30) & all(est$weight_estimate > 5)
  check_all <- data.frame(check_beta = check_beta,
                          check_pvalue = check_pvalue,
                          check_vif = check_vif,
                          check_weight = check_weight)
  check_all %>% return()
}


f_mfa_fit_all <- function(df, y = "good_bad", min_vars = 5, max_vars = 12){
  # Danh sách biến
  predictors <- df %>% names() %>% setdiff(y)
  k <- length(predictors)
  max_vars <- ifelse(is.null(max_vars) | max_vars > k, k, max_vars)
  # Kết hợp biến
  combine_predictor <- f_mfa_combn_predictors(predictors, y, min_vars, max_vars)
  # Xuất dữ liệu
  f_fit <- function(fm){
    fit <- glm(formula = as.formula(fm), family = binomial(link="logit"), data = df)
  }

  fit_all_model <- tibble(formula = combine_predictor) %>%
    mutate(fm = map(formula, as.formula),
           fit = map(fm, f_fit),
           metrics = map(fit, f_mfa_metrics),
           estimate = map(fit, f_mfa_estimate),
           perf = map(fit, f_perf),
           check = map(estimate, f_check_est)) %>%
    mutate(model_name = paste0("mod_", row_number())) %>%
    select(model_name, everything()) %>%
    unnest(metrics) %>%
    unnest(check) %>%
    mutate(check_auc = auc > 0.75) %>%
    mutate(check_all = rowSums(select(., contains("check"))) == 5)

  fit_all_model %>% return()

}

# Các thước đo về mô hình
f_mfa_turn_cutoffs <- function(fit){
  library(ROCR)
  logit_scores <- prediction(fit$fitted.values, fit$y)
  logit_perf_roc <- performance(logit_scores, "tpr", "fpr")
  logit_perf_prec_rec <- performance(logit_scores, "prec", "rec")
  logit_perf_sens_spec <- performance(logit_scores, "sens", "spec")
  logit_perf_lift <- performance(logit_scores, "lift", "rpp")

  logit_measure <- c("acc", "err", "fpr", "tpr", "fnr", "tnr",
                     "ppv", "npv", "phi", "lift", "f", "rch",
                     "auc", "prbe", "cal", "rmse")

  # ppv Positive predictive value = Precision
  # tpr True positive rate = Recall
  # mat Matthews correlation coefficient
  perf_all <- map(logit_measure, function(x){
    pref <- performance(logit_scores, x)
    value <- pref@y.values %>% unlist() %>%  as.numeric()
  })

  names(perf_all) <- logit_measure
  list(
    logit_perf_roc = logit_perf_roc,
    logit_perf_prec_rec = logit_perf_prec_rec,
    logit_perf_sens_spec = logit_perf_sens_spec,
    logit_perf_lift = logit_perf_lift,
    perf_all = perf_all
  ) %>% return()

}

filter_mfa_stepwise <- function(df,
                                y = "good_bad",
                                var_skip = NULL) {

  # Danh sách biến
  predictors <- df %>% names() %>% setdiff(c(y, var_skip))
  df2 <- df %>% select(c(y, predictors))
  # stepwise

  m1 <- glm(as.formula(paste(y , "~ .")), family = binomial(), data = df2)

  m_step <- step(m1, direction = "both", trace = FALSE)

  fit <- eval(m_step$call)

  auc = f_mfa_metrics(fit)$auc

  return(list(fit = fit, auc = auc))

}

#=============================================================================
mfa_plan = drake_plan(
  mfa_result = filter_mfa_stepwise(df_cor, y = "label"),
  final_model = mfa_result$fit,
  final_formula = mfa_result$fit$formula,
  mfa_auc = mfa_result$auc,
  mfa_pred = predict(final_model, df_test, type = 'response'),
  logit_scores = prediction(mfa_pred, df_test$label),
  logit_perf_auc = performance(logit_scores, "auc")@y.values[[1]]
)
