predict_plan = drake_plan(
  m = glm(formula = final_formula, family = binomial(), data = df_gb),
  pred = predict(m, df_pred, type = 'response'),
  write_result = pred %>%
    set_names(30000:49999) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names(c("id", "label")) %>%
    rio::export("result.csv")
)
