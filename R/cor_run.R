#' my cor reporting func
#'
#' report correlations as a ordered printed data frame
#'
#' @param df A data frame containing variables.
#' @return A printed data frame contains pairwise correlations and descriptions between variables

#' @export
cor_run <- function(df, vars) {
  # 生成所有变量的两两组合
  var_combinations <- expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    filter(var1 != var2) %>% # 移除自身与自身的组合
    mutate(
      var1_type = map_chr(var1, ~ ifelse(is.numeric(df[[.]]), "continuous", "categorical")),
      var2_type = map_chr(var2, ~ ifelse(is.numeric(df[[.]]), "continuous", "categorical"))
    )

  # 计算相关性
  results <- var_combinations %>%
    mutate(
      result = pmap(list(var1, var2, var1_type, var2_type), function(v1, v2, t1, t2) {
        x_var <- df[[v1]]
        y_var <- df[[v2]]
        
        # 处理缺失值
        complete_cases <- complete.cases(x_var, y_var)
        x_var <- x_var[complete_cases]
        y_var <- y_var[complete_cases]
        
        # 检查变量有效性
        if (length(unique(x_var)) <= 1 || length(unique(y_var)) <= 1) {
          return(tibble(p.value = NA, method = "invalid", correlation = NA, trend = "n/a"))
        }

        # 连续-连续
        if (t1 == "continuous" && t2 == "continuous") {
          test <- tryCatch(cor.test(x_var, y_var, method = "pearson"), error = function(e) NULL)
          if (!is.null(test)) {
            return(tibble(
              p.value = test$p.value,
              method = "Pearson",
              correlation = test$estimate,
              trend = ifelse(test$estimate > 0, "↑ positive", "↓ negative")
            ))
          } else {
            return(tibble(p.value = NA, method = "Pearson error", correlation = NA, trend = "n/a"))
          }
        }
        # 连续-分类
        else if (t1 == "continuous" && t2 == "categorical") {
          y_factor <- as.factor(y_var)
          if (nlevels(y_factor) == 2) {
            # 二分类变量
            test <- tryCatch(t.test(x_var ~ y_factor), error = function(e) NULL)
            cor_val <- tryCatch(cor(x_var, as.numeric(y_factor)), error = function(e) NA)
            group_means <- tapply(x_var, y_factor, mean)
            levels_order <- names(group_means)
            trend_desc <- "n/a"
            
            if (length(group_means) == 2 && !any(is.na(group_means))) {
              if (group_means[2] > group_means[1]) {
                trend_desc <- paste0("↑ from '", levels_order[1], "' to '", levels_order[2], "'")
              } else {
                trend_desc <- paste0("↓ from '", levels_order[2], "' to '", levels_order[1], "'")
              }
            }
            
            if (!is.null(test)) {
              return(tibble(p.value = test$p.value, method = "t-test", correlation = cor_val, trend = trend_desc))
            } else {
              return(tibble(p.value = NA, method = "t-test error", correlation = cor_val, trend = trend_desc))
            }
          } else {
            # 多分类变量
            fit <- tryCatch(lm(as.numeric(y_factor) ~ x_var), error = function(e) NULL)
            aov_fit <- tryCatch(lm(x_var ~ y_factor), error = function(e) NULL)
            trend_desc <- "n/a"
            cor_val <- NA
            p_val <- NA
            
            if (!is.null(fit)) {
              slope <- coef(fit)[2]
              predicted <- levels(y_factor)[order(coef(fit)[1] + coef(fit)[2] * seq(min(x_var), max(x_var), length.out=length(levels(y_factor))))]
              if (!is.na(slope)) {
                if (slope > 0) {
                  trend_desc <- paste0("↑ more likely to be '", predicted[length(predicted)], "'")
                } else {
                  trend_desc <- paste0("↓ more likely to be '", predicted[1], "'")
                }
              }
            }
            
            if (!is.null(aov_fit)) {
              aov_table <- tryCatch(anova(aov_fit), error = function(e) NULL)
              if (!is.null(aov_table)) {
                ss_between <- aov_table[1, "Sum Sq"]
                ss_total <- sum(aov_table[,"Sum Sq"], na.rm = TRUE)
                eta_sq <- ss_between / ss_total
                cor_val <- sqrt(eta_sq)
                p_val <- tryCatch(tidy(aov_table)[1, "p.value", drop=TRUE], error = function(e) NA)
                return(tibble(p.value = p_val, method = "ANOVA", correlation = cor_val, trend = trend_desc))
              }
            }
            
            return(tibble(p.value = NA, method = "ANOVA error", correlation = cor_val, trend = trend_desc))
          }
        }
        # 分类-连续（交换变量顺序，调用连续-分类逻辑）
        else if (t1 == "categorical" && t2 == "continuous") {
          y_factor <- as.factor(x_var)
          x_cont <- y_var
          if (nlevels(y_factor) == 2) {
            test <- tryCatch(t.test(x_cont ~ y_factor), error = function(e) NULL)
            cor_val <- tryCatch(cor(x_cont, as.numeric(y_factor)), error = function(e) NA)
            group_means <- tapply(x_cont, y_factor, mean)
            levels_order <- names(group_means)
            trend_desc <- "n/a"
            
            if (length(group_means) == 2 && !any(is.na(group_means))) {
              if (group_means[2] > group_means[1]) {
                trend_desc <- paste0("↑ from '", levels_order[1], "' to '", levels_order[2], "'")
              } else {
                trend_desc <- paste0("↓ from '", levels_order[2], "' to '", levels_order[1], "'")
              }
            }
            
            if (!is.null(test)) {
              return(tibble(p.value = test$p.value, method = "t-test", correlation = cor_val, trend = trend_desc))
            } else {
              return(tibble(p.value = NA, method = "t-test error", correlation = cor_val, trend = trend_desc))
            }
          } else {
            fit <- tryCatch(lm(as.numeric(y_factor) ~ x_cont), error = function(e) NULL)
            aov_fit <- tryCatch(lm(x_cont ~ y_factor), error = function(e) NULL)
            trend_desc <- "n/a"
            cor_val <- NA
            p_val <- NA
            
            if (!is.null(fit)) {
              slope <- coef(fit)[2]
              predicted <- levels(y_factor)[order(coef(fit)[1] + coef(fit)[2] * seq(min(x_cont), max(x_cont), length.out=length(levels(y_factor))))]
              if (!is.na(slope)) {
                if (slope > 0) {
                  trend_desc <- paste0("↑ more likely to be '", predicted[length(predicted)], "'")
                } else {
                  trend_desc <- paste0("↓ more likely to be '", predicted[1], "'")
                }
              }
            }
            
            if (!is.null(aov_fit)) {
              aov_table <- tryCatch(anova(aov_fit), error = function(e) NULL)
              if (!is.null(aov_table)) {
                ss_between <- aov_table[1, "Sum Sq"]
                ss_total <- sum(aov_table[,"Sum Sq"], na.rm = TRUE)
                eta_sq <- ss_between / ss_total
                cor_val <- sqrt(eta_sq)
                p_val <- tryCatch(tidy(aov_table)[1, "p.value", drop=TRUE], error = function(e) NA)
                return(tibble(p.value = p_val, method = "ANOVA", correlation = cor_val, trend = trend_desc))
              }
            }
            
            return(tibble(p.value = NA, method = "ANOVA error", correlation = cor_val, trend = trend_desc))
          }
        }
        # 分类-分类
        else {
          x_factor <- as.factor(x_var)
          y_factor <- as.factor(y_var)
          test <- tryCatch(chisq.test(table(x_factor, y_factor)), error = function(e) NULL)
          cor_val <- tryCatch(cramerV(table(x_factor, y_factor)), error = function(e) NA)
          if (!is.null(test)) {
            return(tibble(
              p.value = test$p.value,
              method = "Chi-square",
              correlation = cor_val,
              trend = "n/a"
            ))
          } else {
            return(tibble(p.value = NA, method = "Chi-square error", correlation = cor_val, trend = "n/a"))
          }
        }
      })
    ) %>%
    unnest(result) %>%
    select(var1, var2, var1_type, var2_type, p.value, method, correlation, trend) |> arrange(var1)

  return(results)
}