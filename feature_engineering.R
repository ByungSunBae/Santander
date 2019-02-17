library("tidyverse")
library("caret")
library("Information")
library("moments")

fread <- data.table::fread
fwrite <- data.table::fwrite

train_df <- fread("data/train.csv", data.table = FALSE) %>% as_tibble()
test_df <- fread("data/test.csv", data.table = FALSE) %>% as_tibble()

all_vars <- names(train_df)
target <- "target"
except_var <- "ID_code"

# features <- setdiff(all_vars, c(except_var, target))
# basic_summary <- 
#   train_df %>%
#   gather("var", "val", starts_with("var_")) %>%
#   group_by_at("var") %>%
#   summarise(
#     "mean" = mean(val),
#     "sd" = sd(val),
#     "min" = min(val),
#     "q10" = quantile(val, probs = 0.1),
#     "q25" = quantile(val, probs = 0.25),
#     "median" = median(val),
#     "q60" = quantile(val, probs = 0.6),
#     "q75" = quantile(val, probs = 0.75),
#     "q95" = quantile(val, probs = 0.95),
#     "max" = max(val)
#   ) %>% 
#   mutate(CV_abs = abs(sd / mean * 100))
# 
# basic_summary %>% 
#   select_at(c("var", "CV_abs")) %>% 
#   filter(CV_abs == max(CV_abs))
#   
# train_df %>% 
#   ggplot(aes(var_170)) + geom_density(aes(fill = as.factor(target)), alpha = 0.5)
# 
# basic_summary %>% 
#   select_at(c("var", "CV_abs")) %>% 
#   filter(CV_abs == min(CV_abs))
# 
# train_df %>% 
#   ggplot(aes(var_68)) + geom_density(aes(fill = as.factor(target)), alpha = 0.5)
# 
# basic_summary %>% 
#   select_at(c("var", "CV_abs")) %>% 
#   filter(var == "var_81")
# 
# IV_table <- create_infotables(train_df %>% 
#                                 select_at(c("var_170", "var_68", "var_81", "target")), 
#                               y = target, 
#                               bins = 10)
# IV_table

IV_table <- create_infotables(train_df %>% 
                                select(-ID_code), 
                              y = target, 
                              bins = 10)

features <- IV_table$Summary$Variable
sampling_weight <- IV_table$Summary$IV/sum(IV_table$Summary$IV)

crv <- 0.06

history_v <- c()

for(i in 1:5){
  cat(i, "th----\n")
  smpd_vars <- sort(sample(features, 
                           size = sample(2:100, size = 1), 
                           replace = FALSE, 
                           prob = sampling_weight)
  )
  cat("# of Sample variables:", length(smpd_vars), "\n")
  
  if(sum(history_v %in% paste(smpd_vars, collapse = ".")) >= 1){
    next
  } else {
    if(length(smpd_vars) < 30){
      Add_variables <- train_df %>% 
        select_at(c(smpd_vars, target)) %>% 
        transmute(
          sqmean = apply(select(., smpd_vars), 1, function(x) mean(x^2)),
          soft_max = apply(select(., smpd_vars), 1, function(x) max(exp(x * 0.1)/sum(exp(x * 0.1)))),
          exp_mean = apply(select(., smpd_vars), 1, function(x) mean(exp(x))),
          max = apply(select(., smpd_vars), 1, function(x) max(x)),
          min = apply(select(., smpd_vars), 1, function(x) min(x)),
          log_maxabs = apply(select(., smpd_vars), 1, function(x) log(0.5 + max(abs(x)))),
          log_absmax = apply(select(., smpd_vars), 1, function(x) log(0.5 + abs(max(x)))),
          max_inv_exp = apply(select(., smpd_vars), 1, function(x) max(1/exp(x))),
          target = apply(select(., target), 1, function(x) x)
        )
      
      Add_variables_test <- test_df %>% 
        select_at(c(smpd_vars)) %>% 
        transmute(
          sqmean = apply(select(., smpd_vars), 1, function(x) mean(x^2)),
          soft_max = apply(select(., smpd_vars), 1, function(x) max(exp(x * 0.1)/sum(exp(x * 0.1)))),
          exp_mean = apply(select(., smpd_vars), 1, function(x) mean(exp(x))),
          max = apply(select(., smpd_vars), 1, function(x) max(x)),
          min = apply(select(., smpd_vars), 1, function(x) min(x)),
          log_maxabs = apply(select(., smpd_vars), 1, function(x) log(0.5 + max(abs(x)))),
          log_absmax = apply(select(., smpd_vars), 1, function(x) log(0.5 + abs(max(x)))),
          max_inv_exp = apply(select(., smpd_vars), 1, function(x) max(1/exp(x)))
        )
      
      IV_table_t <- create_infotables(data = Add_variables, y = target)
      IV_sum <- IV_table_t$Summary
    } else {
      Add_variables <- train_df %>% 
        select_at(c(smpd_vars, target)) %>% 
        transmute(
          sqmean = apply(select(., smpd_vars), 1, function(x) mean(x^2)),
          soft_max = apply(select(., smpd_vars), 1, function(x) max(exp(x * 0.1)/sum(exp(x * 0.1)))),
          exp_mean = apply(select(., smpd_vars), 1, function(x) mean(exp(x))),
          max = apply(select(., smpd_vars), 1, function(x) max(x)),
          min = apply(select(., smpd_vars), 1, function(x) min(x)),
          log_maxabs = apply(select(., smpd_vars), 1, function(x) log(0.5 + max(abs(x)))),
          log_absmax = apply(select(., smpd_vars), 1, function(x) log(0.5 + abs(max(x)))),
          max_inv_exp = apply(select(., smpd_vars), 1, function(x) max(1/exp(x))),
          sd = apply(select(., smpd_vars), 1, function(x) sd(x)),
          sd_sq = apply(select(., smpd_vars), 1, function(x) sd(x^2)),
          skew = apply(select(., smpd_vars), 1, function(x) skewness(x)),
          skew_sq = apply(select(., smpd_vars), 1, function(x) skewness(x^2)),
          kurt = apply(select(., smpd_vars), 1, function(x) kurtosis(x)),
          kurt_sq = apply(select(., smpd_vars), 1, function(x) kurtosis(x^2)),
          q1_exp = apply(select(., smpd_vars), 1, function(x) quantile(exp(x), probs = 0.25)),
          q3_exp = apply(select(., smpd_vars), 1, function(x) quantile(exp(x), probs = 0.75)),
          CV_c = apply(select(., smpd_vars), 1, function(x) sd(x)/mean(x)),
          target = apply(select(., target), 1, function(x) x)
        )
      
      Add_variables_test <- 
        test_df %>% 
        select_at(c(smpd_vars)) %>% 
        transmute(
          sqmean = apply(select(., smpd_vars), 1, function(x) mean(x^2)),
          soft_max = apply(select(., smpd_vars), 1, function(x) max(exp(x * 0.1)/sum(exp(x * 0.1)))),
          exp_mean = apply(select(., smpd_vars), 1, function(x) mean(exp(x))),
          max = apply(select(., smpd_vars), 1, function(x) max(x)),
          min = apply(select(., smpd_vars), 1, function(x) min(x)),
          log_maxabs = apply(select(., smpd_vars), 1, function(x) log(0.5 + max(abs(x)))),
          log_absmax = apply(select(., smpd_vars), 1, function(x) log(0.5 + abs(max(x)))),
          max_inv_exp = apply(select(., smpd_vars), 1, function(x) max(1/exp(x))),
          sd = apply(select(., smpd_vars), 1, function(x) sd(x)),
          sd_sq = apply(select(., smpd_vars), 1, function(x) sd(x^2)),
          skew = apply(select(., smpd_vars), 1, function(x) skewness(x)),
          skew_sq = apply(select(., smpd_vars), 1, function(x) skewness(x^2)),
          kurt = apply(select(., smpd_vars), 1, function(x) kurtosis(x)),
          kurt_sq = apply(select(., smpd_vars), 1, function(x) kurtosis(x^2)),
          q1_exp = apply(select(., smpd_vars), 1, function(x) quantile(exp(x), probs = 0.25)),
          q3_exp = apply(select(., smpd_vars), 1, function(x) quantile(exp(x), probs = 0.75)),
          CV_c = apply(select(., smpd_vars), 1, function(x) sd(x)/mean(x))
        )
      
      IV_table_t <- create_infotables(data = Add_variables, y = target)
      IV_sum <- IV_table_t$Summary
    }
    
    addss <- IV_sum %>% filter(IV >= 0.01)
    
    if(nrow(addss) >= 1){
      cat("Make variables that IV is greater than crv", crv, "----------\n")
      tmp <- map(addss$Variable, function(x) x) %>% unlist()
      names(tmp) <- paste0("newvar", i, "_", addss$Variable)
      
      
      train_df <- 
        train_df %>% 
        cbind(., Add_variables %>% select(!!tmp))
      
      test_df <- 
        train_df %>% 
        cbind(., Add_variables_test %>% select(!!tmp))
    } else {
      cat("Hmm.. we can't make variable...\n")
    }
    
    history_v <- c(paste(smpd_vars, collapse = "."), history_v)
  }
}



