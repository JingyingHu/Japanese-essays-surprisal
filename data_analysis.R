################# citation ###########################################
version # R version 4.5.1 
citation() # show all package citation

# save the citation information of R to a text file
sink("package_citations.txt")
citation()
sink()

# save the citation information of all packages to a text file 
pkgs <- c("readr","tidyr","readxl","dplyr","psych","stringr","car",
          "purrr","rstatix","ggplot2","ggpubr","gridExtra",
          "corrplot","coin","caret","nnet","pscl")

lapply(pkgs, citation)

sink("citations.txt")
lapply(pkgs, citation)
sink()


############### load the packages ###############
library(readr) 
library(tidyr) 
library(readxl) 
library(dplyr) 
library(psych) 
library(stringr) 
library(car) 
library(purrr) 
library(rstatix)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(corrplot)
library(coin)
library(caret) # for confusionMatrix
library(nnet) # for multinomial logistics regression
library(pscl) # for pseudo R2
library(caret)
library(yardstick)






##########################  1. prepare data  ##########################
# set working directory
setwd("~/Desktop/Japanese surprisal project")

# read data and select everything except columns 1–3
df <- read.csv("essay_data_with_stats_surprisal_final.csv") %>% select(-(1:3))

# convert proficiency levels to factors
table(df$level)
df$level <- factor(df$level, levels = c("beginner", "intermediate", "advanced", "native"))
str(df$level)

# add a column to indicate L1 vs L2
data <- df  %>% mutate(group = ifelse(level == "native", "L1", "L2"))
# make this as factor
data$group <- factor(data$group, levels = c("L1", "L2"))
table(data$group)




##########################  check the distribution (model assumption)  ##########################
vars <- c("llama_8B_surp", "tokyollm_8B_surp", "jpllm_3.7b_surp",  
          "llama_8B_Instruct_surp", "tokyollm_8B_Instruct_surp","jpllm_3.7b_Instruct_surp",
          # "num_sentences" , "num_tokens" ,
          "mean_tokens_per_sentence",  
          "type_token_ratio" , "num_verbs" ,"total_char_count"  , 
          "LIM_count" , "HIM_count" , "wago_count" , "kango_count")

# Shapiro test: check the normality of the distribution for each variable
normality <- lapply(vars, function(x) {
  shapiro.test(df[[x]])
})

names(normality) <- vars
normality

normality_tbl <- purrr::map_df(vars, function(x){
  test <- shapiro.test(df[[x]])
  tibble(
    variable = x,
    W = test$statistic,
    p = test$p.value
  )
})

normality_tbl 
# p < .05 → statistically detectable deviation from normality
# all variables violate the normality assumption, so we will use non-parametric tests for subsequent analyses.



# Levenen's test: check the homogeneity of variance for each variable across different proficiency levels
levene_results <- lapply(vars, function(x) {
  car::leveneTest(as.formula(paste(x, "~ level")), data = df)
})


levene_tbl <- purrr::map_df(vars, function(x) {
  res <- car::leveneTest(as.formula(paste(x, "~ level")), data = df)
  
  tibble(
    variable = x,
    F = res[1, "F value"],
    p = res[1, "Pr(>F)"]
  )
})

levene_tbl
# p < .05 → statistically detectable deviation from homogeneity of variance

# Conclusion: we should use non-parametric tests for subsequent analyses, 
# such as the Wilcoxon rank-sum test for comparing L1 vs L2 groups 
# and the Kruskal-Wallis test for comparing different proficiency levels within the L2 group.





##########################  surprisal profiles among different group  ##########################

# subset1: for visualization
# we just focus on the surprisal: df_long for visualization (a figure for many subplots corresponding for different LLMs)
# select relevant variables: surprisal 
plot_vars <- c("llama_8B_surp", "tokyollm_8B_surp", "jpllm_3.7b_surp",  
               "llama_8B_Instruct_surp", "tokyollm_8B_Instruct_surp","jpllm_3.7b_Instruct_surp")

# for classic indices (optional): 
# plot_vars <- c("num_sentences" , "num_tokens" , "mean_tokens_per_sentence",  
#               "type_token_ratio", "num_verbs" ,"total_char_count", "LIM_count", 
#               "HIM_count" , "wago_count" , "kango_count")

# reshape the data
df_long <- data %>% pivot_longer(cols = all_of(plot_vars), names_to = "measure", values_to = "value")

df_long$level <- factor(df_long$level, levels = c("beginner", "intermediate", "advanced", "native"))

df_long$measure <- factor(df_long$measure, levels = plot_vars,
                          labels = c("LLaMA 8B","TokyoLM 8B", "JP-LLM 3.7B", 
                                     "LLaMA 8B Instruct", "TokyoLM 8B Instruct", "JP-LLM 3.7B Instruct"))

# summary statistics: mean and sd for each proficiency level and each measure
summary_long  <- df_long %>% 
  group_by(level, measure) %>%
  summarise( n = sum(!is.na(value)),
             mean = mean(value, na.rm = TRUE),
             sd = sd(value, na.rm = TRUE),
             .groups = "drop") %>%
  arrange(measure, level)

summary_long 

# visualization
ggplot(df_long, aes(x = level, y = value, fill = level)) +
  geom_boxplot(alpha = 0.7) + 
  labs(fill = "Proficiency levels")+ 
  facet_wrap(~measure, scales = "free_y",ncol = 3) +  
  labs(x = NULL, # "CEFR Level", 
       y = "mean surprisal") +
  theme_bw(base_size = 12)+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "bottom",
        strip.text = element_text(size = 8,face = "bold"))




##########################  RQ1. surprisal: compare l1 vs l2  ##########################
# calculate the mean and sd for L1 vs L2
# df_long %>%
#   group_by(group, measure) %>%
#   summarise( n = sum(!is.na(value)),
#              mean = mean(value, na.rm = TRUE),
#              sd = sd(value, na.rm = TRUE),
#              .groups = "drop") %>%
#   arrange(measure, group)

# descriptive statistics
desc <- df_long %>%
  group_by(measure, group) %>% # group: l1 L2     
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = group,
    values_from = c(n, mean, sd),
    names_sep   = "_")

desc 

# statistical analysis: Wilcoxon rank-sum test (Mann-Whitney U test) for comparing L1 vs L2 groups for each measure
test <- df_long %>%
  group_by(measure) %>%
  rstatix::wilcox_test(value ~ group) %>%     
  add_significance("p") %>%          # add 给 p.signif（*, **, ***）
  adjust_pvalue(method = "bonferroni")  %>%
  add_significance("p.adj") %>%
  ungroup()


# Effect size for each measure
eff <- df_long %>%
  group_by(measure) %>%
  wilcox_effsize(value ~ group) %>%   #
  ungroup()


# final for each measure, the descriptive statistics for L1 vs L2, the test statistic, p-value, effect size and its magnitude
# present this in the paper
res2 <- test %>%
  left_join(desc, by = "measure") %>%
  left_join(eff %>% select(measure, effsize, magnitude), by = "measure") %>%
  select(
    measure,
    starts_with("n_"), starts_with("mean_"), starts_with("sd_"),
    statistic, 
    p, p.signif, 
    # p.adj,p.adj.signif,
    effsize, magnitude)

res2


# visualization: figure for L1 vs L2 surprisal
# present this in the paper
df_long$group <- factor(df_long$group, levels = c("L1", "L2"))

ggplot(df_long, aes(x = group, y = value, fill = group)) +
  geom_boxplot(alpha = 0.7) + 
  labs(fill = "group")+ 
  facet_wrap(~measure, scales = "free_y",ncol = 3) +  
  labs(x = NULL, # "CEFR Level", 
       y = "mean surprisal") +
  theme_bw(base_size = 18)+
  stat_compare_means(
    comparisons = list(c("L1", "L2")),
    method = "wilcox.test", 
    p.adjust.method = "bonferroni",
    label = "p.signif", 
    size = 8, 
    vjust = 0.7)+  
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "bottom",
        strip.text = element_text(size = 10,face = "bold"))




##########################  RQ2. compare different proficiency levels within L2 group   ##########################

# filter out rows of L2 group: 
df_long_l2 <- df_long %>% filter(group == "L2") %>% droplevels()
table(df_long_l2$level)

# statistical analysis: 
# Kruskal-Wallis test for comparing different proficiency levels within the L2 group for each measure, 
# and calculate the effect size for each measure
results <- map(plot_vars, ~ {                                   # plot_vars means surprisal columns
  var <- .
  kruskal_res <- rstatix::kruskal_test(as.formula(paste(var, "~ level")), data =  data)
  effsize_res <- rstatix::kruskal_effsize(as.formula(paste(var, "~ level")), data =  data)
  list(variable = var, kruskal = kruskal_res, effect_size = effsize_res)})

kruskal_results_df <- map_dfr(results, function(res) {
  tibble( variable = res$variable,                              # Variable name
          statistic = round(res$kruskal$statistic,3),           # Kruskal-Wallis statistic
          p_value = round(res$kruskal$p,4),                       # Rounded p-value
          effect_size = round(res$effect_size$effsize,3),       # Effect size
          magnitude = res$effect_size$magnitude)})                # Effect size magnitude

kruskal_results_df

# visualization & post-hoc test: figure for different proficiency levels within L2 group
ggplot(df_long_l2, aes(x = level, y = value, fill = level)) +
  geom_boxplot(alpha = 0.7) + 
  labs(fill = "level")+ 
  facet_wrap(~measure, scales = "free_y",ncol = 3) +  
  labs(x = NULL, # "CEFR Level", 
       y = "mean surprisal") +
  theme_bw(base_size = 14)+
  stat_compare_means(
    comparisons = list(c("beginner", "intermediate"),
                       c("intermediate", "advanced"),
                       c("beginner", "advanced")),
    method = "wilcox.test", 
    p.adjust.method = "bonferroni",
    label = "p.signif",
    size = 5, 
    vjust = 0.7)+  
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.position = "bottom",
        strip.text = element_text(size = 10,face = "bold"))





##########################   RQ3. correlation analysis ##########################
# here we just use L2 data
d <- data %>% filter(group == "L2") %>%  droplevels()
table(d$level)
table(d$group)

# select relavant variables for correlation analysis
colnames(d)
cordata <- d %>% select(-c(ID, L1, L1_2, text, shortened_text, level,group,num_sentences,num_tokens,total_char_count))
str(cordata) # all variales should be numeircal variables

# get correlation coefficient matrix
corr_matrix <- cor(cordata, method = "spearman", use = "pairwise.complete.obs")

# get p-value matrix
testRes = cor.mtest(cordata, method = "spearman",conf.level = 0.95)


# draw the correlation plot (present this figure in the paper)
corrplot(corr_matrix, 
         p.mat = testRes$p,  
         tl.pos = 'td',    
         order = 'original', 
         addrect = 2,
         type= "upper",
         insig = "blank",  # 不显著的 cell 直接空白。
         sig.level = c(0.001, 0.01, 0.05),  
         pch.cex = 1, 
         pch.col = 'grey20',
         tl.cex = 0.9,  
         addCoef.col = "black",   
         tl.col = "black")




# ##########################  RQ4. Multinomial Logistic Regression for L2 Proficiency Prediction##########################
# ============================================================
# Predictive evaluation: Multinomial Logistic Regression for L2 Proficiency Prediction
# ============================================================

# This script compares three feature configurations:
# 1. Surprisal features only
# 2. Classic linguistic features only
# 3. Combined features
# The outcome variable is L2 proficiency level: beginner, intermediate, advanced
#
# The resuly reports:
# - In-sample classification performance
# - McFadden pseudo R2
# - Likelihood ratio test for incremental contribution of surprisal
# - Repeated stratified 5-fold cross-validation results

# -----------------------------
# 1. Define feature sets
# -----------------------------

classic_vars <- c(
  "mean_tokens_per_sentence",
  "type_token_ratio",
  "num_verbs",
  "total_char_count",
  "LIM_count",
  "HIM_count",
  "wago_count",
  "kango_count")

surprisal_vars <- c(
  "tokyollm_8B_Instruct_surp",
  "jpllm_3.7b_Instruct_surp",
  "jpllm_3.7b_surp",
  "tokyollm_8B_surp",
  "llama_8B_surp",
  "llama_8B_Instruct_surp")

all_vars <- c(classic_vars, surprisal_vars)

# -----------------------------
# 2. Prepare data
# -----------------------------
# Keep only the outcome variable and predictors used in the models.
# Rows with missing values are removed.
model_df <- m_df %>%
  select(level, all_of(all_vars)) %>%
  na.omit()

# Make sure the outcome variable is treated as a factor.
model_df$level <- factor(model_df$level)

# Standardize all predictors
model_df[all_vars] <- scale(model_df[all_vars])

# -----------------------------
# 3. Cross-validation function
# -----------------------------
# Model performance was evaluated using repeated stratified 5-fold cross-validation, repeated 10 times. 
# For each fold, models were trained on the training partitions and evaluated on held-out testing partitions 
# to reduce overfitting and assess robustness. 
run_cv_multinom <- function(data, predictors, outcome = "level",
                            model_name = "model",
                            k = 5, repeats = 10, seed = 123) {
  
  set.seed(seed)
  # training = 4/5 = 80%; testing = 1/5 = 20%
  
  # Repeated stratified folds
  folds <- createMultiFolds(
    y = data[[outcome]],
    k = k,
    times = repeats)
  
  results <- data.frame()
  all_predictions <- data.frame()
  
  for (i in seq_along(folds)) {
    
    train_index <- folds[[i]]
    
    train_data <- data[train_index, ]
    test_data  <- data[-train_index, ]
    
    formula <- as.formula(
      paste(outcome, "~", paste(predictors, collapse = " + ")))
    
    # Fit model on training data only
    model <- multinom(
      formula,
      data = train_data,
      trace = FALSE)
    
    # Predict held-out testing data
    pred <- predict(model, newdata = test_data)
    
    fold_pred <- data.frame(
      truth = test_data[[outcome]],
      estimate = pred)
    
    # Compute evaluation metrics
    acc <- accuracy(
      fold_pred,
      truth = truth,
      estimate = estimate
    )$.estimate
    
    precision_macro <- precision(
      fold_pred,
      truth = truth,
      estimate = estimate,
      estimator = "macro"
    )$.estimate
    
    recall_macro <- recall(
      fold_pred,
      truth = truth,
      estimate = estimate,
      estimator = "macro"
    )$.estimate
    
    f1_macro <- f_meas(
      fold_pred,
      truth = truth,
      estimate = estimate,
      estimator = "macro"
    )$.estimate
    
    results <- bind_rows(
      results,
      data.frame(
        Model = model_name,
        Fold = names(folds)[i],
        Accuracy = acc,
        Precision = precision_macro,
        Recall = recall_macro,
        Macro_F1 = f1_macro
      )
    )
    
    all_predictions <- bind_rows(
      all_predictions,
      data.frame(
        Model = model_name,
        Fold = names(folds)[i],
        truth = fold_pred$truth,
        estimate = fold_pred$estimate
      )
    )
  }
  
  return(list(
    fold_results = results,
    predictions = all_predictions
  ))
}


# -----------------------------
# 4. Run repeated stratified 5-fold CV
# -----------------------------

cv_surprisal <- run_cv_multinom(
  data = model_df,
  predictors = surprisal_vars,
  model_name = "Surprisal only")

cv_classic <- run_cv_multinom(
  data = model_df,
  predictors = classic_vars,
  model_name = "Classic only")

cv_combined <- run_cv_multinom(
  data = model_df,
  predictors = all_vars,
  model_name = "Combined")

# -----------------------------
# 5. Combine fold-level results
# -----------------------------

cv_results <- bind_rows(
  cv_surprisal$fold_results,
  cv_classic$fold_results,
  cv_combined$fold_results
)

cv_summary <- cv_results %>%
  group_by(Model) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy),
    SD_Accuracy = sd(Accuracy),
    Mean_Precision = mean(Precision),
    SD_Precision = sd(Precision),
    Mean_Recall = mean(Recall),
    SD_Recall = sd(Recall),
    Mean_Macro_F1 = mean(Macro_F1),
    SD_Macro_F1 = sd(Macro_F1),
    .groups = "drop"
  )

cv_summary

# -----------------------------
# 6. Confusion matrices
# -----------------------------

cm_classic <- conf_mat(
  filter(all_predictions, Model == "Classic only"),
  truth = truth,
  estimate = estimate
)

cm_surprisal <- conf_mat(
  filter(all_predictions, Model == "Surprisal only"),
  truth = truth,
  estimate = estimate
)

cm_combined <- conf_mat(
  filter(all_predictions, Model == "Combined"),
  truth = truth,
  estimate = estimate
)

cm_classic
cm_surprisal
cm_combined




