################# citation ###########################################
version # R version 4.5.1 (2025-06-13)
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




##########################  1. prepare data  ##########################
# set working directory
setwd("~/Desktop/Japanese surprisal project")

# read data and select everything except columns 1–3
df <- read.csv("essay_data_with_stats_surprisal_final.csv") %>% select(-(1:3))

# convert proficiency levels to factors
table(df$level)
df$level <- factor(df$level, levels = c("beginner", "intermediate", "advanced", "native"))
str(df$level)

# random sampling: alread did by Akari in Python
# # set a seed for reproducibility
# set.seed(123)  
# # check the number of essays in each level (before sampling)
# table(df$level)
# # randomly downsample the intermediate group to 100 samples while keeping all other levels unchanged
# data <- bind_rows(
#   df %>% filter(level != "intermediate"),
#   df %>% filter(level == "intermediate") %>% slice_sample(n = 100))
# # check the number of essays in each level (before sampling)
# table(data$level)
# advanced     beginner intermediate       native 
# 92           96          100           48 

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

# for classic indices (optional, only if we also want to show if classic indices can also distinguish different proficiency levels): 
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
cordata <- d %>% select(-c(ID, L1, L1_2, text, shortened_text, level,group,num_sentences,num_tokens))
str(cordata) # all variales should be numeircal variables

# get correlation coefficient matrix
corr_matrix <- cor(cordata, method = "spearman", use = "pairwise.complete.obs") # use spearman correlation for non-parametric data, and use pairwise complete observations to handle missing data

# get p-value matrix
testRes = cor.mtest(cordata, conf.level = 0.95)


# draw the correlation plot (present this figure in the paper)
corrplot(corr_matrix, 
         p.mat = testRes$p,  
         tl.pos = 'td',    
         order = 'original', 
         addrect = 2,
         type= "upper",
         insig = "blank",  # 不显著的 cell 直接空白。
         sig.level = c(0.001, 0.01, 0.05),  
         pch.cex = 1.2, 
         pch.col = 'grey20',
         tl.cex = 1.2,  
         addCoef.col = "black",   
         tl.col = "black")




##########################  RQ4. multinomial logistic regression ##########################
# Multinomial logistic regression (MLR) is a classification method used to predict 
# a categorical dependent variable with more than two nominal (unordered) categories, 
# such as predicting color preferences (red, blue, green)

# filter the data; just used the L2 data
m_df <- data %>% filter(group == "L2")  %>% select(-c(ID, L1, L1_2, text, shortened_text, group)) %>% droplevels()
m_df$level <- droplevels(m_df$level)
table(m_df$level)
dim(m_df)
summary(m_df)
colnames(m_df)

# convert level to ordered factor
m_df$level <- factor(m_df$level, levels = c("beginner", "intermediate", "advanced"))


# standardize the variables (why? because the variables are on different scales, and standardizing them can help improve the convergence of the model and make the coefficients more interpretable)
# select the variables to be standardized (all except the dependent variable "level")
vars <- c( "mean_tokens_per_sentence", "type_token_ratio", "num_verbs", "total_char_count",
           "LIM_count", "HIM_count", "wago_count", "kango_count",
           "tokyollm_8B_Instruct_surp", "jpllm_3.7b_Instruct_surp",
           "jpllm_3.7b_surp", "tokyollm_8B_surp",
           "llama_8B_surp", "llama_8B_Instruct_surp")
# scale() function will center the data to have a mean of 0 and scale it to have a standard deviation of 1
m_df[vars] <- scale(m_df[vars])
# sanity check: after standardization, the mean should be close to 0 and the sd should be close to 1
summary(m_df)


# model including only surprisal features (without classic features)
# the dependent variable is "level", and the independent variables are the surprisal features from different LLMs
m_surprisal <- multinom(level ~ tokyollm_8B_Instruct_surp + jpllm_3.7b_Instruct_surp + jpllm_3.7b_surp + 
                          tokyollm_8B_surp + llama_8B_surp + llama_8B_Instruct_surp, 
                        m_df)
# summary of the model
summary(m_surprisal)
# show p values for each predictor (report log-odds)
broom::tidy(m_surprisal, exponentiate = FALSE) 
# predict the proficiency level based on the surprisal features
pred_surprisal  <- predict(m_surprisal, m_df) 
# evaluate the model performance using confusion matrix and pseudo R2
confusionMatrix(data = pred_surprisal,reference = m_df$level)
pR2(m_surprisal)


# model including only classic features (without surprisal features)
m_classic <- multinom(level ~ mean_tokens_per_sentence + type_token_ratio + num_verbs + total_char_count + 
                        LIM_count + HIM_count + wago_count + kango_count, 
                      m_df)
summary(m_classic)
broom::tidy(m_classic, exponentiate = FALSE) 
pred_classic <- predict(m_classic , m_df) 
confusionMatrix(data = pred_classic,reference = m_df$level)
pR2(m_classic)


# model including all variables
m_all <- multinom(level ~ mean_tokens_per_sentence + type_token_ratio + num_verbs + total_char_count + 
                    LIM_count + HIM_count + wago_count + kango_count + 
                    tokyollm_8B_Instruct_surp + jpllm_3.7b_Instruct_surp + jpllm_3.7b_surp + 
                    tokyollm_8B_surp + llama_8B_surp + llama_8B_Instruct_surp, 
                  data = m_df)
summary(m_all)
broom::tidy(m_all, exponentiate = FALSE) 
pred <- predict(m_all, m_df)
confusionMatrix(data = pred,reference = m_df$level) 
pR2(m_all) # show r square



##########################  Optional: draw the figure for classic indices ##########################

# for classic indices  
# we also want to show if classic indices can also distinguish different proficiency levels
classic_vars <- c("num_sentences" , "num_tokens" , "mean_tokens_per_sentence",
              "type_token_ratio", "num_verbs" ,"total_char_count", "LIM_count",
              "HIM_count" , "wago_count" , "kango_count")

# reshape the data
df_classic <- data %>% pivot_longer(cols = all_of(classic_vars), names_to = "measure", values_to = "value")

df_classic$level <- factor(df_classic$level, levels = c("beginner", "intermediate", "advanced", "native"))

df_classic$measure <- factor(df_classic$measure, levels = classic_vars,
                          labels = c("num_sentences" , "num_tokens" , "mean_tokens_per_sentence",
                                     "type_token_ratio", "num_verbs" ,"total_char_count", "LIM_count",
                                     "HIM_count" , "wago_count" , "kango_count"))


# visualization & post-hoc test: figure for different proficiency levels within L2 group
ggplot(df_classic, aes(x = level, y = value, fill = level)) +
  geom_boxplot(alpha = 0.7) + 
  labs(fill = "level")+ 
  facet_wrap(~measure, scales = "free_y",ncol = 5) +  
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


