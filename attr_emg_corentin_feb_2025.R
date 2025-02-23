library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)
options(scipen = 999)

# Summary statistics for target variables: raw and imputed --------------------------------------------

attr_emg_input <- fread("../data/attr_emg_input.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

df_target_vars <- attr_emg_input %>% select(NISLL, FAP, MedianMotorRight_Wrist_ThumbAbduction:Score_UlnSPI)

# Create an empty data frame to store results
summary_table <- data.frame(
  Variable = character(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column in the data frame
for (col_name in names(df_target_vars)) {
  
  # Extract the column
  col_data <- df_target_vars[[col_name]]
  
  # Calculate statistics while ignoring NAs
  col_stats <- data.frame(
    Variable = col_name,
    Mean = round(mean(col_data, na.rm = TRUE),2),
    SD = round(sd(col_data, na.rm = TRUE),2),
    Median = round(median(col_data, na.rm = TRUE),2),
    Q1 = round(quantile(col_data, probs = 0.25, na.rm = TRUE),2),
    Q3 = round(quantile(col_data, probs = 0.75, na.rm = TRUE),2)
  )
  
  # Append the statistics to the summary table
  summary_table <- rbind(summary_table, col_stats)
}

# Print the summary table
print(summary_table)


summary_table %>% 
  mutate(mean=paste0(Mean, " ± ", SD), 
         median=paste0(Median, " [", Q1, "-", Q3, "]")) %>% 
  select(Variable, mean, median)




df_target_vars %>% drop_na() # 63 left 63/453 14% only, not a chance

# paste0(names(df_target_vars), collapse = '","')
# 
# 
# target_column <- "NISLL"
# 
# columns_to_compare <- c("NISLL","FAP","MedianMotorRight_Wrist_ThumbAbduction",
#                         "MedianMotorLeft_Wrist_ThumbAbduction","UlnarMotorRight_Wrist_FingerAdduction",
#                         "UlnarMotorLeft_Wrist_FingerAdduction","SciatiquePopliteExterneMotorRight_Foot_DorsalisPedis",
#                         "SciatiquePopliteExterneMotorLeft_Foot_DorsalisPedis","SciatiquePopliteInterneMotorRight_Ankle_CFPI",
#                         "SciatiquePopliteInterneMotorLeft_Ankle_CFPI","RadialSensoryRight","RadialSensoryLeft",
#                         "MedianSensoryRight","MedianSensoryLeft","UlnarSensoryRight","UlnarSensoryLeft",
#                         "MusculocutaneousSensoryRight","MusculocutaneousSensoryLeft","SuralSensitifD","SuralSensoryLeft",
#                         "MedianVelocityRight","MedianVelocityLeft","MedianDistalLatencyRight","MedianDistalLatencyLeft",
#                         "Score_Total","Score_Hemi_Right","Score_UlnSPI")
# 
# correlation_results <- list()
# 
# for (col in columns_to_compare) {
#   
#   # Calculate Spearman correlation while dropping NAs
#   correlation <- cor(df_target_vars[[target_column]], df_target_vars[[col]], method = "spearman", use = "complete.obs")
#   
#   # Store the result
#   correlation_results[[col]] <- correlation
# }
# 
# # Print the correlation results
# print(correlation_results)


cor_matrix <- cor(df_target_vars %>% select(where(is.numeric)), method = "spearman", use = "complete.obs")
print(cor_matrix)

cor_matrix <- data.frame(cor_matrix)

fwrite(cor_matrix, "cor_matrix_raw.csv")


# Impute missing values using PCA

sum(is.na(df_target_vars)) / (dim(df_target_vars)[1] * dim(df_target_vars)[2])

df_target_vars_imputed <- imputePCA(df_target_vars,ncp=20, scale = T)

df_target_vars_imputed <- data.frame(df_target_vars_imputed$completeObs)

# Create an empty data frame to store results
summary_table <- data.frame(
  Variable = character(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column in the data frame
for (col_name in names(df_target_vars_imputed)) {
  
  # Extract the column
  col_data <- df_target_vars_imputed[[col_name]]
  
  # Calculate statistics while ignoring NAs
  col_stats <- data.frame(
    Variable = col_name,
    Mean = round(mean(col_data, na.rm = TRUE),2),
    SD = round(sd(col_data, na.rm = TRUE),2),
    Median = round(median(col_data, na.rm = TRUE),2),
    Q1 = round(quantile(col_data, probs = 0.25, na.rm = TRUE),2),
    Q3 = round(quantile(col_data, probs = 0.75, na.rm = TRUE),2)
  )
  
  # Append the statistics to the summary table
  summary_table <- rbind(summary_table, col_stats)
}

# Print the summary table
print(summary_table)



sum(df_target_vars_imputed<0) / (dim(df_target_vars_imputed)[1] * dim(df_target_vars_imputed)[2])

df_target_vars_imputed[df_target_vars_imputed<0] <- 0

summary_table %>% 
  mutate(mean=paste0(Mean, " ± ", SD), 
         median=paste0(Median, " [", Q1, "-", Q3, "]")) %>% 
  select(Variable, mean, median)





df_target_vars
df_target_vars_imputed




cor_matrix <- cor(df_target_vars_imputed %>% select(where(is.numeric)), method = "spearman", use = "complete.obs")
print(cor_matrix)

cor_matrix <- data.frame(cor_matrix)

fwrite(cor_matrix, "cor_matrix_raw_imp.csv")


df_plot <- df_target_vars %>% mutate(Set="Raw") %>%
  bind_rows(df_target_vars_imputed %>% mutate(Set="Imputed")) 




# Function to create density plots for each feature

create_density_plot <- function(feature_name) {
  ggplot(df_plot, aes(x = !!sym(feature_name), colour=Set, fill=Set )) +
    geom_density(alpha = 0.5) +
    ggpubr::theme_pubclean() +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    labs(y = "Patient density", title = feature_name)
}


names(df_plot)

# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set

plots <- map(feature_names, create_density_plot)








fwrite(df_target_vars, "../data/df_target_vars.txt")
fwrite(df_target_vars_imputed, "../data/df_target_vars_imputed.txt")



# First visit only

df_target_vars <- attr_emg_input %>% select(Patient, Visite) %>%
  bind_cols(df_target_vars)

df_target_vars <- df_target_vars %>% group_by(Patient) %>%
  filter(Visite==min(Visite))

df_target_vars <- df_target_vars %>% ungroup() %>% select(-Patient, -Visite)

# Create an empty data frame to store results
summary_table <- data.frame(
  Variable = character(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column in the data frame
for (col_name in names(df_target_vars)) {

  # Extract the column
  col_data <- df_target_vars[[col_name]]

  # Calculate statistics while ignoring NAs
  col_stats <- data.frame(
    Variable = col_name,
    Mean = round(mean(col_data, na.rm = TRUE),2),
    SD = round(sd(col_data, na.rm = TRUE),2),
    Median = round(median(col_data, na.rm = TRUE),2),
    Q1 = round(quantile(col_data, probs = 0.25, na.rm = TRUE),2),
    Q3 = round(quantile(col_data, probs = 0.75, na.rm = TRUE),2)
  )

  # Append the statistics to the summary table
  summary_table <- rbind(summary_table, col_stats)
}

# Print the summary table
print(summary_table)


summary_table %>%
  mutate(mean=paste0(Mean, " ± ", SD),
         median=paste0(Median, " [", Q1, "-", Q3, "]")) %>%
  select(Variable, mean, median)






cor_matrix <- cor(df_target_vars %>% select(where(is.numeric)), method = "spearman", use = "complete.obs")
print(cor_matrix)

cor_matrix <- data.frame(cor_matrix)

fwrite(cor_matrix, "cor_matrix_raw.csv")






df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite) %>%
  bind_cols(df_target_vars_imputed)

df_target_vars_imputed <- df_target_vars_imputed %>% group_by(Patient) %>%
  filter(Visite==min(Visite))

df_target_vars_imputed <- df_target_vars_imputed %>% ungroup() %>% select(-Patient, -Visite)

# Create an empty data frame to store results
summary_table <- data.frame(
  Variable = character(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column in the data frame
for (col_name in names(df_target_vars_imputed)) {

  # Extract the column
  col_data <- df_target_vars_imputed[[col_name]]

  # Calculate statistics while ignoring NAs
  col_stats <- data.frame(
    Variable = col_name,
    Mean = round(mean(col_data, na.rm = TRUE),2),
    SD = round(sd(col_data, na.rm = TRUE),2),
    Median = round(median(col_data, na.rm = TRUE),2),
    Q1 = round(quantile(col_data, probs = 0.25, na.rm = TRUE),2),
    Q3 = round(quantile(col_data, probs = 0.75, na.rm = TRUE),2)
  )

  # Append the statistics to the summary table
  summary_table <- rbind(summary_table, col_stats)
}

# Print the summary table
print(summary_table)


summary_table %>%
  mutate(mean=paste0(Mean, " ± ", SD),
         median=paste0(Median, " [", Q1, "-", Q3, "]")) %>%
  select(Variable, mean, median)



cor_matrix <- cor(df_target_vars_imputed %>% select(where(is.numeric)), method = "spearman", use = "complete.obs")
print(cor_matrix)

cor_matrix <- data.frame(cor_matrix)

fwrite(cor_matrix, "cor_matrix_raw_imp.csv")




df_plot <- df_target_vars %>% mutate(Set="Raw") %>%
  bind_rows(df_target_vars_imputed %>% mutate(Set="Imputed")) 




# Function to create density plots for each feature

create_density_plot <- function(feature_name) {
  ggplot(df_plot, aes(x = !!sym(feature_name), colour=Set, fill=Set )) +
    geom_density(alpha = 0.5) +
    ggpubr::theme_pubclean() +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    labs(y = "Patient density", title = feature_name)
}


names(df_plot)

# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set

plots <- map(feature_names, create_density_plot)





fwrite(df_target_vars, "../data/df_target_vars_first.txt")
fwrite(df_target_vars_imputed, "../data/df_target_vars_imputed_first.txt")









# Function to create correlation plots for each feature all visits NISLL

df_target_vars <- fread("../data/df_target_vars.txt")
df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")


df_plot <- df_target_vars %>% mutate(Set="Raw") %>%
  bind_rows(df_target_vars_imputed %>% mutate(Set="Imputed")) 


create_density_plot <- function(feature_name) {
  ggplot(df_plot, aes(x = !!sym(feature_name), y=NISLL, colour=Set, fill=Set )) +
    geom_jitter(alpha=0.8, shape=1, stroke=2, width = 0.2, height = 0.2, size=2) +
    geom_smooth( alpha=0.3) +
    ggpubr::theme_pubclean() +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    labs(y = "NISLL", x=feature_name , title = feature_name)
}


names(df_plot)

# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set

plots <- map(feature_names, create_density_plot)








# Function to create correlation plots for each feature first visit NISLL

df_target_vars_first <- fread( "../data/df_target_vars_first.txt")
df_target_vars_imputed_first <- fread( "../data/df_target_vars_imputed_first.txt")


df_plot_first <- df_target_vars_first %>% mutate(Set="Raw") %>%
  bind_rows(df_target_vars_imputed_first %>% mutate(Set="Imputed")) 

create_density_plot <- function(feature_name) {
  ggplot(df_plot_first, aes(x = !!sym(feature_name), y=NISLL, colour=Set, fill=Set )) +
    geom_jitter(alpha=0.8, shape=1, stroke=2, width = 0.2, height = 0.2, size=2) +
    geom_smooth( alpha=0.3) +
    ggpubr::theme_pubclean() +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    labs(y = "NISLL", x=feature_name , title = feature_name)
}


names(df_plot)

# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set

plots <- map(feature_names, create_density_plot)



df_target_vars
df_target_vars_first

df_target_vars_imputed
df_target_vars_imputed_first

unique(df_target_vars$FAP)
unique(df_target_vars_first$FAP)
unique(df_target_vars_imputed$FAP)
unique(df_target_vars_imputed_first$FAP)



                     

# Function to create  plots for each feature all visits FAP

df_plot <- df_target_vars %>% mutate(Set="Raw") %>% filter(!is.na(FAP)) %>%
  mutate(FAP=as.factor(FAP)) %>%
  bind_rows(df_target_vars_imputed %>% mutate(Set="Imputed") %>% 
              filter(!is.na(FAP)) %>% mutate(FAP=round(FAP)) %>% mutate(FAP=as.factor(FAP)) 
            
            
create_density_plot <- function(feature_name) {
  ggplot(df_plot, aes(x = FAP, y=!!sym(feature_name), colour=Set, fill=Set )) +
    geom_jitter( alpha=0.7, shape=1, stroke=2, width = 0.3, height = 0.2, size=2) +
    geom_boxplot(alpha=0.7, shape=1, stroke=2, width = 0.5, height = 0.5, size=1, notch = TRUE) +
    ggpubr::theme_pubclean() +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    labs(y = feature_name , x="FAP" , title = feature_name)
}


names(df_plot)

# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set

plots <- map(feature_names, create_density_plot)




# Function to create  plots for each feature first visit FAP

df_plot <- df_target_vars_first %>% mutate(Set="Raw") %>% filter(!is.na(FAP)) %>%
  mutate(FAP=as.factor(FAP)) %>%
  bind_rows(df_target_vars_imputed_first %>% mutate(Set="Imputed") %>% 
              filter(!is.na(FAP)) %>% mutate(FAP=round(FAP)) %>% mutate(FAP=as.factor(FAP))) 
            
            
create_density_plot <- function(feature_name) {
  ggplot(df_plot, aes(x = FAP, y=!!sym(feature_name), colour=Set, fill=Set )) +
  geom_jitter( alpha=0.7, shape=1, stroke=2, width = 0.3, height = 0.2, size=2) +
  geom_boxplot(alpha=0.7, shape=1, stroke=2, width = 0.5, height = 0.5, size=1, notch = TRUE) +
  ggpubr::theme_pubclean() +
  scale_fill_manual(values = c("#183555", "#FAC67A")) +
  scale_colour_manual(values = c("#183555", "#FAC67A")) +
  labs(y = feature_name , x="FAP" , title = feature_name)
}
            
            
names(df_plot)
            
# Generate density plots for all columns except Set
feature_names <- colnames(df_plot)[-28]  # Exclude Set
            
plots <- map(feature_names, create_density_plot)



# ----------

# Ridge, LASSO and Best Subset NISLL ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed <- fread( "../data/df_target_vars_imputed.txt")

df_target_vars_imputed <- df_target_vars_imputed %>% select(-c(FAP,Score_Total ,Score_Hemi_Right , Score_UlnSPI))


# Ensure predictors are scaled
df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(across(where(is.numeric), scale))


#Best Subset Selection
set.seed(1)
regit_full <- regsubsets(NISLL  ~ ., data = df_target_vars_imputed, nvmax = 22, really.big=T)
reg_summary <- summary(regit_full)


ignore <- data.frame(reg_summary$which)

fwrite(ignore, "NISLL_best_subset_all.csv")


NISLL_best_subset_all <- fread("NISLL_best_subset_all.csv")

names(NISLL_best_subset_all)

# "#183555", "#FAC67A"

NISLL_best_subset_all %>% gather(Var, Pres, MedianMotorRight_Wrist_ThumbAbduction:MedianDistalLatencyLeft) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=Vars , y=Var, fill = Predictor_Included)) +
  geom_tile(color = "snow", size = 0.1, show.legend = F) +
  scale_fill_manual( values= c("snow", "#183555") ) +
  #scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")


# Plot RSS, Adjusted R², Cp, and BIC

# Set up the plot layout with 2 rows and 2 columns
par(mfrow = c(2, 2))  # Arrange plots in a grid

# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = "#FAC67A")
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = "#183555")
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = "#FAC67A")
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = "#183555")



# Ensure predictors are scaled
X <- model.matrix(NISLL ~ .  , data = df_target_vars_imputed)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed$NISLL  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.1se")
print(ridge_coeffs)




# Elastic net

elastic_net_cv <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE, maxit = 1e6)

elastic_net_cv$lambda.min
elastic_net_cv$lambda.1se

elastic_net_coeffs <- coef(elastic_net_cv, s = "lambda.1se")
print(elastic_net_coeffs)







# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
elastic_mse <- min(elastic_net_cv$cvm)

print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))
print(paste("Elastic MSE:", elastic_mse))



# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.1se")
ridge_predictions <- predict(ridge_cv, newx = X, s = "lambda.1se")
elastic_predictions <- predict(elastic_net_cv, newx = X, s = "lambda.1se")

cor(lasso_predictions, data.frame(y), method="spearman")
cor(ridge_predictions, data.frame(y), method="spearman")
cor(elastic_predictions, data.frame(y), method="spearman")


# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)

ridge_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(ridge_predictions)
)


elastic_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(elastic_predictions)
)


# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Ridge Plot
ggplot(ridge_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "Ridge: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Elastic Plot
ggplot(elastic_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "ElasticNet: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")


# ----------
# Ridge, LASSO and Best Subset FAP ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed <- fread( "../data/df_target_vars_imputed.txt")

df_target_vars_imputed <- df_target_vars_imputed %>% select(-c(NISLL ,Score_Total ,Score_Hemi_Right , Score_UlnSPI))

unique(df_target_vars_imputed$FAP)

df_target_vars_imputed$FAP <- round(df_target_vars_imputed$FAP)

# Ensure predictors are scaled
df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(across(where(is.numeric), scale))


# Best Subset Selection
set.seed(1)
regit_full <- regsubsets(FAP  ~ ., data = df_target_vars_imputed, nvmax = 22, really.big=T)
reg_summary <- summary(regit_full)


ignore <- data.frame(reg_summary$which)

fwrite(ignore, "FAP_best_subset_all.csv")


FAP_best_subset_all <- fread("FAP_best_subset_all.csv")

names(FAP_best_subset_all)

# "#183555", "#FAC67A"

FAP_best_subset_all %>% gather(Var, Pres, MedianMotorRight_Wrist_ThumbAbduction:MedianDistalLatencyLeft) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=Vars , y=Var, fill = Predictor_Included)) +
  geom_tile(color = "snow", size = 0.1, show.legend = F) +
  scale_fill_manual( values= c("snow", "#183555") ) +
  #scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")


# Plot RSS, Adjusted R², Cp, and BIC

# Set up the plot layout with 2 rows and 2 columns
par(mfrow = c(2, 2))  # Arrange plots in a grid

# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = "#FAC67A")
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = "#183555")
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = "#FAC67A")
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = "#183555")



# Ensure predictors are scaled
X <- model.matrix(FAP ~ .  , data = df_target_vars_imputed)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed$FAP  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.1se")
print(ridge_coeffs)




# Elastic net

elastic_net_cv <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE, maxit = 1e6)

elastic_net_cv$lambda.min
elastic_net_cv$lambda.1se

elastic_net_coeffs <- coef(elastic_net_cv, s = "lambda.1se")
print(elastic_net_coeffs)







# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
elastic_mse <- min(elastic_net_cv$cvm)

print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))
print(paste("Elastic MSE:", elastic_mse))



# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.1se")
ridge_predictions <- predict(ridge_cv, newx = X, s = "lambda.1se")
elastic_predictions <- predict(elastic_net_cv, newx = X, s = "lambda.1se")

cor(lasso_predictions, data.frame(y), method="spearman")
cor(ridge_predictions, data.frame(y), method="spearman")
cor(elastic_predictions, data.frame(y), method="spearman")


# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)

ridge_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(ridge_predictions)
)


elastic_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(elastic_predictions)
)


# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Ridge Plot
ggplot(ridge_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "Ridge: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Elastic Plot
ggplot(elastic_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "ElasticNet: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")


# ----------
# Ridge, LASSO and Best Subset NISLL First Visit ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed_first <- fread( "../data/df_target_vars_imputed_first.txt")

df_target_vars_imputed_first <- df_target_vars_imputed_first %>% select(-c(FAP,Score_Total ,Score_Hemi_Right , Score_UlnSPI))


# Ensure predictors are scaled
df_target_vars_imputed_first <- df_target_vars_imputed_first %>%
  mutate(across(where(is.numeric), scale))


# Best Subset Selection
set.seed(1)
regit_full <- regsubsets(NISLL  ~ ., data = df_target_vars_imputed_first, nvmax = 22, really.big=T)
reg_summary <- summary(regit_full)


ignore <- data.frame(reg_summary$which)

fwrite(ignore, "NISLL_best_subset_all_first.csv")


NISLL_best_subset_all_first <- fread("NISLL_best_subset_all_first.csv")

names(NISLL_best_subset_all_first)

# "#183555", "#FAC67A"

NISLL_best_subset_all_first %>% gather(Var, Pres, MedianMotorRight_Wrist_ThumbAbduction:MedianDistalLatencyLeft) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=Vars , y=Var, fill = Predictor_Included)) +
  geom_tile(color = "snow", size = 0.1, show.legend = F) +
  scale_fill_manual( values= c("snow", "#183555") ) +
  #scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")


# Plot RSS, Adjusted R², Cp, and BIC

# Set up the plot layout with 2 rows and 2 columns


# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = "#FAC67A")
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = "#183555")
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = "#FAC67A")
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = "#183555")



# Ensure predictors are scaled
X <- model.matrix(NISLL ~ .  , data = df_target_vars_imputed_first)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed_first$NISLL  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.1se")
print(ridge_coeffs)




# Elastic net

elastic_net_cv <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE, maxit = 1e6)

elastic_net_cv$lambda.min
elastic_net_cv$lambda.1se

elastic_net_coeffs <- coef(elastic_net_cv, s = "lambda.1se")
print(elastic_net_coeffs)







# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
elastic_mse <- min(elastic_net_cv$cvm)

print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))
print(paste("Elastic MSE:", elastic_mse))



# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.1se")
ridge_predictions <- predict(ridge_cv, newx = X, s = "lambda.1se")
elastic_predictions <- predict(elastic_net_cv, newx = X, s = "lambda.1se")

cor(lasso_predictions, data.frame(y), method="spearman")
cor(ridge_predictions, data.frame(y), method="spearman")
cor(elastic_predictions, data.frame(y), method="spearman")


# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)

ridge_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(ridge_predictions)
)


elastic_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(elastic_predictions)
)


# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Ridge Plot
ggplot(ridge_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "Ridge: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Elastic Plot
ggplot(elastic_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "ElasticNet: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")


# ----------
# Ridge, LASSO and Best Subset FAP First Visit ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed_first <- fread( "../data/df_target_vars_imputed_first.txt")

df_target_vars_imputed_first <- df_target_vars_imputed_first %>% select(-c(NISLL ,Score_Total ,Score_Hemi_Right , Score_UlnSPI))

unique(df_target_vars_imputed_first$FAP)

df_target_vars_imputed_first$FAP <- round(df_target_vars_imputed_first$FAP)

# Ensure predictors are scaled
df_target_vars_imputed_first <- df_target_vars_imputed_first %>%
  mutate(across(where(is.numeric), scale))


# Best Subset Selection
set.seed(1)
regit_full <- regsubsets(FAP  ~ ., data = df_target_vars_imputed_first, nvmax = 22, really.big=T)
reg_summary <- summary(regit_full)


ignore <- data.frame(reg_summary$which)

fwrite(ignore, "FAP_best_subset_all_first.csv")


FAP_best_subset_all_first <- fread("FAP_best_subset_all_first.csv")

names(FAP_best_subset_all_first)

# "#183555", "#FAC67A"

FAP_best_subset_all_first %>% gather(Var, Pres, MedianMotorRight_Wrist_ThumbAbduction:MedianDistalLatencyLeft) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=Vars , y=Var, fill = Predictor_Included)) +
  geom_tile(color = "snow", size = 0.1, show.legend = F) +
  scale_fill_manual( values= c("snow", "#183555") ) +
  #scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  theme_minimal() +
  # scale_x_continuous(breaks = seq(min(Best_Subset_Predictors$vars),max(Best_Subset_Predictors$vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Number of Predictors") +ylab("Predictor Included (yes/no)")


# Plot RSS, Adjusted R², Cp, and BIC

# Set up the plot layout with 2 rows and 2 columns


# Plot RSS with alternating colors and thicker lines
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", lwd = 3, col = "#FAC67A")
# # Plot Adjusted R² with alternating colors and thicker lines
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l", lwd = 3, col = "#183555")
# # Plot Cp with alternating colors and thicker lines
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", lwd = 3, col = "#FAC67A")
# # Plot BIC with alternating colors and thicker lines
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", lwd = 3, col = "#183555")



# Ensure predictors are scaled
X <- model.matrix(FAP ~ .  , data = df_target_vars_imputed_first)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed_first$FAP  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Plot LASSO cross-validation results
plot(lasso_cv)
title("LASSO Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)



# Ridge Regression (alpha = 0)
set.seed(1)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Best lambda
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Plot Ridge cross-validation results
plot(ridge_cv)
title("Ridge Cross-Validation", line = 2.5)

# Extract coefficients for the best lambda
ridge_coeffs <- coef(ridge_cv, s = "lambda.1se")
print(ridge_coeffs)




# Elastic net

elastic_net_cv <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE, maxit = 1e6)

elastic_net_cv$lambda.min
elastic_net_cv$lambda.1se

elastic_net_coeffs <- coef(elastic_net_cv, s = "lambda.1se")
print(elastic_net_coeffs)







# Compare MSE
lasso_mse <- min(lasso_cv$cvm)
ridge_mse <- min(ridge_cv$cvm)
elastic_mse <- min(elastic_net_cv$cvm)

print(paste("LASSO MSE:", lasso_mse))
print(paste("Ridge MSE:", ridge_mse))
print(paste("Elastic MSE:", elastic_mse))



# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.1se")
ridge_predictions <- predict(ridge_cv, newx = X, s = "lambda.1se")
elastic_predictions <- predict(elastic_net_cv, newx = X, s = "lambda.1se")

cor(lasso_predictions, data.frame(y), method="spearman")
cor(ridge_predictions, data.frame(y), method="spearman")
cor(elastic_predictions, data.frame(y), method="spearman")


# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)

ridge_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(ridge_predictions)
)


elastic_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(elastic_predictions)
)


# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Ridge Plot
ggplot(ridge_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "Ridge: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")

# Elastic Plot
ggplot(elastic_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.2, height = 0.2) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "ElasticNet: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")


# ----------
# Plot variables over time; Raw and Imputed ---------------
df_target_vars <- fread( "../data/df_target_vars.txt")

attr_emg_input <- fread("../data/attr_emg_input.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars)

raw <- df_target_vars %>% mutate(Set="Raw")

attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed)

imputed <- df_target_vars_imputed %>% mutate(Set="Imputed")

to_plot <- imputed %>% bind_rows(raw)


create_density_plot <- function(feature_name) {
  ggplot(to_plot, aes(x = Visite_date, y=!!sym(feature_name), colour=Set, fill=Set )) +
    geom_line(aes(group=interaction(Set, Patient)), size=1, alpha=0.2) +
    geom_jitter(  alpha=0.4, shape=1, stroke=2, width = 0.3, height = 0.3, size=1) +
    stat_smooth(method="loess",  alpha=0.4, lwd=1.5, se=TRUE)+
    theme_minimal() +
    labs(y = paste0(feature_name, "\n"), x="\n Elapsed Number of Days Since 1st Eval" , title = feature_name) +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "right") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12, vjust = -0.5),
          axis.title.y = element_text(size = 12, vjust = -0.5),
          plot.margin = margin(5, 5, 5, 5, "pt")) 
}


names(to_plot)[3:29]

# Generate density plots for all columns except Set
feature_names <- colnames(to_plot)[3:29]  # Exclude Set

plots <- map(feature_names, create_density_plot)

plots[[2]]
plots

# -----------
# Plot variable Deltas over time; Raw and Imputed -----------

df_target_vars <- fread( "../data/df_target_vars.txt")

attr_emg_input <- fread("../data/attr_emg_input.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars)


attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed)





df <- df_target_vars_imputed

# First, let's define the function to compute normalized delta for each variable

calculate_deltas <- function(df, emg_vars) {
  df <- df %>%
    group_by(Patient) %>%
    arrange(Patient, Visite_date)  # Ensure data is sorted by patient and visit day
  
  # Calculate deltas for each EMG variable
  for (emg_var in emg_vars) {
    delta_var <- paste0("delta_", emg_var)  # Create new column name for delta
    df <- df %>%
      mutate(!!delta_var := ifelse(
        row_number() > 1,  # Skip first row (no previous data)
        (get(emg_var) - lag(get(emg_var))) ,
        NA  # First visit delta is NA
      ))
  }
  
  
  return(df)
}

# List of EMG variable names (EMG1 to EMG30)
emg_vars <- paste0(names(df)[3:29])

# Apply the function to calculate deltas
df_with_deltas <- calculate_deltas(df, emg_vars)

df_with_deltas_imputed <- df_with_deltas %>% select(Patient, Visite_date, 30:56) %>% ungroup() %>% drop_na()






df <- df_target_vars

# First, let's define the function to compute normalized delta for each variable

calculate_deltas <- function(df, emg_vars) {
  df <- df %>%
    group_by(Patient) %>%
    arrange(Patient, Visite_date)  # Ensure data is sorted by patient and visit day
  
  # Calculate deltas for each EMG variable
  for (emg_var in emg_vars) {
    delta_var <- paste0("delta_", emg_var)  # Create new column name for delta
    df <- df %>%
      mutate(!!delta_var := ifelse(
        row_number() > 1,  # Skip first row (no previous data)
        (get(emg_var) - lag(get(emg_var))) ,
        NA  # First visit delta is NA
      ))
  }
  
  
  return(df)
}

# List of EMG variable names (EMG1 to EMG30)
emg_vars <- paste0(names(df)[3:29])

# Apply the function to calculate deltas
df_with_deltas <- calculate_deltas(df, emg_vars)

df_with_deltas <- df_with_deltas %>% select(Patient, Visite_date, 30:56) %>% ungroup() %>% filter(Visite_date!=0)


df_with_deltas_imputed <- df_with_deltas_imputed %>% mutate(Set="Imputed")
df_with_deltas <- df_with_deltas %>% mutate(Set="Raw")



to_plot <- df_with_deltas_imputed %>% bind_rows(df_with_deltas)



create_density_plot <- function(feature_name) {
  ggplot(to_plot, aes(x = Visite_date, y=!!sym(feature_name), colour=Set, fill=Set )) +
    geom_line(aes(group=interaction(Set, Patient)), size=1, alpha=0.2) +
    geom_jitter(  alpha=0.4, shape=1, stroke=2, width = 0.3, size=1) +
    stat_smooth(method="loess",  alpha=0.4, lwd=1.5, se=TRUE)+
    theme_minimal() +
    labs(y = paste0(feature_name, "\n"), x="\n Elapsed Number of Days Since 1st Eval" , title = feature_name) +
    scale_fill_manual(values = c("#183555", "#FAC67A")) +
    scale_colour_manual(values = c("#183555", "#FAC67A")) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "right") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12, vjust = -0.5),
          axis.title.y = element_text(size = 12, vjust = -0.5),
          plot.margin = margin(5, 5, 5, 5, "pt")) 
}


names(to_plot)[3:29]

# Generate density plots for all columns except Set
feature_names <- colnames(to_plot)[3:29]  # Exclude Set

plots <- map(feature_names, create_density_plot)

plots[[1]]
plots


# ----------

# Linear Mixed-effects Models - Imputed - Change Over Time ---------------


attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)


sum(is.na(df_target_vars_imputed))


df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed)


df_target_vars_imputed <- df_target_vars_imputed %>% ungroup()



df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(across(where(is.numeric), scale))


library(lme4)


df <- df_target_vars_imputed

emg_vars <- paste0(names(df_target_vars_imputed)[3:29])


# Fit models for each variable and store results
results <- list()

for (emg_var in emg_vars) {
  # Fit the mixed-effects model
  model <- lmer(as.formula(paste(emg_var, "~ Visite_date + (Visite_date | Patient )")), data = df)
  
  # Save the model summary
  results[[emg_var]] <- summary(model)
}







emg_vars <- paste0(names(df_target_vars_imputed)[3:29])

results <- list()

for (emg_var in emg_vars) {
  # Fit the model
  model <- lmer(as.formula(paste(emg_var, "~ Visite_date + (Visite_date | Patient)")), data = df)
  
  # Extract fixed effects
  summary_model <- summary(model)
  fixed_effects <- data.frame(
    Variable = emg_var,
    Term = rownames(summary_model$coefficients),
    Estimate = summary_model$coefficients[, "Estimate"],
    StdError = summary_model$coefficients[, "Std. Error"],
    tValue = summary_model$coefficients[, "t value"],
    pValue = 2 * pt(-abs(summary_model$coefficients[, "t value"]), 
                    df = nrow(df) - length(fixef(model)))
  )
  
  # Store results
  results[[emg_var]] <- fixed_effects
}

# Combine all results into a single data frame
all_fixed_effects <- do.call(rbind, results)

print(all_fixed_effects)

# Assume 'p_values' is your vector of p-values
adjusted_pvalues_bonferroni <- p.adjust(all_fixed_effects$pValue, method = "bonferroni")
adjusted_pvalues_holm <- p.adjust(all_fixed_effects$pValue, method = "holm")
adjusted_pvalues_bh <- p.adjust(all_fixed_effects$pValue, method = "BH")  # Benjamini-Hochberg

# Add adjusted p-values to your results data frame
all_fixed_effects$Bonferroni <- adjusted_pvalues_bonferroni
all_fixed_effects$Holm <- adjusted_pvalues_holm
all_fixed_effects$BH <- adjusted_pvalues_bh


all_fixed_effects %>% filter(Term=="Visite_date")

# -----------
# Linear Mixed-effects Models - Imputed - Delta NISLL vs Delta EMG ---------------



attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed)

sum(is.na(df_target_vars_imputed))




df <- df_target_vars_imputed

# First, let's define the function to compute normalized delta for each variable

calculate_deltas <- function(df, emg_vars) {
  df <- df %>%
    group_by(Patient) %>%
    arrange(Patient, Visite_date)  # Ensure data is sorted by patient and visit day
  
  # Calculate deltas for each EMG variable
  for (emg_var in emg_vars) {
    delta_var <- paste0("delta_", emg_var)  # Create new column name for delta
    df <- df %>%
      mutate(!!delta_var := ifelse(
        row_number() > 1,  # Skip first row (no previous data)
        (get(emg_var) - lag(get(emg_var))) ,
        NA  # First visit delta is NA
      ))
  }
  
  
  return(df)
}

# List of EMG variable names (EMG1 to EMG30)
emg_vars <- paste0(names(df)[3:29])

# Apply the function to calculate deltas
df_with_deltas <- calculate_deltas(df, emg_vars)

df_with_deltas_imputed <- df_with_deltas %>% select(Patient, Visite_date, 30:56) %>% ungroup() %>% drop_na()



df_with_deltas_imputed <- df_with_deltas_imputed %>%
  mutate(across(where(is.numeric), scale))


library(lme4)


df <- df_with_deltas_imputed


emg_vars <- paste0(names(df)[5:29])


# Fit models for each variable and store results
results <- list()

for (emg_var in emg_vars) {
  # Fit the mixed-effects model
  model <- lmer(as.formula(paste("delta_NISLL  ~", emg_var, "+ (1 | Patient)")), data = df)
  
  # Save the model summary
  results[[emg_var]] <- summary(model)
}




emg_vars <- paste0(names(df)[5:29])

results <- list()

for (emg_var in emg_vars) {
  # Fit the model
  model <-  lmer(as.formula(paste("delta_NISLL  ~", emg_var, "+ (1 | Patient)")), data = df)
  
  # Extract fixed effects
  summary_model <- summary(model)
  fixed_effects <- data.frame(
    Variable = emg_var,
    Term = rownames(summary_model$coefficients),
    Estimate = summary_model$coefficients[, "Estimate"],
    StdError = summary_model$coefficients[, "Std. Error"],
    tValue = summary_model$coefficients[, "t value"],
    pValue = 2 * pt(-abs(summary_model$coefficients[, "t value"]), 
                    df = nrow(df) - length(fixef(model)))
  )
  
  # Store results
  results[[emg_var]] <- fixed_effects
}

# Combine all results into a single data frame
all_fixed_effects <- do.call(rbind, results)

print(all_fixed_effects)

# Assume 'p_values' is your vector of p-values
adjusted_pvalues_bonferroni <- p.adjust(all_fixed_effects$pValue, method = "bonferroni")
adjusted_pvalues_holm <- p.adjust(all_fixed_effects$pValue, method = "holm")
adjusted_pvalues_bh <- p.adjust(all_fixed_effects$pValue, method = "BH")  # Benjamini-Hochberg

# Add adjusted p-values to your results data frame
all_fixed_effects$Bonferroni <- adjusted_pvalues_bonferroni
all_fixed_effects$Holm <- adjusted_pvalues_holm
all_fixed_effects$BH <- adjusted_pvalues_bh


all_fixed_effects %>% filter(Term!="(Intercept)")

row.names(all_fixed_effects) <- NULL


all_fixed_effects %>% filter(Term!="(Intercept)") %>% select(-Term)

# -----------
# Linear Mixed-effects Models - Imputed - Lead NISLL vs Lag EMG ---------------


attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

sum(is.na(df_target_vars_imputed))

names(attr_emg_input)

unique(df_target_vars_imputed$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)



df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed) %>% ungroup()


df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(across(where(is.numeric), scale))

sum(is.na(df_target_vars_imputed))

library(lme4)



df <- df_target_vars_imputed


# Create lagged dataset
df_lagged <- df %>%
  group_by(Patient) %>%
  arrange(Visite_date) %>%
  mutate(NISLL_future = lead(NISLL, n = 1)) %>%
  filter(!is.na(NISLL_future))

# Fit mixed-effects model
model_lagged <- lmer(NISLL_future ~ Score_Total  + (1 | Patient), data = df_lagged)







emg_vars <- paste0(names(df_lagged)[5:29])

results <- list()


sum(is.na(df_lagged))

for (emg_var in emg_vars) {
  # Fit the model
  model <-  lmer(as.formula(paste("NISLL_future  ~", emg_var, "+ (1 | Patient)")), data = df_lagged)
  
  # Extract fixed effects
  summary_model <- summary(model)
  fixed_effects <- data.frame(
    Variable = emg_var,
    Term = rownames(summary_model$coefficients),
    Estimate = summary_model$coefficients[, "Estimate"],
    StdError = summary_model$coefficients[, "Std. Error"],
    tValue = summary_model$coefficients[, "t value"],
    pValue = 2 * pt(-abs(summary_model$coefficients[, "t value"]), 
                    df = nrow(df) - length(fixef(model)))
  )
  
  # Store results
  results[[emg_var]] <- fixed_effects
}

# Combine all results into a single data frame
all_fixed_effects <- do.call(rbind, results)

print(all_fixed_effects)



# Assume 'p_values' is your vector of p-values
adjusted_pvalues_bonferroni <- p.adjust(all_fixed_effects$pValue, method = "bonferroni")
adjusted_pvalues_holm <- p.adjust(all_fixed_effects$pValue, method = "holm")
adjusted_pvalues_bh <- p.adjust(all_fixed_effects$pValue, method = "BH")  # Benjamini-Hochberg

# Add adjusted p-values to your results data frame
all_fixed_effects$Bonferroni <- adjusted_pvalues_bonferroni
all_fixed_effects$Holm <- adjusted_pvalues_holm
all_fixed_effects$BH <- adjusted_pvalues_bh



all_fixed_effects %>% filter(Term!="(Intercept)") %>% select(-c(Term, Variable))


# -----------
# Linear Mixed-effects Models - Imputed - Delta FAP vs Delta EMG ---------------


attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed) %>% ungroup()



df <- df_target_vars_imputed


sum(is.na(df))

# First, let's define the function to compute normalized delta for each variable

calculate_deltas <- function(df, emg_vars) {
  df <- df %>%
    group_by(Patient) %>%
    arrange(Patient, Visite_date)  # Ensure data is sorted by patient and visit day
  
  # Calculate deltas for each EMG variable
  for (emg_var in emg_vars) {
    delta_var <- paste0("delta_", emg_var)  # Create new column name for delta
    df <- df %>%
      mutate(!!delta_var := ifelse(
        row_number() > 1,  # Skip first row (no previous data)
        (get(emg_var) - lag(get(emg_var))) ,
        NA  # First visit delta is NA
      ))
  }
  
  
  return(df)
}

# List of EMG variable names (EMG1 to EMG30)
emg_vars <- paste0(names(df)[3:29])

# Apply the function to calculate deltas
df_with_deltas <- calculate_deltas(df, emg_vars)

sum(is.na(df_with_deltas))

df_with_deltas_imputed <- df_with_deltas %>% select(Patient, Visite_date, 30:56) %>% ungroup() %>% drop_na()


df_with_deltas_imputed <- df_with_deltas_imputed %>%
  mutate(across(where(is.numeric), scale))


library(lme4)


df <- df_with_deltas_imputed


emg_vars <- paste0(names(df)[5:29])


# Fit models for each variable and store results
results <- list()

for (emg_var in emg_vars) {
  # Fit the mixed-effects model
  model <- lmer(as.formula(paste("delta_FAP  ~", emg_var, "+ (1 | Patient)")), data = df)
  
  # Save the model summary
  results[[emg_var]] <- summary(model)
}




emg_vars <- paste0(names(df)[5:29])

results <- list()

for (emg_var in emg_vars) {
  # Fit the model
  model <-  lmer(as.formula(paste("delta_FAP  ~", emg_var, "+ (1 | Patient)")), data = df)
  
  # Extract fixed effects
  summary_model <- summary(model)
  fixed_effects <- data.frame(
    Variable = emg_var,
    Term = rownames(summary_model$coefficients),
    Estimate = summary_model$coefficients[, "Estimate"],
    StdError = summary_model$coefficients[, "Std. Error"],
    tValue = summary_model$coefficients[, "t value"],
    pValue = 2 * pt(-abs(summary_model$coefficients[, "t value"]), 
                    df = nrow(df) - length(fixef(model)))
  )
  
  # Store results
  results[[emg_var]] <- fixed_effects
}

# Combine all results into a single data frame
all_fixed_effects <- do.call(rbind, results)

print(all_fixed_effects)

# Assume 'p_values' is your vector of p-values
adjusted_pvalues_bonferroni <- p.adjust(all_fixed_effects$pValue, method = "bonferroni")
adjusted_pvalues_holm <- p.adjust(all_fixed_effects$pValue, method = "holm")
adjusted_pvalues_bh <- p.adjust(all_fixed_effects$pValue, method = "BH")  # Benjamini-Hochberg

# Add adjusted p-values to your results data frame
all_fixed_effects$Bonferroni <- adjusted_pvalues_bonferroni
all_fixed_effects$Holm <- adjusted_pvalues_holm
all_fixed_effects$BH <- adjusted_pvalues_bh


all_fixed_effects %>% filter(Term!="(Intercept)")

row.names(all_fixed_effects) <- NULL


all_fixed_effects %>% filter(Term!="(Intercept)") %>% select(-c(Variable, Term))

# -----------
# Linear Mixed-effects Models - Imputed - Lead FAP vs Lag EMG ---------------


attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

sum(is.na(df_target_vars_imputed))

names(attr_emg_input)

unique(df_target_vars_imputed$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)



df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date) %>%
  bind_cols(df_target_vars_imputed) %>% ungroup()


df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(across(where(is.numeric), scale))

sum(is.na(df_target_vars_imputed))

library(lme4)


df <- df_target_vars_imputed


# Create lagged dataset
df_lagged <- df %>%
  group_by(Patient) %>%
  arrange(Visite_date) %>%
  mutate(FAP_future = lead(FAP, n = 1)) %>%
  filter(!is.na(FAP_future))

# Fit mixed-effects model
model_lagged <- lmer(FAP_future ~ Score_Total  + (1 | Patient), data = df_lagged)
summary(model_lagged)






emg_vars <- paste0(names(df_lagged)[5:29])

results <- list()

for (emg_var in emg_vars) {
  # Fit the model
  model <-  lmer(as.formula(paste("FAP_future  ~", emg_var, "+ (1 | Patient)")), data = df_lagged)
  
  # Extract fixed effects
  summary_model <- summary(model)
  fixed_effects <- data.frame(
    Variable = emg_var,
    Term = rownames(summary_model$coefficients),
    Estimate = summary_model$coefficients[, "Estimate"],
    StdError = summary_model$coefficients[, "Std. Error"],
    tValue = summary_model$coefficients[, "t value"],
    pValue = 2 * pt(-abs(summary_model$coefficients[, "t value"]), 
                    df = nrow(df) - length(fixef(model)))
  )
  
  # Store results
  results[[emg_var]] <- fixed_effects
}

# Combine all results into a single data frame
all_fixed_effects <- do.call(rbind, results)

print(all_fixed_effects)



# Assume 'p_values' is your vector of p-values
adjusted_pvalues_bonferroni <- p.adjust(all_fixed_effects$pValue, method = "bonferroni")
adjusted_pvalues_holm <- p.adjust(all_fixed_effects$pValue, method = "holm")
adjusted_pvalues_bh <- p.adjust(all_fixed_effects$pValue, method = "BH")  # Benjamini-Hochberg

# Add adjusted p-values to your results data frame
all_fixed_effects$Bonferroni <- adjusted_pvalues_bonferroni
all_fixed_effects$Holm <- adjusted_pvalues_holm
all_fixed_effects$BH <- adjusted_pvalues_bh



all_fixed_effects %>% filter(Term!="(Intercept)") %>% select(-c(Term, Variable))


# -----------
# Get the coeficientes for simpler LASSO Model ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed <- fread( "../data/df_target_vars_imputed.txt")

df_target_vars_imputed <- df_target_vars_imputed %>% select(-c(FAP,Score_Total ,Score_Hemi_Right , Score_UlnSPI))


# Ensure predictors are scaled
#df_target_vars_imputed <- df_target_vars_imputed %>%
# mutate(across(where(is.numeric), scale))



# Ensure predictors are scaled
X <- model.matrix(NISLL ~ .  , data = df_target_vars_imputed)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed$NISLL  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = FALSE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se


# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)


# (Intercept)                                           46.37375791
# MedianMotorRight_Wrist_ThumbAbduction                  .         
# MedianMotorLeft_Wrist_ThumbAbduction                   .         
# UlnarMotorRight_Wrist_FingerAdduction                 -0.85954254
# UlnarMotorLeft_Wrist_FingerAdduction                  -0.86771705
# ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis  .         
# ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis  -0.36677401
# InternalPoplitealSciaticMotorRight_Ankle_CFPI         -1.13347535
# InternalPoplitealSciaticMotorLeft_Ankle_CFPI           .         
# RadialSensoryRight                                     .         
# RadialSensoryLeft                                     -0.20191231
# MedianSensoryRight                                     0.15193386
# MedianSensoryLeft                                      0.11927266
# UlnarSensoryRight                                      0.33440684
# UlnarSensoryLeft                                      -0.09857558
# MusculocutaneousSensoryRight                           .         
# MusculocutaneousSensoryLeft                           -0.21570357
# SuralSensitifD                                        -0.32001038
# SuralSensoryLeft                                      -0.16372944
# MedianVelocityRight                                   -0.06083141
# MedianVelocityLeft                                    -0.19897490
# MedianDistalLatencyRight                               .         
# MedianDistalLatencyLeft                                . 





# First VISIT


df_target_vars_imputed_first <- fread( "../data/df_target_vars_imputed_first.txt")

df_target_vars_imputed_first <- df_target_vars_imputed_first %>% select(-c(FAP,Score_Total ,Score_Hemi_Right , Score_UlnSPI))



# Ensure predictors are scaled
X <- model.matrix(NISLL ~ .  , data = df_target_vars_imputed_first)[, -1]  # Design matrix (exclude intercept)
y <- df_target_vars_imputed_first$NISLL  # Response variable

# LASSO Regression (alpha = 1)
set.seed(1)
lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = FALSE, maxit = 1e6)

# Best lambda
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se


# Extract coefficients for the best lambda
lasso_coeffs <- coef(lasso_cv, s = "lambda.1se")
print(lasso_coeffs)


# (Intercept)                                           34.75210301
# MedianMotorRight_Wrist_ThumbAbduction                  .         
# MedianMotorLeft_Wrist_ThumbAbduction                   .         
# UlnarMotorRight_Wrist_FingerAdduction                  .         
# UlnarMotorLeft_Wrist_FingerAdduction                   .         
# ExternalPoplitealSciaticMotorRight_Foot_DorsalisPedis  .         
# ExternalPoplitealSciaticMotorLeft_Foot_DorsalisPedis   .         
# InternalPoplitealSciaticMotorRight_Ankle_CFPI          .         
# InternalPoplitealSciaticMotorLeft_Ankle_CFPI          -0.39611534
# RadialSensoryRight                                     .         
# RadialSensoryLeft                                     -0.32607547
# MedianSensoryRight                                     .         
# MedianSensoryLeft                                      .         
# UlnarSensoryRight                                      .         
# UlnarSensoryLeft                                       .         
# MusculocutaneousSensoryRight                           .         
# MusculocutaneousSensoryLeft                            .         
# SuralSensitifD                                        -0.31479721
# SuralSensoryLeft                                      -0.00492449
# MedianVelocityRight                                   -0.09836101
# MedianVelocityLeft                                    -0.14182843
# MedianDistalLatencyRight                               .         
# MedianDistalLatencyLeft                                .    


# Predict function for glmnet models
lasso_predictions <- predict(lasso_cv, newx = X, s = "lambda.1se")

cor(lasso_predictions, data.frame(y), method="spearman")


# Create data frames for plotting
lasso_plot_data <- data.frame(
  Actual = y,
  Predicted = as.numeric(lasso_predictions)
)



# LASSO Plot
ggplot(lasso_plot_data, aes(x = Actual, y = Predicted)) +
  geom_abline(slope = 1, intercept = 0, linewidth=1, color = "#FAC67A") +
  geom_jitter(alpha = 0.6, color = "#183555", shape=1, stroke=2, width = 0.3, height = 0.3) +
  theme_minimal() +
  xlim(-3,3) + ylim(-3,3) +
  labs(title = "LASSO: Actual vs Predicted", x = "Actual Standardized Values", y = "Predicted Standardized Values")



# ----------
# Drug Treatments ---------------

attr_emg_input <- fread("../data/attr_emg_input.txt")

df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

sum(is.na(df_target_vars_imputed))

names(attr_emg_input)

unique(df_target_vars_imputed$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date, Treatment,Traitement_change ) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% 
  bind_cols(df_target_vars_imputed) %>% ungroup()

unique(df_target_vars_imputed$Traitement_change)
unique(df_target_vars_imputed$Treatment)

df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(Traitement_change=ifelse(is.na(Traitement_change),0,Traitement_change))

df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(Treatment=ifelse(is.na(Treatment),NA,Treatment)) %>%
  mutate(Treatment=ifelse(Treatment=="", NA, Treatment)) %>%
  mutate(Treatment=ifelse(Treatment=="1", NA, Treatment)) 


df_target_vars_imputed <- df_target_vars_imputed %>%
  mutate(Treatment=ifelse(Treatment=="0", NA, Treatment))
  

df_target_vars_imputed <- df_target_vars_imputed %>% 
  mutate(Treatment=ifelse(Treatment=="I", "(I) Inotersen",
                          ifelse(Treatment=="P", "(P) Patisiran",
                                 ifelse(Treatment=="T", "(T) Tafamidis",
                                        ifelse(Treatment=="T+P", "(T)+(P)",
                                               ifelse(Treatment=="V", "(V) Vutrisiran", Treatment))))))




Treatment     n    perc
# 1 0           126 0.27842  
# 2 I             4 0.00883
# 3 P           122 0.269  
# 4 T           163 0.360  
# 5 T+P          30 0.0662 
# 6 V             8 0.0177 



to_plot <- df_target_vars_imputed %>% group_by(Treatment ) %>% count() %>% mutate(perc=n/453)

to_plot$perc_label <- paste0(round(to_plot$perc * 100, 2), "%")

custom_colors <- c("#FAC67A", "#3A6EA5", "#183555", "#789BC4",  "#FFA559",  "gray")


ggplot(to_plot, aes(x = Treatment, y = n, fill = Treatment)) +
  geom_bar(stat = "identity", alpha=0.9) +
  geom_text(aes(label = perc_label), vjust = -0.5, size = 4) +
  #scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Number|percentage of all patient-visits \nON each Treatment [n=453]",
    x = "\n Treatment ON",
    y = "Absolute number of all patient-visits \n"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) 


Treatment     n    perc

#   1 0            49 0.5829 
# 2 P             3 0.0357
# 3 T            32 0.381 


to_plot <-  df_target_vars_imputed %>% group_by(Patient) %>% filter(Visite_date==min(Visite_date)) %>%
  group_by(Treatment ) %>% count() %>% mutate(perc=n/84)

to_plot$perc_label <- paste0(round(to_plot$perc * 100, 2), "%")

custom_colors <- c("#3A6EA5", "#183555",  "gray")


ggplot(to_plot, aes(x = Treatment, y = n, fill = Treatment)) +
  geom_bar(stat = "identity", alpha=0.9) +
  geom_text(aes(label = perc_label), vjust = -0.5, size = 4) +
  #scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Number|percentage of the 1st patient-visits \nON each Treatment [n=84]",
    x = "\n Treatment ON",
    y = "Absolute number of 1st patient-visits \n"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) 


first_visits <- df_target_vars_imputed %>% group_by(Patient) %>% filter(Visite_date==min(Visite_date))


# Select the variables from NISLL onwards
variables_to_plot <- names(first_visits)[which(names(first_visits) == "NISLL"):ncol(first_visits)]

constant_vars <- variables_to_plot[sapply(first_visits[variables_to_plot], function(x) sd(x, na.rm = TRUE) == 0)]

print(constant_vars)

scale(first_visits$NISLL)

# Scale continuous variables (optional)
data_scaled <- first_visits

for (var in variables_to_plot) {
  data_scaled[[var]] <- scale(first_visits[[var]])
}

# Calculate mean and SD for each variable by treatment group
summary_data <- data_scaled %>%
  pivot_longer(cols = all_of(variables_to_plot), names_to = "Variable", values_to = "Value") %>%
  group_by(Treatment, Variable) %>%
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Create the forest plot
ggplot(summary_data, aes(x = Mean, y = Variable, color = Treatment)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = Mean - SD, xmax = Mean + SD), 
                 height = 0.4, 
                 position = position_dodge(width = 0.5)) +
  labs(
    title = "Forest Plot of Mean ± SD for \nClinical|Electromyographic Variables by Treatment",
    x = "\n Mean ± SD (Scaled Values)",
    y = "Clinical|Electromyographic Variable \n"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )



custom_colors <- c("#183555", "firebrick",  "gray")

ggplot(summary_data, aes(x = Mean, y = Variable, color = Treatment)) +
  # Thicker error bars using geom_segment
  geom_segment(aes(x = Mean - SD, xend = Mean + SD, y = Variable, yend = Variable),
               size = 1, position = position_dodge(width = 0.4), alpha=0.6) +
  # Add points for the means
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  labs(
    title = "Forest Plot of Mean ± SD for \nClinical|Electromyographic Variables by Treatment",
    x = "\n Mean ± SD (Scaled Values)",
    y = "Clinical|Electromyographic Variable \n"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) 


ggplot(summary_data, aes(x = Treatment, y = Mean, color = Treatment)) +
  # Box plot for each treatment group
  geom_boxplot(aes(group = Treatment), 
               position = position_dodge(width = 0.6), 
               alpha = 0.4) +
  # Add points for the means (to overlay over the box plots)
  geom_point(aes(x = Treatment, y = Mean), 
             size = 3, 
             color = "black", 
             position = position_dodge(width = 0.6)) +
  labs(
    title = "Box Plot of Mean ± SD for \nClinical|Electromyographic Variables by Treatment",
    x = "\n Treatment",
    y = "Clinical|Electromyographic Variable \n"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors)



# Reshape the data into long format for ggplot
data_long <- data_scaled %>%
  pivot_longer(cols = all_of(variables_to_plot),  # Adjust with the correct variables
               names_to = "Variable",
               values_to = "Value") %>%
  filter(!is.na(Value))  # Remove any NA values if present


custom_colors <- c("#FAC67A", "#183555",  "gray")


# Create the box plots
ggplot(data_long, aes(x = Treatment, y = Value, fill = Treatment)) +
  # Box plot for each variable
  geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.95, notch = T) +
  facet_wrap(~Variable, scales = "free_y") +  # Create separate boxplots for each variable
  labs(
    title = "Scaled Clinical|Electromyographic Variables Variables by Treatment",
    x = "\nTreatment",
    y = "Scaled Values \n"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_colour_manual(values = custom_colors) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
       # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 






# Function to create boxplots for each feature
create_boxplot <- function(feature_name) {
  ggplot(first_visits, aes(x = Treatment, y = !!sym(feature_name), fill = Treatment, colour=Treatment)) +
    geom_jitter(shape=1, stroke=2, width = 0.2, height = 0.2, size=2, alpha=0.6) +
    geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.8, notch = TRUE) +
    labs(
      y = paste0(feature_name, "\n"),
      x = "\n Treatment Group",
      title = feature_name
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 12, vjust = -0.5),
      axis.title.y = element_text(size = 12, vjust = -0.5),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    scale_fill_manual(values = c("firebrick", "#183555", "lightgray")) +  # Customize colors for treatments
    scale_colour_manual(values = c("firebrick", "#183555", "lightgray"))
}


# List of variables to loop through (adjust column indices as needed)
feature_names <- colnames(first_visits)[5:ncol(first_visits)]  # Columns starting from 5th onwards

# Generate box plots for each feature (variable)
plots <- map(feature_names, create_boxplot)

# Display the second plot as an example
plots[[25]]
# All plots will be stored in `plots` and can be accessed by index

plots





unique(df_target_vars_imputed$Treatment)


# Function to create boxplots for each feature
create_boxplot <- function(feature_name) {
  ggplot(df_target_vars_imputed, aes(x = Treatment, y = !!sym(feature_name), fill = Treatment, colour=Treatment)) +
    geom_jitter(shape=1, stroke=2, width = 0.2, height = 0.2, size=2, alpha=0.6) +
    geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.8, notch = TRUE) +
    labs(
      y = paste0(feature_name, "\n"),
      x = "\n Treatment Group",
      title = feature_name
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 12, vjust = -0.5),
      axis.title.y = element_text(size = 12, vjust = -0.5),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    scale_fill_manual(values = c("#FAC67A", "firebrick", "#183555", "#789BC4",  "#FFA559",  "gray")) +  # Customize colors for treatments
    scale_colour_manual(values = c("#FAC67A", "firebrick", "#183555", "#789BC4",  "#FFA559",  "gray"))
}



# List of variables to loop through (adjust column indices as needed)
feature_names <- colnames(df_target_vars_imputed)[5:ncol(df_target_vars_imputed)]  # Columns starting from 5th onwards

# Generate box plots for each feature (variable)
plots <- map(feature_names, create_boxplot)

# Display the second plot as an example
plots[[25]]
# All plots will be stored in `plots` and can be accessed by index

plots


to_plot <- df_target_vars_imputed %>% select(Patient, Visite_date, Treatment) %>% mutate(exp=1) %>%
  distinct() %>%
  spread(key=Treatment, value=exp) %>%
  mutate(`(P) Patisiran`=ifelse(is.na(`(T)+(P)`), `(P) Patisiran`, 1)) %>%
  mutate(`(T) Tafamidis`=ifelse(is.na(`(T)+(P)`), `(T) Tafamidis`, 1)) %>%
  select(-`<NA>`) 

to_plot[is.na(to_plot)] <- 0

to_plot <- to_plot %>% group_by(Patient) %>%
  mutate(`(I) Inotersen`=cumsum(`(I) Inotersen`)) %>% mutate(`(I) Inotersen`=ifelse(`(I) Inotersen`==0,0,1)) %>%
  mutate(`(P) Patisiran`=cumsum(`(P) Patisiran`)) %>% mutate(`(P) Patisiran`=ifelse(`(P) Patisiran`==0,0,1)) %>%
  mutate( `(T) Tafamidis`=cumsum( `(T) Tafamidis`)) %>% mutate(`(T) Tafamidis`=ifelse( `(T) Tafamidis`==0,0,1)) %>%
  mutate(`(V) Vutrisiran`=cumsum(`(V) Vutrisiran`)) %>% mutate(`(V) Vutrisiran`=ifelse(`(V) Vutrisiran`==0,0,1)) %>%
  select(-`(T)+(P)`)


all_dates <- to_plot %>%
  pull(Visite_date) %>%
  unique() %>%
  sort()

expanded_data <- to_plot %>% ungroup() %>%
  complete(Patient, Visite_date = all_dates) %>%
  arrange(Patient, Visite_date)

expanded_data_filled <- expanded_data %>%
  group_by(Patient) %>%
  fill(`(I) Inotersen`, `(P) Patisiran`, `(T) Tafamidis` ,`(V) Vutrisiran`, .direction = "down") %>%
  ungroup()

cumulative_data <- expanded_data_filled %>%
  mutate(
    on_inotersen = `(I) Inotersen` > 0,
    on_patisiran = `(P) Patisiran` > 0,
    on_tafamidis = `(T) Tafamidis` > 0,
    on_vutrisiran = `(V) Vutrisiran` > 0
  ) %>%
  group_by(Visite_date) %>%
  summarise(
    cumulative_inotersen = sum(on_inotersen),
    cumulative_patisiran = sum(on_patisiran),
    cumulative_tafamidis = sum(on_tafamidis),
    cumulative_vutrisiran = sum(on_vutrisiran)
  ) %>%
  ungroup()


ggplot(cumulative_data, aes(x = Visite_date)) +
  geom_line(aes(y = 100*cumulative_inotersen/84, color = "Inotersen"), size = 2) +
  geom_line(aes(y = 100*cumulative_patisiran/84, color = "Patisiran"), size = 2) +
  geom_line(aes(y = 100*cumulative_tafamidis/84, color = "Tafamidis"), size = 2) +
  geom_line(aes(y = 100*cumulative_vutrisiran/84, color = "Vutrisiran"), size = 2) +
  labs(
    title = "Cumulative % Patients \nWho Have Been ON Each Drug Over Time",
    x = "\n Visit Date [Elapsed Time From Baseline]]",
    y = "Cumulative Patient Percentage (%) \n",
    color = "Drug"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#FAC67A", "firebrick", "#183555", "#789BC4")) +  # Customize colors for treatments
  scale_colour_manual(values = c("#FAC67A", "firebrick", "#183555", "#789BC4"))






data_with_change <- df_target_vars_imputed %>%
  arrange(Patient, Visite_date) %>%
  group_by(Patient) %>%
  mutate(
    previous_NISLL = lag(NISLL), # Severity score before the visit
    previous_FAP = lag(FAP),    # Severity score before the visit
    treatment_initiated = ifelse(Traitement_change == 1, 1, 0)
  ) %>%
  ungroup()


patients_on_treatment <- data_with_change %>%
  filter(treatment_initiated == 1)


severity_changes <- patients_on_treatment %>%
  filter(!is.na(previous_NISLL)) %>%
  mutate(
    change_in_NISLL = NISLL - previous_NISLL,
    change_in_FAP = FAP - previous_FAP
  )


mean(severity_changes$change_in_NISLL)
t.test(severity_changes$change_in_NISLL)
mean(severity_changes$change_in_FAP)
t.test(severity_changes$change_in_FAP)

severity_changes %>% filter(Treatment!="(I) Inotersen") %>%
  ggplot(aes(change_in_NISLL, colour=Treatment, fill=Treatment)) +
  geom_density( alpha = 0.75, adjust=2, size=2) +
  labs(
    title = "Change in NISLL Scores Before and After Treatment Initiation",
    x = "\n NISLL Change From Pre-Treatment Initiation",
    y = "Patient density (Gaussian Kernel) \n"
  ) +
  theme_minimal() +
  #facet_wrap(~Treatment ) +
  scale_fill_manual(values = c( "firebrick", "#183555", "#FAC67A",  "gray")) +  # Customize colors for treatments
  scale_colour_manual(values = c( "firebrick", "#183555","#FAC67A",  "gray"))



severity_changes %>% filter(Treatment!="(I) Inotersen") %>%
  ggplot(aes(change_in_FAP, colour=Treatment, fill=Treatment)) +
  geom_density( alpha = 0.75, adjust=2, size=2) +
  labs(
    title = "Change in NISLL Scores Before and After Treatment Initiation",
    x = "\n NISLL Change From Pre-Treatment Initiation",
    y = "Patient density (Gaussian Kernel) \n"
  ) +
  theme_minimal() +
  #facet_wrap(~Treatment ) +
  scale_fill_manual(values = c( "firebrick", "#183555", "#FAC67A",  "gray")) +  # Customize colors for treatments
  scale_colour_manual(values = c( "firebrick", "#183555","#FAC67A",  "gray"))




# Create a cumulative post-treatment variable
data <- df_target_vars_imputed %>%
  arrange(Patient, Visite_date) %>%  # Ensure the data is sorted by Patient and Visit_date
  group_by(Patient) %>% 
  mutate(
    post_treatment = cumsum(Traitement_change) > 0  # 1 after treatment is initiated, remains 1
  ) %>%
  ungroup()


# Fit the ITS model (segmented regression)
its_model <- lm(NISLL ~ Visite_date + post_treatment + Visite_date * post_treatment, data = data)

# View the summary of the model
summary(its_model)


data %>% ggplot(aes(Visite_date, NISLL, color=post_treatment, fill=post_treatment)) +
  geom_smooth()


# ------
# Best Subset NISLL get coeficients ----------


library(leaps)
library(glmnet)
library(car)

df_target_vars_imputed <- fread( "../data/df_target_vars_imputed.txt")

df_target_vars_imputed <- df_target_vars_imputed %>% select(-c(FAP,Score_Total ,Score_Hemi_Right , Score_UlnSPI))


# Ensure predictors are scaled
#df_target_vars_imputed <- df_target_vars_imputed %>%
#  mutate(across(where(is.numeric), scale))


#Best Subset Selection
set.seed(1)


regit_full <- regsubsets(NISLL  ~ ., data = df_target_vars_imputed, nvmax = 22, really.big=T)

reg_summary <- summary(regit_full)


library(leaps)

# Get the best model of each size
best_model_vars <- summary(regit_full)$which

# Initialize a list to store coefficients
coef_list <- list()

# Loop through different model sizes
for (i in 1:nrow(best_model_vars)) {
  selected_vars <- names(best_model_vars[i, ])[best_model_vars[i, ] == TRUE]
  formula_str <- paste("NISLL ~", paste(selected_vars[-1], collapse = " + "))  # Exclude intercept
  
  model <- lm(as.formula(formula_str), data = df_target_vars_imputed)
  
  coef_list[[paste0("Model_", i)]] <- coef(model)  # Store coefficients
}

# View coefficients for each model
coef_list

coef_values <- lapply(coef_list, function(x) as.numeric(x))


jsonlite::write_json(coef_values, "model_coefficients.json", auto_unbox = TRUE, pretty = TRUE)

# Extract just the variable names
coef_names <- lapply(coef_list, function(x) names(x))

# Save variable names as JSON
jsonlite::write_json(coef_names, "model_variables.json", auto_unbox = TRUE, pretty = TRUE)

# ----------
# Process data for deep learning ---------
attr_emg_input <- fread("../data/attr_emg_input.txt")

attr_emg_input <- attr_emg_input %>%
  mutate(Treatment=ifelse(is.na(Treatment),NA,Treatment)) %>%
  mutate(Treatment=ifelse(Treatment=="", NA, Treatment)) %>%
  mutate(Treatment=ifelse(Treatment=="1", NA, Treatment)) 


attr_emg_input <- attr_emg_input %>%
  mutate(Treatment=ifelse(Treatment=="0", NA, Treatment))
  

attr_emg_input <- attr_emg_input %>% 
  mutate(Treatment=ifelse(Treatment=="I", "Inotersen",
                          ifelse(Treatment=="P", "Patisiran",
                                 ifelse(Treatment=="T", "Tafamidis",
                                        ifelse(Treatment=="T+P", "T_plus_P",
                                               ifelse(Treatment=="V", "Vutrisiran", Treatment))))))



attr_emg_input %>% group_by(Treatment) %>% count()

attr_emg_input <- attr_emg_input %>% mutate(Treatment=ifelse(Treatment=="TH", "none",
                                           ifelse(is.na(Treatment), "none", Treatment)))


df_target_vars_imputed <- fread("../data/df_target_vars_imputed.txt")

sum(is.na(df_target_vars_imputed))

names(attr_emg_input)

unique(attr_emg_input$Enrolled)

attr_emg_input <- attr_emg_input %>% filter(is.na(Enrolled))

attr_emg_input <- attr_emg_input %>% select(Patient, Visite_date, Treatment) %>%
  mutate(Visite_date=as.Date(Visite_date)) %>%
  arrange(Patient, Visite_date) %>% group_by(Patient) %>%
  mutate(first=min(Visite_date)) %>%
  mutate(Visite_date=as.numeric(Visite_date-min(Visite_date))) %>% select(-first)

df_target_vars_imputed <- attr_emg_input %>% select(Patient, Visite_date, Treatment) %>%
  bind_cols(df_target_vars_imputed)

sum(is.na(df_target_vars_imputed))

sum(is.na(df_target_vars_imputed$Treatment))


df_target_vars_imputed <- df_target_vars_imputed %>% mutate(Treatment=ifelse(is.na(Treatment), "none", Treatment))


df_target_vars_imputed <- df_target_vars_imputed %>% drop_na()

df <- df_target_vars_imputed

df <- df %>% select(-c(FAP, Score_Total, Score_Hemi_Right, Score_UlnSPI))

unique(df$Treatment)

df <- df %>% ungroup() %>%
  mutate(Exp=1) %>%
   spread(key=Treatment, value=Exp)

df[is.na(df)] <- 0

df <- df %>% select(-none)

names(df)

df <- df %>% mutate(Patisiran=ifelse(T_plus_P==1,1,Patisiran)) %>%
  mutate(Tafamidis=ifelse(T_plus_P==1,1,Tafamidis)) %>% select(-T_plus_P)
  
# Normalize numerical variables (NISLL & EMG features)
# num_vars <- names(df)[!(names(df) %in% c("Patient", "Visite_date", "Inotersen", "Patisiran", "Tafamidis", "Vutrisiran"))]  # Exclude categorical vars
# preproc <- caret::preProcess(df[num_vars], method = c("center", "scale"))  # Normalize
# df[num_vars] <- predict(preproc, df[num_vars])

# Ensure data is sorted by Patient and Visit_date
df <- df %>%
  arrange(Patient, Visite_date   )


write.csv(df, "processed_data.csv", row.names = FALSE)


# -----------