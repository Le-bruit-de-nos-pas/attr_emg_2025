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
