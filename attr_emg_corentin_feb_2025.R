library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)

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



                     ) 

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

# ----------
