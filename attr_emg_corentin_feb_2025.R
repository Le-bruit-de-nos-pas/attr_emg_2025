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

#fwrite(df_target_vars, "../data/df_target_vars.txt")
#fwrite(df_target_vars_imputed, "../data/df_target_vars_imputed.txt")



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





# ----------
