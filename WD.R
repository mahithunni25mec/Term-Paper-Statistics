library(dplyr)
library(ggplot2)
library(ggrepel)
library(readODS)
library(tidyverse)
library(plm)
library(lmtest)
library(reshape2)
library(scales)
library(stringr)
library(geodata)
library(sf)
library(viridis)

# SET YOUR WORKING DIRECTORY HERE
setwd('C:/Users/Lenovo/Desktop/misc/NEW PROJ/APU files/Sem1/R Programming/Term Paper/datafiles/data_dump/6th onwards')

# --- Helper Function ---
clean_udise_data <- function(raw_df) {
  colnames(raw_df)[1] <- "Year"
  clean_df <- raw_df %>% filter(str_detect(Year, "^\\d{4}-\\d{2}$"))
  return(clean_df)
}

# =============================================================================
# STEP 1: PREPARE ENROLLMENT DATA (MERGING BOYS & GIRLS)
# =============================================================================

# 1.1 Read Girls File
raw_girls <- read_ods("Enrollment_Girls_Total_Students_6th.ods")
df_girls_wide <- clean_udise_data(raw_girls)
df_girls_wide[,-1] <- sapply(df_girls_wide[,-1], as.numeric)

# 1.2 Read Boys File
raw_boys <- read_ods("Enrollment_Boys_Total_Students_6th.ods")
df_boys_wide <- clean_udise_data(raw_boys)
df_boys_wide[,-1] <- sapply(df_boys_wide[,-1], as.numeric)

class_list <- list()

# Loop for Classes 6 to 12
for (j in 1:7) {
  actual_class_num <- j + 5  # 1->6, 7->12
  start_col <- 2 + (j - 1) * 2
  
  # A. Extract Girls Data (Year, Total, Girls)
  temp_girls <- df_girls_wide[, c(1, start_col, start_col + 1)]
  colnames(temp_girls) <- c("Year", "Class_Total", "Class_Girls")
  
  # B. Extract Boys Data (Year, Boys) 
  # Assuming Boys file has same structure: Year | Total, Boys | Total, Boys...
  # We only need the "Boys" column (start_col + 1) because Total is already in girls file
  temp_boys <- df_boys_wide[, c(1, start_col + 1)]
  colnames(temp_boys) <- c("Year", "Class_Boys")
  
  # C. Merge them for this class
  temp_merged <- left_join(temp_girls, temp_boys, by = "Year")
  
  # D. Add Metadata & Ratios
  temp_merged$Class <- actual_class_num
  temp_merged$Girl_Student_GER <- temp_merged$Class_Girls / temp_merged$Class_Total
  temp_merged$Boy_Student_GER  <- temp_merged$Class_Boys  / temp_merged$Class_Total
  
  class_list[[j]] <- temp_merged
}

df_panel_enroll <- bind_rows(class_list)

# =============================================================================
# STEP 2: PREPARE NATIONAL VARIABLES
# =============================================================================
# Teachers
raw_teachers <- read_ods("Number of Teachers 6th onwards.ods")
df_teach <- clean_udise_data(raw_teachers)
df_teach[,-1] <- sapply(df_teach[,-1], as.numeric)

df_teach <- df_teach %>%
  mutate(
    Raw_Total_Teachers = .[[2]] + .[[4]] + .[[6]],
    Raw_Female_Teachers = .[[3]] + .[[5]] + .[[7]],
    Female_Teacher_Ratio = Raw_Female_Teachers / Raw_Total_Teachers
  ) %>%
  select(Year, Raw_Total_Teachers, Female_Teacher_Ratio)

# Expenditure
raw_expend <- read_ods("Edu_exp.ods")
df_exp <- clean_udise_data(raw_expend) %>%
  rename(Raw_Budget = 2) %>%
  mutate(Raw_Budget = as.numeric(Raw_Budget)) %>%
  select(Year, Raw_Budget)

# =============================================================================
# STEP 3: FINAL MERGE & PTR CALCULATION
# =============================================================================
national_students_df <- df_panel_enroll %>%
  group_by(Year) %>%
  summarise(National_Total_Students = sum(Class_Total, na.rm = TRUE))

final_panel_df <- df_panel_enroll %>%
  left_join(df_teach, by = "Year") %>%
  left_join(df_exp, by = "Year") %>%
  left_join(national_students_df, by = "Year") %>%
  arrange(Class, Year) %>%
  mutate(
    Policy_BBBP = ifelse(Year >= "2015-16", 1, 0),
    Class = as.factor(Class),
    PTR = National_Total_Students / Raw_Total_Teachers
  )

# Verify that Boys data is visible
print("--- DATA PREVIEW (CHECK FOR CLASS_BOYS) ---")
print(head(final_panel_df))

# =============================================================================
# PART 2: ANALYSIS
# =============================================================================

print("--- 2.1 PANEL DATA REGRESSION (Class-wise) ---")
# Controls for class-specific differences
model_panel <- plm(Girl_Student_GER ~ Female_Teacher_Ratio + log(Raw_Budget) + PTR + Policy_BBBP, 
                   data = final_panel_df, 
                   index = c("Class", "Year"), 
                   model = "within")
print(summary(model_panel))

print("--- 2.2 TIME SERIES REGRESSION (National Trend) ---")
# Looks at the overall movement of the entire system over time
model_ts <- lm(Girl_Student_GER ~ Female_Teacher_Ratio + log(Raw_Budget) + PTR + Policy_BBBP + Time_Trend, 
               data = final_ts_df)
print(summary(model_ts))
print(dwtest(model_ts)) # Check for autocorrelation

# =============================================================================
# PART 3: VISUALIZATIONS
# =============================================================================

# --- Plot A: Panel View (Class-wise Trends) ---
ggplot(final_panel_df, aes(x = Year, y = Girl_Student_GER, group = Class, color = Class)) +
  geom_line(size = 1) +
  labs(title = "Panel Analysis: Girl Enrollment by Class",
       subtitle = "Trends for Classes 6 through 12",
       y = "Girl Student Ratio") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot B: Time Series View (National Trend with Prediction) ---
# Create prediction data for plotting
final_ts_df$Predicted_GER <- predict(model_ts)

ggplot(final_ts_df, aes(x = Year)) +
  geom_line(aes(y = Girl_Student_GER, color = "Actual Data", group = 1), size = 1.5) +
  geom_point(aes(y = Girl_Student_GER, color = "Actual Data"), size = 3) +
  geom_line(aes(y = Predicted_GER, color = "Model Fit", group = 1), linetype = "dashed", size = 1.2) +
  labs(title = "Time Series Analysis: National Enrollment Trend",
       subtitle = "Actual vs Model Predicted Values (Classes 6-12 Aggregated)",
       y = "Overall Girl Student Ratio",
       color = "Legend") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot C: Correlation Heatmap (Panel Data) ---
numeric_cols <- final_panel_df %>%
  select(Girl_Student_GER, Female_Teacher_Ratio, Raw_Budget, PTR, National_Total_Students)
cormat <- round(cor(numeric_cols, use = "complete.obs"), 2)
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limit = c(-1,1), name = "Corr") +
  labs(title = "Correlation Matrix", x="", y="") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot D: Budget Bar Chart ---
ggplot(final_ts_df, aes(x = Year, y = Raw_Budget)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
  geom_text(aes(label = round(Raw_Budget, 0)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Education Budget Allocation", y = "Budget (Rs. Crore)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Option A: Boys' Student Ratio (GER) - Trends by Class
ggplot(final_panel_df, aes(x = Year, y = Boy_Student_GER, group = Class, color = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Boys' Enrollment Ratio (GER) by Class",
       subtitle = "Trends for Classes 6 through 12 (2012-2022)",
       y = "Boy Student Ratio (GER)",
       x = "Academic Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Option B: Boys' Raw Enrollment (Absolute Numbers) - Trends by Class
ggplot(final_panel_df, aes(x = Year, y = Class_Boys, group = Class, color = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) + # Adds commas to large numbers (e.g. 1,000,000)
  labs(title = "Boys' Absolute Enrollment by Class",
       subtitle = "Number of boys enrolled in each class over time",
       y = "Number of Boys",
       x = "Academic Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# MASTER SCRIPT: INDIA HEATMAP (FIXED BORDERS & ODISHA)
# ==============================================================================

# 1. SETUP & LIBRARIES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readODS, dplyr, stringr, tidyr, ggplot2, sf, geodata, viridis)

setwd("C:/Users/Lenovo/Desktop/misc/NEW PROJ/APU files/Sem1/R Programming/Term Paper/datafiles/data_dump/Statwise")

# 2. DEFINE FILE LIST (All 30+ files)
all_state_files <- c(
  "Bihar.ods", "Delhi.ods", "Haryana.ods", "HimachalPradesh.ods", 
  "JK.ods", "Punjab.ods", "Rajasthan.ods", "Uttarakhand.ods", "UttarPradesh.ods",
  "AndhraPradesh.ods", "Karnataka.ods", "Kerala.ods", "TamilNadu.ods", 
  "Telangana.ods", "Goa.ods", "Gujarat.ods", "Maharashtra.ods",
  "ArunachalPradesh.ods", "Assam.ods", "Manipur.ods", "Meghalaya.ods", 
  "Mizroram.ods", "Nagaland.ods", "Sikkim.ods", "Tripura.ods", "MadhyaPradesh.ods",
  "Chhattisgarh.ods", "Jharkhand.ods", "Odisha.ods", "WestBengal.ods"
)

# 3. PROCESSING FUNCTION
process_state_file <- function(filename) {
  if (!file.exists(filename)) return(NULL)
  state_name <- gsub(".ods", "", filename)
  raw_df <- read_ods(filename, col_names = FALSE)
  colnames(raw_df)[1] <- "Year"
  clean_df <- raw_df %>% filter(str_detect(Year, "^\\d{4}-\\d{2}$"))
  clean_df[, 2:15] <- sapply(clean_df[, 2:15], as.numeric)
  
  class_list <- list()
  classes <- 6:12
  for (i in seq_along(classes)) {
    total_col <- 2 + (i-1)*2
    girls_col <- total_col + 1
    temp <- clean_df[, c(1, total_col, girls_col)]
    colnames(temp) <- c("Year", "Total_Students", "Total_Girls")
    temp$Class <- classes[i]
    temp$State <- state_name
    class_list[[i]] <- temp
  }
  return(bind_rows(class_list))
}

# 4. MERGE DATA
All_India_Detailed_df <- lapply(all_state_files, process_state_file) %>% bind_rows()

# 5. AGGREGATE (2012 vs 2022)
Heatmap_Data <- All_India_Detailed_df %>%
  filter(Year %in% c("2012-13", "2021-22")) %>%
  group_by(State, Year) %>%
  summarise(State_Girl_Ratio = sum(Total_Girls, na.rm = TRUE) / sum(Total_Students, na.rm = TRUE)) %>%
  ungroup()

# 6. DOWNLOAD MAP & FIX NAMES
india_sp <- gadm(country = "IND", level = 1, path = tempdir())
india_sf <- st_as_sf(india_sp)

# --- FIXING NAMES ---
Heatmap_Plot_Data <- Heatmap_Data %>%
  mutate(
    # Add spaces (e.g., UttarPradesh -> Uttar Pradesh)
    State_Fixed = gsub("([a-z])([A-Z])", "\\1 \\2", State),
    
    # Manual Fixes
    State_Fixed = case_when(
      State == "JK" ~ "Jammu and Kashmir",
      State == "NCTofDelhi" ~ "Delhi",
      State == "AndamanandNicobarIslands" ~ "Andaman and Nicobar",
      # REMOVED THE ODISHA FIX so it stays "Odisha"
      TRUE ~ State_Fixed
    )
  )

# --- DIAGNOSTIC: CHECK MISMATCHES ---
# This prints states that didn't match so you can fix them!
print("--- CHECKING MISMATCHES ---")
unmatched <- setdiff(Heatmap_Plot_Data$State_Fixed, india_sf$NAME_1)
if(length(unmatched) > 0) {
  print("WARNING: The following states in your data did not match the map:")
  print(unmatched)
  print("Valid Map Names are:")
  print(sort(india_sf$NAME_1))
} else {
  print("SUCCESS: All states matched perfectly!")
}

# 7. MERGE & PLOT (Using inner_join to keep only valid data)
map_data_merged <- inner_join(india_sf, Heatmap_Plot_Data, by = c("NAME_1" = "State_Fixed"))

ggplot(data = map_data_merged) +
  # FIX: Changed color to 'gray40' (Dark Grey) so outlines are visible
  geom_sf(aes(fill = State_Girl_Ratio), color = "gray40", size = 0.3) +
  
  scale_fill_viridis_c(
    option = "magma", 
    name = "Girl Enrollment\nRatio (GER)",
    direction = -1,
    na.value = "grey90"
  ) +
  facet_wrap(~Year) + 
  labs(
    title = "Change in Girl Enrollment Ratio (2012 vs 2022)",
    subtitle = "Comparison of Girl Student Proportion (Classes 6-12)",
    caption = "Source: UDISE+ Data"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )