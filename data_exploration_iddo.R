rm(list=ls())
gc()

pacman::p_load(
  dplyr, haven, sandwich, plm, data.table, tidyverse, stargazer, ggplot2, 
  lmtest, fixest, readxl, sf
)

select <- dplyr::select
options(digits=3)
options(scipen=999)
set.seed(123)

## change the working directory
setwd("C:/Users/iddo2/Downloads/unhcr oecd/bangladesh")

### bangladesh -----                                        
bang_hh <- read_dta("2023/UNHCR_BGD_2023_msnahost_datam_anon.dta")                           
bang_ind <- read_dta("2023/UNHCR_BGD_2023_msnahost_datai_anon.dta")

bang_ind21 <- read_dta("2021/UNHCR_BGD_2021_msnahost_datai_anon.dta")                        
bang_ind20 <- read_dta("2020/UNHCR BGD 2020 JMSNA Refugees Individuals.dta")

bang_ref_hh <- read_dta("2023/UNHCR_BGD_2023_msnaref_datam_anon.dta")
bang_ref_ind <- read_dta("2023/UNHCR_BGD_2023_msnaref_datai_anon.dta")

admin <- read_sf("bgd_adm_bbs_20201113_SHP/bgd_admbnda_adm4_bbs_20201113.shp") %>% 
  filter(
    ADM2_EN == "Cox's Bazar", # actually unnecessary as the next line overwrites it
    ADM3_EN %in% c("Teknaf", "Ukhia"),
    ADM4_EN != "St.Martin Dwip" # outside out the relevant zone and doesnt exist in survey
    ) %>% 
  select(ADM3_EN, ADM4_EN)

roads <- read_sf("20240519_A3_Camp_SubBlock_Outlines/190121_Access_Road_Footpath_all_camps.shp")
camp <- read_sf("20240519_A3_Camp_SubBlock_Outlines/20240519_A3_Camp_SubBlock_Outlines.shp")

ggplot() + 
  geom_sf(data = admin, aes(fill = ADM4_EN)) + 
  geom_sf(data = roads, aes(fill = Road_Type))


## exploring ----
common_vars <- intersect(colnames(bang_ref_hh), colnames(bang_hh))
bang_ref_hh_common <- bang_ref_hh[, common_vars]
bang_hh_common <- bang_hh %>% 
  filter(union_name %in% c(
    #"Palong Khali", "Nhilla", "Whykong"
    5, 4, 11
  )
  )
bang_hh_common <- bang_hh_common[, common_vars]  # doesnt actually matter

bang_ref_hh_common$group <- "refugee"
bang_hh_common$group <- "host"
bang_hh_full <- bind_rows(bang_ref_hh_common, bang_hh_common)

numeric_vars <- bang_hh_full %>%
  select(where(is.numeric)) %>%
  #select(-group) %>%
  colnames()

# Function to get mean and t-test
summary_stats <- map_dfr(numeric_vars, function(var) {
  refugee_vals <- bang_hh_full %>% filter(group == "refugee") %>% pull(var)
  host_vals <- bang_hh_full %>% filter(group == "host") %>% pull(var)
  
  # Remove NAs
  refugee_vals <- refugee_vals[!is.na(refugee_vals)]
  host_vals <- host_vals[!is.na(host_vals)]
  
  # Only continue if we have more than 1 unique value in each group
  if (length(unique(refugee_vals)) <= 1 || length(unique(host_vals)) <= 1) {
    return(tibble(
      variable = var,
      mean_refugee = mean(refugee_vals, na.rm = TRUE),
      mean_host = mean(host_vals, na.rm = TRUE),
      t_statistic = NA_real_,
      p_value = NA_real_
    ))
  }
  
  ttest_result <- tryCatch(
    t.test(refugee_vals, host_vals),
    error = function(e) return(NULL)
  )
  
  if (is.null(ttest_result)) {
    return(tibble(
      variable = var,
      mean_refugee = mean(refugee_vals, na.rm = TRUE),
      mean_host = mean(host_vals, na.rm = TRUE),
      t_statistic = NA_real_,
      p_value = NA_real_
    ))
  }
  
  tibble(
    variable = var,
    mean_refugee = mean(refugee_vals, na.rm = TRUE),
    mean_host = mean(host_vals, na.rm = TRUE),
    t_statistic = ttest_result$statistic,
    p_value = ttest_result$p.value
  )
})

hist(bang_ref_hh$income_v1_total_30_days)


convert_labelled_to_value <- function(x) {
  lbls <- labelled::val_labels(x)
  lbl_map <- setNames(as.numeric(names(lbls)), lbls)
  return(lbl_map[as.character(x)])
}

income_v1_cols <- intersect(
  bang_hh %>% select(starts_with("income_v1")) %>% colnames(),
  bang_ref_hh %>% select(starts_with("income_v1")) %>% colnames()
)

for (col in income_v1_cols) {
  new_col <- paste0(col, "_real")
  bang_ref_hh[[new_col]] <- convert_labelled_to_value(bang_ref_hh[[col]])
  bang_hh[[new_col]] <- convert_labelled_to_value(bang_hh[[col]])
}

income_v1_base_cols <- income_v1_cols[!income_v1_cols %in% "income_v1_total_30_days"]
income_v1_real_cols <- paste0(income_v1_base_cols, "_real")

# For bang_hh
bang_hh <- bang_hh %>%
  mutate(income_v1_total_30_days_real_computed = rowSums(across(all_of(income_v1_real_cols), ~ ., .names = "tmp"), na.rm = TRUE))

# For bang_ref_hh
bang_ref_hh <- bang_ref_hh %>%
  mutate(income_v1_total_30_days_real_computed = rowSums(across(all_of(income_v1_real_cols), ~ ., .names = "tmp"), na.rm = TRUE))

bang_hh <- bang_hh %>%
  mutate(across(
    all_of(income_v1_real_cols),
    ~ . / income_v1_total_30_days_real_computed,
    .names = "{.col}_share"
  ))

# For bang_ref_hh
bang_ref_hh <- bang_ref_hh %>%
  mutate(across(
    all_of(income_v1_real_cols),
    ~ . / income_v1_total_30_days_real_computed,
    .names = "{.col}_share"
  ))

bang_hh %>%
  select(ends_with("_real_share")) %>%
  pivot_longer(cols = everything(), names_to = "income_source", values_to = "share") %>%
  mutate(income_source = gsub("income_v1_|_real_share", "", income_source)) %>%
  ggplot(aes(x = income_source, y = share)) +
  geom_boxplot(fill = "#69b3a2", outlier.alpha = 0.2) +
  labs(x = "Income Source", y = "Share of Total Income",
       title = "Distribution of Income Shares Across Households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_data <- bind_rows(
  bang_hh %>%
    mutate(group = "host"),
  bang_ref_hh %>%
    mutate(group = "refugee")
)

income_v1_share_cols <- combined_data %>%
  select(contains("share")) %>% 
  select(contains("real")) %>% 
  colnames()

# Calculate mean share for each income type for each group
mean_shares <- combined_data %>%
  select(group, income_v1_share_cols) %>%
  group_by(group) %>%
  summarise(across(income_v1_share_cols, ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = income_v1_share_cols, 
               names_to = "income_type", values_to = "mean_share") %>%
  mutate(income_type = gsub("income_v1_real_", "", income_type))


# Add a group column to compare hosts vs refugees
mean_shares$group <- rep(c("host", "refugee"), each = nrow(mean_shares)/2)

# Plot the mean share of each income type for hosts vs refugees
ggplot(mean_shares, aes(x = income_type, y = mean_share, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Mean Share of Income Types for Hosts vs Refugees",
       x = "Income Type", y = "Mean Share") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## more 

bang_ref_hh_for_join <- bang_ref_hh %>% 
  select(
    (
      contains("share") & contains("real")
    ) | pseudo_parent_id
  ) 

bang_hh_for_join <- bang_hh %>% 
  select(
    (
      contains("share") & contains("real")
    ) | pseudo_parent_id
  ) 


cant_afford_educ_ref <- bang_ref_ind %>% 
  filter(
    ind_age < 4
  ) %>% 
  left_join(
    bang_ref_hh_for_join, by = "pseudo_parent_id"
  ) %>% 
  mutate(
    health = case_when(
      health_received_healthcare == "yes" ~ 1,
      health_received_healthcare == "no" ~ 0,
      TRUE ~ NA
    )
  ) %>% 
  select(
    (
      contains("share") & contains("real")
    ) | pseudo_parent_id | health
  ) 

cant_afford_educ_hosts <- bang_ind %>% 
  filter(
    ind_age < 4
  ) %>% 
  left_join(
    bang_hh_for_join, by = "pseudo_parent_id"
  ) %>% 
  mutate(
    health = case_when(
      health_received_healthcare == "yes" ~ 1,
      health_received_healthcare == "no" ~ 0,
      TRUE ~ NA
    )
  ) %>% 
  select(
    (
      contains("share") & contains("real")
    ) | pseudo_parent_id | health
  ) 


combined_data_kids <- bind_rows(
  cant_afford_educ_hosts %>%
    mutate(group = "host"),
  cant_afford_educ_ref %>%
    mutate(group = "refugee")
) %>% 
  filter(!is.na(health))




long_data <- combined_data_kids %>%
  pivot_longer(cols = starts_with("income_v1_"), 
               names_to = "income_source", 
               values_to = "share") %>%
  filter(!is.na(share)) # Remove rows with NA values in 'share'

# Plot health (binary) against the share of income variables, faceted by the group variable
ggplot(long_data, aes(x = share, y = health)) +
  geom_point(alpha = 0.5) + # Scatter plot
  geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(color = group), se = FALSE) + # Logistic regression
  facet_wrap(~ income_source) + # Facet by income source (share variable)
  labs(x = "Share of Income", y = "Health (Binary)", title = "Relationship Between Health and Income Share by Group") +
  theme_minimal() +
  theme(legend.position = "top")

### old -----
UNHCR_SSD_2024_CBP_hh_data_v1 <- fread("UNHCR_SSD_2024_CBP_hh_data_v1.csv")

ltu <- fread("UNHCR_LTU_2024_SEIS_indv_data_v1.csv")
svk <- read_excel("indv_dat_anonym_svk_final.xlsx")

panama <- fread("UNHCR_PAN_2021_HFS_Q4_v2.1.csv")

bang <- read_dta("UNHCR_BGD_2021_msnahost_datam_anon.dta")


h <- UNHCR_SSD_2024_CBP_hh_data_v1 %>% 
  filter(!is.na(reasons_children_not_enrolled)) %>% 
  mutate(type = case_when(
    living_situation_description ==
      "I am a host community member, having lived in my current location all my life without being displaced or having lived in another country." ~ "host",
    living_situation_description ==
      "I was internally displaced within South Sudan but have now returned to my place of origin." ~ "internally displaced",
    living_situation_description ==
    "I have returned to South Sudan from another country but am not living in my place of origin." ~ "returnee",
    living_situation_description ==
    "I have returned to South Sudan from another country and am currently living in my place of origin." ~ "returnee",
    living_situation_description ==
    "I am currently internally displaced within South Sudan and living in a location different from my original home." ~ "internally displaced",
    living_situation_description ==
    "I am a refugee and have been displaced from my home country" ~ "refugee"
    , TRUE ~ NA
  )
  )

g <- as.data.frame(prop.table(table(h$reasons_children_not_enrolled_lack_of_required_documentation_for_enrollment, h$type)), margin = 1) %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(reason = Var1)
