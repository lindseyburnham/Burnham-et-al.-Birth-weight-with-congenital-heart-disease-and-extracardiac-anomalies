
## Data from https://www.nber.org/research/data/vital-statistics-natality-birth-data

library(tidyverse)
library(data.table)
library(gtsummary)
library(ggeffects)
library(gt)
library(scales)
library(car)
library(dplyr)
library(tidyr)


dat2024<-fread("natality2024us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2024$year<-2024

dat2023<-fread("natality2023us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi","rf_gdiab" )
dat2023$year<-2023
dat2022<-fread("natality2022us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2022$year<-2022

dat2021<-fread("natality2021us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2021$year<-2021
dat2020<-fread("natality2020us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2020$year<-2020
dat2019<-fread("natality2019us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2019$year<-2019
dat2018<-fread("natality2018us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2018$year<-2018
dat2017<-fread("natality2017us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2017$year<-2017
dat2016<-fread("natality2016us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi", "rf_gdiab" )
dat2016$year<-2016
dat2015<-fread("natality2015us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt",
         "cig_1", "cig_2", "cig_3", "bmi","rf_gdiab")
dat2015$year<-2015

decade_dat<-rbind(dat2015,dat2016, dat2017, dat2018, dat2019, dat2020,
                  dat2021,dat2022,dat2023,dat2024, fill = TRUE)

decade_dat <- decade_dat |>
  mutate(
    ca_down = coalesce(ca_down, ca_downs)
  ) |>
  select(-ca_downs)


rm(dat2015,dat2016, dat2017, dat2018, dat2019, dat2020,
   dat2021,dat2022,dat2023,dat2024)

yn_to01 <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% c("Y","y","C","c","P","p", 1L)] <- 1L
  out[x %in% c("N","n",0L)]                <- 0L
  out
}


library(dplyr)
library(tidyr)
library(scales)

flow <- tibble(
  Step = character(),
  Excluded = numeric(),
  Remaining = numeric()
)

track <- function(data, step, previous_n) {
  current_n <- nrow(data)
  
  flow <<- bind_rows(
    flow,
    tibble(
      Step = step,
      Excluded = previous_n - current_n,
      Remaining = current_n
    )
  )
  
  current_n
}

n_prev <- nrow(decade_dat)

flow <- bind_rows(
  flow,
  tibble(
    Step = "All live births, 2015–2024",
    Excluded = NA_real_,
    Remaining = n_prev
  )
)

# 1. Remove missing Down syndrome status
df <- decade_dat |>
  select(-contains("uca")) |>
  filter(!is.na(ca_down), ca_down != "")

n_prev <- track(df, "Missing Down syndrome status", n_prev)

# 2. Remove unknown congenital anomaly status
anomaly_vars <- c(
  "ca_cchd", "ca_mnsb", "ca_cleft", "ca_clpal",
  "ca_hypo", "ca_down", "ca_disor", "ca_gast",
  "ca_limb", "ca_omph", "ca_cdh", "ca_anen"
)

df <- df |>
  filter(
    if_all(
      all_of(anomaly_vars),
      ~ !is.na(.x) & .x != "" & .x != "U"
    )
  )

n_prev <- track(df, "Unknown congenital anomaly status", n_prev)

# 3. Restrict gestational age
df <- df |>
  mutate(
    bw_g = as.numeric(dbwt),
    ga_wk = as.numeric(oegest_comb),
    sex = toupper(sex)
  ) |>
  filter(ga_wk %in% 34:41)

n_prev <- track(df, "Gestational age outside 34–41 weeks", n_prev)

# 4. Maternal age restriction
df <- df |>
  filter(mager > 12, mager < 50)

n_prev <- track(df, "Maternal age outside 13–49 years", n_prev)

# 5. Recode anomalies
df <- df |>
  rename(
    chd = ca_cchd,
    spina = ca_mnsb,
    cleft = ca_cleft,
    clpal = ca_clpal,
    hypo = ca_hypo,
    anen = ca_anen,
    cdh = ca_cdh,
    omph = ca_omph,
    gast = ca_gast,
    limb = ca_limb,
    disor = ca_disor,
    down = ca_down
  ) |>
  mutate(
    across(
      c(chd, spina, cleft, clpal, hypo, anen,
        cdh, omph, gast, limb, disor, down),
      yn_to01
    )
  )

# 6. Chromosomal disorders
df <- df |>
  filter(disor != 1, down != 1)

n_prev <- track(df, "Down syndrome or suspected chromosomal disorder", n_prev)

# 7. Other major congenital anomalies
df <- df |>
  filter(
    anen != 1,
    cdh != 1,
    omph != 1,
    gast != 1,
    limb != 1
  )

n_prev <- track(df, "Other major congenital anomalies", n_prev)

# 8. Congenital anomaly reporting flags
df <- df |>
  filter(!if_any(contains("F_CA"), ~ .x == 0))

n_prev <- track(df, "Congenital anomaly reporting flag exclusion", n_prev)

# 9. Missing maternal covariates
df <- df |>
  filter(
    bmi != 99.9,
    cig_1 != 99,
    cig_2 != 99,
    cig_3 != 99,
    rf_gdiab != "U"
  )

n_prev <- track(df, "Unknown maternal BMI, smoking, or diabetes status", n_prev)

# 10. Calculate birth weight z-scores
ref_stats <- df |>
  group_by(ga_wk, sex) |>
  summarise(
    bw_mean = mean(bw_g, na.rm = TRUE),
    bw_sd = sd(bw_g, na.rm = TRUE),
    .groups = "drop"
  )

df <- df |>
  left_join(ref_stats, by = c("ga_wk", "sex")) |>
  mutate(
    bw_z_internal = (bw_g - bw_mean) / bw_sd
  )

# 11. Complete-case exclusion
# Match the columns retained in the original script
df <- df |>
  select(
    chd, hypo, spina, cleft, clpal,
    bw_g, ga_wk, mager, mbstate_rec, mrace6,
    sex, cig_1, cig_2, cig_3, bmi, rf_gdiab,
    bw_mean, bw_sd, bw_z_internal
  ) |>
  mutate(
    anom_count = rowSums(across(c(spina, hypo, cleft, clpal)))
  ) |>
  na.omit()

n_prev <- track(df, "Missing data (complete-case exclusion)", n_prev)

# 12. Exclude extreme birth weight z-scores
df <- df |>
  filter(
    bw_z_internal >= -5,
    bw_z_internal <= 5
  )

n_prev <- track(df, "Birth weight z-score outside ±5", n_prev)

# Final cohort flow table

flow_display <- flow |>
  mutate(
    Excluded = comma(Excluded),
    Remaining = comma(Remaining)
  )

print(flow_display, n = Inf)

write.csv(
  flow,
  "Cohort_Inclusion_Exclusion_Counts.csv",
  row.names = FALSE
)

# Check that exclusions reconcile
stopifnot(
  sum(flow$Excluded, na.rm = TRUE) ==
    flow$Remaining[1] - nrow(df)
)

cat("\nFINAL ANALYTIC COHORT:", comma(nrow(df)), "\n")


# Restore analysis variable names and encode maternal race categorically
# Retain raw race codes in mrace6_raw for auditability.
df <- df |>
  rename(
    `Maternal age` = mager,
    `Mother's nativity` = mbstate_rec,
    `Maternal race/ethnicity` = mrace6
  ) |>
  mutate(
    mrace6_raw = `Maternal race/ethnicity`,
    `Maternal race/ethnicity` = factor(
      case_when(
        mrace6_raw == 1 ~ "White",
        mrace6_raw == 2 ~ "Black",
        mrace6_raw == 3 ~ "American Indian and Alaska Native",
        mrace6_raw == 4 ~ "Asian",
        mrace6_raw == 5 ~ "Native Hawaiian and Other Pacific Islander",
        mrace6_raw %in% c(6, 10, 41, 40, 30, 61, 51, 20) ~ "More than one race",
        TRUE ~ paste0("Other/unknown code: ", mrace6_raw)
      ),
      levels = c("White", "Black", "American Indian and Alaska Native",
                 "Asian", "Native Hawaiian and Other Pacific Islander",
                 "More than one race",
                 sort(unique(paste0("Other/unknown code: ", mrace6_raw[
                   !mrace6_raw %in% c(1, 2, 3, 4, 5, 6, 10, 41, 40, 30, 61, 51, 20)
                 ]))) )
    )
  )

rm(decade_dat)

#Data analysis

#Figure 2: distribution of birth weight in 2024-2015 cohort +/- CHD and other anomalies

df<-df|>
  mutate(anomaly = case_when(
    chd == 0 & cleft == 0 & spina == 0 & hypo == 0 & clpal == 0 ~ "No anomaly",
    chd == 1 & cleft == 0 & spina == 0 & hypo == 0 & clpal == 0 ~ "CCHD",
    chd == 0 & cleft == 1 & spina == 0 & hypo == 0 & clpal == 0 
    ~ "Cleft lip with or without cleft palate",
    chd == 0 & cleft == 0 & spina == 1 & hypo == 0 & clpal == 0 ~ "Spina bifida",
    chd == 0 & cleft == 0 & spina == 0 & hypo == 1 & clpal == 0 ~ "Hypospadias",
    chd == 0 & cleft == 0 & spina == 0 & hypo == 0 & clpal == 1 ~ "Cleft palate",
    chd == 1 & cleft == 1 & spina == 0 & hypo == 0 & clpal == 0 
    ~ "CCHD and Cleft lip with or without cleft palate",
    chd == 1 & cleft == 0 & spina == 1 & hypo == 0 & clpal == 0 
    ~ "CCHD and Spina bifida",
    chd == 1 & cleft == 0 & spina == 0 & hypo == 1 & clpal == 0 
    ~ "CCHD and Hypospadias",
    chd == 1 & cleft == 0 & spina == 0 & hypo == 0 & clpal == 1 
    ~ "CCHD and Cleft palate",
    chd == 0 & anom_count == 2 ~ "2 extracardiac anomalies and no CCHD"
  ))

# Figure-specific sample counts (not additional cohort exclusions)
figure_counts <- tibble(
  Analysis = c(
    "Full analytic cohort",
    "Figure 2: classified anomaly groups",
    "Figure 3: defined CHD/anomaly groups"
  ),
  N = c(
    nrow(df),
    sum(!is.na(df$anomaly)),
    sum(df$chd %in% 0:1 & df$anom_count %in% 0:2)
  )
)
print(figure_counts)
write.csv(figure_counts, "Figure_Analysis_Counts.csv", row.names = FALSE)

#doing by mean so can do anova
sum <- df |>
  dplyr::group_by(anomaly) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(bw_z_internal, na.rm = TRUE),
    se = sd(bw_z_internal, na.rm = TRUE) / sqrt(n),
    lo_ci = mean - 1.96 * se,
    hi_ci = mean + 1.96 * se,
    .groups = "drop"
  )
sum<-sum|>
  na.omit()
sum <- sum |>
  mutate(anomaly = factor(anomaly,
                          levels = 
                            c("CCHD and Cleft palate", 
                              "CCHD and Hypospadias",
                              "CCHD and Spina bifida",
                              "CCHD and Cleft lip with or without cleft palate",
                              "Hypospadias",
                              "Spina bifida",
                              "Cleft lip with or without cleft palate", 
                              "CCHD", "No anomaly")))|>
  arrange(anomaly) |>
  mutate(anomaly_label = paste0(anomaly, "\n(n=", comma(n), ")"))|>
  na.omit()

sum |>
  ggplot() +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = 4.5,
    alpha = 0.02, color = "grey"
  ) +
  geom_point(aes(x = mean, y = anomaly)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbarh(aes(y = anomaly, xmin = lo_ci, xmax = hi_ci), 
                 lineend = "butt",
                 width = 0.4) +
  scale_y_discrete(labels = sum$anomaly_label) +
  theme_classic() +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(
    y = "Anomalies Present",
    x = "Mean Birth Weight z-score") 

ggsave("Figure 2.png", units = "in", width = 8, height = 4)
# Exact one-way ANOVA and Tukey-Kramer tests from group summaries.
# Avoids fitting an enormous aov() model to tens of millions of rows.
summary_anova_tukey <- function(group_summary) {
  group_summary <- group_summary |>
    dplyr::filter(!is.na(group), n > 0, !is.na(mean),
                  is.finite(variance), n > 1)
  N <- sum(group_summary$n)
  k <- nrow(group_summary)
  if (k < 2) stop("At least two groups are required")
  overall <- weighted.mean(group_summary$mean, group_summary$n)
  ss_between <- sum(group_summary$n * (group_summary$mean - overall)^2)
  ss_within <- sum((group_summary$n - 1) * group_summary$variance)
  df_between <- k - 1
  df_within <- N - k
  mse <- ss_within / df_within
  f_stat <- (ss_between / df_between) / mse
  omnibus <- tibble::tibble(
    n = N, groups = k, df_between = df_between,
    df_within = df_within, F_statistic = f_stat,
    p_value = stats::pf(f_stat, df_between, df_within, lower.tail = FALSE)
  )
  idx <- utils::combn(seq_len(k), 2)
  critical <- stats::qtukey(0.95, k, df_within)
  tukey <- purrr::map_dfr(seq_len(ncol(idx)), function(i) {
    a <- group_summary[idx[1, i], ]
    b <- group_summary[idx[2, i], ]
    difference <- a$mean - b$mean
    se <- sqrt(mse / 2 * (1 / a$n + 1 / b$n))
    q <- abs(difference) / se
    tibble::tibble(
      comparison = paste(a$group, "-", b$group),
      difference = difference,
      lower_95CI = difference - critical * se,
      upper_95CI = difference + critical * se,
      p_adjusted = stats::ptukey(q, k, df_within, lower.tail = FALSE)
    )
  })
  list(anova = omnibus, tukey = tukey)
}

# Figure 2: all classified groups, including the 2-anomaly/no-CCHD group.
figure2_groups <- df |>
  dplyr::filter(!is.na(anomaly)) |>
  dplyr::group_by(anomaly) |>
  dplyr::summarise(n = dplyr::n(), mean = mean(bw_z_internal),
                   variance = var(bw_z_internal), .groups = "drop") |>
  dplyr::rename(group = anomaly)
figure2_tests <- summary_anova_tukey(figure2_groups)
print(figure2_tests$anova)
print(figure2_tests$tukey, n = Inf)
write.csv(figure2_tests$anova, "Figure2_ANOVA.csv", row.names = FALSE)
write.csv(figure2_tests$tukey, "Figure2_Tukey_Comparisons.csv", row.names = FALSE)

# Count infants with CCHD and 2 extracardiac anomalies
n_cchd_two <- sum(df$chd == 1 & df$anom_count == 2, na.rm = TRUE)

cat(
  "\nCCHD + 2 extracardiac anomalies: n =",
  n_cchd_two, "\n"
)

##dose-dependent plot (just by anom count)
chd_anom <- df |>
  dplyr::filter(anom_count %in% 0:2,!(chd == 1 & anom_count == 2)) |>
  dplyr::mutate(CHD_anom = case_when(
    chd == 1 & anom_count == 0 ~ "CCHD",
    chd == 1 & anom_count == 1 ~ "CCHD and 1 extracardiac anomaly",
    
    chd == 0 & anom_count == 0 ~ "No anomalies",
    chd == 0 & anom_count == 1 ~ "1 extracardiac anomaly",
    chd == 0 & anom_count == 2 ~ "2 extracardiac anomalies"
  )) |>
  dplyr::filter(!is.na(CHD_anom)) |>
  dplyr::group_by(CHD_anom) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(bw_z_internal, na.rm = TRUE),
    se = sd(bw_z_internal, na.rm = TRUE) / sqrt(n),
    lo_ci = mean - 1.96 * se,
    hi_ci = mean + 1.96 * se,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    CHD_anom = factor(
      CHD_anom,
      levels = c(
        "No anomalies",
        "CCHD",
        "1 extracardiac anomaly",
        "2 extracardiac anomalies",
        "CCHD and 1 extracardiac anomaly"
  
      )
    )
  ) |>
  dplyr::arrange(CHD_anom) |>
  dplyr::mutate(
    anomaly_label = paste0(CHD_anom, "\n(n=", scales::comma(n), ")")
  )

chd_anom<-chd_anom|>
  na.omit()

chd_anom<-chd_anom|>
  mutate(CHD_anom = factor(CHD_anom, levels = rev(levels(CHD_anom))))
chd_anom|>
  ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1.5,
           color = "grey", alpha = 0.03) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_point(aes(y = CHD_anom, x = mean)) +
  geom_errorbarh(aes(y = CHD_anom, xmin = lo_ci, xmax = hi_ci), width = 0.2) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(
    y = "Number of Anomalies Present",
    x = "Mean Birth Weight z-score"
  ) 

ggsave("Figure 3.png", units = "in", width = 8, height = 4)

# Figure 3 ANOVA and Tukey-Kramer: five CCHD/anomaly-count groups.
figure3_groups <- df |>
  dplyr::filter(anom_count %in% 0:2, chd %in% 0:1, !(chd == 1 & anom_count == 2)) |>
  dplyr::group_by(chd, anom_count) |>
  dplyr::summarise(n = dplyr::n(), mean = mean(bw_z_internal),
                   variance = var(bw_z_internal), .groups = "drop") |>
  dplyr::mutate(group = paste0("CCHD=", chd, ", anomalies=", anom_count)) |>
  dplyr::select(group, n, mean, variance)
figure3_tests <- summary_anova_tukey(figure3_groups)
print(figure3_tests$anova)
print(figure3_tests$tukey, n = Inf)
write.csv(figure3_tests$anova, "Figure3_ANOVA.csv", row.names = FALSE)
write.csv(figure3_tests$tukey, "Figure3_Tukey_Comparisons.csv", row.names = FALSE)

#Chi square for if CHD is associated with extracardiac anomalies
count(df, anom_count)
chi<-df|>
  select(anom_count, chd)|>
  mutate(anom_status = case_when(
    anom_count >= 1 ~ "Anomalies",
    anom_count == 0 ~ "No anomalies")
  )|>
  count(chd, anom_status) |> 
  pivot_wider(names_from = anom_status, values_from = n, values_fill = 0)

chi_table<-chi|>
  column_to_rownames("chd")|>
  as.matrix()

ctest<-chisq.test(chi_table)
print(ctest)

#btwn anom count, chd, and bw
df_model <- df |>
  filter(anom_count != 3) |>
  rename(
    Maternal_age = `Maternal age`,
    mr_e = `Maternal race/ethnicity`
  ) |>
  mutate(
    av_cig = (cig_1 + cig_2 + cig_3) / 3,
    rf_gdiab = factor(rf_gdiab, levels = c("N", "Y"))
  )

n_excluded_fig3 <- sum(
  df$anom_count >= 3 |
    (df$chd == 1 & df$anom_count >= 2),
  na.rm = TRUE
)

cat("Total unique infants excluded from Figure 3:",
    n_excluded_fig3, "\n")

# Maternal race is a factor (categorical), with White as reference.
stopifnot(is.factor(df_model$mr_e))

# Verify the model population; lm() silently omits rows with missing covariates.
model_vars <- c("bw_z_internal", "chd", "anom_count", "av_cig",
                "bmi", "rf_gdiab", "Maternal_age", "mr_e")
model_complete <- stats::complete.cases(df_model[, ..model_vars])
cat("Regression input rows:", nrow(df_model), "\n")
cat("Regression complete-case rows:", sum(model_complete), "\n")
if (sum(model_complete) != nrow(df_model)) {
  warning("Some rows will be omitted by lm(); inspect missing model covariates")
  print(vapply(df_model[, model_vars], function(x) sum(is.na(x)), numeric(1)))
}


int_chd_anom <- lm(
  bw_z_internal ~ chd * anom_count + av_cig + bmi + rf_gdiab +
    Maternal_age + mr_e,
  data = df_model
)

summary(int_chd_anom)
stopifnot(nobs(int_chd_anom) == sum(model_complete))

mod_vif<-lm(bw_z_internal ~ chd + anom_count + av_cig + bmi + rf_gdiab +
              Maternal_age + mr_e,
            data = df_model)
vif(mod_vif)

#btwn anom count, chd, and bw
set.seed(1)
anoms<-ggpredict(int_chd_anom,
                 terms = c("chd", "anom_count"))
anoms<-anoms|>
  mutate(`Cyanotic Congenital Heart Disease`  = case_when(
    x == 0 ~ "Not Present",
    x == 1 ~ "Present"
  ))|>
  na.omit()
anoms|>
  ggplot() +
  geom_line(aes(x=as.numeric(as.character(group)), y = predicted,
                color = `Cyanotic Congenital Heart Disease`,
                group = `Cyanotic Congenital Heart Disease`))+
  geom_point(aes(x = as.numeric(as.character(group)), y = predicted,
                 color = `Cyanotic Congenital Heart Disease`,
                 group = `Cyanotic Congenital Heart Disease`)) + 
  geom_ribbon(
    aes(x = as.numeric(as.character(group)),
        ymin = conf.low,
        ymax = conf.high,
        fill = `Cyanotic Congenital Heart Disease`,
        group = `Cyanotic Congenital Heart Disease`),
    alpha = 0.2)  +
  theme_classic() +
  coord_cartesian(ylim = c(-1.5, 0)) +
  scale_x_continuous(breaks = 0:2,limits = c(0, 2.1), expand = c(0, 0))+
  labs(
    x = "Number of extracardiac anomalies",
    y = "Birthweight z-score"
  )

# Memory-efficient regression table
library(broom)
library(dplyr)
library(gt)

int_table <- broom::tidy(
  int_chd_anom,
  conf.int = FALSE
) |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `Beta (95% CI)` = sprintf(
      "%.3f (%.3f to %.3f)",
      estimate, conf.low, conf.high
    ),
    `P value` = format.pval(p.value, digits = 3, eps = 0.001)
  ) |>
  select(
    Variable = term,
    `Beta (95% CI)`,
    `P value`
  )

int_table |>
  gt() |>
  tab_header(
    title = "Adjusted Linear Regression",
    subtitle = "Outcome: Birth Weight z-score"
  )

#sensitivity analysis 
library(sensemakr)
sens <- sensemakr(model = int_chd_anom,
                  treatment = "chd",
                  benchmark_covariates = c("bmi", "Maternal_age", "av_cig"),
                  kd = 1:3,
                  ky = 1:3)
summary(sens)
plot(sens)
sens_anom <- sensemakr(model = int_chd_anom,
                  treatment = "anom_count",
                  benchmark_covariates = c("bmi", "Maternal_age", "av_cig"),
                  kd = 1:3,
                  ky = 1:3)
summary(sens_anom)

sens_int <- sensemakr(model = int_chd_anom,
                       treatment = "chd:anom_count",
                       benchmark_covariates = c("bmi", "Maternal_age", "av_cig"),
                       kd = 1:3,
                       ky = 1:3)
summary(sens_int)

#Table 1:  baseline characteristics of 2024-2015 birth certificate cohort
df_table <- df

df_table <- df_table |>
  rename(
    `Meningomyelocele/Spina bifida` = spina,
    `Cleft Lip with or without Cleft Palate` = cleft,
    `Cleft Palate alone` = clpal,
    `Hypospadias` = hypo,
    `Gestational age (weeks)` = ga_wk,
    `Birth weight (grams)` = bw_g,
    `Birth weight z-score` = bw_z_internal
  )
anom_vars <- c("Meningomyelocele/Spina bifida",
               "Cleft Lip with or without Cleft Palate",
               "Cleft Palate alone", 
               "Hypospadias")
df_table<-df_table|>
  mutate(
    across(all_of(anom_vars),
           ~ case_when(
             .x %in% c(1, "1") ~ "Present",
             .x %in% c(0, "0") ~ "Absent",
             TRUE ~ NA_character_ 
           )))

df_table<-df_table|>
  select(`Maternal age`, 
         sex,
         `Gestational age (weeks)`,
         `Birth weight (grams)`,
         `Birth weight z-score`,
         `Maternal race/ethnicity`,
         `Meningomyelocele/Spina bifida`,
         `Cleft Lip with or without Cleft Palate`,
         `Cleft Palate alone`,
         `Hypospadias`,
         anom_count,
         chd)
df_table <- df_table |> 
  mutate(`Gestational age (weeks)` = as.numeric(`Gestational age (weeks)`))

table<-df_table|>
  tbl_summary(include = c(`Maternal age`, 
                          sex, 
                          `Gestational age (weeks)`,
                          `Birth weight (grams)`,
                          `Birth weight z-score`,
                          `Maternal race/ethnicity`,
                          `Meningomyelocele/Spina bifida`,
                          `Cleft Lip with or without Cleft Palate`,
                          `Cleft Palate alone`,
                          `Hypospadias`,
                          anom_count),
              type = list(`Gestational age (weeks)` ~ "continuous"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              by = chd)
print(table)

####citations
citation("tidyverse")
citation("data.table")
citation("gtsummary")
citation("ggeffects")
citation("gt")
citation("scales")
citation("sensemakr")


### output data

# Figure 2: Individual anomaly categories
figure2_table <- df |>
  dplyr::filter(!is.na(anomaly)) |>
  dplyr::group_by(anomaly) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(bw_z_internal),
    sd = sd(bw_z_internal),
    se = sd / sqrt(n),
    lower_95CI = mean - qt(0.975, df = n - 1) * se,
    upper_95CI = mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `Mean (95% CI)` = sprintf(
      "%.3f (%.3f to %.3f)",
      mean, lower_95CI, upper_95CI
    )
  )

print(figure2_table, n = Inf)

write.csv(
  figure2_table,
  "Figure2_Means_95CI.csv",
  row.names = FALSE
)


# Figure 3: CCHD and extracardiac anomaly count
# Includes CCHD + 2 anomalies

figure3_table <- df |>
  dplyr::filter(anom_count %in% 0:2, chd %in% 0:1) |>
  dplyr::mutate(
    CHD_anom = dplyr::case_when(
      chd == 0 & anom_count == 0 ~ "No anomalies",
      chd == 0 & anom_count == 1 ~ "1 extracardiac anomaly",
      chd == 0 & anom_count == 2 ~ "2 extracardiac anomalies",
      chd == 1 & anom_count == 0 ~ "CCHD",
      chd == 1 & anom_count == 1 ~ "CCHD + 1 extracardiac anomaly",
      chd == 1 & anom_count == 2 ~ "CCHD + 2 extracardiac anomalies"
    )
  ) |>
  dplyr::group_by(CHD_anom, chd, anom_count) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(bw_z_internal),
    sd = sd(bw_z_internal),
    se = sd / sqrt(n),
    lower_95CI = mean - qt(0.975, df = n - 1) * se,
    upper_95CI = mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `Mean (95% CI)` = sprintf(
      "%.3f (%.3f to %.3f)",
      mean, lower_95CI, upper_95CI
    )
  ) |>
  dplyr::arrange(chd, anom_count)

print(figure3_table, n = Inf)

write.csv(
  figure3_table,
  "Figure3_Means_95CI.csv",
  row.names = FALSE
)

=
# REGRESSION MODEL RESULTS

library(broom)

# Function to extract all regression coefficients
# Function to extract regression coefficients without
# memory-intensive confidence interval calculations

extract_regression <- function(model, model_name) {
  
  broom::tidy(
    model,
    conf.int = FALSE
  ) |>
    dplyr::mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      Model = model_name,
      `Estimate (95% CI)` = sprintf(
        "%.4f (%.4f to %.4f)",
        estimate, conf.low, conf.high
      )
    ) |>
    dplyr::select(
      Model,
      term,
      estimate,
      std.error,
      conf.low,
      conf.high,
      statistic,
      p.value,
      `Estimate (95% CI)`
    )
}
# Coefficient results
regression_results <- dplyr::bind_rows(
  extract_regression(
    int_chd_anom,
    "CCHD x anomaly count interaction model"
  ),
  extract_regression(
    mod_vif,
    "Additive model"
  )
)

print(regression_results, n = Inf)

write.csv(
  regression_results,
  "Regression_All_Coefficients_95CI.csv",
  row.names = FALSE
)

# Model-level statistics
model_statistics <- dplyr::bind_rows(
  broom::glance(int_chd_anom) |>
    dplyr::mutate(Model = "Interaction model"),
  broom::glance(mod_vif) |>
    dplyr::mutate(Model = "Additive model")
)

print(model_statistics, width = Inf)

write.csv(
  model_statistics,
  "Regression_Model_Statistics.csv",
  row.names = FALSE
)

# Full model summaries in a text file
sink("Regression_Full_Summaries.txt")

cat("INTERACTION MODEL\n")
print(summary(int_chd_anom))

cat("\n\nADDITIVE MODEL\n")
print(summary(mod_vif))

cat("\n\nINTERACTION MODEL ANOVA\n")
print(anova(int_chd_anom))

cat("\n\nADDITIVE MODEL ANOVA\n")
print(anova(mod_vif))

sink()

# Figure 2 and Figure 3 ANOVA and Tukey results were calculated and
# exported above using summary_anova_tukey(), avoiding full-data aov().
