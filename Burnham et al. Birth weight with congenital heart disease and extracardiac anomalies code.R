
## Data from https://www.nber.org/research/data/vital-statistics-natality-birth-data

## 0. Libraries
library(tidyverse)
library(data.table)
library(gtsummary)
library(ggeffects)
library(gt)
library(scales)

## load in data
setwd("~/Desktop/Hays lab/birth certificate chd lbw")
unzip("natality2024us.csv.zip", list = T)
dat2024<-fread(cmd = "unzip -p natality2024us.csv.zip natality2024us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2024$year<-2024

dat2023<-fread(cmd = "unzip -p natality2023us.csv.zip natality2023us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2023$year<-2023
dat2022<-fread(cmd = "unzip -p natality2022us.csv.zip natality2022us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2022$year<-2022
dat2021<-fread(cmd = "unzip -p natality2021us.csv.zip natality2021us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2021$year<-2021
dat2020<-fread(cmd = "unzip -p natality2020us.csv.zip natality2020us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2020$year<-2020
dat2019<-fread(cmd = "unzip -p natality2019us.csv.zip natality2019us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2019$year<-2019
dat2018<-fread(cmd = "unzip -p natality2018us.csv.zip natality2018us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2018$year<-2018
dat2017<-fread(cmd = "unzip -p natality2017us.csv.zip natality2017us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2017$year<-2017
dat2016<-fread(cmd = "unzip -p natality2016us.csv.zip natality2016us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2016$year<-2016
dat2015<-fread(cmd = "unzip -p natality2015us.csv.zip natality2015us.csv")|>
  select(contains("CA_"),
         "mager",
         "mbstate_rec", 
         "mrace6", 
         "sex","oegest_comb","dbwt")
dat2015$year<-2015

decade_dat<-rbind(dat2015,dat2016, dat2017, dat2018, dat2019, dat2020,
                  dat2021,dat2022,dat2023,dat2024, fill = TRUE)

rm(dat2015,dat2016, dat2017, dat2018, dat2019, dat2020,
   dat2021,dat2022,dat2023,dat2024)

###############################
# Helper to recode anomaly flags
###############################
yn_to01 <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% c("Y","y","C","c","P","p", 1L)] <- 1L
  out[x %in% c("N","n",0L)]                <- 0L
  out
}
###############################
#Filter to 34–41 weeks, clean BW and sex
###############################
df <- decade_dat |>
  select(-contains("uca"))|>
  mutate(ca_down = coalesce(ca_downs, ca_down))|>
  select(-ca_downs)|>
  filter(ca_down != "")

df<-df|>
  rename(
    "Maternal age" = "mager",
    "Mother's nativity" = "mbstate_rec",
    "Maternal race/ethnicity" = "mrace6")|>
  filter(
    ca_cchd != "U",
    ca_mnsb != "U",
    ca_cleft != "U",
    ca_clpal != "U",
    ca_hypo != "U",
    ca_down != "U",
    ca_disor != "U",
    ca_gast !="U",
    ca_limb !="U",
    ca_omph !="U",
    ca_cdh !="U",
    ca_anen !="U")
df<-df|>
  mutate(
    bw_g = as.numeric(dbwt),
    ga_wk = as.numeric(oegest_comb),
    sex   = toupper(sex))|>
  filter(ga_wk %in% 34:41)

df<-df|>
  filter(`Maternal age`<50,`Maternal age`>12)

df<-df|>
  rename(
    chd= ca_cchd,
    spina = ca_mnsb,
    cleft= ca_cleft,
    clpal= ca_clpal,
    hypo= ca_hypo,
    anen= ca_anen,
    cdh= ca_cdh,
    omph= ca_omph,
    gast = ca_gast,
    limb = ca_limb,
    disor = ca_disor,
    down = ca_down
  )

df<-df|>
  mutate(
    chd     = yn_to01(chd),
    spina   = yn_to01(spina),
    cleft   = yn_to01(cleft),
    clpal   = yn_to01(clpal),
    hypo    = yn_to01(hypo),
    anen  = yn_to01(anen),
    cdh= yn_to01(cdh),
    omph= yn_to01(omph),
    gast = yn_to01(gast),
    limb = yn_to01(limb),
    disor = yn_to01(disor),
    down = yn_to01(down))
    
df<-df|>
  filter(
    disor  != "1",
    down != "1"
  )

df<-df|>
    filter(anen  != 1,
    cdh != 1,
    omph != 1,
    gast  != 1,
    limb  != 1)

df<-df|>
  filter(!if_any(contains("F_CA"), ~ .x == 0))

###############################
# Compute internal mean/SD by (ga_wk, sex)
###############################

ref_stats <- df %>%
  dplyr::group_by(ga_wk, sex) %>%
  dplyr::summarise(
    bw_mean = mean(bw_g, na.rm = TRUE),
    bw_sd   = sd(bw_g,   na.rm = TRUE),
    .groups = "drop"
  )

# Join means/SDs to each row and get z-score
df<-df|>
  select(chd,hypo,spina,cleft,clpal,bw_g,ga_wk,"Maternal age",
         "Mother's nativity","Maternal race/ethnicity",sex)
df <- df %>%
  left_join(ref_stats, by = c("ga_wk","sex")) %>%
  mutate(
    bw_z_internal = (bw_g - bw_mean) / bw_sd
  )

df<-df|>
  mutate(
    anom_count = rowSums(across(c(spina, hypo, cleft, clpal))))

df<-na.omit(df)
df<-df|>
  filter(
    bw_z_internal<=5,
    bw_z_internal>=-5
  )

rm(decade_dat)


#Data analysis

#Table 1:  baseline characteristics of 2024-2015 birth certificate cohort
df_table <- df |> 
  mutate(`Maternal race/ethnicity` = case_when(
    `Maternal race/ethnicity` == 1 ~ "White",
    `Maternal race/ethnicity` == 2 ~ "Black",
    `Maternal race/ethnicity` == 3 ~ "American Indian and Alaska Native",
    `Maternal race/ethnicity` == 4 ~ "Asian",
    `Maternal race/ethnicity` == 5 ~ "Native Hawaiian and Other Pacific Islander",
    `Maternal race/ethnicity` %in% c(6, 10, 41, 40, 30, 61, 51, 20)~ "More than one race",
    TRUE ~ as.character(`Maternal race/ethnicity`) 
  ))

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

#Figure 2: distribution of birth weight in 2024-2015 cohort +/- CHD and other anomalies
#reloading data so that don't run out of memory

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

#doing by mean so can do anova
sum<-df|>
  group_by(anomaly)|>
  summarize(
    n = n(),
    mean = mean(bw_z_internal, na.rm = TRUE),
    se = sd(bw_z_internal, na.rm = TRUE)/sqrt(n),
    lo_ci = mean - 1.96* se,
    hi_ci = mean + 1.96*se,
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
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 4.5),
            alpha = 0.02, color = "grey") +
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
anova<-aov(bw_z_internal ~ anomaly,
           data = df|>
             select(anomaly, bw_z_internal))
TukeyHSD(anova)

##dose-dependent plot (just by anom count)
chd_anom<-df|>
  filter(anom_count != 3)|>
  mutate(CHD_anom = case_when(
    chd == 1 & anom_count == 0 ~ "CCHD",
    chd == 1 & anom_count == 1 ~ "CCHD and 1 extracardiac anomaly",
    chd == 0 & anom_count == 0 ~ "No anomalies",
    chd == 0 & anom_count == 1 ~ "1 extracardiac anomaly",
    chd == 0 & anom_count == 2 ~ "2 extracardiac anomalies"))|>
  group_by(CHD_anom)|>
  summarize(
    n = n(),
    mean = mean(bw_z_internal, na.rm = TRUE),
    se = sd(bw_z_internal, na.rm = TRUE)/sqrt(n),
    lo_ci = mean - 1.96* se,
    hi_ci = mean + 1.96*se,
    .groups = "drop"
  )|>
  mutate(CHD_anom = factor(CHD_anom,
                           levels = c("No anomalies", "CCHD", "1 extracardiac anomaly",
                                      "2 extracardiac anomalies",
                                      "CCHD and 1 extracardiac anomaly"))) |>
  arrange(CHD_anom) |>
  mutate(anomaly_label = paste0(CHD_anom, "\n(n=", comma(n), ")"))
chd_anom<-chd_anom|>
  na.omit()

chd_anom<-chd_anom|>
  mutate(CHD_anom = factor(CHD_anom, levels = rev(levels(CHD_anom))))
chd_anom|>
ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1.5),
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

#anova 
anova <- df |>
  filter(anom_count != 3) |>
  mutate(CHD_anom = case_when(
    chd == 1 & anom_count == 0 ~ "CCHD",
    chd == 1 & anom_count == 1 ~ "CCHD and 1 anomaly",
    chd == 0 & anom_count == 0 ~ "No anomalies",
    chd == 0 & anom_count == 1 ~ "1 anomaly",
    chd == 0 & anom_count == 2 ~ "2 anomalies"))

anova_dose <- aov(bw_z_internal ~ CHD_anom, data = anova)
TukeyHSD(anova_dose)
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

##LM model:effect of CHD and other anomalies on bw
interaction_neutral<-lm(
  bw_z_internal ~ (chd) * (.),
  data = df|>
    dplyr::select(bw_z_internal, chd, spina, hypo, cleft, clpal)
)
summary(interaction_neutral)
df_model<-df|>
  filter(
    anom_count != 3
  )
#btwn anom count, chd, and bw
int_chd_anom<-lm(
  bw_z_internal ~ chd * anom_count,
  data = df_model|>
    dplyr::select(bw_z_internal, chd, anom_count)
)

summary(int_chd_anom)



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

int_table<-tbl_regression(int_chd_anom)
as_gt(int_table)

####citations
citation("tidyverse")
citation("data.table")
citation("gtsummary")
citation("ggeffects")
citation("gt")
citation("scales")
