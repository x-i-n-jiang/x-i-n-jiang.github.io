rm(list = ls())

library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(stargazer)

data <- read_csv(
  "long_data1.csv",
)

##############################################################################################

# Generate Table 3

##############################################################################################

data <- data %>%
  dplyr::mutate(
    information_status = dplyr::case_when(
      know != "unknown"  ~ "Fully informed",
      know == "unknown" & Display > 0 ~ "Partially informed",
      know == "unknown" & (Display == 0 | is.na(Display)) ~ "Uninformed",
      TRUE ~ NA_character_
    ),
    Condition_label = dplyr::case_when(
      Condition == 0 ~ "SU",
      Condition == 1 ~ "SA",
      TRUE ~ NA_character_
    ),
  )

# Panel A: SU, SA, and pooled information-acquisition status.

summarise_panel_a <- function(df, grouping_variables) {
  df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_variables))
    ) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Mean_RT = mean(observed_RT, na.rm = TRUE),
      SD = stats::sd(observed_RT, na.rm = TRUE),
      Median_RT = stats::median(observed_RT, na.rm = TRUE),
      Q1 = as.numeric(
        stats::quantile(observed_RT, 0.25, na.rm = TRUE)
      ),
      Q3 = as.numeric(
        stats::quantile(observed_RT, 0.75, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

panel_a <- summarise_panel_a(
  data,
  c("Condition_label", "information_status")
) %>%
  dplyr::group_by(Condition_label) %>%
  dplyr::mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Condition = Condition_label,
    Classification = information_status,
    N,
    Percent,
    Mean_RT,
    SD,
    Median_RT,
    Q1,
    Q3
  )

# Panel B: Message classification

panel_b <- summarise_panel_a(
  data,
  c("Condition_label", "message")
) %>%
  dplyr::group_by(Condition_label) %>%
  dplyr::mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Condition = factor(
      Condition_label,
      levels = c("SU", "SA")
    ),
    Classification = factor(
      message,
      levels = c(
        "honest_y",
        "honest_n",
        "deceptive_y",
        "deceptive_n",
        "nfi_y"
      )
    )
  ) %>%
  dplyr::arrange(
    Condition,
    Classification
  ) %>%
  dplyr::select(
    Condition,
    Classification,
    N,
    Percent,
    Mean_RT,
    SD,
    Median_RT,
    Q1,
    Q3
  )

##################### Related Statistics

# page 15, fisher test of fully informed rates in two Conditions
fisher.test(
  table(
    data$Condition,
    data$information_status == "Fully informed"
  ),
  alternative = "two.sided"
)


# page 16, ratio of clicking 6 buttons among informed messages
table(data$Display[data$information_status=="Fully informed"])

############################################################################################################

# Generate Figure 1

############################################################################################################

library(dplyr)
library(ggplot2)

# Keep only the relevant variables
df_clean <- data %>%
  dplyr::select(observed_RT, round, message, Condition)

df_clean_SU <- df_clean %>% 
  filter(Condition==0)

df_clean_SA <- df_clean %>% 
  filter(Condition==1)

# read the number of each type of message
table(df_clean_SU$message)
table(df_clean_SA$message)

# Calculate median RT by message and round
data_by_message_SU <- df_clean_SU %>%
  dplyr::group_by(round, message) %>%
  dplyr::summarize(median_RT = median(observed_RT, na.rm = TRUE), .groups = 'drop')

data_by_message_SA <- df_clean_SA %>%
  dplyr::group_by(round, message) %>%
  dplyr::summarize(median_RT = median(observed_RT, na.rm = TRUE), .groups = 'drop')


# Calculate overall median RT for each round (across all messages)
data_overall_SU <- df_clean_SU %>%
  dplyr::group_by(round) %>%
  dplyr::summarize(median_RT = median(observed_RT, na.rm = TRUE), .groups = 'drop') %>%
  mutate(message = "Overall")  # Add a label for plotting

data_overall_SA <- df_clean_SA %>%
  dplyr::group_by(round) %>%
  dplyr::summarize(median_RT = median(observed_RT, na.rm = TRUE), .groups = 'drop') %>%
  mutate(message = "Overall")  # Add a label for plotting


# Combine the two datasets
data_combined_SU <- bind_rows(data_by_message_SU, data_overall_SU)
data_combined_SA <- bind_rows(data_by_message_SA, data_overall_SA)


# Plot median, SU
# the number of observations is consistent with the number in df_clean_SU
ggplot(data_combined_SU, aes(x = round, y = median_RT, color = message)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +  # <-- ensures round 1 to 10 are shown
  labs(x = "Round", y = "Median RT"#, title = "Median RT by Message and Round (with Overall)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Overall" = "black",
                                "deceptive_n" = "brown",
                                "deceptive_y" = "red",
                                "honest_n" = "blue",
                                "honest_y" = "orange",
                                "nfi_y" = "grey"),
                     breaks = c(
                       "deceptive_n",
                       "deceptive_y",
                       "honest_n",
                       "honest_y",
                       "nfi_y",
                       "Overall"
                     ),
                     labels = c( "deceptive n (1)",
                                 "deceptive y (51)",
                                 "honest n (138)",
                                 "honest y (86)",
                                 "not fully\ninformed y (24)",
                                 "overall (300)"))

# SA
# the number of observations is consistent with the number in df_clean_SA
ggplot(data_combined_SA, aes(x = round, y = median_RT, color = message)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +  # <-- ensures round 1 to 10 are shown
  labs(x = "Round", y = "Median RT"#, title = "Median RT by Message and Round (with Overall)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Overall" = "black",
                                "deceptive_n" = "brown",
                                "deceptive_y" = "red",
                                "honest_n" = "blue",
                                "honest_y" = "orange",
                                "nfi_y" = "grey"),
                     breaks = c(
                       "deceptive_n",
                       "deceptive_y",
                       "honest_n",
                       "honest_y",
                       "nfi_y",
                       "Overall"
                     ),
                     labels = c("Overall" = "overall (320)",
                                "deceptive_n" = "deceptive n (3)",
                                "deceptive_y" = "deceptive y (45)",
                                "honest_n" = "honest n (160)",
                                "honest_y" = "honest y (87)",
                                "nfi_y" = "not fully\ninformed y (25)"))


#############################################################################################################

# Generate Figure 2

##############################################################################################################

# select subjects who both send honest y and deceptive y at the same time in SU
both_y0 <- data %>% filter(Condition==0) %>%
  filter(message %in% c("honest_y", "deceptive_y") ) %>%
  group_by(index) %>%
  filter(all(c("honest_y", "deceptive_y") %in% message))  # 17 subjects

# calculate the mean of each type of message in SU
agg_data0 <- aggregate(observed_RT~ message + index, data = subset(both_y0), FUN = mean) 
agg_data0 <- agg_data0 %>%
  pivot_wider(names_from = message, values_from = observed_RT) 

# Define the x-range for the band
x_min <- -2
x_max <- 37
agg_data0$intercept = agg_data0$honest_y-agg_data0$deceptive_y

# Create a data frame for the ribbon
band_data <- data.frame(
  x = c(x_min, x_max),
  ymin = c(x_min + min(agg_data0$intercept), x_max + min(agg_data0$intercept)),  # line with intercept -5
  ymax = c(x_min + max(agg_data0$intercept), x_max + max(agg_data0$intercept))   # line with intercept +5
)

# draw the plot for SU
ggplot() +
  # Shaded ribbon between two lines
  geom_ribbon(data = band_data, aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "gray80", alpha = 0.5) +
  geom_point(data=agg_data0, aes(x = deceptive_y, y = honest_y), color="blue") +
  labs(
    #title = "Relationship between RT for genuine and deceptive Y",
    x = "mean RT for deceptive y",
    y = "mean RT for honest y"
  ) +
  coord_cartesian(xlim = c(0, 35), ylim = c(0,35))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  guides(color = guide_legend(override.aes = list(size = 5)))+  # Adjust legend size
  scale_color_identity(guide = FALSE)+ # Remove the legend for color
  theme(legend.position = "none")  # Remove the legend


# select subjects who both send honest y and deceptive y at the same time in SA
both_y1 <- data %>% filter(Condition==1) %>%
  filter(message %in% c("honest_y", "deceptive_y") ) %>%
  group_by(index) %>%
  filter(all(c("honest_y", "deceptive_y") %in% message))  # 14 subjects

# calculate the mean of each type of message in SA
agg_data1 <- aggregate(observed_RT~ message + index, data = subset(both_y1), FUN = mean) 
agg_data1 <- agg_data1 %>%
  pivot_wider(names_from = message, values_from = observed_RT) 

# Define the x-range for the band
x_min <- -2
x_max <- 37
agg_data1$intercept = agg_data1$honest_y-agg_data1$deceptive_y

# Create a data frame for the ribbon
band_data1 <- data.frame(
  x = c(x_min, x_max),
  ymin = c(x_min + min(agg_data1$intercept), x_max + min(agg_data1$intercept)),  # line with intercept -5
  ymax = c(x_min + max(agg_data1$intercept), x_max + max(agg_data1$intercept))   # line with intercept +5
)

# draw the plot for SA
ggplot() +
  # Shaded ribbon between two lines
  geom_ribbon(data = band_data1, aes(x = x, ymin = ymin, ymax = ymax), 
              fill = "gray80", alpha = 0.5) +
  geom_point(data=agg_data1, aes(x = deceptive_y, y = honest_y), color="blue") +
  labs(
    #title = "Relationship between RT for genuine and deceptive Y",
    x = "mean RT for deceptive y",
    y = "mean RT for honest y"
  ) +
  coord_cartesian(xlim = c(0, 35), ylim = c(0,35))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  guides(color = guide_legend(override.aes = list(size = 5)))+  # Adjust legend size
  scale_color_identity(guide = FALSE)+ # Remove the legend for color
  theme(legend.position = "none")  # Remove the legend


#############################################################################################

# Generate Table 4

#############################################################################################

data$message <- factor(data$message)  # 先变成 factor
data$message  <- relevel(data$message, ref = "honest_y")  # set honest-y as the reference

# run OLS with clustered standard error, for round 2-10

# with round as variable
model0 <- lm(observed_RT ~  message  + round, data = subset(data, Condition == 0 & round>1))
model1 <- lm(observed_RT ~  message  + round, data = subset(data, Condition == 1& round>1))
vcov_model0 <- vcovCL(model0, cluster = data$index[data$Condition==0 & data$round>1]) 
vcov_model1 <- vcovCL(model1, cluster = data$index[data$Condition==1& data$round>1])
coeftest_model0<-coeftest(model0,vcov. = vcov_model0)
coeftest_model1<-coeftest(model1,vcov. = vcov_model1)

# OLS with fixed effect, round 2-10
# with round as variable
model0_fx <- lm(observed_RT ~  message + factor(index) + round, data = subset(data, Condition == 0& round>1))
model1_fx <- lm(observed_RT ~  message + factor(index) + round, data = subset(data, Condition == 1& round>1))

vcov_model0_fx <- vcovCL(model0_fx, cluster = data$index[data$Condition==0& data$round>1]) 
vcov_model1_fx <- vcovCL(model1_fx, cluster = data$index[data$Condition==1& data$round>1]) 
coeftest_model0_fx<-coeftest(model0_fx,vcov. = vcov_model0_fx)
coeftest_model1_fx<-coeftest(model1_fx,vcov. = vcov_model1_fx)

# OLS dishonest senders, round 2-10
# with round as variable
model0_other <- lm(observed_RT ~  message + factor(index) + round, data = subset(data, Condition == 0 & type2=="other" & round>1))
model1_other <- lm(observed_RT ~  message + factor(index) + round, data = subset(data, Condition == 1 & type2=="other" & round>1))
vcov_model0_other <- vcovCL(model0_other, cluster = data$index[data$Condition==0& data$type2=="other" & data$round>1]) 
vcov_model1_other <- vcovCL(model1_other, cluster = data$index[data$Condition==1& data$type2=="other" & data$round>1]) 
coeftest_model0_other<-coeftest(model0_other,vcov. = vcov_model0_other)
coeftest_model1_other<-coeftest(model1_other,vcov. = vcov_model1_other)

stargazer(
  model0,
  model1,
  model0_fx,
  model1_fx,
  model0_other,
  model1_other,
  
  type = "latex",
  header = FALSE,
  align = TRUE,
  model.names = FALSE,
  
  title = "Response-Time Regressions",
  label = "tab:RT_regression",
  
  column.labels = c(
    "SU", "SA",
    "SU", "SA",
    "SU", "SA"
  ),
  
  # Clustered standard errors
  se = list(
    coeftest_model0[, 2],
    coeftest_model1[, 2],
    coeftest_model0_fx[, 2],
    coeftest_model1_fx[, 2],
    coeftest_model0_other[, 2],
    coeftest_model1_other[, 2]
  ),
  
  # Clustered p-values
  p = list(
    coeftest_model0[, 4],
    coeftest_model1[, 4],
    coeftest_model0_fx[, 4],
    coeftest_model1_fx[, 4],
    coeftest_model0_other[, 4],
    coeftest_model1_other[, 4]
  ),
  
  omit = "factor\\(index\\)",
  
  star.cutoffs = c(0.10, 0.05, 0.01),
  out = "RegressionResults2-10.tex"
)

##############################################################################################

# Generate Figure 3 

###############################################################################################


# frequency identification
data$Ptype = case_when(data$Yes <=4 ~ "low-frequency",
                       data$Yes >=8 ~ "high-frequency",
                       TRUE ~ "intermediate-frequency")

data9 <- data %>%
  dplyr::filter(round != 1)

# generate QuadRT9 using rounds2-10
data9 <- data9 %>%
  dplyr::group_by(Condition, index) %>%
  dplyr::mutate(
    median_RT9 = median(
      observed_RT[observed_RT > 0],
      na.rm = TRUE
    ),
    
    quadRT9 = dplyr::case_when(
      observed_RT == 0 ~ "Immediate",
      observed_RT < median_RT9 ~ "Fast",
      observed_RT == median_RT9 ~ "Median",
      observed_RT > median_RT9 ~ "Slow",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::ungroup()


credibility_fs <- data9 %>% 
  filter(Report==1,
         quadRT9 %in% c("Fast", "Slow")) %>%
  dplyr::select(Condition, Yes, quadRT9, Credibility, round, index, Ptype)

credibility_fs$Ptype <- factor(credibility_fs$Ptype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# prepare the statistics: honesty rates, 95% binomial CI 

credibility_summary <- credibility_fs %>%
  dplyr::group_by(Condition, Ptype, quadRT9) %>%
  dplyr::summarise(
    honest = sum(Credibility == 1),
    size = n(),
    mean_credibility = honest / size,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  dplyr::mutate(
    wilson_test = list(
      prop.test(
        x = honest,
        n = size,
        p = 0.5,
        alternative = "two.sided",
        conf.level = 0.95,
        correct = FALSE
      )
    ),
    ci_lower = wilson_test$conf.int[1],
    ci_upper = wilson_test$conf.int[2],
    p_vs_50 = wilson_test$p.value
  ) %>%
  ungroup() 

# Plot with points and 95% CI bars in SU
pd <- position_dodge(width = 0.2)
ggplot(data = credibility_summary %>% dplyr::filter(Condition == 0),
       aes(
         x = Ptype,
         y = mean_credibility,
         color = quadRT9,
         group = quadRT9
       )
) +
  geom_hline(
    yintercept = 0.5,
    color = "blue",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(
    position = pd,
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position = pd,
    width = 0.10,
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Fast" = "red3",
      "Slow" = "orange"
    ),
  ) +
  scale_x_discrete(
    labels = c(
      "low-frequency" = "Low\n(0–4)",
      "intermediate-frequency" = "Intermediate\n(5–7)",
      "high-frequency" = "High\n(8–10)"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
  ) +
  labs(
    color = "RT rank",
    x = "Frequency of message y",
    y = "Honesty rate of message y",
  ) +
  theme_minimal()


# Plot with points and 95% CI bars in SA
ggplot(data = credibility_summary %>% dplyr::filter(Condition == 1),
       aes(
         x = Ptype,
         y = mean_credibility,
         color = quadRT9,
         group = quadRT9
       )
) +
  geom_hline(
    yintercept = 0.5,
    color = "blue",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(
    position = pd,
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position = pd,
    width = 0.10,
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Fast" = "red3",
      "Slow" = "orange"
    ),
  ) +
  scale_x_discrete(
    labels = c(
      "low-frequency" = "Low\n(0–4)",
      "intermediate-frequency" = "Intermediate\n(5–7)",
      "high-frequency" = "High\n(8–10)"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
  ) +
  labs(
    color = "RT rank",
    x = "Frequency of message y",
    y = "Honesty rate of message y",
  ) +
  theme_minimal()

######################### Related Statistics

# page 20, number of immediate y messages
table(data9$quadRT9[data9$Report==1], data9$Condition[data9$Report==1])

# page 21, observations for different frequencies
frequency_table <- table(
  data9$Condition,
  data9$Ptype
)/9

frequency_table

chisq.test(frequency_table)

# page 24, honesty rate of informed message
informed_data <- data %>%
  filter(information_status == "Fully informed")

informed_data %>%
  group_by(Condition) %>%
  summarise(
    N = n(),
    honest = sum(Credibility == 1),
    honesty_rate = mean(Credibility),
    .groups = "drop"
  )

honesty_model <- glm(
  Credibility ~ Condition,
  data = informed_data,
  family = binomial
)

honesty_clustered <- coeftest(
  honesty_model,
  vcov. = vcovCL(
    honesty_model,
    cluster = ~ index,
    type = "HC1"
  )
)

honesty_clustered


#####################################################################################

# Generate Table 5

#####################################################################################

# panel a
table_vs50 <- credibility_summary %>%
  transmute(
    Condition,
    Frequency = Ptype,
    RT = quadRT9,
    `Honest/N` = paste0(honest, "/", size),
    `Honesty rate` =
      sprintf("%.1f%%", 100 * mean_credibility),
    `95% CI` =
      sprintf(
        "[%.1f%%, %.1f%%]",
        100 * ci_lower,
        100 * ci_upper
      ),
    `p-value` =
      ifelse(
        p_vs_50 < .001,
        "<.001",
        sprintf("%.3f", p_vs_50)
      )
  )

table_vs50

# run fisher tests
fisher_results <- credibility_fs %>%
  group_by(Condition, Ptype) %>%
  group_modify(~ {
    contingency_table <- table(
      factor(
        .x$quadRT9,
        levels = c("Fast", "Slow")
      ),
      factor(
        .x$Credibility,
        levels = c(0, 1)
      )
    )
    
    fisher_result <- fisher.test(
      contingency_table,
      alternative = "two.sided"
    )
    
    fast_data <- .x %>%
      filter(quadRT9 == "Fast")
    
    slow_data <- .x %>%
      filter(quadRT9 == "Slow")
    
    tibble(
      fast_honest = sum(fast_data$Credibility),
      fast_n = nrow(fast_data),
      fast_rate = mean(fast_data$Credibility),
      
      slow_honest = sum(slow_data$Credibility),
      slow_n = nrow(slow_data),
      slow_rate = mean(slow_data$Credibility),
      
      slow_minus_fast = slow_rate - fast_rate,
      
      # With the table arranged as above, this is Slow/Fast odds ratio
      odds_ratio = unname(fisher_result$estimate),
      or_ci_lower = fisher_result$conf.int[1],
      or_ci_upper = fisher_result$conf.int[2],
      p_fast_slow = fisher_result$p.value
    )
  }) %>%
  ungroup() 

# panel b 
table_fast_slow <- fisher_results %>%
  transmute(
    Condition,
    Frequency = Ptype,
    
    `Fast honesty ` =
      sprintf("%.1f%%", 100 * fast_rate),
    
    `Slow honesty` =
      sprintf("%.1f%%", 100 * slow_rate),
    
    
    `Slow/Fast odds ratio` =
      sprintf("%.3f", odds_ratio),
    
    `OR 95% CI` =
      sprintf(
        "[%.3f, %.3f]",
        or_ci_lower,
        or_ci_upper
      ),
    
    `Fisher p-value` =
      ifelse(
        p_fast_slow < .001,
        "<.001",
        sprintf("%.3f", p_fast_slow)
      )
  )

table_fast_slow


##################################################################################################

#  Draw Figure 4

#################################################################################################

# new variables
data$SenderPtype = case_when(data$OthersYes <=4 ~ "low-frequency",
                             data$OthersYes >=8 ~ "high-frequency",
                             TRUE ~ "intermediate-frequency")

data9 <- data %>%
  dplyr::filter(round != 1)

data9 <- data9 %>%
  dplyr::group_by(Condition, index) %>%
  dplyr::mutate(
    Sender_median_RT9 = median(
      Sender_RT[Sender_RT > 0],
      na.rm = TRUE
    ),
    
    Sender_quadRT9 = dplyr::case_when(
      Sender_RT == 0 ~ "Immediate",
      Sender_RT < Sender_median_RT9 ~ "Below",
      Sender_RT == Sender_median_RT9 ~ "Median",
      Sender_RT > Sender_median_RT9 ~ "Above",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::ungroup()


# select data
trust_fs <- data9 %>% 
  filter(OthersReport==1,
         Sender_quadRT9 %in% c("Below", "Above")) %>%
  dplyr::select(Condition, Sender_quadRT9, SenderPtype, NRTtrust, RTtrust)


trust_fs$SenderPtype <- factor(trust_fs$SenderPtype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# Calculate NRT and RT trust rates and 95% Wilson CIs
trust_summary <- trust_fs %>%
  dplyr::group_by(
    Condition,
    SenderPtype,
    Sender_quadRT9
  ) %>%
  dplyr::summarise(
    # NRT trust
    NRTtrusted = sum(NRTtrust == 1, na.rm = TRUE),
    NRT_size = sum(!is.na(NRTtrust)),
    
    # RT trust
    RTtrusted = sum(RTtrust == 1, na.rm = TRUE),
    RT_size = sum(!is.na(RTtrust)),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    # Trust rates
    mean_NRTtrust = dplyr::if_else(
      NRT_size > 0,
      NRTtrusted / NRT_size,
      NA_real_
    ),
    
    mean_RTtrust = dplyr::if_else(
      RT_size > 0,
      RTtrusted / RT_size,
      NA_real_
    ),
    
    # Critical value for 95% CI
    z = qnorm(0.975),
    
    # NRT Wilson CI
    NRT_wilson_center =
      (mean_NRTtrust + z^2 / (2 * NRT_size)) /
      (1 + z^2 / NRT_size),
    
    NRT_wilson_margin =
      z / (1 + z^2 / NRT_size) *
      sqrt(
        mean_NRTtrust * (1 - mean_NRTtrust) / NRT_size +
          z^2 / (4 * NRT_size^2)
      ),
    
    NRT_ci_lower = dplyr::if_else(
      NRT_size > 0,
      NRT_wilson_center - NRT_wilson_margin,
      NA_real_
    ),
    
    NRT_ci_upper = dplyr::if_else(
      NRT_size > 0,
      NRT_wilson_center + NRT_wilson_margin,
      NA_real_
    ),
    
    # RT Wilson CI
    RT_wilson_center =
      (mean_RTtrust + z^2 / (2 * RT_size)) /
      (1 + z^2 / RT_size),
    
    RT_wilson_margin =
      z / (1 + z^2 / RT_size) *
      sqrt(
        mean_RTtrust * (1 - mean_RTtrust) / RT_size +
          z^2 / (4 * RT_size^2)
      ),
    
    RT_ci_lower = dplyr::if_else(
      RT_size > 0,
      RT_wilson_center - RT_wilson_margin,
      NA_real_
    ),
    
    RT_ci_upper = dplyr::if_else(
      RT_size > 0,
      RT_wilson_center + RT_wilson_margin,
      NA_real_
    )
  ) %>%
  dplyr::select(
    Condition,
    SenderPtype,
    Sender_quadRT9,
    
    NRTtrusted,
    NRT_size,
    mean_NRTtrust,
    NRT_ci_lower,
    NRT_ci_upper,
    
    RTtrusted,
    RT_size,
    mean_RTtrust,
    RT_ci_lower,
    RT_ci_upper
  )


pd <- position_dodge(width = 0.2)
# SU-RU
ggplot(data=subset(trust_summary, Condition==0), aes(x = SenderPtype, y = mean_NRTtrust, color = Sender_quadRT9)) +
  geom_point(position = pd,
             size = 2) +  # Add points
  geom_errorbar(aes(ymin = NRT_ci_lower,
                    ymax = NRT_ci_upper),
                position = pd, 
                width = 0.2) +  # Add error bars
  scale_color_manual(values = c(#"Immediate" = "black", 
    "Below" = "red3", 
    #"Median" = "grey", 
    "Above" = "orange"),
    labels = c("Below" = "Fast", "Above" = "Slow")) +  # Update label 
  scale_x_discrete(labels = c("low-frequency" = "low\n(0-4)", 
                              "intermediate-frequency" = "intermediate\n(5-7)", 
                              "high-frequency" = "high\n(8-10)")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_abline(intercept = 0.5, slope = 0, color = "blue", linetype = "dashed")+
  labs(
    color = "RT rank",  # Change legend title here
    x = "Sender's Frequency of Message y", 
    y = "Trust Rate of Message y",
    #title = "Sender_quad Honesty Rate For Non-immediate Reports"
  ) +
  theme_minimal()


# SU-RI
ggplot(data=subset(trust_summary, Condition==0), aes(x = SenderPtype, y = mean_RTtrust, color = Sender_quadRT9)) +
  geom_point(position = pd,
             size = 2) +  # Add points
  geom_errorbar(aes(ymin = RT_ci_lower,
                    ymax = RT_ci_upper),
                position = pd, 
                width = 0.2) +  # Add error bars
  scale_color_manual(values = c(#"Immediate" = "black", 
    "Below" = "red3", 
    #"Median" = "grey", 
    "Above" = "orange"),
    labels = c("Below" = "Fast", "Above" = "Slow")) +  # Update label 
  scale_x_discrete(labels = c("low-frequency" = "low\n(0-4)", 
                              "intermediate-frequency" = "intermediate\n(5-7)", 
                              "high-frequency" = "high\n(8-10)")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  geom_abline(intercept = 0.5, slope = 0, color = "blue", linetype = "dashed")+
  labs(
    color = "RT rank",  # Change legend title here
    x = "Sender's Frequency of Message y", 
    y = "Trust Rate of Message y",
    #title = "Sender_quad Honesty Rate For Non-immediate Reports"
  ) +
  theme_minimal()

# SA-RU
ggplot(data=subset(trust_summary, Condition==1), aes(x = SenderPtype, y = mean_NRTtrust, color = Sender_quadRT9)) +
  geom_point(position = pd,
             size = 2) +  # Add points
  geom_errorbar(aes(ymin = NRT_ci_lower,
                    ymax = NRT_ci_upper),
                position = pd, 
                width = 0.2) +  # Add error bars
  scale_color_manual(values = c(#"Immediate" = "black", 
    "Below" = "red3", 
    #"Median" = "grey", 
    "Above" = "orange"),
    labels = c("Below" = "Fast", "Above" = "Slow")) +  # Update label 
  scale_x_discrete(labels = c("low-frequency" = "low\n(0-4)", 
                              "intermediate-frequency" = "intermediate\n(5-7)", 
                              "high-frequency" = "high\n(8-10)")) +
  scale_y_continuous(limits=c(0,1),
                     breaks = seq(0, 1, by = 0.1)) +
  geom_abline(intercept = 0.5, slope = 0, color = "blue", linetype = "dashed")+
  labs(
    color = "RT rank",  # Change legend title here
    x = "Sender's Frequency of Message y", 
    y = "Trust Rate of Message y",
    #title = "Sender_quad Honesty Rate For Non-immediate Reports"
  ) +
  theme_minimal()


# SA-RI
ggplot(data=subset(trust_summary, Condition==1), aes(x = SenderPtype, y = mean_RTtrust, color = Sender_quadRT9)) +
  geom_point(position = pd,
             size = 2) +  # Add points
  geom_errorbar(aes(ymin = RT_ci_lower,
                    ymax = RT_ci_upper),
                position = pd, 
                width = 0.2) +  # Add error bars
  scale_color_manual(values = c(#"Immediate" = "black", 
    "Below" = "red3", 
    #"Median" = "grey", 
    "Above" = "orange"),
    labels = c("Below" = "Fast", "Above" = "Slow")) +  # Update label 
  scale_x_discrete(labels = c("low-frequency" = "low\n(0-4)", 
                              "intermediate-frequency" = "intermediate\n(5-7)", 
                              "high-frequency" = "high\n(8-10)")) +
  scale_y_continuous(limits=c(0,1),
                     breaks = seq(0, 1, by = 0.1)) +
  geom_abline(intercept = 0.5, slope = 0, color = "blue", linetype = "dashed")+
  labs(
    color = "RT rank",  # Change legend title here
    x = "Sender's Frequency of Message y", 
    y = "Trust Rate of Message y",
    #title = "Sender_quad Honesty Rate For Non-immediate Reports"
  ) +
  theme_minimal()


######################  Related Statisitcs

# page 25, Yes decision in NRT and RT conditions
receiver_rates <- data %>%
  group_by(Condition) %>%
  summarise(
    RU_yes_rate = mean(NRTDecision),
    RI_yes_rate = mean(RTDecision),
    .groups = "drop"
  )

receiver_rates

receiver_choices <- data %>%
  group_by(index, Condition) %>%
  summarise(
    RU_yes = sum(NRTDecision),
    RI_yes = sum(RTDecision),
    .groups = "drop"
  )

wilcox.test(
  receiver_choices$RU_yes[
    receiver_choices$Condition == 0
  ],
  receiver_choices$RI_yes[
    receiver_choices$Condition == 0
  ],
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE
)

wilcox.test(
  receiver_choices$RU_yes[
    receiver_choices$Condition == 1
  ],
  receiver_choices$RI_yes[
    receiver_choices$Condition == 1
  ],
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE
)

#################################################################################################

# Generate Table 6

#################################################################################################

# combine RU and RI，for treatment effect

trust_RURI <- data9 %>%
  dplyr::filter(
    OthersReport == 1
  ) %>%
  tidyr::pivot_longer(
    cols = c(NRTtrust, RTtrust),
    names_to = "information_treatment",
    values_to = "Trust"
  ) %>%
  dplyr::mutate(
    RI = as.integer(information_treatment == "RTtrust")
  ) %>%
  dplyr::select(
    Trust, RI, Sender_quadRT9, SenderPtype, round, index, type2, Condition
  )

trust_RURI$SenderPtype <- factor(trust_RURI$SenderPtype, 
                                 levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# All receivers, SU
model_all_SU <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 0
)

vcov_all_SU <- sandwich::vcovCL(
  model_all_SU,
  cluster = ~ index,
  type = "HC1"
)

result_all_SU <- lmtest::coeftest(
  model_all_SU,
  vcov. = vcov_all_SU
)

# (2) All receivers: SA
model_all_SA <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 1
)

vcov_all_SA <- sandwich::vcovCL(
  model_all_SA,
  cluster = ~ index,
  type = "HC1"
)

result_all_SA <- lmtest::coeftest(
  model_all_SA,
  vcov. = vcov_all_SA
)

# (3) Honest receivers: SU
model_honest_SU <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 0 & type2 == "honest"
)

vcov_honest_SU <- sandwich::vcovCL(
  model_honest_SU,
  cluster = ~ index,
  type = "HC1"
)

result_honest_SU <- lmtest::coeftest(
  model_honest_SU,
  vcov. = vcov_honest_SU
)

# (4) Honest receivers: SA
model_honest_SA <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 1 & type2 == "honest"
)

vcov_honest_SA <- sandwich::vcovCL(
  model_honest_SA,
  cluster = ~ index,
  type = "HC1"
)

result_honest_SA <- lmtest::coeftest(
  model_honest_SA,
  vcov. = vcov_honest_SA
)

# (5) other receivers: SU
model_other_SU <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 0 & type2 == "other"
)

vcov_other_SU <- sandwich::vcovCL(
  model_other_SU,
  cluster = ~ index,
  type = "HC1"
)

result_other_SU <- lmtest::coeftest(
  model_other_SU,
  vcov. = vcov_other_SU
)

# (6) other receivers: SA
model_other_SA <- lm(
  Trust ~ RI * Sender_quadRT9 * SenderPtype +
    factor(index),
  data = trust_RURI,
  subset = Condition == 1 & type2 == "other"
)

vcov_other_SA <- sandwich::vcovCL(
  model_other_SA,
  cluster = ~ index,
  type = "HC1"
)

result_other_SA <- lmtest::coeftest(
  model_other_SA,
  vcov. = vcov_other_SA
)

#clustered standard errors：
se_all_SU <- sqrt(diag(vcov_all_SU))
se_all_SA <- sqrt(diag(vcov_all_SA))

se_honest_SU <- sqrt(diag(vcov_honest_SU))
se_honest_SA <- sqrt(diag(vcov_honest_SA))

se_other_SU <- sqrt(diag(vcov_other_SU))
se_other_SA <- sqrt(diag(vcov_other_SA))

#clustered p-values
p_all_SU <- result_all_SU[, "Pr(>|t|)"]
p_all_SA <- result_all_SA[, "Pr(>|t|)"]

p_honest_SU <- result_honest_SU[, "Pr(>|t|)"]
p_honest_SA <- result_honest_SA[, "Pr(>|t|)"]

p_other_SU <- result_other_SU[, "Pr(>|t|)"]
p_other_SA <- result_other_SA[, "Pr(>|t|)"]


# LaTeX：
stargazer(
  model_all_SU,
  model_all_SA,
  model_honest_SU,
  model_honest_SA,
  model_other_SU,
  model_other_SA,
  
  type = "latex",
  
  se = list(
    se_all_SU,
    se_all_SA,
    se_honest_SU,
    se_honest_SA,
    se_other_SU,
    se_other_SA
  ),
  
  #  clustered standard errors and p-values
  p = list(
    p_all_SU,
    p_all_SA,
    p_honest_SU,
    p_honest_SA,
    p_other_SU,
    p_other_SA
  ),
  
  column.labels = c(
    "SU",
    "SA",
    "SU",
    "SA",
    "SU",
    "SA"
  ),
  
  column.separate = c(1, 1, 1, 1, 1, 1),
  
  dep.var.labels = "Trust",
  
  model.numbers = FALSE,
  intercept.bottom = FALSE,
  
  title = paste(
    "RI Treatment Effects by Sender Response Time",
    "and Receiver Type"
  ),
  
  label = "trust_regressions",
  
  omit.stat = c("f", "ser"),
  digits = 3,
  star.cutoffs = c(0.10, 0.05, 0.01),
  
  out = "trust regression2-10.tex"
)

###################################################

# Generate Appendix Table 7

##################################################

data$message <- factor(data$message)  # 先变成 factor
data$message  <- relevel(data$message, ref = "honest_y")  # set honest-y as the reference


#  with round fixed effect, all senders
model0 <- lm(observed_RT ~  message+ factor(round), data = subset(data, Condition == 0 & round>1))
model1 <- lm(observed_RT ~  message+ factor(round), data = subset(data, Condition == 1& round>1))

vcov_model0 <- vcovCL(model0, cluster = data$index[data$Condition==0 & data$round>1]) 
vcov_model1 <- vcovCL(model1, cluster = data$index[data$Condition==1& data$round>1])
coeftest_model0<-coeftest(model0,vcov. = vcov_model0)
coeftest_model1<-coeftest(model1,vcov. = vcov_model1)


# with round and individual fixed effect, round 2-10, all senders
model0_fx <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 0& round>1))
model1_fx <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 1& round>1))

vcov_model0_fx <- vcovCL(model0_fx, cluster = data$index[data$Condition==0& data$round>1]) 
vcov_model1_fx <- vcovCL(model1_fx, cluster = data$index[data$Condition==1& data$round>1]) 
coeftest_model0_fx<-coeftest(model0_fx,vcov. = vcov_model0_fx)
coeftest_model1_fx<-coeftest(model1_fx,vcov. = vcov_model1_fx)


# with round and individual fixed variables, round 2-10, dishonest people 
model0_other <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 0 & type2!="honest" & round>1))
model1_other <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 1 & type2!="honest" & round>1))


vcov_model0_other <- vcovCL(model0_other, cluster = data$index[data$Condition==0& data$type2!="honest" & data$round>1]) 
vcov_model1_other <- vcovCL(model1_other, cluster = data$index[data$Condition==1& data$type2!="honest"& data$round>1]) 
coeftest_model0_other<-coeftest(model0_other,vcov. = vcov_model0_other)
coeftest_model1_other<-coeftest(model1_other,vcov. = vcov_model1_other)


# with round and individual as fixed variables, dishonest people, round 6-10
model0_g5 <- lm(observed_RT ~  message + factor(index)+ factor(round), data = subset(data, Condition == 0 & round>5& type2!="honest"))
model1_g5 <- lm(observed_RT ~  message + factor(index)+ factor(round), data = subset(data, Condition == 1 & round>5& type2!="honest"))

vcov_model0_g5 <- vcovCL(model0_g5, cluster = data$index[data$Condition==0& data$round>5& data$type2!="honest"]) 
vcov_model1_g5 <- vcovCL(model1_g5, cluster = data$index[data$Condition==1& data$round>5& data$type2!="honest"]) 
coeftest_model0_g5<-coeftest(model0_g5,vcov. = vcov_model0_g5)
coeftest_model1_g5<-coeftest(model1_g5,vcov. = vcov_model1_g5)


# with round and invidual as fixed variables, dishonest people, round 2-5
model0_l5 <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 0 & round>1 & round<=5& type2!="honest"))
model1_l5 <- lm(observed_RT ~  message + factor(index) + factor(round), data = subset(data, Condition == 1 & round>1 &round<=5& type2!="honest"))

vcov_model0_l5 <- vcovCL(model0_l5, cluster = data$index[data$Condition==0& data$round>1 & data$round<=5& data$type2!="honest"]) 
vcov_model1_l5 <- vcovCL(model1_l5, cluster = data$index[data$Condition==1& data$round>1& data$round<=5& data$type2!="honest"]) 
coeftest_model0_l5<-coeftest(model0_l5,vcov. = vcov_model0_l5)
coeftest_model1_l5<-coeftest(model1_l5,vcov. = vcov_model1_l5)


stargazer(
  model0,
  model1,
  model0_fx,
  model1_fx,
  model0_other,
  model1_other,
  model0_l5,
  model1_l5,
  model0_g5,
  model1_g5,
  
  type = "latex",
  header = FALSE,
  align = TRUE,
  model.names = FALSE,
  
  title = "Response-Time Regressions",
  label = "tab:RT_regression",
  
  column.labels = c(
    "SU", "SA",
    "SU", "SA",
    "SU", "SA",
    "SU", "SA",
    "SU", "SA"
  ),
  
  # Clustered standard errors
  se = list(
    coeftest_model0[, 2],
    coeftest_model1[, 2],
    coeftest_model0_fx[, 2],
    coeftest_model1_fx[, 2],
    coeftest_model0_other[, 2],
    coeftest_model1_other[, 2],
    coeftest_model0_l5[, 2],
    coeftest_model1_l5[, 2],
    coeftest_model0_g5[, 2],
    coeftest_model1_g5[, 2]
  ),
  
  # Clustered p-values
  p = list(
    coeftest_model0[, 4],
    coeftest_model1[, 4],
    coeftest_model0_fx[, 4],
    coeftest_model1_fx[, 4],
    coeftest_model0_other[, 4],
    coeftest_model1_other[, 4],
    coeftest_model0_l5[, 4],
    coeftest_model1_l5[, 4],
    coeftest_model0_g5[, 4],
    coeftest_model1_g5[, 4]
  ),
  
  omit = c(
    "factor\\(index\\)",
    "factor\\(round\\)"
  ),
  
  
  star.cutoffs = c(0.10, 0.05, 0.01),
  out = "RegressionResults_fixedround.tex"
)

################# Related Statistic

coeftest_model0_fx[
  c("messagedeceptive_y", "messagenfi_y"),
]

##################################################################

# Appendix Figure 7

################################################################
app_data<-data

# new variable
app_data$Ptype = case_when(app_data$Yes <=3 ~ "low-frequency",
                           app_data$Yes >=8 ~ "high-frequency",
                           TRUE ~ "intermediate-frequency")

app_data9 <- app_data %>%
  dplyr::filter(round != 1)

app_data9 <- app_data9 %>%
  dplyr::group_by(Condition, index) %>%
  dplyr::mutate(
    median_RT9 = median(
      observed_RT[observed_RT > 0],
      na.rm = TRUE
    ),
    
    quadRT9 = dplyr::case_when(
      observed_RT == 0 ~ "Immediate",
      observed_RT < median_RT9 ~ "Fast",
      observed_RT == median_RT9 ~ "Median",
      observed_RT > median_RT9 ~ "Slow",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::ungroup()

app_data9 <- app_data9 %>%
  dplyr::group_by(Condition, index) %>%
  dplyr::mutate(
    Sender_median_RT9 = median(
      Sender_RT[Sender_RT > 0],
      na.rm = TRUE
    ),
    
    Sender_quadRT9 = dplyr::case_when(
      Sender_RT == 0 ~ "Immediate",
      Sender_RT < Sender_median_RT9 ~ "Below",
      Sender_RT == Sender_median_RT9 ~ "Median",
      Sender_RT > Sender_median_RT9 ~ "Above",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::ungroup()


credibility_fs <- app_data9 %>% 
  filter(Report==1,
         quadRT9 %in% c("Fast", "Slow")) %>%
  dplyr::select(Condition, Yes, quadRT9, Credibility, round, index, Ptype)

credibility_fs$Ptype <- factor(credibility_fs$Ptype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# prepare the statistics: honesty rates, 95% binomial CI 

credibility_summary <- credibility_fs %>%
  dplyr::group_by(Condition, Ptype, quadRT9) %>%
  dplyr::summarise(
    honest = sum(Credibility == 1),
    size = n(),
    mean_credibility = honest / size,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  dplyr::mutate(
    wilson_test = list(
      prop.test(
        x = honest,
        n = size,
        p = 0.5,
        alternative = "two.sided",
        conf.level = 0.95,
        correct = FALSE
      )
    ),
    ci_lower = wilson_test$conf.int[1],
    ci_upper = wilson_test$conf.int[2],
    p_vs_50 = wilson_test$p.value
  ) %>%
  ungroup() 


# Plot with points and 95% CI bars in SU
pd <- position_dodge(width = 0.2)
ggplot(data = credibility_summary %>% dplyr::filter(Condition == 0),
       aes(
         x = Ptype,
         y = mean_credibility,
         color = quadRT9,
         group = quadRT9
       )
) +
  geom_hline(
    yintercept = 0.5,
    color = "blue",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(
    position = pd,
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position = pd,
    width = 0.10,
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Fast" = "red3",
      "Slow" = "orange"
    ),
  ) +
  scale_x_discrete(
    labels = c(
      "low-frequency" = "Low\n(0–3)",
      "intermediate-frequency" = "Intermediate\n(4–7)",
      "high-frequency" = "High\n(8–10)"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
  ) +
  labs(
    color = "RT rank",
    x = "Frequency of message y",
    y = "Honesty rate of message y",
  ) +
  theme_minimal()


# Plot with points and 95% CI bars in SA
ggplot(data = credibility_summary %>% dplyr::filter(Condition == 1),
       aes(
         x = Ptype,
         y = mean_credibility,
         color = quadRT9,
         group = quadRT9
       )
) +
  geom_hline(
    yintercept = 0.5,
    color = "blue",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(
    position = pd,
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    position = pd,
    width = 0.10,
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Fast" = "red3",
      "Slow" = "orange"
    ),
  ) +
  scale_x_discrete(
    labels = c(
      "low-frequency" = "Low\n(0–3)",
      "intermediate-frequency" = "Intermediate\n(4–7)",
      "high-frequency" = "High\n(8–10)"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
  ) +
  labs(
    color = "RT rank",
    x = "Frequency of message y",
    y = "Honesty rate of message y",
  ) +
  theme_minimal()

################### Related Statistics

# Page 40, fast and slow comparisons among intermediate frequency
intermediate_SU <- app_data9 %>%
  filter(
    Condition == 0,
    Ptype == "intermediate-frequency",
    Report == 1,
    quadRT9 %in% c("Fast", "Slow")
  )

intermediate_SU %>%
  group_by(quadRT9) %>%
  summarise(
    honest = sum(Credibility == 1),
    N = n(),
    honesty_rate = mean(Credibility),
    .groups = "drop"
  )

fisher.test(
  table(
    intermediate_SU$quadRT9,
    intermediate_SU$Credibility
  ),
  alternative = "two.sided"
)

# Two-sided proportion test of the Slow honesty rate against 50%:
slow_SU <- intermediate_SU %>%
  filter(quadRT9 == "Slow")

prop.test(
  x = sum(slow_SU$Credibility == 1),
  n = nrow(slow_SU),
  p = 0.5,
  alternative = "two.sided",
  correct = FALSE
)


