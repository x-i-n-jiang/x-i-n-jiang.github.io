rm(list = ls())

library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(stargazer)

data <- read_csv(
  "long_data2.csv",
)

###################################################################

# Draw Figure 5

#####################################################################

# frequency identification
data$Ptype = case_when(data$Yes <=4 ~ "low-frequency",
                       data$Yes >=8 ~ "high-frequency",
                       TRUE ~ "intermediate-frequency")


data9 <- data %>%
  dplyr::filter(round != 1)

# generate QuadRT9 using rounds2-10
data9 <- data9 %>%
  dplyr::group_by(index) %>%
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
  dplyr::select(Yes, quadRT9, Credibility, round, index, Ptype)

credibility_fs$Ptype <- factor(credibility_fs$Ptype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# prepare the statistics: honesty rates, 95% binomial CI 

credibility_summary <- credibility_fs %>%
  dplyr::group_by(Ptype, quadRT9) %>%
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

# Plot with points and 95% CI bars
pd <- position_dodge(width = 0.2)
ggplot(data = credibility_summary,
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

###########################Related Statistics

# page 29, fisher tests
figure5_fisher <- credibility_fs %>%
  group_by(Ptype) %>%
  group_modify(~ {
    test <- fisher.test(
      table(.x$quadRT9, .x$Credibility),
      alternative = "two.sided"
    )
    
    tibble(
      fast_rate = mean(
        .x$Credibility[.x$quadRT9 == "Fast"]
      ),
      slow_rate = mean(
        .x$Credibility[.x$quadRT9 == "Slow"]
      ),
      fisher_p = test$p.value
    )
  })

figure5_fisher

###################################################################

# Draw Figure 6

#####################################################################

# new variable
data$SenderPtype = case_when(data$OthersYes <=4 ~ "low-frequency",
                             data$OthersYes >=8 ~ "high-frequency",
                             TRUE ~ "intermediate-frequency") 

data9 <- data %>%
  dplyr::filter(round != 1)

data9 <- data9 %>%
  dplyr::group_by(index) %>%
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
  dplyr::select(Sender_quadRT9, SenderPtype, NRTtrust, RTtrust, type2)


trust_fs$SenderPtype <- factor(trust_fs$SenderPtype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# Calculate NRT and RT trust rates and 95% Wilson CIs
trust_summary <- trust_fs %>%
  dplyr::group_by(
    type2,
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
    type2,
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

# honest receivers, RT
ggplot(data=subset(trust_summary, type2=="honest"), aes(x = SenderPtype, y = mean_RTtrust, color = Sender_quadRT9)) +
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
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.1)) +
  geom_abline(intercept = 0.5, slope = 0, color = "blue", linetype = "dashed")+
  labs(
    color = "RT rank",  # Change legend title here
    x = "Sender's Frequency of Message y", 
    y = "Trust Rate of Message y",
    #title = "Sender_quad Honesty Rate For Non-immediate Reports"
  ) +
  theme_minimal()


# dishonest receivers, RT
ggplot(data=subset(trust_summary, type2=="others"), aes(x = SenderPtype, y = mean_RTtrust, color = Sender_quadRT9)) +
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

############################################################################################

#    Draw Appendix Figure 8

############################################################################################

# calculate the min observed_RT for each participant
data <- data %>%
  group_by(index) %>%
  mutate(min_RT_true = {
    if (any(message == "true_y")) {
      min(observed_RT[message == "true_y"], na.rm = TRUE)
    } else if (any(message == "true_n")) {
      min(observed_RT[message == "true_n"], na.rm = TRUE)
    } else {
      NA_real_
    }
  }) %>%
  ungroup()

# benchmark of sender_RT
data$Sender_RT3 = case_when(data$Sender_RT<data$min_RT_true ~ "Faster",
                            data$Sender_RT== data$min_RT_true ~ "Equal",
                            TRUE ~ "Slower")
table(data$Sender_RT3)

data$SenderPtype = case_when(data$OthersYes <=4 ~ "low-frequency",
                             data$OthersYes >=8 ~ "high-frequency",
                             TRUE ~ "intermediate-frequency") 


# select data
trust_fs <- data %>% 
  filter(OthersReport==1) %>%
  dplyr::select(Sender_RT3, SenderPtype, NRTtrust, RTtrust,type2)


trust_fs$SenderPtype <- factor(trust_fs$SenderPtype, 
                               levels = c("low-frequency", "intermediate-frequency", "high-frequency"))


# Calculate NRT and RT trust rates and 95% Wilson CIs
trust_summary <- trust_fs %>%
  dplyr::group_by(
    type2,
    SenderPtype,
    Sender_RT3
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
    type2,
    SenderPtype,
    Sender_RT3,
    
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

# dishonest receivers
ggplot(data=subset(trust_summary, type2=="others"), aes(x = SenderPtype, y = mean_RTtrust, color = Sender_RT3)) +
  geom_point(position = pd,
             size = 2) +  # Add points
  geom_errorbar(aes(ymin = RT_ci_lower,
                    ymax = RT_ci_upper),
                position = pd, 
                width = 0.2) +  # Add error bars
  scale_color_manual(values = c(#"Immediate" = "black", 
    "Faster" = "red3", 
    "Equal" = "pink", 
    "Slower" = "orange"),
    labels = c("Faster" = "Faster than min", "Equal"="Equal to min", "Slower" = "Slower than min")) +  # Update label 
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

############################## Related Statistics

figure8_test_data <- data %>%
  filter(
    OthersReport == 1,
    type2 == "others",
    SenderPtype == "high-frequency"
  )

table(
  figure8_test_data$Sender_RT3,
  figure8_test_data$RTtrust
)

fisher.test(
  table(
    figure8_test_data$Sender_RT3,
    figure8_test_data$RTtrust
  ),
  alternative = "two.sided"
)
