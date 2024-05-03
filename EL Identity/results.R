#clean the environment
rm(list=ls())
#locate the R file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#Well-defined functions library
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(titanic)
library(ggplot2)
library(plyr)
library(tidyr)
library(stargazer)
# read data
alldatapt <- read_csv("alldatapt.csv")
#################################################################
#  draw figure 1 
#################################################################

# Replace all occurrences of 2 with "X"
alldatapt$payoff1 <- ifelse(alldatapt$payoff1 == 2, "X", alldatapt$payoff1)
#order 0, X, 3
alldatapt$payoff1 <- factor(alldatapt$payoff1, levels = c(0, "X", 3))

ggplot(alldatapt, aes(x=dis03_1, linetype=payoff1))  +
  stat_ecdf(geom = "step") +
  labs(#title = "CDF of distributions to the unfortunate recipient",
    x = "Distributions to the unfortunate recipient",
    y = "CDF") +
  scale_linetype_manual(values=c("solid", "dotdash", "longdash"))

##################################################################
#    draw table1
##################################################################

#define new variable
alldatapt$time = alldatapt$seconds_on_page.count1+alldatapt$seconds_on_page.count2+alldatapt$seconds_on_page.count3+
  alldatapt$seconds_on_page.count4+alldatapt$seconds_on_page.count5+ alldatapt$seconds_on_page.count6 +
  alldatapt$seconds_on_page.count7 + alldatapt$seconds_on_page.count8 + alldatapt$seconds_on_page.count9+
  alldatapt$seconds_on_page.count10

# with the control of time
ols.1 = lm(dis03_1~I(payoff1==0)+I(payoff1==3), data= alldatapt)
# with the control of time
ols.2 = lm(dis03_1~I(payoff1==0)+I(payoff1==3)+time, data= alldatapt)
# summary
table1 <- stargazer(ols.1,ols.2, type = "text")
