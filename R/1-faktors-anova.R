#For at installere nødvendige pakker første gang, fjern '#' foran nedenstående
#renv::restore()

#Load packages
library(lmerTest)
library(emmeans)
library(here)
library(dplyr)
library(ggplot2)

#Load data
df <- read.csv2(here::here('data-raw/2-faktors-anova.csv')) %>% 
  dplyr::mutate(
    treatment = factor(treatment, levels = c("placebo", "formoterol")),
    time = factor(time, levels = c("post", "pre"))
  )


#Evt. tag et hurtigt kig på data
ggplot(df, aes(x = treatment, y = lean, fill = time))+
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 1))+
  geom_point(position = position_dodge(width = 1))

#Fit model
model <- lmerTest::lmer(
  lean ~ treatment * time + (1 | id),
  data = df,
  REML = FALSE)

anova(model)
emmeans::emmeans(model, pairwise ~ time | treatment, adjust = "none")
confint(emmeans::emmeans(model, pairwise ~ time | treatment, adjust = "none"))
