library(readxl)
library(ggplot2)
library("dplyr")
pacman::p_load(tidyverse, haven, lmtest, stargazer, 
               ARDL, modelsummary, car)
df = read_excel('../Assignment_Module2_Part1.xlsx')

# Bit of a lazy technique to delete till 1960; but it brings you home
df <-df[-c(1:51), ]
# idem
df.2013 <-df[-c(217:239), ]

# A bit of a Verbose code in order to generate all the columns
# This could be useful in order to provide a more indepth 
columns <- c("gdpg", "interest", "cpi", "nettrade")
for (col in columns){
  for(Ln in 1:8) {
    df[paste0(col,".L",Ln)] <- lag(df[col], n = Ln)
  }
}
columns <- c("gdpg", "interest", "cpi", "nettrade")
for (col in columns){
  for(Ln in 1:8) {
    df.2013[paste0(col,".L",Ln)] <- lag(df.2013[col], n = Ln)
  }
}

Reg.19602013.L0 <- lm(cpi ~  gdpg+interest+nettrade, data = df.2013)

df.2013$ehat <- residuals(Reg.19602013.L0)
df.2013$ehat_l1 <- lag(df.2013$ehat, n = 1)
df.2013$ehat_l2 <- lag(df.2013$ehat, n = 2)
df.2013$ehat_l3 <- lag(df.2013$ehat, n = 3)
df.2013$ehat_l4 <- lag(df.2013$ehat, n = 4)
df.2013$ehat_l5 <- lag(df.2013$ehat, n = 5)
df.2013$ehat_l6 <- lag(df.2013$ehat, n = 6)
df.2013$ehat_l7 <- lag(df.2013$ehat, n = 7)
df.2013$ehat_l8 <- lag(df.2013$ehat, n = 8)

df.2013 %>% select(ehat:ehat_l2) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l3) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l4) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l5) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l6) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l7) %>%  cor(use = "pairwise")
df.2013 %>% select(ehat:ehat_l8) %>%  cor(use = "pairwise")

acf(df.2013$ehat_l3)

Reg.19602013.L1 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,1,1,1))
Reg.19602013.L2 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,2,2,2))
Reg.19602013.L3 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,3,3,3))
Reg.19602013.L4 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,4,4,4))
Reg.19602013.L5 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,5,5,5))
Reg.19602013.L6 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,6,6,6))
Reg.19602013.L7 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,7,7,7))
Reg.19602013.L8 <- ardl(cpi ~  gdpg+interest+nettrade, data = df.2013, order = c(1,8,8,8))
models <- list(Reg.19602013.L1, Reg.19602013.L2,Reg.19602013.L3,Reg.19602013.L4,Reg.19602013.L5,Reg.19602013.L6,Reg.19602013.L7,Reg.19602013.L8)
modelsummary(models)

bgt_1_6 <- bgtest(Reg.19602013.L6, order = 1)
bgt_1_6

bgt_1_7 <- bgtest(Reg.19602013.L7, order = 1)
bgt_1_7

bgt_1_8 <- bgtest(Reg.19602013.L8, order = 1)
bgt_1_8


Reg.19602018.L8 <- ardl(cpi ~  gdpg+interest+nettrade, data = df, order = c(1,8,8,8))

df$residuals <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,residuals(Reg.19602018.L8))

mean(residuals(Reg.19602018.L8))

df
plot.Residual = ggplot() + 
  geom_line(data = df, aes(x = quarter, y = cpi, color = "CPI")) +
  geom_point(data = df, aes(x = quarter, y = residuals, color = "Residuals")) 

plot.Residual
