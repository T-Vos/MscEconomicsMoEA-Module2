library(readxl)
library(ggplot2)
pacman::p_load(tidyverse, haven, lmtest, stargazer, 
               ARDL, modelsummary, car,tseries,mFilter)
df = read_excel('../Assignment_Module2_Part3.xlsx')

# In order to simplify the return function get a lag
df$AHOLD.L1 = lag(df$AHOLD, n = 1)
df$AEX.L1 = lag(df$AEX, n = 1)

df$returnAhold = ((df$AHOLD-df$AHOLD.L1)/df$AHOLD.L1)
df$returnAEX = ((df$AEX-df$AEX.L1)/df$AEX.L1)

reg.1 = lm(df$returnAhold ~ df$returnAEX, data=df)
summary(reg.1)


plot.1 <- ggplot() + 
  geom_line(data = df, aes(x = day, y = AEX.L1, color = "PCE")) +
  geom_line(data = df, aes(x = day, y = AHOLD.L1, color = "DPI")) +
  theme(legend.position="bottom") +
  xlab('Date') +
  ylab('Change in PCE and DPI')

plot.1

df$Residuals = c(NA,resid(reg.1))
plot.Residual = plot(df$day, df$Residuals, ylab="Residuals", 
                     xlab="Time", 
                     main="Residuals plot") 
plot.Residual

durbinWatsonTest(reg.1)

library(tseries)
df.1rowLess = df[-c(1:1), ]
adf.test(c(df.1rowLess$returnAEX,df.1rowLess$returnAhold))
