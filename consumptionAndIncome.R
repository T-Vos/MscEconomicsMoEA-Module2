library(readxl)
library(ggplot2)

dataFrame = read_excel('../Assignment_Module2_Part2.xlsx')

reg.1 = lm(dataFrame$PCE ~ dataFrame$DPI, data=dataFrame)

summary(reg.1)
cor(dataFrame$PCE, dataFrame$DPI)

reg.1.res = resid(reg.1) 
dataFrame$Residuals = resid(reg.1)

plot.1 <- ggplot() + 
  geom_line(data = dataFrame, aes(x = Quarter, y = PCE, color = "PCE")) +
  geom_line(data = dataFrame, aes(x = Quarter, y = DPI, color = "DPI")) +
  theme(legend.position="bottom") +
  xlab('Date') +
  ylab('Change in PCE and DPI')

plot.1

plot.Residual = plot(dataFrame$Quarter, dataFrame$Residuals, ylab="Residuals", 
                     xlab="Time", 
                     main="Residuals plot") 

plot.Residual
