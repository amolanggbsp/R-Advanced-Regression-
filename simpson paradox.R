library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
alpha = 0.5

set.seed(123)
n = 1000

Neuroticism = rnorm(n)
Performance = rnorm(n) + Neuroticism * 0.1

Performance = rescale(Performance, to = c(0, 100))
# summary(Performance)
Neuroticism = rescale(Neuroticism, to = c(0, 7))
# summary(Neuroticism)

data <- data.frame(
  Performance,
  Neuroticism
)

options = c("Technical","Service")
technical = 
  (data$Performance > mean(data$Performance) & 
     data$Neuroticism > mean(data$Neuroticism)) |
  (data$Performance < mean(data$Performance) & 
     data$Neuroticism < mean(data$Neuroticism))

data$Job[technical] <- sample(options, sum(technical), T, c(0.6, 0.2))
data$Job[is.na(data$Job)] <- sample(options, sum(is.na(data$Job)), T, c(0.2, 0.8))

p <- data %>% ggplot(aes(Neuroticism, Performance)) 
p + geom_point(alpha = alpha) + geom_smooth(method = 'lm')




p +
  geom_point(aes(col = Job), alpha = alpha) + 
  geom_smooth(aes(col = Job), method = 'lm') +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1))


