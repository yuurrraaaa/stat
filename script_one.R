library(ggplot2)
library(Hmisc)

data <- data.frame(before, after)
data$id <- 1:nrow(data)
data$direction <- sign(data$before - data$after)
data

plot.data <- data.frame(value = c(data$before, data$after),
                        key = factor(c(rep('before', nrow(data)), rep('after', nrow(data))), levels = c('before', 'after')),
                        id = c(data$id, data$id),
                        direction = factor(c(data$direction, data$direction)))
plot.data
ggplot(plot.data, aes(y = value, x = key)) +
  geom_point() +
  geom_line(aes(group = id, color = direction)) +
  scale_color_manual(values = c('red', 'black', 'blue'))

plot.data <- data.frame(change = data$before - data$after)
ggplot(plot.data, aes(y = change, x = 'Change')) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', size = 4, shape = 8, color = 'red') +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .1, color = 'red') +
  scale_y_continuous(lim = c(-250, 100), breaks = seq(-250, 100, 50)) +
  theme(axis.title.x = element_blank())

mean_cl_normal(plot.data$change)

tst <- t.test(before, after, paired = T)
tst
dt(tst$statistic, 18)


change <- before - after
change.mean <- mean(change)
change.sd <- sd(change)
change.se <- change.sd/sqrt(length(change))
t_statistic <- change.mean / change.se
tst$statistic

pts <- seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
abline(v = t_statistic, col = 'blue')


pt(tst$statistic, 18)

abline(v = c(qt(0.025, 18), qt(0.975, 18)), col = 'green')

# on dfs

pts <- seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=10),col='darkred',type='l')
lines(pts,dt(pts,df=5),col='red',type='l')
lines(pts,dt(pts,df=3),col='orange',type='l')

abline(v = c(qt(0.025, 18), qt(0.975, 18)), col = 'green')


# normality check
plot.data <- data.frame(value = c(data$before, data$after),
                        key = factor(c(rep('before', nrow(data)), rep('after', nrow(data))), levels = c('before', 'after')))
ggplot(plot.data, aes(value)) +
  geom_histogram(color = 'black', fill = 'white', binwidth = 100) +
  facet_grid(~key)

plot.data <- data.frame(value = before - after)
ggplot(plot.data, aes(value)) +
  geom_histogram(color = 'black', fill = 'white', binwidth = 30)
nrow(plot.data)

shapiro.test(before - after)

rank(before-after)

wilcox.test(before, after, paired = T)
