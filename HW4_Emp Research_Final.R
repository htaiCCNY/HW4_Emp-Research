## HW 04 
## Holli Tai & Joaquin Sanchez Gomez

## Question 1


library(tidyverse)
data("anscombe")
attach(anscombe)
print(anscombe)

anscombe_pivot <- anscombe%>%
  pivot_longer(cols=c(x1, x2, x3, x4, y1,y2,y3,y4))
anscombe_pivot

sd <- anscombe_pivot %>%
  group_by(name) %>%
  summarise(sd_value = sd(value))

mean <- anscombe_pivot %>%
  group_by(name) %>%
  summarise(mean_value = mean(value))

mean
sd

lm(y1 ~ x1, data = anscombe)
lm(y2 ~ x2, data = anscombe)
lm(y3 ~ x3, data = anscombe)  
lm(y4 ~x4, data= anscombe)

## There is not much deviation of coeffecients, for the intercept or x values. 
## There is not much variation in the data. There are no extreme values that would seem to skew distribution.

## Question 2

library(ggplot2)

lm1 <- lm(y1 ~ x1, data = anscombe)
lm2 <- lm(y2 ~ x2, data = anscombe)
lm3 <- lm(y3 ~ x3, data = anscombe)  
lm4 <- lm(y4 ~x4, data= anscombe)

xs<- anscombe[1:4]
xs


ys <- anscombe[5:8]
ys



ggplot1 <- ggplot(data, aes(xs, ys)) +
  geom_point( )+ stat_smooth(method = "lm", 
                  formula = y ~x, geom ="smooth")



ggplot(data = anscombe, aes(x= xs,
                           y= ys)) +
            geom_point(color = 'forestgreen',
             size = 2) +
  theme_bw() + 
  ggtitle("This is Our beautiful Plot :)") + 
  xlab('X')+
  ylab('Y')
