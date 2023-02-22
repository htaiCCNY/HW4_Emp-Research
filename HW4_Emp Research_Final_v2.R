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
## In any case there is a positive relationship between x and y.
#  It means that for every change in X, Y increases in a certain amount


## Question 2

library(ggplot2)

lm1 <- lm(y1 ~ x1, data = anscombe)
lm2 <- lm(y2 ~ x2, data = anscombe)
lm3 <- lm(y3 ~ x3, data = anscombe)  
lm4 <- lm(y4 ~x4, data= anscombe)


ggplot(data = lm1, aes(x= x1,
                           y= y1)) +
            geom_point(color = 'forestgreen',
             size = 2) +
  theme_bw() + 
  ggtitle("This is Our beautiful Plot for LM1 :)") + 
  xlab('X')+
  ylab('Y')

ggplot(data = lm2, aes(x= x2,
                       y= y2)) +
  geom_point(color = 'forestgreen',
             size = 2) +
  theme_bw() + 
  ggtitle("This is Our beautiful Plot for LM2 :)") + 
  xlab('X')+
  ylab('Y')


ggplot(data = lm3, aes(x= x3,
                       y= y3)) +
  geom_point(color = 'forestgreen',
             size = 2) +
  theme_bw() + 
  ggtitle("This is Our beautiful Plot for LM3 :)") + 
  xlab('X')+
  ylab('Y')



ggplot(data = lm4, aes(x= x4,
                       y= y4)) +
  geom_point(color = 'forestgreen',
             size = 2) +
  theme_bw() + 
  ggtitle("This is Our beautiful Plot for LM4 :)") + 
  xlab('X')+
  ylab('Y')



#  It seems that regressions explain some positive relationships, as happen in LM1 and LM3
#  but in LM2 it seems that it has marginal decreasing relationship
# for LM4, X does not affects Y.