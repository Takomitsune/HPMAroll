library(dplyr)
library(ggplot2)


df <- read.csv("prob.csv") %>% 
  mutate(y1 = (x - y) /n)

df1 <- df %>% 
  filter(Type == 1)

l1 <- lm(data = df1, formula = y1 ~ I(x/n) +0 )
s1 <-  summary(l1)
s1
df1 <- data.frame(df1, yp = predict(l1, data = df1$x/df1$n))


ggplot(df1, aes(x = x, y = y1)) +
  geom_point() + 
  geom_line(aes(y = yp, col = "拟合")) +
  labs(
    x = "待分配概率 /%",
    y = "非稀有物品概率 /%",
    col = ""
  ) + 
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_blank()
  )
ggsave("1类拟合.png", path = "./plot/hpma")

df2 <- df %>% 
  filter(Type == 2)

l2 <- lm(data = df2, formula = y1 ~ I(x/n) +0 )
s2 <-  summary(l2)
s2
df2 <- data.frame(df2, yp = predict(l2, data = df2$x/df2$n))


ggplot(df2, aes(x = x, y = y1)) +
  geom_point() + 
  geom_line(aes(y = yp, col = "拟合")) +
  labs(
    x = "待分配概率 /%",
    y = "非稀有物品概率 /%",
    col = ""
  ) + 
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_blank()
  )
ggsave("2类拟合.png", path = "./plot/hpma")


df3 <- df %>% 
  filter(Type == 3)

l3 <- lm(data = df3, formula = y1 ~ I(x/n) +0 )
s3 <-  summary(l3)
s3
df3 <- data.frame(df3, yp = predict(l3, data = df3$x/df3$n))


ggplot(df3, aes(x = x, y = y1)) +
  geom_point() + 
  geom_line(aes(y = yp, col = "拟合")) +
  labs(
    x = "待分配概率 /%",
    y = "非稀有物品概率率 /%",
    col = ""
  ) + 
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_blank()
  )
ggsave("3类拟合.png", path = "./plot/hpma")



df0 <- df %>% 
  filter(Type == 1)
l0 <- lm(data = df0, formula = y ~ x +0 )
s0 <-  summary(l0)

df0 <- data.frame(df0, yp = predict(l0, data = df$x))


p1 <- ggplot(df0, aes(x = x, y = y)) +
  geom_point() + 
  geom_line(aes(y = yp, col = "拟合")) +
  labs(
    x = "待分配概率 /%",
    y = "稀有物品概率 /%",
    col = ""
  ) + 
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_blank()
  )

ggsave("稀有拟合.png", path = "./plot/hpma")
