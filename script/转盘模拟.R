library(ggplot2)
library(dplyr)
library(patchwork)

rm(list = ls())

change_prob <- function(p0, index){
  n <- sum(p0>0) - 1
  if(index <= 3)
    px <- p0[index] * 0.819244 
  else if(index == 4 | index == 6)
    px <- p0[index] * 0.699842 
  else
    px <- p0[index] * 0.49950
  p0[p0!=0] <- p0[p0!=0] + px / n
  p0[index] <- 0
  return(p0)
}

get_p <- function(p0){
  p <- rep(1,6)
  p[1] <- p0[1]
  for(i in 2:6){
    p[i] <- p0[i] + p[i-1]
  }
  return(p)
}

lottery <- function(p){
  x <- runif(1)
  for(i in 1:6)
    if(x < p[i])
      return(i)
  return(7)
}
  
  
itemprob0 <- c(0.15,0.15,0.15,0.18,0.18,0.18)
itemNo0 <- c(8, 5, 6, 8, 8, 22) #4振奋5晶石
itemType0 <- 6
p0 <- get_p(itemprob0)

set.seed(114514)
N <- 1e7
res <- rep(0,N)
res1 <- rep(0,N)
res2 <- rep(0,N)
for(i in 1:N){
  k <- 0
  p <- p0
  prob <- itemprob0
  itemn <- itemNo0
  itemType <- itemType0
  n <- 0
  while(1){
    n <- n + 1
    x <- lottery(p)
    if(x < 7){
      in1 <- itemn[x] - 1
      if(in1 == 0){
        if(itemType <= (3-k)){
          in1 <- 1
          k <- k + 1
          if(k == 1)
            res1[i] <- n
          if(k == 2)
            res2[i] <- n
          if(k==3)break
        }else{
          prob <- change_prob(prob, x)
          p <- get_p(prob)
          itemType <- itemType - 1
        }
      }
      itemn[x] <- in1
    }else{
      k <- k + 1
      if(k == 1)
        res1[i] <- n
      if(k == 2)
        res2[i] <- n
      if(k==3)break
    }
  }
  res[i] <- n
}

sr <- summary(res)
sr1 <- summary(res1)
sr2 <- summary(res2)

df <- data.frame(N = res)
df1 <- data.frame(N = res1)
df2 <- data.frame(N = res2)

p1 <- ggplot(df, aes(x = N)) + 
  stat_ecdf() + 
  scale_x_continuous(minor_breaks = 0:60, breaks = seq(0, 60,5)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.05), labels = paste(seq(0,100,5), "%")) +
  geom_hline(yintercept = c(0.25,0.75)) +
  geom_vline(xintercept = c(sr[2], sr[5])) +
  labs(x = "抽取次数N",
       y = "N次以内清空的概率")


ggsave(plot = p1, filename = "转盘模拟.png", path = "./plot/hpma")

p2 <- ggplot() +             
  geom_boxplot(data = df, aes(y = N, x = "全部清空", col = "全部清空")) +
  geom_boxplot(data = df1, aes(y = N, x = "第一个稀有物品", col = "第一个稀有物品")) +
  geom_boxplot(data = df2, aes(y = N, x = "第二个稀有物品", col = "第二个稀有物品")) +
  scale_y_continuous(minor_breaks = 0:60, breaks = seq(0, 60,5)) +
  labs(
    x = "",
    y = "抽取次数N",
    col = ""
  )
p2

ggsave(plot = p2, filename = "转盘模拟2.png", path = "./plot/hpma")

df.s <- data.frame(df, l1 = 1) %>% 
  group_by(N) %>% 
  summarise(
    count = sum(l1),
    prob = sum(l1)/1e5
  )
df.s$acc <- 0
df.s$acc[1] = df.s$prob[1]
for(i in 2:nrow(df.s)){
  df.s$acc[i] <- df.s$acc[i-1] + df.s$prob[i]
}
write.csv(df.s, file = "转盘概率分布.csv")
