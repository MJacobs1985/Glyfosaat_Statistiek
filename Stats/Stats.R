#### CLEAR ENVIRONMENT ----
rm(list = ls())
#### LOAD LIBRARIES ----
library(tidyverse)
library(simstudy)
library(ggpubr)
library(cowplot)
library(rstatix)
library(lessR)
library(grid)
library(lsr)
library(effsize)
library(pwr)
library(MonteCarlo)
library(readxl)
library(DescTools)
library(multcomp)
library(emmeans)
library(drc)
library(pscl)
library(brms)
library(tidybayes)
library(emmeans)
library(bayesplot)
library(bayesrules)
library(rethinking)
library(forcats)
library(lme4)
library(bayestestR)
library(modelr)

#### ZOEK DE VERSCHILLEN ----
set.seed(42);
m<-rnorm(1000, 184, 7)
df1<-data.frame(m)
m<-rnorm(1000, 170.6, 6.3)
df2<-data.frame(m)
df<-rbind(df1,df2)

gghistogram(
  df, x = "m", 
  add = "mean", rug = TRUE, 
  add_density = TRUE,  fill = "lightgray", ggtheme = theme_minimal())+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn gecombineerd")

gghistogram(
  df, x = "m", 
  add = "mean", rug = TRUE, 
  add_density = TRUE,  fill = "lightgray", ggtheme = theme_minimal())+
geom_vline(xintercept=200, col="red", lty=2)+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn gecombineerd")


cutoff <- 200
hist.y <- density(df$m, from = min(df$m), to = max(df$m)) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
the.plot <- ggplot(data = hist.y, aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon(show.legend = FALSE) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Lengte = 200 cm', hjust = -0.1)+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn gecombineerd")
print(the.plot)

hist.y%>%
  select(y)%>%
  summarise (sum = sum(y))
hist.y%>%
  filter(area=="TRUE")%>%
  select(y)%>%
  summarise (sum = sum(y))
hist.y%>%
  filter(area=="FALSE")%>%
  select(y)%>%
  summarise(sum =sum(y))


set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 184, 7)))
gghistogram(wdata, x = "Lengte",bins=40,
            add = "mean", rug = TRUE,
            add.params = list(col="black"),col="black",
            fill = "Geslacht", palette = c("#00AFBB", "#E7B800"),
            add_density = TRUE)+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn apart")

cutoff <- 175
hist.y <- density(wdata$Lengte[wdata$Geslacht =="Man"], 
                  from = min(wdata$Lengte[wdata$Geslacht =="Man"]), 
                  to = max(wdata$Lengte[wdata$Geslacht =="Man"])) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
g1 <- ggplot(data = hist.y, aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon(show.legend = FALSE) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Lengte = 175 cm', hjust = -0.1)+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen", 
       subtitle="Verdeling wordt gesplitst bij 175 cm")
hist.y <- density(wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                  from = min(wdata$Lengte[wdata$Geslacht =="Vrouw"]), 
                  to = max(wdata$Lengte[wdata$Geslacht =="Vrouw"])) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
g2 <- ggplot(data = hist.y, aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon(show.legend = FALSE) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Lengte = 175 cm', hjust = -0.1)+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij vrouwen", 
       subtitle="Verdeling wordt gesplitst bij 175 cm")
gridExtra::grid.arrange(g1,g2,ncol=1)

pnorm(175, 170.6, 6.3)
pnorm(175, 184, 7)

cutoff <- 175
hist.m <- density(wdata$Lengte[wdata$Geslacht =="Man"], 
                  from = min(wdata$Lengte[wdata$Geslacht =="Man"]), 
                  to = max(wdata$Lengte[wdata$Geslacht =="Man"])) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
hist.f <- density(wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                  from = min(wdata$Lengte[wdata$Geslacht =="Vrouw"]), 
                  to = max(wdata$Lengte[wdata$Geslacht =="Vrouw"])) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)

ggplot()+ 
  geom_ribbon(data = hist.m, aes(x = x, ymin = 0, ymax = y, fill = area), show.legend = FALSE, alpha=0.5) +
  geom_line(data = hist.m, aes(x = x, y = y)) +
  geom_ribbon(data = hist.f, aes(x = x, ymin = 0, ymax = y, fill = area), show.legend = FALSE, alpha=0.5) +
  geom_line(data = hist.f, aes(x = x, y = y))+
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Lengte = 175 cm', fontface = "bold", hjust = -0.1, vjust=-35)+
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = paste(c("\u2264", round(pnorm(175, 184, 7)*100,1), "%"), collapse = " "), fontface = "bold", hjust = 1, vjust=18)+
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = paste(c("\u2264", round(pnorm(175, 170.6, 6.3)*100,1), "%"), collapse = " "), fontface = "bold", hjust = 1, vjust=-10)+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen % vrouwen", 
       subtitle="Verdeling wordt gesplitst bij 175 cm")


m<-rnorm(1000, 184, 7)
df1<-data.frame(m)
f<-rnorm(1000, 170.6, 6.3)
df2<-data.frame(f)
df<-cbind(df1,df2)

df%>%
  mutate(diff=m-f)%>%
  gghistogram(., x = "diff",bins=40,
              add = "mean", rug = TRUE,
              add_density = TRUE)+
  geom_vline(xintercept = 0, color = 'red', lty=2) +
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van het verschil in lengte", 
       subtitle="Tussen mannen en vrouwen")


set.seed(42);
m<-rnorm(1000, 184, 7)
df1<-data.frame(m)
m<-rnorm(1000, 170.6, 6.3)
df2<-data.frame(m)
df<-rbind(df1,df2)
df<-df%>%mutate(diff=m-f)%>%select(diff)
mean(df$diff)

cutoff <- 0
hist.y <- density(df$diff,from = min(df$diff), to = max(df$diff)) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
ggplot(data = hist.y, aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon(show.legend = FALSE) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Verschil = 0 cm', hjust = 1.1, vjust = -10)+
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = paste(c("\u2264", round(pnorm(0,mean(df$diff),sd(df$diff))*100,1),"%"), collapse = " "), 
           fontface = "bold", hjust = 1.1, vjust=10, size=8)+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van verschil tussen mannen en vrouwen", 
       subtitle="Hoeveel kleiner dan of gelijk aan 0 verschil")

#### FUNDAMENTEN VAN DE FREQUENTISTISCHE STATISTIEK ----
set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 184, 7)))
cutoff <- 200
hist.y <- density(wdata$Lengte[wdata$Geslacht =="Man"], 
                  from = min(wdata$Lengte[wdata$Geslacht =="Man"]), 
                  to = max(wdata$Lengte[wdata$Geslacht =="Man"])) %$% 
  data.frame(x = x, y = y) %>% 
  mutate(area = x >= cutoff)
ggplot(data = hist.y, aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon(show.legend = FALSE) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'black', lty=2) +
  annotate(geom = 'text', x = cutoff, y = 0.025, color = 'black', label = 'Lengte = 200 cm', hjust = -0.1, size=6)+
  annotate(geom = 'text', x = cutoff, y = 0.025, color = '#00BFC4', label = paste(c("\u2265", round(1-(pnorm(200,184,7)),3),"%"), collapse = " "), 
           fontface = "bold", hjust = -0.1, vjust=13, size=6)+  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen", 
       subtitle="Frequentie van mannen groter of gelijk aan 200 cm")

mean=round(mean(wdata$Lengte[wdata$Geslacht =="Man"]),0)
sd = sd(wdata$Lengte[wdata$Geslacht =="Man"])
min = round(min(wdata$Lengte[wdata$Geslacht =="Man"]),0)
max = round(max(wdata$Lengte[wdata$Geslacht =="Man"]),0)
N=100
sample=200
upper = round(qnorm(0.975, 184, 7))
lower = round(qnorm(0.025, 184, 7))
x <- seq(min,max,length.out = N)
y <- dnorm(x, mean, sd)
df <- data.frame(x=x, y=y)
area <- round(1-(pnorm(sample,mean,sd)),3)
ggplot(df, aes(x=x, y=y)) + geom_line() +
  stat_function(fun = ~dnorm(.x, mean, sd),
                xlim = c(upper, max),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean, sd),
                xlim = c(lower, min),
                geom = "area",
                fill='salmon') +
  scale_x_continuous(breaks=c(min, mean, lower, max, upper)) +
  geom_vline(xintercept = mean, color = 'black', lty=2) +
  geom_vline(xintercept = lower, color = 'red', lty=2) +
  geom_vline(xintercept = upper, color = 'red', lty=2) +
  geom_vline(xintercept = sample, color = 'blue', lty=2) +
  annotate("text", label = paste(area, "%", " (p=", area/N, ")", sep=""), size=3) +
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen", 
       subtitle="Frequentie van mannen groter of gelijk aan 200 cm")

hist(wdata$Lengte[wdata$Geslacht =="Man"])
t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
       mu = 200)
qt(p = .025, df = 999)
qt(p = .975, df = 999)


set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 184, 7)))

t_test_result <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                        wdata$Lengte[wdata$Geslacht =="Vrouw"])
t_test_result


ggboxplot(wdata, x = "Geslacht", y = "Lengte",
               color = "Geslacht", palette = "jco",
               add = "jitter") + 
  stat_compare_means(method = "t.test")+
  theme_bw() +
  labs(y="Lengte (cm)", 
       x="Geslacht", 
       title="Boxplot: Frequentieverdeling van lengte bij mannen en vrouwen", 
       subtitle="Nulhypothese: mannen en vrouwen zijn even lang", 
       caption="De T-test toets of het verschil tussen mannen en vrouwen mogelijk is wanneer het 'echte' verschil nul is")
  
ttest(Lengte ~ Geslacht, data=wdata, paired=FALSE)

x_m = replicate(10000, mean(rnorm(1000, 184, 7)))
x_v = replicate(10000, mean(rnorm(1000, 170.6, 6.3)))
ggplot()+
  geom_density(aes(x=x_m))+
  geom_density(aes(x=x_v))+
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen en vrouwen", 
       subtitle="De frequentieverdeling is er nu van gemiddelden", 
       caption="op basis van 1000 trekkingen met elk 1000 observaties")

df=data.frame(x_m)
ggplot(df, aes(x = x_m)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", nbins=30) +
  geom_density(lwd = 1, colour = 'salmon')+
  geom_vline(xintercept = mean(x_m), color = 'black', lty=1) +
  geom_vline(xintercept = 183.9, color = 'blue', lty=1) +
  geom_vline(xintercept = qnorm(0.975, mean(x_m), sd(x_m)), color = 'salmon', lty=1) +
  geom_vline(xintercept = qnorm(0.025, mean(x_m), sd(x_m)), color = 'salmon', lty=1) +
  stat_function(fun = ~dnorm(.x, mean(x_m), sd(x_m)),
                xlim = c(qnorm(0.975, mean(x_m), sd(x_m)), max(x_m)),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean(x_m), sd(x_m)),
                xlim = c(qnorm(0.025, mean(x_m), sd(x_m)), min(x_m)),
                geom = "area",
                fill='salmon') +
  theme_bw()+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte bij mannen", 
       subtitle="De frequentieverdeling is er nu van gemiddelden", 
       caption="op basis van 10,000 trekkingen met elk 1,000 observaties")

qnorm(0.025, mean(x_m), sd(x_m))
qnorm(0.975, mean(x_m), sd(x_m))  
qnorm(0.025, mean(x_v), sd(x_v))
qnorm(0.975, mean(x_v), sd(x_v))

x_diff = replicate(10000, mean(rnorm(1000, 184, 7))) - replicate(1000, mean(rnorm(1000, 170.6, 6.3)))
df=data.frame(x_diff)
ggplot(df, aes(x = x_diff)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins=100) +
  geom_density(lwd = 1, colour = 'salmon')+
  geom_vline(xintercept = mean(x_diff), color = 'black', lty=1) +
  geom_vline(xintercept = qnorm(0.975, mean(x_diff), sd(x_diff)), color = 'salmon', lty=1) +
  geom_vline(xintercept = qnorm(0.025, mean(x_diff), sd(x_diff)), color = 'salmon', lty=1) +
  stat_function(fun = ~dnorm(.x, mean(x_diff), sd(x_diff)),
                xlim = c(qnorm(0.975, mean(x_diff), sd(x_diff)), max(x_diff)),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean(x_diff), sd(x_diff)),
                xlim = c(qnorm(0.025, mean(x_diff), sd(x_diff)), min(x_diff)),
                geom = "area",
                fill='salmon') +
  theme_bw()+
  labs(x="Verschil in lengte (cm) tussen mannen en vrouwen", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van verschil in lengte tussen mannen en vrouwen", 
       subtitle="De frequentieverdeling is er nu een van gemiddelden", 
       caption="op basis van 10,000 trekkingen met elk 1,000 observaties")
mean(x_diff)
qnorm(0.025, mean(x_diff), sd(x_diff))
qnorm(0.975, mean(x_diff), sd(x_diff))
  


x_diff = replicate(10000, mean(rnorm(1000, 184, (7*20)))) - 
  replicate(10000, mean(rnorm(1000, 170.6, (6.3*20))))
df=data.frame(x_diff)
ggplot(df, aes(x = x_diff)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins=100) +
  geom_density(lwd = 1, colour = 'salmon')+
  geom_vline(xintercept = mean(x_diff), color = 'black', lty=1) +
  geom_vline(xintercept = qnorm(0.975, mean(x_diff), sd(x_diff)), color = 'salmon', lty=1) +
  geom_vline(xintercept = qnorm(0.025, mean(x_diff), sd(x_diff)), color = 'salmon', lty=1) +
  stat_function(fun = ~dnorm(.x, mean(x_diff), sd(x_diff)),
                xlim = c(qnorm(0.975, mean(x_diff), sd(x_diff)), max(x_diff)),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean(x_diff), sd(x_diff)),
                xlim = c(qnorm(0.025, mean(x_diff), sd(x_diff)), min(x_diff)),
                geom = "area",
                fill='salmon') +
  geom_vline(xintercept = 0, color = 'blue', lty=1) +
  theme_bw()+
  labs(x="Verschil in lengte (cm) tussen mannen en vrouwen", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van verschil in lengte tussen mannen en vrouwen", 
       subtitle="De frequentieverdeling is er nu een van gemiddelden", 
       caption="op basis van 10,000 trekkingen met elk 1,000 observaties")

x_diff = replicate(1000, mean(rnorm(1000, 184, (7)))) - 
  replicate(1000, mean(rnorm(1000, 170.6, (6.3))))
qnorm(0.025, mean(x_diff), sd(x_diff))
qnorm(0.975, mean(x_diff), sd(x_diff))



set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=30)),
  Lengte = c(rnorm(30, 170.6, 6.3), rnorm(30, 184, 7)))
ttest(Lengte ~ Geslacht, data=wdata, paired=FALSE)


set.seed(42)
x <- runif(1e6, 0, 1)
sample_size <- 5:120
calc_sigma_m <- function(n, x) {
  sd(sample(x, n, replace = TRUE))/sqrt(n)
}
calc_sigma_m_mean <- function(n, x) {
  mean(replicate(1000, sd(sample(x, n, replace = TRUE))/sqrt(n)))
}        

df <- data.frame(sample_size, sigma_m = sapply(sample_size, calc_sigma_m_mean, x))
df <- data.frame(sample_size, 
                 sigma_m = sapply(sample_size, calc_sigma_m, x), 
                 sigma_m_mean = sapply(sample_size, calc_sigma_m_mean, x))
ggplot(df)+ 
  geom_line(aes(sample_size, sigma_m, col="1")) + 
  geom_line(aes(sample_size, sigma_m_mean, col="1000"), lty=2) +
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Aantal observaties", 
       y="Standaard Deviatie", 
       col="Aantal steekproeven per steekproefgrootte", 
       title="Relatie tussen het aantal observaties en de standaard deviatie", 
       subtitle="De rode lijn is gebasseerd op 1 observatie, de zware lijn het gemiddelde van 1000 observaties")

set.seed(42)
x <- 184
n <- 5:120
calc_mean_m <- function(x,n) {
  mean(rnorm(x, n, 1))
}
calc_mean_m_mean <- function(x,n) {
  mean(replicate(1000, mean(rnorm(x, n, 1))))
}        
df <- data.frame(n, 
                 mean_m = sapply(n, calc_mean_m, x), 
                 mean_m_mean = sapply(n, calc_mean_m_mean, x))
df
ggplot(df)+ 
  geom_line(aes(n, mean_m, col="1")) + 
  geom_line(aes(n, mean_m_mean, col="1000"), lty=2) +
  geom_hline(aes(yintercept = 184, col="Populatie"), lty=2) +
  scale_color_manual(values = c("black", "red", "blue"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Aantal observaties", 
       y="Gemiddelde", 
       col="Aantal steekproeven per steekproefgrootte", 
       title="Relatie tussen het aantal observaties en het gemiddelde", 
       subtitle="De rode lijn is gebasseerd op 1 observatie, de zware lijn het gemiddelde van 1000 observaties")

set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 184, 7)))
cohens_d(Lengte ~ Geslacht, data = wdata)

es.labs <- c("Klein", "Middel", "Groot", "Enorm")
es.d <- c(0.2, 0.5, 0.8, 2.0)
es.ss <- c(1000, 1000, 1000, 1000)
plots <- vector("list", length(es.d))
for ( i in 1:4) {
  m1 <- 170.6; sd <- 6.3; #sample 1 mean and standard deviation
  d <- es.d[i]; #Cohen's d
  m2 <- m1 + d * sd; #sample 2
  n <- es.ss[i]; #sample size (i.e. the number of pairs, or the number in each group)
  df <- data.frame(pop1 = rnorm(n, m1, sd), 
                   pop2 = rnorm(n, m2, sd))
  g <- ggplot (df) + 
    geom_density(aes(x=pop1), color="firebrick", fill="firebrick", alpha=0.4) +
    geom_density(aes(x=pop2), color="steelblue", fill="steelblue", alpha=0.4) + 
    labs(x= paste0("Cohen's d=",d), title=paste(es.labs[i], "effect")) +
    theme_bw()
  plots[[i]] <- g
}
gridExtra::grid.arrange(grobs = plots)



M1  = 184                     # Mean for sample 1
M2  = 170.6                   # Mean for sample 2
S1  =  7                      # Std dev for sample 1
S2  =  6.3                    # Std dev for sample 2
proportion_significant <- function (s, n, d, alpha, beta, returnVector){
  if(missing(alpha)) alpha <- 0.05
  if(missing(beta)) beta   <- 0.2
  if(missing(d)) d <- 2 #Small difference; 0.5=medium; 0.8=large
  if(missing(n)) {
    library(pwr)
    n <- pwr.t.test(d=d, sig.level=alpha, power=1-beta, type="two.sample")$n
  } 
  if(missing(returnVector)){returnVector = FALSE}
  if(missing(s)) s <- 30
  p_value_vector <- c()
  for (i in 1:s) {  #s is the number of studies, e.g. 1000
    #if (i %% 100 if == 0) {cat(i)} else {cat(".")}
    df <- data.frame(a = rnorm(n, m1, sd), b = rnorm (n, m1 + d * sd, sd))
    p_value_vector <- c(p_value_vector, t.test(df$a, df$b)$p.value )
    i <- i+1
  }
  p <- data.frame(p=p_value_vector); head(p)
  p <- p[order(p$p),]
  p <- data.frame(x=1:s,p=p); p
  power_count <- length(subset(p$p, p$p < alpha)); 
  proportion_significant <- power_count/s ; proportion_significant
  if (returnVector == TRUE) {
    return(p[,-1])}
  else {
    return(proportion_significant)
  }
}
proportion_significant(s=100, n=5, d=2) # changes every time

alpha = 0.05
beta = 0.8
n <- 400
df <-  data.frame(x=seq(5, n, 5)) 
df$klein <- sapply(df$x, FUN=function(x){proportion_significant(n, x, 0.2, alpha, beta)})
df$middel <- sapply(df$x, FUN=function(x){proportion_significant(n, x, 0.5, alpha, beta)})
df$groot <- sapply(df$x, FUN=function(x){proportion_significant(n, x, 0.8, alpha, beta)})
df$enorm <- sapply(df$x, FUN=function(x){proportion_significant(n, x, 2, alpha, beta)})
cols<-c("klein" = "darkgray", 
        "middel" = "steelblue", 
        "groot" = "orange", 
        "enorm" = "firebrick")
df%>%
  pivot_longer(!x, 
               names_to = "ES", 
               values_to = "Power")%>%
  mutate(ES=factor(ES))%>%
  ggplot()  + 
  geom_smooth(aes(x=x, y=Power, group=ES, col=ES, fill=ES)) +
  geom_hline(yintercept = beta) + 
  scale_y_continuous(limits = c(0, 1.05), breaks=seq(0,1,0.1)) + 
  scale_x_continuous(limits = c(0, n), breaks=seq(0, n, 25)) + 
  theme_bw()+
  scale_color_manual(name="Effectgrootte", 
                     values = cols)+
  scale_fill_manual(name="Effectgrootte", 
                    values = cols)+
  labs(x="Aantal observaties per groep", 
       y="Power van een studie", 
       col="Effectgrootte",
       title="Power van een studie als functie van de power en de effectgrootte", 
       subtitle="Onder alpha =0.05 en beta=0.2")


n <- 400
k <- 100  # no of simulations
alpha <- 0.05; beta <- 0.8; m1 <- 170; sd <- 6.3; 
pf <- function(d) {
  return(pwr::pwr.t.test(d=d, sig.level=alpha, power=beta, type="two.sample")$n)
}
pf(0.1)
x <- seq (0.2, 2, 0.1) #d values
df <- data.frame(d=x, 
                 n=sapply(x, FUN=function(x){pf(d=x)}))
ggplot(df)  + 
  geom_smooth(aes(x=d, y=n), color="gray") + 
  geom_point(aes(x=d, y=n), color="firebrick4", size=4, alpha=I(2/3)) + 
  geom_hline(yintercept = pf(0.2), lty=2) + 
  geom_hline(yintercept = pf(0.5), lty=2) + 
  geom_hline(yintercept = pf(0.8), lty=2) + 
  geom_hline(yintercept = pf(2), lty=2) + 
  annotate("text", x=0.5, y=pf(0.2)+10, label="Klein effect", size=9, color="darkgray") +
  annotate("text", x=0.5, y=pf(0.5)+10, label="Middel effect", size=9, color="steelblue") +
  annotate("text", x=0.5, y=pf(0.8)+10, label="Groot effect", size=9, color="orange") +
  annotate("text", x=0.5, y=pf(2.0)+10, label="Enorm effect", size=9, color="firebrick") +
  scale_y_continuous(limits = c(0, 410), breaks=seq(0,420,20)) + 
  scale_x_continuous(limits = c(0.2, 2), breaks=seq(0.2, 2, 0.1)) +
  labs(x="Cohen's d", 
       y="Aantal observaties per groep", 
       title="Benodigde steekproefgrootte als functie van Cohens'd", 
       subtitle="Onder alpha =0.05 en beta=0.2") +
  theme_bw()


n <- 400
k <- 100  # no of simulations
alpha <- 0.05; beta <- 0.8; m1 <- 170; sd <- 6.3; 
pf <- function(d) {
  return(pwr::pwr.t.test(d=d, sig.level=alpha, power=beta, type="two.sample")$n)
}
pf(0.1)
x <- seq (0.2, 2, 0.1) #d values

df <- data.frame(d=x, 
                 n=sapply(x, FUN=function(x){pf(d=x)}))
ggplot(df)  + 
  geom_smooth(aes(x=d, y=n), color="gray") + 
  geom_point(aes(x=d, y=n), color="firebrick4", size=4, alpha=I(2/3)) + 
  geom_hline(yintercept = pf(0.2), lty=2) + 
  geom_hline(yintercept = pf(0.5), lty=2) + 
  geom_hline(yintercept = pf(0.8), lty=2) + 
  geom_hline(yintercept = pf(2), lty=2) + 
  annotate("text", x=0.5, y=pf(0.2)+10, label="Klein effect", size=9, color="darkgray") +
  annotate("text", x=0.5, y=pf(0.5)+10, label="Middel effect", size=9, color="steelblue") +
  annotate("text", x=0.5, y=pf(0.8)+10, label="Groot effect", size=9, color="orange") +
  annotate("text", x=0.5, y=pf(2.0)+10, label="Enorm effect", size=9, color="firebrick") +
  scale_y_continuous(limits = c(0, 410), breaks=seq(0,420,20)) + 
  scale_x_continuous(limits = c(0.2, 2), breaks=seq(0.2, 2, 0.1)) +
  labs(x="Cohen's d", 
       y="Aantal observaties per groep", 
       title="Benodigde steekproefgrootte als functie van Cohens'd", 
       subtitle="Onder alpha =0.05 en beta=0.2") +
  theme_bw()


df<-tibble(id=1)%>%
  crossing(.,n = 2:100)%>%
  crossing(.,sim = 1:100)%>%
  group_by(n, sim)%>%
  reframe(Vrouwen = rnorm(n, 170.6,6.3),
          Mannen = rnorm(n, 184,7))%>%
  pivot_longer(!c(n,sim), names_to = "Geslacht", values_to = "Lengte")%>%
  group_by(n, sim)%>%
  nest()%>%
  mutate(models = map(data, ~lm(Lengte ~ Geslacht, data = .x)))%>%
  mutate(anova = map(models, car::Anova),
         anova = map(anova, tidy))%>%
  unnest(anova) %>%
  filter(term == "Geslacht") %>%
  select(n, sim, p.value)
df %>%
  group_by(n) %>%
  summarize(beta = sum(p.value > 0.05)/n(),
            power = 1-beta) %>%
  ggplot(data = ,
       mapping = aes(x = n, y = power)) +
  geom_point() +
  geom_line()
df%>%
  crossing(tibble(alpha = seq(0.01, 0.2, length.out = 10))) %>%
  group_by(n, alpha) %>%
  summarize(beta = sum(p.value > alpha)/n(),
            power = 1-beta) %>%
  ggplot(data = .)+
  geom_tile(aes(x=n, y=alpha, fill=power))+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Aantal observaties per groep", 
        y="\u03b1 waarde", 
        color="1 - \u03b2", 
       title="Power (1 - \u03b2) van een studie als functie van de \u03b1 waarde", 
       subtitle ="alsmede het aantal observaties per groep bij een effectgrootte van 2")


set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 173, 7)))
cohens_d(Lengte ~ Geslacht, data = wdata)
df<-tibble(id=1)%>%
  crossing(.,n = 2:100)%>%
  crossing(.,sim = 1:1000)%>%
  group_by(n, sim)%>%
  reframe(Vrouwen = rnorm(n, 170.6,6.3),
          Mannen = rnorm(n, 173,7))%>%
  pivot_longer(!c(n,sim), names_to = "Geslacht", values_to = "Lengte")%>%
  group_by(n, sim)%>%
  nest()%>%
  mutate(models = map(data, ~lm(Lengte ~ Geslacht, data = .x)))%>%
  mutate(anova = map(models, car::Anova),
         anova = map(anova, tidy))%>%
  unnest(anova) %>%
  filter(term == "Geslacht") %>%
  select(n, sim, p.value)
df %>%
  group_by(n) %>%
  summarize(beta = sum(p.value > 0.05)/n(),
            power = 1-beta) %>%
  ggplot(data = ,
         mapping = aes(x = n, y = power)) +
  geom_point() +
  geom_line()
df%>%
  crossing(tibble(alpha = seq(0.01, 0.2, length.out = 10))) %>%
  group_by(n, alpha) %>%
  summarize(beta = sum(p.value > alpha)/n(),
            power = 1-beta) %>%
  ggplot(data = .)+
  geom_tile(aes(x=n, y=alpha, fill=power))+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x="Aantal observaties per groep", 
       y="\u03b1 waarde", 
       color="1 - \u03b2", 
       title="Power (1 - \u03b2) van een studie als functie van de \u03b1 waarde", 
       subtitle ="alsmede het aantal observaties per groep bij een effectgrootte van 0.38")

df%>%
  crossing(tibble(alpha = seq(0.01, 0.2, length.out = 1000))) %>%
  group_by(n, alpha) %>%
  summarize(beta = sum(p.value > alpha)/n(),
            power = 1-beta) %>%
  ggplot(data = .)+
  geom_line(aes(x=alpha, y=beta, group=n, col=power))+
  geom_hline(yintercept = 0.2, lty=2, col="black")+
  geom_vline(xintercept = 0.05, lty=2, col="black")+
  scale_colour_viridis_c(option="turbo")+
  theme_bw()+
  labs(group="Aantal observaties per groep", 
       x="\u03b1 waarde", 
       col="1 - \u03b2", 
       y="\u03b2",
       title="Power (1 - \u03b2) van een studie als functie van de \u03b1 en \u03b2 waarde", 
       subtitle ="alsmede het aantal observaties per groep bij een effectgrootte van 0.38")

set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 126), rnorm(1000, 184, 140)))

t_test_result <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                        wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                        conf.level = 0.99)
t_test_result

set.seed(42)
samples <- plyr::rlply(100, rnorm(5000, mean = 0, sd = 1))
hist(samples[[2]])
info <- function(x){ 
  M <- mean(x)
  DF <- length(x) - 1
  SE <- 1 / sqrt(length(x))
  INT <- qt(.975, DF) * SE
  return(c(M, M - 0, SE, M - INT, M + INT))
}
format <- c("Mean" = 0, "Bias" = 0, "Std.Err" = 0, "Lower" = 0, "Upper" = 0)
results <- samples %>%
  vapply(., info, format) %>%
  t() %>%
  as.data.frame() %>%
  mutate(Covered = Lower < 0 & 0 < Upper)%>%
  mutate(Covered = as.integer(Covered))%>%
  mutate(Covered = if_else(Covered == 0, "Nee", "Ja"))
colMeans(results)
limits <- aes(ymax = results$Upper, ymin = results$Lower)
cols<-c("Ja" = "darkgreen",
        "Nee" = "firebrick")
ggplot(results, aes(y=Mean, x=1:100, colour = Covered)) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 2) + 
  geom_pointrange(limits) +
  labs(x="Simulaties 1-100",
       y="Gemiddelde and 95% Betrouwbaarheidsinterval",
       title="Aantal keer dat we een verschil zien als er geen verschil is",
       subtitle="op basis van \u03b1 = 5%")+
  scale_color_manual(name="Dekking", 
                     values = cols)+
  theme_bw()


p <- 0.5  
prop.head <- c()
num.head <- c()
coin.result <- list()
for (i in 1:1000) {
  coin.result[[i]] = rbinom(i, size = 1, prob = p)
  num.head[i] <- sum(coin.result[[i]])
  prop.head[i] <- num.head[i]/i
}
plot(1:1000, prop.head, type = "l", xlab = "Aantal keer gooien", ylab = "proportie 'kop'", 
     main = "Simulaties van munt opgooien")
abline(a = p, b = 0, col = "red")


library(pwr)
mde <- 0.1  # minimum detectable effect
cr_a <- 0.25 # the expected outcome for group A
alpha <- 0.05 # the false positive rate
power <- 0.80 # 1-false negative rate
ptpt <- pwr.2p.test(h = ES.h(p1 = cr_a, p2 = (1+mde)*cr_a), 
                    sig.level = alpha, 
                    power = power)
n_obs <- ceiling(ptpt$n)
set.seed(2)
# make our "true" effect equal to the mde
effect <- mde
cr_b <- (1+effect)*cr_a
observations <- 2*n_obs
# a sequence of {0,1} conversions
conversions_a <- rbinom(observations, 1, cr_a)
conversions_b <- rbinom(observations, 1, cr_b)
table_ca<-table(conversions_a)[2]/(table(conversions_a)[1]+table(conversions_a)[2])
table_cb<-table(conversions_b)[2]/(table(conversions_b)[1]+table(conversions_b)[2])
table_ca-table_cb
# Calculate p-values at each simultaneous observation of the a and b groups
tt <- sapply(10:observations, function(x){
  prop.test(c(sum(conversions_a[1:x]),sum(conversions_b[1:x])), c(x,x))$p.value
})
tt <- data.frame(p.value = unlist(tt))
# for plots
conf_95 <- data.frame( x = c(-Inf, Inf), y = 0.95 )
obs_limit_line <- data.frame( x = n_obs, y = c(-Inf, Inf) )
# plot the evolution of p-value over time, if "peeking"
ggplot(tt, aes(x=seq_along(p.value), y=1-p.value)) + 
  geom_line() + 
  geom_line(aes(x, y, color="\u03b1 = 5%"), linetype=3, conf_95) + 
  geom_line(aes(x, y, color="Einde studie"), linetype=4, obs_limit_line) +
  labs(x="Observaties", 
      y="1 - \u03c1-waarde",
      title="Aantal observaties wat nodig is om de geschatte \u03b1 waarde te halen")+
  scale_color_discrete(name = "Legenda") +
  theme_bw()+
  theme(legend.position="bottom")+
  ylim(c(0,1))

monte_carlo <- function(n_simulations, callback, ...){
  simulations <- 1:n_simulations
  sapply(1:n_simulations, function(x){
    callback(...)
  })
}
reject_at_i_beta <- function(observations, i){
  conversions_a <- rbinom(observations, 1, cr_a)
  conversions_b <- rbinom(observations, 1, cr_b)
  ( prop.test(c(sum(conversions_a[1:i]),sum(conversions_b[1:i])), c(i,i))$p.value ) < alpha
}
rejected.H0 <- monte_carlo(1000, 
                           callback=reject_at_i_beta,
                           observations=n_obs,
                           i=n_obs)
table(rejected.H0)
table(rejected.H0)[2]/(table(rejected.H0)[1]+table(rejected.H0)[2])
reject_at_i_alpha <- function(observations, i){
  conversions_a <- rbinom(observations, 1, cr_a)
  conversions_b <- rbinom(observations, 1, cr_a) # this is now the same conversion rate
  ( prop.test(c(sum(conversions_a[1:i]),sum(conversions_b[1:i])),c(i,i))$p.value ) < alpha
  
}
rejected.H0 <- monte_carlo(1000, 
                           callback=reject_at_i_alpha,
                           observations=n_obs,
                           i=n_obs
)
table(rejected.H0)
table(rejected.H0)[2]/(table(rejected.H0)[1]+table(rejected.H0)[2])

peeking_method <- function (observations, by=1){
  conversions_a <- rbinom(observations, 1, cr_a)
  conversions_b <- rbinom(observations, 1, cr_a)  # no effect
  reject <- FALSE;
  for (i in seq(from=by,to=observations,by=by)) {
    tryCatch(
      {
        reject <- ( prop.test(c(sum(conversions_a[1:i]),sum(conversions_b[1:i])), c(i,i))$p.value ) < alpha
        if(reject){
          break;
        }
      }, error=function(e){
        print(e)
      }
    )
  }
  reject
}
rejected.H0 <- monte_carlo(1000,
                           callback=peeking_method,
                           observations=n_obs,
                           by=100
)
table(rejected.H0)

a=0.01
1-((1-a)^20)


wdata = data.frame(Lengte = 
                     c(rnorm(1, 165.4, 6.12), 
                       rnorm(1, 168.5, 6.23),
             rnorm(1, 170.7, 6.32), 
             rnorm(1, 169.3, 6.26), 
             rnorm(1, 175.6, 6.67), 
             rnorm(1, 181.7, 6.90),
             rnorm(1, 183.9, 6.99), 
             rnorm(1, 182.9, 6.95)))


set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=4000)),
  Jaar = factor(rep(c("1930", "1960", "1980", "2001"), each=1000)),
  Lengte = c(rnorm(1000, 165.4, 6.12), 
             rnorm(1000, 168.5, 6.23),
             rnorm(1000, 170.7, 6.32), 
             rnorm(1000, 169.3, 6.26), 
             rnorm(1000, 175.6, 6.67), 
             rnorm(1000, 181.7, 6.90),
             rnorm(1000, 183.9, 6.99), 
             rnorm(1000, 182.9, 6.95)))
wdata%>%
  group_by(Geslacht, Jaar)%>%
  summarise(mean(Lengte))
gghistogram(wdata, x = "Lengte",bins=40,
            add = "mean", rug = TRUE,
            add.params = list(col="black"),col="black",
            fill = "Geslacht", palette = c("#00AFBB", "#E7B800"),
            add_density = TRUE)+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn apart")
gghistogram(wdata, x = "Lengte",bins=40,
            add = "mean", rug = TRUE,
            add.params = list(col="black"),col="black",
            fill = "Jaar",
            add_density = TRUE)+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over jaren zijn apart")
gghistogram(wdata, x = "Lengte",bins=40,
            add = "mean", rug = TRUE,
            add.params = list(col="black"),col="black",
            fill = "Geslacht", palette = c("#00AFBB", "#E7B800"),
            add_density = TRUE, 
            facet.by = "Jaar")+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn apart per jaar")
gghistogram(wdata, x = "Lengte",bins=40,
            add = "mean", rug = TRUE,
            add.params = list(col="black"),col="black",
            fill = "Jaar", 
            add_density = TRUE, 
            facet.by = "Geslacht")+
  labs(x="Lengte (cm)", 
       y="Dichtheid (%)", 
       title="Frequentieverdeling van lengte", 
       subtitle="Gegevens over mannen en vrouwen zijn apart per jaar")

my_comparisons_jaar <- list(c("1930", "1960"), 
                        c("1930", "1980"),
                        c("1930", "2001"), 
                        c("1960", "1980"),
                        c("1960", "2001"),
                        c("1980", "2001"))
my_comparisons_geslacht <- list(c("Man", "Vrouw"))
g1<-ggboxplot(wdata, x="Jaar", 
          y="Lengte", 
          facet.by = "Geslacht", 
          col="Geslacht") + 
  stat_compare_means(comparisons = my_comparisons_jaar)+ 
  stat_compare_means(label.y = 250) + 
  stat_compare_means(method = "anova", label.y = 245)
g2<-ggboxplot(wdata, x="Geslacht", 
          y="Lengte", 
          facet.by = "Jaar", 
          col="Jaar") + 
  stat_compare_means(comparisons = my_comparisons_geslacht)+ 
  stat_compare_means(label.y = 235) + 
  stat_compare_means(method = "anova", label.y = 245)
gridExtra::grid.arrange(g1,g2,ncol=1)



model1<-aov(Lengte~Geslacht*Jaar, data=wdata)
anova(model1)
summary(model1)
confint(model1)
TukeyHSD(model1)
pairwise.t.test(wdata$Lengte, wdata$Geslacht, p.adjust.method ="none")
pairwise.t.test(wdata$Lengte, wdata$Geslacht,p.adjust.method ="BH")
pairwise.t.test(wdata$Lengte, wdata$Jaar, p.adjust.method ="none")
pairwise.t.test(wdata$Lengte, wdata$Jaar,p.adjust.method ="BH")

boxplot(Lengte~Geslacht*Jaar,data=wdata)
glht_geslacht <-glht(model1, mcp(Geslacht="Tukey"))
glht_jaar     <-glht(model1, mcp(Jaar="Tukey"),vcov = sandwich)
confint(glht_geslacht)
confint(glht_jaar)
plot(glht_geslacht)
plot(glht_jaar)

model2<-lm(Lengte~Geslacht*Jaar, data=wdata)
summary(model2)
summary(model2)$coefficients[,4]
K <- diag(length(coef(model2)))[-1,]
rownames(K) <- names(coef(model2))[-1]
plot(confint(glht(model2,linfct = K)))

K1 <- glht(model2, mcp(Geslacht = "Tukey"))$linfc
K2 <- glht(model2, mcp(Jaar = "Tukey"))$linfct
summary(glht(model2, linfct = rbind(K1, K2)))

tmp <- expand.grid(Geslacht = unique(wdata$Geslacht),
                    Jaar = unique(wdata$Jaar))
X <- model.matrix(~ Geslacht * Jaar, data = tmp)
glht(model2, linfct = X)
Tukey <- contrMat(table(wdata$Jaar), "Tukey")
K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K1) <- paste(levels(wdata$Geslacht)[1], rownames(K1), sep = ":")
K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
rownames(K2) <- paste(levels(wdata$Geslacht)[2], rownames(K2), sep = ":")
K <- rbind(K1, K2)
colnames(K) <- c(colnames(Tukey), colnames(Tukey))
summary(glht(model2, linfct = K %*% X))

wdata$tw <- with(wdata, interaction(Geslacht, Jaar))
cell <- lm(Lengte ~ tw - 1, data = wdata)
summary(glht(cell, linfct = K))


summary (model2)
ref_grid(model2)

plot(emmeans(model2, ~ Jaar | Geslacht))
emmip(model2, ~ Jaar | Geslacht, CIs = TRUE, type = "response", style="factor") +
  geom_point(aes(x = Jaar, y = Lengte), data = wdata, pch = 2, color = "blue")

emmeans(model2, pairwise ~ Jaar | Geslacht, adjust = "none")
emmeans(model2, pairwise ~ Jaar | Geslacht, adjust = "Tukey")
pairs(emmeans(model2, ~ Jaar | Geslacht))
pairs(emmeans(model2, ~ Geslacht | Jaar))
test(pairs(emmeans(model2, ~ Jaar | Geslacht)), by=NULL, adjust="Bonferroni")
test(pairs(emmeans(model2, ~ Jaar | Geslacht)), adjust="tukey", cross.adjust="Bonferroni")
emmeans(model2, pairwise ~ Jaar | Geslacht, adjust = "bonferroni")
emmeans(model2, pairwise ~ Jaar | Geslacht, adjust = "holm")
emmeans(model2, pairwise ~ Jaar | Geslacht, adjust = "fdr")

df.cells <- emmeans(model2, ~ Jaar * Geslacht)
pwpp(df.cells, type = "response", by="Jaar")
pwpp(df.cells, type = "response", by="Geslacht")

p<-summary(pairs(emmeans(model2, ~ Jaar | Geslacht), adjust="none"))$p.value

round(p.adjust(p, "BH"), 3)
round(p.adjust(p, "holm"), 3)
round(p.adjust(p, "fdr"), 3)

p.adjust.M <- p.adjust.methods[p.adjust.methods != "fdr"]
p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(p, meth))
p.adj.60 <- sapply(p.adjust.M, function(meth) p.adjust(p, meth, n = 60))
stopifnot(identical(p.adj[,"none"], p), p.adj <= p.adj.60)
round(p.adj, 3)
p.adj%>%
  as.data.frame()%>%
  cbind(test(pairs(emmeans(model2, ~ Jaar | Geslacht)), by=NULL, adjust="Bonferroni")[1:2])%>%
  pivot_longer(-c(Geslacht, contrast), names_to = "Method", values_to = "pvalue")%>%
  ggplot()+
  geom_tile(aes(x=Method, y=contrast, fill=pvalue))+
  facet_grid(~Geslacht)+
  scale_fill_viridis_c(option="turbo")+
  theme_bw()+
  labs(x = "Correctie methode", 
       y="Vergelijking", 
       fill="\u03c1-waarde", 
       title="\u03c1-waarde als gevolg van wel of niet corrigeren voor meerdere testen")
  









df <- data.frame(country=c("a", "b", "c"), 
                 mean=c(1, 10, 100), 
                 sd=c(1, 2, 10))



A <- rerun(.n = 1000, rnorm(n = 100, mean = 0, sd = 1))
B <- rerun(.n = 1000, rnorm(n = 100, mean = 0, sd = 1))
C <- rerun(.n = 1000, rnorm(n = 100, mean = 0, sd = 1))
triple_t <- function(x,y,z) {
  t1 <- t.test(x,y)
  t2 <- t.test(x,z)
  t3 <- t.test(y,z)
  return(c(t1$p.value, t2$p.value, t3$p.value))
}
test2 <- pmap(list(A, B, C), triple_t) %>% lapply(function(z) min(z) < 0.05)
mean(unlist(test2))























mu_ <- 0
sd_ <- 1
max_sample_size = 250
repeats = 10
results <- NULL
upper_and_lower <- NULL
for (sample_size_ in 3:max_sample_size) {
  for (sample_number_ in 1:repeats) {
    sample_ <- rnorm(sample_size_, mean=mu_, sd=sd_)
    ci_test <- t.test(sample_, conf.level=0.1)
    df <- data.frame(
      sample_size = sample_size_
      , low_ci = ci_test$conf.int[[1]] 
      , up_ci = ci_test$conf.int[[2]]
      , mean = mean(sample_)
    )
    results <- rbind(results, df)
  }
  df <- data.frame(
    sample_size = sample_size_
    , upper95 = mu_ + (qnorm((1-0.95)/2) * (sd_/sqrt(sample_size_)))
    , lower95 = mu_ - (qnorm((1-0.95)/2) * (sd_/sqrt(sample_size_)))
    , upper99 = mu_ + (qnorm((1-0.99)/2) * (sd_/sqrt(sample_size_)))
    , lower99 = mu_ - (qnorm((1-0.99)/2) * (sd_/sqrt(sample_size_)))
    , upper90 = mu_ + (qnorm((1-0.90)/2) * (sd_/sqrt(sample_size_)))
    , lower90 = mu_ - (qnorm((1-0.90)/2) * (sd_/sqrt(sample_size_)))
    , upper80 = mu_ + (qnorm((1-0.80)/2) * (sd_/sqrt(sample_size_)))
    , lower80 = mu_ - (qnorm((1-0.80)/2) * (sd_/sqrt(sample_size_)))
  )
  upper_and_lower <- rbind(upper_and_lower, df)
}
cols<-c("80%" = "blue",
        "90%" = "purple", 
        "95%" = "magenta",
        "99%" = "red")
ggplot() +
  geom_hline(yintercept = 0, col="red", lty=2)+
  geom_point(data=results, aes(y = mean, x = sample_size), alpha=0.5) +
  geom_line(data=upper_and_lower, aes(y = upper95, x = sample_size, col="95%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = lower95, x = sample_size, col="95%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = upper99, x = sample_size, col="99%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = lower99, x = sample_size, col="99%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = upper90, x = sample_size, col="90%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = lower90, x = sample_size, col="90%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = upper80, x = sample_size, col="80%"), size=1, lty=2) +
  geom_line(data=upper_and_lower, aes(y = lower80, x = sample_size, col="80%"), size=1, lty=2) +
  scale_color_manual(name="Dekkingsgraad", 
                     values = cols)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Aantal observaties per steekproef", 
       y="Gemiddelde van elke steekproef", 
       title="Dekkingsgraad als functie van de \u03b1 waarde")


set.seed(42)
mu = 0
sigma = 1
n.obs = 10
n.sim = 1000000
sim.means = rep(0,n.sim)
mu.lower = rep(0,n.sim)
mu.upper = rep(0,n.sim)
qt.975 = qt(0.975, n.obs-1)
for (sim in 1:n.sim) {
  sim.ret = rnorm(n.obs,mean=mu,sd=sigma)
  sim.means[sim] = mean(sim.ret)
  se.muhat = sd(sim.ret)/sqrt(n.obs)
  mu.lower[sim] = sim.means[sim]-qt.975*se.muhat
  mu.upper[sim] = sim.means[sim]+qt.975*se.muhat
}
in.interval.mu = (mu >= mu.lower) & (mu <= mu.upper)
ans = rbind(c(sum(in.interval.mu)/n.sim,0.95))
colnames(ans) = c("MC Coverage", "True Coverage")
rownames(ans) = c("Mu")
ans


#### EENZIJDIG EN TWEEZIJDIG TOETSEN ----
set.seed(123)
group.1 <- rnorm(n = 1000,mean = 184,sd = 7)
group.2 <- rnorm(n = 1000,mean = 170.6,sd = 6.3)
df <- data.frame(Man = group.1,Vrouw = group.2)
p1 <- df %>% 
  gather() %>% 
  ggplot(aes(x=value,
             fill=key))+
  geom_density(alpha = .5,
               linewidth = 1)+
  theme_bw()+
  scale_fill_manual(values = c("black","white"))+
  geom_vline(aes(xintercept = mean(group.1)),
             color = "red",
             linetype = "dashed",
             linewidth = 1)+
  geom_vline(aes(xintercept = mean(group.2)),
             color = "red",
             linetype = "dashed",
             linewidth = 1)+
  geom_vline(aes(xintercept = mean(group.1) + 1.96*sd(group.1)),
             color = "blue",
             linetype = "dashed",
             linewidth = 1)+
  geom_vline(aes(xintercept = mean(group.1) - 1.96*sd(group.1)),
             color = "blue",
             linetype = "dashed",
             linewidth = 1)+
  labs(x="Lengte",
       y = "Dichtheid (%)",
       fill = "Geslacht",
       title = "Vergelijking van lengte tussen groepen", 
       subtitle = "Op basis van tweezijdig toetsen")+
  scale_x_continuous(n.breaks = 10)
p2 <- df %>% 
  gather() %>% 
  ggplot(aes(x=value,
             fill=key))+
  geom_density(alpha = .5,
               linewidth = 1)+
  theme_bw()+
  scale_fill_manual(values = c("black","white"))+
  geom_vline(aes(xintercept = mean(group.1)),
             color = "red",
             linetype = "dashed",
             linewidth = 1)+
  geom_vline(aes(xintercept = mean(group.2)),
             color = "red",
             linetype = "dashed",
             linewidth = 1)+
  geom_vline(aes(xintercept = mean(group.1) + 1.645*sd(group.1)),
             color = "blue",
             linetype = "dashed",
             linewidth = 1)+
  labs(x="Lengte",
       y = "Dichtheid (%)",
       fill = "Geslacht",
       title = "Vergelijking van lengte tussen groepen", 
       subtitle = "Op basis van eenzijdig toetsen")+
  scale_x_continuous(n.breaks = 10)
ggarrange(p1,p2, ncol = 1)

n.obs=1000
a = 0.05
qt(1-(a/2), n.obs-1)
qt(1-a, n.obs-1)



wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 126), rnorm(1000, 184, 140)))

set.seed(42)
t_test_result_2sided <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                        wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                        conf.level = 0.95, 
                        alternative = "two.sided")
t_test_result_less <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                               wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                               conf.level = 0.95, 
                               alternative = "less")
t_test_result_greater <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                               wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                               conf.level = 0.95, 
                               alternative = "greater")
t_test_result_2sided;t_test_result_less;t_test_result_greater


t_test_result_less <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                             wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                             conf.level = 0.975, 
                             alternative = "less")
t_test_result_greater <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                                wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                                conf.level = 0.975, 
                                alternative = "greater")
t_test_result_less;t_test_result_greater


t_test_result_2sided <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                               wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                               conf.level = 0.99, 
                               alternative = "two.sided")
t_test_result_less <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                             wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                             conf.level = 0.99, 
                             alternative = "less")
t_test_result_greater <- t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
                                wdata$Lengte[wdata$Geslacht =="Vrouw"], 
                                conf.level = 0.99, 
                                alternative = "greater")
t_test_result_2sided;t_test_result_less;t_test_result_greater



diff_twosided<-TOSTER::simple_htest(formula = Lengte ~ Geslacht,
       data = wdata,
       mu = 0,
       alternative="two.sided")
diff_twosided
TOSTER::describe_htest(diff_twosided)


tTOST<-TOSTER::t_TOST(formula = Lengte ~ Geslacht,
                    data = wdata,
                    eqb = 1,
                    alpha = 0.05,
                    alternative="two.sided",
                    smd_ci = "goulet")
tTOSTb<-TOSTER::boot_t_TOST(formula = Lengte ~ Geslacht,
                      data = wdata,
                      eqb = 1,
                      alpha = 0.05,
                      alternative="two.sided",
                      smd_ci = "goulet", 
                      R=999)
tTOST;tTOSTb
plot(tTOST, type="cd",
     ci_shades = c(.90,.95,.99))
plot(tTOSTb, type="cd",
     ci_shades = c(.90,.95,.99))
plot(tTOST, type = "c",
     ci_lines =  c(.90,.95,.99))

TOSTER::describe_htest(diff_twosided)

res <- TOSTER::tsum_TOST(m1 = 184, m2 = 170.6, 
                         sd1 = 7, sd2 = 6.3,
                         n1 = 30, n2 = 30, low_eqbound = -0.5, high_eqbound = 0.5)
plot(res, type = "tnull")







#### GLYFOSAAT: BESCHRIJVING EN VISUALISATIE VAN DE DATA ----
df <- read_excel("~/Studie_data.xlsx")
g1<-df%>%
  filter(Studie=="Knezevich and Hogan")%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(prop=(Cases/N)*100, 
         Dosering = factor(Dosering))%>%
  ggplot(.,aes(x=Dosering, y=prop, group=1))+
  geom_point()+
  geom_line() +
  theme_bw() +
  labs(x="Dosering", 
       y="Proportie kankergevallen (totaal)", 
       title="Proportie kanker per dosering ")
g2<-df%>%
  filter(Studie=="Knezevich and Hogan")%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering, Geslacht)%>%
  group_by(Geslacht, Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(prop=(Cases/N)*100, 
         Dosering = factor(Dosering))%>%
  ggplot(.,aes(x=Dosering, y=prop, col=Geslacht, group=Geslacht))+
  geom_point()+
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")+
  labs(x="Dosering", 
       y="Proportie kankergevallen (totaal)", 
       col="Geslacht",
       title="Proportie kanker per dosering en geslacht")
gridExtra::grid.arrange(g1,g2,ncol=1)  
  
df%>%
  dplyr::filter(Studie=="Knezevich and Hogan")%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Geslacht, Dosering, Tumor)%>%
  dplyr::mutate(prop=(Cases/N)*100, 
         Dosering = factor(Dosering))%>%
  ggplot(.)+
  geom_tile(aes(x=Dosering, fill=prop, y=Tumor))+
  facet_grid(~Geslacht, scales="free")+
  theme_bw()+
  scale_fill_viridis_c(option="turbo")+
  labs(x="Dosering", 
       fill="Proportie kankergevallen (totaal)", 
       y="Tumorsoort",
       title="Proportie kanker per tumorsoort per dosering en geslacht")+
  theme(legend.position = "bottom")

Cases  <- c( 1, 0, 1, 3)
N <- c(49, 49, 50, 50)
prop.test(Cases, N)
prop.trend.test(Cases, N)

xtab <- as.table(rbind(
  c(1, 0, 1, 3),
  c(48, 49, 49, 47)
))
dimnames(xtab) <- list(
  cancer = c("yes", "no"),
  dose = c("0", "1", "2", "3")
)
rstatix::prop_trend_test(xtab)

fisher.test(matrix(c(1, 0, 1, 3,  
                     48, 49, 49, 47), nrow = 4))
fisher.test(matrix(c(1, 0, 1, 3,  
                     48, 49, 49, 47), nrow = 4), 
            alternative="less")
fisher.test(matrix(c(1, 0, 1, 3,  
                     48, 49, 49, 47), nrow = 4), 
            alternative="greater") 
fisher.test(matrix(c(1, 0, 1, 3,  
                     48, 49, 49, 47), nrow = 4), 
            alternative="two.sided")
fisher.test(matrix(c(1, 0, 1, 3,  
                     48, 49, 49, 47), nrow = 4), 
            alternative="two.sided", 
            conf.level = 0.99)

dose <- matrix(c(48,1,
                 49,0, 
                 49,1,
                 47,3), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=0:3))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")

xtab <- as.table(rbind(
  c(48, 49, 49, 47),
  c(1, 0, 1, 3)
))
dimnames(xtab) <- list(
  cancer = c(0, 1),
  dose = c("0", "1", "2", "3")
)
Desc(xtab)
CochranArmitageTest(xtab)
CochranArmitageTest(dose)



Cases  <- c( 1, 1, 1, 5)
N      <- c(50, 48, 49, 49)
prop.test(Cases, N)
prop.trend.test(Cases, N)
fisher.test(matrix(c(1, 1, 1, 5,  
                     49, 47, 48, 44), nrow = 4))

df <- data.frame("Kanker" = c(1, 1,1,5), 
                 "Geen_kanker" = c(49, 47, 48, 44), 
                 row.names = c("0", "1", "2", "3"))
df

mosaicplot(df, color = TRUE)  
stats::fisher.test(df)
rstatix::fisher_test(as.matrix(df), detailed = TRUE)
dose <- matrix(c(49,1,
                 47,1, 
                 48,1,
                 44,5), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=0:3))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")


df <- read_excel("~/Studie_data.xlsx")
df%>%
  dplyr::select(Studie, Tumor)%>%
  distinct()%>%
  ggplot(.,aes(y=Tumor))+
  geom_bar()+
  theme_bw() +
  xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12") +
  labs(y="Kankersoort", 
       x="Hoe vaak gezien in 12 studies", 
       title="Aantal keer dat een bepaalde kankersoort is genoemd")


g1<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  ggplot(.,aes(x=Dosering, y=prop, group=1))+
  geom_point()+
  geom_line() +
  theme_bw() +
  labs(x="Dosering", 
       y="Proportie kankergevallen (totaal)", 
       title="Proportie kanker per dosering ")
g2<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering, Geslacht)%>%
  group_by(Geslacht, Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  ggplot(.,aes(x=Dosering, y=prop, col=Geslacht, group=Geslacht))+
  geom_point()+
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")+
  labs(x="Dosering", 
       y="Proportie kankergevallen (totaal)", 
       col="Geslacht",
       title="Proportie kanker per dosering en geslacht")
gridExtra::grid.arrange(g1,g2,ncol=1)  


dose_response<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases)) %>%
  mutate(prop=(Cases/N)*100)%>%
  mutate(log_Dosering=log10(Dosering))
ggplot(dose_response, 
       aes(x = log_Dosering, y = Cases))+
  geom_point()+
  geom_line()
curved_fit <- drc::drm(
  formula = Cases ~ Dosering,
  data=dose_response,
  fct = drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
)
curved_fit <- drc::drm(
  formula = Cases ~ log_Dosering,
  data=dose_response,
  logDose = 10,
  fct = drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
)
summary(curved_fit)
plot(curved_fit)
coefs <- setNames(
  curved_fit$coefficients,
  c("hill", "min_value", "max_value", "ec_50"))
ic_50 <- with(
  as.list(coefs),
  exp(
    log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))))
ic_50

additional_data <- data.frame(
  conc = seq(0, 8690, length.out = 100))
predict(curved_fit, newdata = additional_data)
curved_fitted_data <- data.frame(
  dose_response,
  pred_cases = predict(curved_fit))
ggplot(curved_fitted_data) +
  geom_point(aes(x = Dosering, y = Cases, col="Geobserveerd")) +
  geom_line(aes(x = Dosering, y = pred_cases, col="Voorspeld")) +
  ylim(0, NA)+
  theme_bw()+
  labs(col="Bron", 
       x="Dosering", 
       y="Aantal kankergevallen", 
       title="Dose response analyse op de gehele dataset")+
  scale_x_continuous(trans = "log10")

library(drc)
ggplot(dose_response, aes(x = Dosering, y = Cases)) +
  geom_point() +
  stat_smooth(
    method = "drm",
    method.args = list(
      fct = drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
    ),
    se = FALSE,
  ) +
  ylim(0, NA)

dose_response_geslacht<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Geslacht, Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases)) %>%
  mutate(log_Dosering=log10(Dosering))%>%
  ungroup()
ggplot(dose_response_geslacht, 
       aes(x = log_Dosering, y = Cases, col=Geslacht))+
  geom_point()+
  geom_line()
curved_fit_geslacht <- drc::drm(
  formula = Cases ~ Dosering,
  data=dose_response_geslacht,
  Geslacht,
  fct = drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
)
compParm(curved_fit_geslacht, "ec_50", "-")
compParm(curved_fit_geslacht, "hill", "-")
summary(curved_fit_geslacht)
plot(curved_fit_geslacht)

geslacht = unique((dose_response_geslacht['Geslacht']))
curves = unique((dose_response_geslacht['Geslacht'])) 
curve_cnt = length(unique(dose_response_geslacht[['Geslacht']])) 
cols <- RColorBrewer::brewer.pal(4,'Set2') 
par(mfrow=c(1,2)) 
for (i in 1:curve_cnt){ 
  DR = dplyr::filter(dose_response_geslacht, 
              Geslacht==paste("",geslacht[i,], sep="")) 
  DR.mi <- drm(Cases ~ Dosering, 
               data= DR,
               robust = 'mean', #non-robust least squares estimation ("mean")
               fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
  plot(DR.mi,
       col = cols[i],
       type= "all",
       pch=16,
       lwd=1,
       cex.axis=0.8,
       legend = TRUE,
       xlab= "Dosering",
       ylab = "Kanker gevallen")
  plot(DR.mi,
       col = cols[i],
       add=TRUE,
       type='confidence')}

g1<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(Dosering = factor(Dosering))%>%
  pivot_longer(-Dosering, names_to = "Groep", values_to = "Waarde")%>%
  ggplot(.)+
  geom_bar(aes(x=Dosering, y=Waarde, fill=Groep), stat = "identity")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x        = "Dosering", 
       fill     = "Groep", 
       y        = "Aantal",
       title    = "Aantal dieren met kanker (Cases) en hoeveelheid dieren per dosering (N)")
g2<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  mutate(Dosering = factor(Dosering))%>%
  ggplot(.,aes(x=Dosering, y=prop, group=1))+
  geom_point()+
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Dosering", 
       y="Proportie (%)", 
       title= "Porportie kankergevallen (totaal)")
g3<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Dosering)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), 
         cum_Cases = cumsum(Cases))%>%
  mutate(prop=(cum_Cases/cum_N)*100, 
         Dosering = factor(Dosering))%>%
  mutate(diff = (prop-prop[which.min(Dosering)]))%>%
  ggplot(.,aes(x=Dosering, y=diff, group=1))+
  geom_point()+
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Dosering", 
       y="verschil", 
       title= "Absolute Verschil in proportie in vergelijking met dosering = 0")
cowplot::plot_grid(g1,g2,g3, align = "v", nrow = 3, rel_heights = c(2/4, 1/4, 1/4))


df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Geslacht, Dosering)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), 
         cum_Cases = cumsum(Cases))%>%
  mutate(prop=(cum_Cases/cum_N)*100, 
         Dosering = factor(Dosering))%>%
  mutate(diff = (prop-prop[which.min(Dosering)]))%>%
  ggplot(.,aes(x=Dosering, y=diff, group=Geslacht))+
  geom_point(col="black")+
  geom_line(col="black") +
  facet_grid("Proportie"~Geslacht, scales="free")+
  geom_bar(aes(fill=Geslacht), stat="identity", alpha=0.4)+
  theme_bw() +
  geom_hline(yintercept = 0, col="black", lty=2)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Dosering", 
       y="verschil", 
       title= "Absolute verschil in proportie in vergelijking met dosering = 0",
       subtitle="per geslacht")


g1<-df%>%
  dplyr::select(Ras, Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Geslacht, Ras, Dosering)%>%
  arrange(Ras, Geslacht)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), 
         cum_Cases = cumsum(Cases))%>%
  mutate(prop=(cum_Cases/cum_N)*100, 
         Dosering = factor(Dosering))%>%
  mutate(diff = (prop-prop[which.min(Dosering)]))%>%
  filter(Ras=="Muizen")%>%
  ggplot(.,aes(x=Dosering, y=diff, group=Geslacht))+
  geom_point(col="black")+
  geom_line(col="black") +
  facet_grid(Ras~Geslacht, scales="free")+
  geom_bar(aes(fill=Geslacht), stat="identity", alpha=0.4, show.legend = FALSE)+
  theme_bw() +
  geom_hline(yintercept = 0, col="black", lty=2)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Dosering", 
       y="verschil", 
       title= "Absolute verschil in proportie in vergelijking met dosering = 0",
       subtitle="per geslacht")
g2<-df%>%
  dplyr::select(Ras, Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Geslacht, Ras, Dosering)%>%
  arrange(Ras, Geslacht)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), 
         cum_Cases = cumsum(Cases))%>%
  mutate(prop=(cum_Cases/cum_N)*100, 
         Dosering = factor(Dosering))%>%
  mutate(diff = (prop-prop[which.min(Dosering)]))%>%
  filter(Ras=="Ratten")%>%
  ggplot(.,aes(x=Dosering, y=diff, group=Geslacht))+
  geom_point(col="black")+
  geom_line(col="black") +
  facet_grid(Ras~Geslacht, scales="free")+
  geom_bar(aes(fill=Geslacht), stat="identity", alpha=0.4)+
  theme_bw() +
  geom_hline(yintercept = 0, col="black", lty=2)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Dosering", 
       y="verschil")
gridExtra::grid.arrange(g1,g2,ncol=1)


df_diff<-df%>%dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Tumor, Dosering)%>%summarise(N      = sum(N), Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), cum_Cases = cumsum(Cases))%>%mutate(prop=(cum_Cases/cum_N)*100)%>%
  mutate(diff = prop-prop[which.min(Dosering)])
colour_breaks <- c(-10, 0, 5, 10, 15)
colours <- c("darkblue" ,"black", "yellow", "orange", "red")
df_diff%>%
  ungroup()%>%
  arrange(Dosering)%>%
  ggplot(.,aes(x=as.factor(Dosering), y=Tumor, fill=diff))+
  geom_tile()+
  scale_fill_gradientn(
    limits  = range(df_diff$diff),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(min(df_diff$diff), 
                scales::rescale(colour_breaks, from = range(df_diff$diff)), 
                max(df_diff$diff)))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x        = "Dosering", 
       fill     = "Verschil (%)", 
       y        = "Tumorsoort",
       title    = "Verschil in proportie in vergelijking met dosering = 0",
       subtitle = "per tumorsoort")

df_diff_geslacht<-df%>%dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  group_by(Geslacht, Tumor, Dosering)%>%summarise(N=sum(N), Cases  = sum(Cases))%>%
  mutate(cum_N     = cumsum(N), cum_Cases = cumsum(Cases))%>%mutate(prop=(cum_Cases/cum_N)*100)%>%
  mutate(diff = prop-prop[which.min(Dosering)])
colour_breaks <- c(-10, 0, 5, 10, 15)
colours <- c("darkblue" ,"black", "yellow", "orange", "red")
df_diff_geslacht%>%
  ungroup()%>%
  arrange(Dosering)%>%
  ggplot(.,aes(x=as.factor(Dosering), y=Tumor, fill=diff))+
  geom_tile()+
  facet_grid(~Geslacht)+
  scale_fill_gradientn(
    limits  = range(df_diff$diff),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(min(df_diff$diff), 
                scales::rescale(colour_breaks, from = range(df_diff$diff)), 
                max(df_diff$diff)))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x        = "Dosering", 
       fill     = "Verschil (%)", 
       y        = "Tumorsoort",
       title    = "Verschil in proportie in vergelijking met dosering = 0",
       subtitle = "per tumorsoort")


df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases)%>%
  group_by(Studie, Jaar, Dosering)%>%
  arrange(Studie, Jaar, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            mean_N         = sum(N)/n())%>%
  ggplot()+
  geom_line(aes(x=Dosering, y=sum_Cases, group=Studie))+
  geom_point(aes(x=Dosering, y=sum_Cases, group=Studie))+
  facet_wrap(~Studie, ncol=3, scales="free_x")+
  theme_bw() + 
  labs(x="Dosering (mg/kg/day)", 
       y="Totaal aantal kankergevallen", 
       title="Totaal aantal kankergevallen per dosering per studie")
  
df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases)%>%
  group_by(Studie, Jaar, Geslacht, Dosering)%>%
  arrange(Studie, Jaar, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            mean_N         = sum(N)/n())%>%
  ggplot()+
  geom_line(aes(x=Dosering, y=sum_Cases, group=Geslacht, col=Geslacht))+
  geom_point(aes(x=Dosering, y=sum_Cases, group=Geslacht, col=Geslacht))+
  facet_wrap(~Studie, ncol=3, scales="free_x")+
  theme_bw() + 
  labs(x="Dosering (mg/kg/day)", 
       y="Totaal aantal kankergevallen", 
       title="Totaal aantal kankergevallen per dosering per studie en geslacht")+
  theme(legend.position = "bottom")

df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases)%>%
  group_by(Studie, Jaar, Geslacht, Dosering)%>%
  arrange(Studie, Jaar, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            mean_N         = sum(N)/n())%>%
  ggplot()+
  geom_line(aes(x=Dosering, y=sum_Cases/mean_N, group=Geslacht, col=Geslacht))+
  geom_point(aes(x=Dosering, y=sum_Cases/mean_N, group=Geslacht, col=Geslacht, size=mean_N))+
  facet_wrap(~Studie, ncol=3, scales="free_x")+
  theme_bw() + 
  labs(x="Dosering (mg/kg/day)", 
       y="Totaal aantal kankergevallen / groepsgrootte", 
       title="Totaal aantal kankergevallen per dosering per studie en geslacht", 
       size="Groepsgrootte")+
  theme(legend.position = "bottom")









#### GLYFOSAAT: EENZIJDIG EN TWEEZIJDIG TOETSEN ----
df <- read_excel("~/Studie_data.xlsx")
df_stats<-read_excel("~/Studie_data_p_values.xlsx")
rowSums(is.na(df))
colSums(is.na(df))

df_stats%>%
  mutate(reported_p_value=as.numeric(reported_p_value))%>%
  dplyr::select(reported_p_value)%>%
  is.na()%>%
  table()

df_stats%>%
  dplyr::select(reported_p_value)%>%
  mutate(reported_p_value=as.numeric(reported_p_value))%>%
  summarise_all(~ sum(is.na(.)))

df_stats%>%
  mutate(reported_p_value=as.numeric(reported_p_value))%>%
  mutate(significant = if_else(reported_p_value<=0.05, "yes", "no"))%>%
  dplyr::select(significant)%>%
  table()

df_stats%>%
  mutate(reported_p_value=as.numeric(reported_p_value))%>%
  mutate(significant = if_else(reported_p_value<=0.01, "yes", "no"))%>%
  dplyr::select(significant)%>%
  table()

df_pvalues<-df%>%
  dplyr::select(-c(Jaar, Duur, Ras, Dosering))%>%
  group_by(Studie, Geslacht, Tumor)%>%
  filter(sum(Cases) >= 1)%>%
  mutate(n = n())%>%
  summarize(pvalue_prop_test=prop.test(Cases, N)$p.value,
            pvalue_prop_trend_test=prop.trend.test(Cases, N)$p.value,
            pvalue_fisher=fisher.test(matrix(c(Cases, N-Cases), nrow =n), 
                                      simulate.p.value=TRUE,B=1000)$p, 
            pvalue_CA_two=CochranArmitageTest(matrix(c(N-Cases, Cases), 
                                              byrow=TRUE, 
                                              nrow=2, 
                                              dimnames=list(resp=0:1, 
                                                            dose=0:(n-1))), 
                                       alternative="two.sided")$p.value,
            pvalue_CA_one=CochranArmitageTest(matrix(c(N-Cases, Cases), 
                                              byrow=TRUE, 
                                              nrow=2, 
                                              dimnames=list(resp=0:1, 
                                                            dose=0:(n-1))), 
                                       alternative="one.sided")$p.value)
df_pvalues%>%
  pivot_longer(!c(Studie, Geslacht, Tumor), names_to = "methode")%>%
  mutate(methode = dplyr::recode(methode,
                                 "pvalue_prop_test" = "Test voor gelijke proporties",
                                 "pvalue_prop_trend_test" = "Chi-kwadraat test", 
                                 "pvalue_fisher" = "Fisher Exact Test", 
                                 "pvalue_CA_two" = "CA test - tweezijdig", 
                                 "pvalue_CA_one" = "CA test - eenzijdig"))%>%
  mutate("p<=0.05" = if_else(value<=0.05, "yes", "no"), 
         "p<=0.01" = if_else(value<=0.01, "yes", "no"))%>%
  group_by(methode)%>%
  dplyr::select(`p<=0.05`)%>%
  table()

df_pvalues%>%
  pivot_longer(!c(Studie, Geslacht, Tumor), names_to = "methode")%>%
  mutate(methode = dplyr::recode(methode,
                                 "pvalue_prop_test" = "Test voor gelijke proporties",
                                 "pvalue_prop_trend_test" = "Chi-kwadraat test", 
                                 "pvalue_fisher" = "Fisher Exact Test", 
                                 "pvalue_CA_two" = "CA test - tweezijdig", 
                                 "pvalue_CA_one" = "CA test - eenzijdig"))%>%
  mutate("p<=0.05" = if_else(value<=0.05, "yes", "no"), 
         "p<=0.01" = if_else(value<=0.01, "yes", "no"))%>%
  group_by(methode)%>%
  dplyr::select(`p<=0.01`)%>%
  table()


library(arsenal)
a<-df_stats%>%dplyr::select(Studie, Geslacht, Tumor)%>%arrange(Studie, Geslacht, Tumor)%>%distinct()
b<-df_pvalues%>%ungroup%>%dplyr::select(Studie, Geslacht, Tumor)%>%arrange(Studie, Geslacht, Tumor)%>%distinct()
summary(comparedf(a, b))


df_pvalues_combined<-df_stats%>%
  mutate(reported_p_value=as.numeric(reported_p_value))%>%
  left_join(., df_pvalues, by=c("Studie", "Geslacht", "Tumor"))%>%
  pivot_longer(!c(Studie, Geslacht, Tumor), names_to = "methode")%>%
  mutate(methode = dplyr::recode(methode,
                                 "pvalue_prop_test" = "Test voor gelijke proporties",
                                 "pvalue_prop_trend_test" = "Chi-kwadraat test", 
                                 "pvalue_fisher" = "Fisher Exact Test", 
                                 "pvalue_CA_two" = "CA test - tweezijdig", 
                                 "pvalue_CA_one" = "CA test - eenzijdig", 
                                 "reported_p_value" = "Gerapporteerde CA test"))%>%
  mutate("p<=0.05" = if_else(value<=0.05, "yes", "no"), 
         "p<=0.01" = if_else(value<=0.01, "yes", "no"))%>%
  ungroup()
 
df_pvalues_combined%>%  
  group_by(methode)%>%
  dplyr::select(`p<=0.05`)%>%
  table()
df_pvalues_combined%>%  
  group_by(methode)%>%
  dplyr::select(`p<=0.01`)%>%
  table()

df_pvalues_combined%>%
  ggplot()+
  geom_boxplot(aes(y=value, x=Geslacht, fill=methode))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geslacht", 
       y="\U0070-waarde", 
       title="Boxplot van \U0070-waarde per methode")

df_pvalues_combined%>%
  ggplot()+
  geom_boxplot(aes(y=value, x=Studie, fill=methode))+
  theme_bw()+
  facet_grid(Geslacht~"Resultaten")+
  theme(legend.position = "bottom")+
  labs(x="Studie", 
       y="\U0070-waarde", 
       title="Boxplot van \U0070-waarde per methode, geslacht en studie")

df_pvalues_combined%>%
  group_by(methode)%>%
  count(`p<=0.05`)%>%
  ggplot()+
  geom_bar(aes(x=n, y=methode, fill=`p<=0.05`), stat="identity")+
  theme_bw()+
  scale_fill_manual(values = c("firebrick", "darkgreen"), na.value="black")+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Methode", 
       y="Significant", 
       title="Boxplot van significantie per methode")

df_pvalues_combined%>%
  group_by(Studie, methode)%>%
  count(`p<=0.05`)%>%
  ggplot()+
  geom_bar(aes(y=n, x=methode, fill=`p<=0.05`), stat="identity")+
  facet_grid(~Studie)+
  theme_bw()+
  scale_fill_manual(values = c("firebrick", "darkgreen"), na.value="black")+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Methode", 
       y="Significant", 
       title="Boxplot van significantie per methode, geslacht en studie")


df_pvalues_combined%>%
  group_by(Tumor, methode)%>%
  count(`p<=0.05`)%>%
  filter(`p<=0.05`=="yes")%>%
  mutate(n=factor(n))%>%
  ggplot()+
  geom_tile(aes(y=Tumor, x=methode, fill=n))+
  theme_bw()+
  scale_fill_viridis_d(option="turbo")+
  labs(x="Methode", 
       y="Tumorsoort", 
       title="Heatmap van statistische significantie per methode en tumorsoort", 
       subtitle = "\U0070-waarde <=0.05")

df_pvalues_combined%>%
  ggplot()+
  geom_point(aes(y=Studie, x=value, col=methode, shape=`p<=0.05`), position="dodge")+
  geom_vline(xintercept = 0.05, lty=2, col="black")+
  theme_bw()+
  theme(legend.position = "bottom")



  

#### GLYFOSAAT: EENZIJDIG EN TWEEZIJDIG TOETSEN: Wat als we meenemen wat we niet zagen? ----
df <- read_excel("~/Studie_data.xlsx")
df_stats<-read_excel("~/Studie_data_p_values.xlsx")

df%>%filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas (original pathology)")%>%
  dplyr::select(Studie)%>%
  table()
df%>%filter(Soort=="CD-1" & Geslacht=="Vrouw" & Tumor=="Spleen Composite Lymphosarcoma")%>%
  dplyr::select(Studie)%>%
  table()
df%>%dplyr::select(Tumor)%>%distinct()%>%n_distinct()
df%>%group_by(Geslacht)%>%       
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Studie=="Knezevich and Hogan")%>%       
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Tumor=="Harderian Gland Carcinomas")%>%       
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%       
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>% 
  dplyr::select(Studie)%>%table()
df_stats%>%dplyr::filter(is.na(reported_p_value))%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df_stats%>%dplyr::filter(is.na(reported_p_value))%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df_stats%>%dplyr::filter(is.na(reported_p_value))%>%
  dplyr::select(Tumor)%>%distinct()%>%list()
df%>%dplyr::filter(Tumor=="Hemangiomas")%>%       
    summarise(Unique_Elements = n_distinct(Studie)) %>%   
    ungroup()
df%>%dplyr::filter(Tumor=="Hemangiomas")%>%
  distinct()
df%>%dplyr::filter(Tumor=="Pituitary Carcinomas")%>%       
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Tumor=="Pituitary Carcinomas")%>%       
  distinct()
df%>%dplyr::filter(Soort=="Wistar")%>%       
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()  
df%>%dplyr::filter(Soort=="Wistar")%>%dplyr::select(Studie)%>%distinct()
df%>%dplyr::filter(Studie=="Brammer" & Tumor=="Pituitary Carcinomas")
df%>%dplyr::filter(Studie=="Wood" & Tumor=="Pituitary Carcinomas")
df%>%dplyr::filter(Studie=="Suresh" & Tumor=="Pituitary Carcinomas")
df%>%dplyr::filter(Studie=="Knezevich and Hogan")       
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  summarise(Unique_Elements = n_distinct(Studie))  
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas (original pathology)")%>%
  summarise(Unique_Elements = n_distinct(Studie))  
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%       
  dplyr::select(Studie)%>%table()
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%       
  summarise(Unique_Elements = n_distinct(Tumor))
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas and Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Hemangiosarcomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas (original pathology)")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas and Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Malignant Lymphomas")

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Studie=="Atkinson_a")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas and Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Carcinomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Hemangiosarcomas")
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas")

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas")%>%
  ggplot()+
  geom_line(aes(x=Dosering, y=Cases/N, group=Studie))+
  theme_bw()+
  labs(x="Dosering", 
       y="Cases / N", 
       title="Proportie Alveolar-Bronchiolar Adenomas per studie per dosering")

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas")%>%
  ggplot()+
  geom_line(aes(x=Dosering, y=Cases/N, group=Studie))+
  theme_bw()+
  labs(x="Dosering", 
       y="Cases / N", 
       title="Proportie Alveolar-Bronchiolar Adenomas per studie per dosering")

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Alveolar-Bronchiolar Adenomas")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)
  
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man" & Tumor=="Kidney Adenomas (original pathology)")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)

df%>%dplyr::select(Geslacht, Soort, Ras)%>%table()

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Vrouw")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Swiss Albino" & Geslacht=="Man")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Swiss Albino" & Geslacht=="Vrouw")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()%>%
  print(n=100)
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Vrouw")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()%>%
  print(n=100)
df%>%dplyr::filter(Soort=="Wistar" & Geslacht=="Man")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Wistar" & Geslacht=="Vrouw")%>%
  group_by(Tumor)%>%
  summarise(Unique_Elements = n_distinct(Studie)) %>%   
  ungroup()

df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Swiss Albino" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Wistar" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()


df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="CD-1" & Geslacht=="Vrouw")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Swiss Albino" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Swiss Albino" & Geslacht=="Vrouw")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Vrouw")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Wistar" & Geslacht=="Man")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()
df%>%dplyr::filter(Soort=="Wistar" & Geslacht=="Vrouw")%>%
  group_by(Studie)%>%
  summarise(Unique_Elements = n_distinct(Tumor)) %>%   
  ungroup()



df_stats%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sign = if_else(reported_p_value<=0.05, 1, 0))%>%
  summarise(count = sum(sign, na.rm=TRUE))
df_stats%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sign = if_else(reported_p_value<=0.05, 1, 0))%>%
  group_by(Studie)%>%
  summarise(count = sum(sign, na.rm=TRUE))%>%
  print(n=30)


df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man")%>%
  dplyr::select(-c(Dosering, N, Cases))%>%
  distinct()%>%
  arrange(Tumor)%>%
  dplyr::select(Tumor)%>%
  table()


df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Liver Carcinomas")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Liver Neoplastic Nodules")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Liver Nodules and Carcinomas")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)
df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Skin Epithelioma (Keratoacanthomas)")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)

df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Thyroid Follicular-Cell Adenomas and Carcinomas")%>%
  mutate(Dosering = if_else(Dosering>0, 1, 0))%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases), 
            ratio=Cases/N)

df_stats%>%dplyr::filter(Geslacht=="Man" & Tumor=="Liver Carcinomas")
df_stats%>%dplyr::filter(Geslacht=="Man" & Tumor=="Liver Neoplastic Nodules")
df_stats%>%dplyr::filter(Geslacht=="Man" & Tumor=="Liver Nodules and Carcinomas")
df_stats%>%dplyr::filter(Geslacht=="Man" & Tumor=="Skin Epithelioma (Keratoacanthomas)")
df_stats%>%dplyr::filter(Geslacht=="Man" & Tumor=="Thyroid Follicular-Cell Adenomas and Carcinomas")

df%>%dplyr::filter(Soort=="Sprague-Dawley" & Geslacht=="Man" & Tumor=="Skin Epithelioma (Keratoacanthomas)")


dose <- matrix(c(49,1,
                 23,2, 
                 19,0,
                 21,0,
                 45,5), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=0:4))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")

dose <- matrix(c(199,1,
                 88,2, 
                 76,0,
                 84,0,
                 195,5), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=0:4))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")


dose <- matrix(c(49,1,
                 23,2, 
                 19,0,
                 21,0,
                 45,5), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=c(0,11,112,320,1147)))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")

dose <- matrix(c(199,1,
                 88,2, 
                 76,0,
                 84,0,
                 195,5), 
               byrow=FALSE, 
               nrow=2, 
               dimnames=list(resp=0:1, dose=c(0,11,112,320,1147)))
Desc(dose)
CochranArmitageTest(dose, alternative = "two.sided")
CochranArmitageTest(dose, alternative = "one.sided")











#### GLYFOSAAT: EENZIJDIG EN TWEEZIJDIG TOETSEN: Het probleem van meerdere testen ----
df<-read_excel("~/Studie_data_p_values.xlsx")
df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  count(reported_p_value)%>%
  summarise(sum = sum(n))

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sig_005 = if_else(reported_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(reported_p_value<=0.01, 1, 0))%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  ggplot(aes(x=reported_p_value))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8")+
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)  +
  geom_vline(xintercept = 0.05, line="black") + 
  geom_vline(xintercept = 0.01, line="black", lty=2) +            
  theme_bw()+
  labs(x="Gerapporteerde \U0070-waarde", 
       y="Verdeling van waarden", 
       title="Verdeling van \U0070-waarde over alle studies en vergelijkingen heen")

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sig_005 = if_else(reported_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(reported_p_value<=0.01, 1, 0))%>%
  group_by(Studie)%>%
  summarise("0.05" = sum(sig_005, na.rm=TRUE), 
            "0.01" = sum(sig_001, na.rm=TRUE))%>%
  pivot_longer(!Studie, names_to = "\u03b1-waarde")%>%
  ggplot()+
  geom_bar(aes(x=value, y=Studie, fill=`a-waarde`), stat="identity", position = "dodge")+
  labs(x="Aantal keer beneden de \u03b1 waarde grens", 
       y="Studie", 
       title="Aantal keer dat een \U0070-waarde beneden de \u03b1 waarde grens is ")+
  theme_bw()

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sig_005 = if_else(reported_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(reported_p_value<=0.01, 1, 0))%>%
  group_by(Geslacht)%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))
df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  mutate(sig_005 = if_else(reported_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(reported_p_value<=0.01, 1, 0))%>%
  group_by(Studie, Geslacht)%>%
  summarise("0.05" = sum(sig_005, na.rm=TRUE), 
            "0.01" = sum(sig_001, na.rm=TRUE))%>%
  pivot_longer(!c(Studie,Geslacht), names_to = "\u03b1-waarde")%>%
  ggplot()+
  geom_bar(aes(x=value, y=Studie, fill=`a-waarde`), stat="identity", position = "dodge")+
  facet_grid(~Geslacht)+
  labs(x="Aantal keer beneden de \u03b1 waarde grens", 
       y="Studie", 
       title="Aantal keer dat een \U0070-waarde beneden de \u03b1 waarde grens is ")+
  theme_bw()

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  dplyr::select(reported_p_value)%>%
  mutate(adjusted_p_value = round(p.adjust(reported_p_value, "BH"), 3))%>%
  mutate(sig_005 = if_else(adjusted_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(adjusted_p_value<=0.01, 1, 0))%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))
df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  dplyr::select(reported_p_value)%>%
  mutate(adjusted_p_value = round(p.adjust(reported_p_value, "holm"), 3))%>%
  mutate(sig_005 = if_else(adjusted_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(adjusted_p_value<=0.01, 1, 0))%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))
df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  dplyr::select(reported_p_value)%>%
  mutate(adjusted_p_value = round(p.adjust(reported_p_value, "fdr"), 3))%>%
  mutate(sig_005 = if_else(adjusted_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(adjusted_p_value<=0.01, 1, 0))%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))
df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  dplyr::select(reported_p_value)%>%
  mutate(adjusted_p_value = round(p.adjust(reported_p_value, "bonferroni"), 3))%>%
  mutate(sig_005 = if_else(adjusted_p_value<=0.05, 1, 0))%>%
  mutate(sig_001 = if_else(adjusted_p_value<=0.01, 1, 0))%>%
  summarise(count_005 = sum(sig_005, na.rm=TRUE), 
            count_001 = sum(sig_001, na.rm=TRUE))

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  group_by(Studie)%>%
  dplyr::select(reported_p_value)%>%
  dplyr::rename(reported=reported_p_value)%>%
  mutate(adjusted = round(p.adjust(reported, "BH"), 3))%>%
  pivot_longer(!Studie, names_to = "method")%>%
  ggplot()+
  geom_density(aes(x=value,fill=method), alpha=0.5)+
  labs(x="De \U0070-waarde", 
       y="Frequentie hoe vaak een \U0070-waarde voorkomt", 
       title="De verdeling van \U0070-waarden voor & na correctie",
       caption="Correctie voor het uitvoeren van meerdere testen op basis van de Benjamini-Hochberg methode")+
  theme_bw()

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  group_by(Studie)%>%
  dplyr::select(reported_p_value)%>%
  dplyr::rename(reported=reported_p_value)%>%
  mutate(adjusted = round(p.adjust(reported, "BH"), 3))%>%
  pivot_longer(!Studie, names_to = "Aanpassing")%>%
  mutate("0.05" = if_else(value<=0.05, 1, 0))%>%
  mutate("0.01" = if_else(value<=0.01, 1, 0))%>%
  dplyr::select(-value)%>%
  mutate(Aanpassing = dplyr::recode(Aanpassing,
                           "reported" = "Geen",
                           "adjusted" = "B-H"))%>%
  pivot_longer(!c(Studie,Aanpassing), names_to = "\u03b1-waarde")%>%
  group_by(Studie, Aanpassing,`a-waarde`)%>%
  summarise(sum = sum(value, na.rm=TRUE))%>%
  ggplot()+
  geom_bar(aes(x=sum, y=Studie, fill=Aanpassing), stat="identity", position = "dodge")+
  facet_grid(~`a-waarde`)+
  labs(x="Aantal \U0070-waarden onder de \u03b1 waarde grens", 
       y="Studie", 
       title="Aantal \U0070-waarden onder de \u03b1 waarde grens voor & na correctie",
       caption="Correctie voor het uitvoeren van meerdere testen op basis van de Benjamini-Hochberg methode", 
       fill="Correctie methode")+
  theme_bw()

df%>%
  mutate(reported_p_value = as.numeric(reported_p_value))%>%
  group_by(Studie, Geslacht)%>%
  dplyr::select(reported_p_value)%>%
  dplyr::rename(reported=reported_p_value)%>%
  mutate(adjusted = round(p.adjust(reported, "BH"), 3))%>%
  pivot_longer(!c(Studie,Geslacht), names_to = "Aanpassing")%>%
  mutate("0.05" = if_else(value<=0.05, 1, 0))%>%
  mutate("0.01" = if_else(value<=0.01, 1, 0))%>%
  dplyr::select(-value)%>%
  mutate(Aanpassing = dplyr::recode(Aanpassing,
                                    "reported" = "Geen",
                                    "adjusted" = "B-H"))%>%
  pivot_longer(!c(Studie,Geslacht, Aanpassing), names_to = "\u03b1-waarde")%>%
  group_by(Studie, Geslacht, Aanpassing,`a-waarde`)%>%
  summarise(sum = sum(value, na.rm=TRUE))%>%
  ggplot()+
  geom_bar(aes(x=sum, y=Studie, fill=Aanpassing), stat="identity", position = "dodge")+
  facet_grid(Geslacht~`a-waarde`)+
  labs(x="Aantal \U0070-waarden onder de \u03b1 waarde grens", 
       y="Studie", 
       title="Aantal \U0070-waarden onder de \u03b1 waarde grens voor & na correctie",
       caption="Correctie voor het uitvoeren van meerdere testen op basis van de Benjamini-Hochberg methode", 
       fill="Correctie methode")+
  theme_bw()





#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - Non-lineaire dose-response analyse ----
df <- read_excel("~/Studie_data.xlsx")
dose_response<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Dosering)%>%
  group_by(Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases)) %>%
  mutate(prop=(Cases/N)*100)%>%
  mutate(log_Dosering=log10(Dosering))
ggplot(dose_response, 
       aes(x = log_Dosering, y = Cases))+
  geom_point()+
  geom_line()
glys.m1 <- drm(Cases ~ log_Dosering, 
               data = dose_response, 
               logDose = exp(10),
               fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
summary(glys.m1)
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,.5,.5), mgp=c(2,.7,0))
plot(glys.m1, broken=TRUE, bty="l",
     xlab="Concentratie Glyfosaat (log schaal)", ylab="Hoeveelheid kanker")
modelFit(glys.m1)
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,2,.5), mgp=c(2,.7,0)) #put two graphs together
plot(residuals(glys.m1) ~ fitted(glys.m1), main="Residuals vs Fitted")
abline(h=0)
qqnorm(residuals(glys.m1))
qqline(residuals(glys.m1))
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,0,.5), mgp=c(2,.7,0)) #put two graphs together
plot(Cases ~ log_Dosering, data = dose_response, bty="l")
linear.m1 <- lm(Cases ~ log_Dosering, data = dose_response)
summary(linear.m1)
abline(linear.m1)
plot(linear.m1, which=1, bty="l")
AIC(glys.m1, linear.m1)
library(drc)
## Comparing log-logistic and Weibull models
glys.m0 <- drm(Cases ~ log_Dosering, 
               data = dose_response, 
               logDose = exp(10), fct = LL.4())
glys.m1 <- drm(Cases ~ log_Dosering, 
               data = dose_response, 
               logDose = exp(10), fct = W1.4())
glys.m2 <- drm(Cases ~ log_Dosering, 
               data = dose_response, 
               logDose = exp(10), fct = W2.4())
par(mfrow=c(1,1), mar=c(3.2,3.2,.5,.5), mgp=c(2,.7,0))
plot(glys.m0, broken=TRUE, xlab="Dose (mM)", ylab="Root length (cm)", lwd=2, 
     cex=1.2, cex.axis=1.2, cex.lab=1.2, bty="l")
plot(glys.m1, add=TRUE, broken=TRUE, lty=2, lwd=2)
plot(glys.m2, add=TRUE, broken=TRUE, lty=3, lwd=2)
modelFit(glys.m2)

ED(glys.m2,50,interval="delta")

edLL<-data.frame(ED(glys.m0,c(10,50,90),interval="delta", 
                    display=FALSE),ll="Log-logistic")            
edW1<-data.frame(ED(glys.m1,c(10,50,90),interval="delta", 
                    display=FALSE),ll="Weibull 1")
edW2<-data.frame(ED(glys.m2,c(10,50,90),interval="delta", 
                    display=FALSE),ll="Weibull 2")
CombED<-rbind(edLL,edW1,edW2)

library(cowplot)
p1 <- ggplot(data=CombED[c(1,4,7),], aes(x=ll, y=Estimate))+
  geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED10")+
  xlab("")
p2 <- ggplot(data=CombED[c(2,5,8),], aes(x=ll, y=Estimate))+
  geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED50")+
  xlab("")

p3 <- ggplot(data=CombED[c(3,6,9),], aes(x=ll, y=Estimate))+
  geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED90")+
  xlab("Sigmoid four parameter models")

plot_grid(p1,p2,p3, ncol=1)


dose_response_geslacht<-df%>%
  dplyr::select(Geslacht, Tumor, Dosering, N, Cases)%>%
  arrange(Geslacht, Dosering)%>%
  group_by(Geslacht, Dosering)%>%
  summarise(N=sum(N), 
            Cases=sum(Cases)) %>%
  mutate(prop=(Cases/N)*100)%>%
  mutate(log_Dosering=log10(Dosering))%>%
  ungroup()
glys.m0_males <- drm(Cases ~ Dosering, 
                     data = dose_response_geslacht[dose_response_geslacht$Geslacht=="Man", ], 
                     fct = lnormal())
glys.m0_females <- drm(Cases ~ Dosering, 
                     data = dose_response_geslacht[dose_response_geslacht$Geslacht=="Vrouw", ], 
                     fct = LL.4())
par(mfrow=c(1,1), mar=c(3.2,3.2,.5,.5), mgp=c(2,.7,0))
plot(glys.m0_males, broken=TRUE, bty="l")
plot(glys.m0_females, col="red", broken=TRUE, add=TRUE, legend=FALSE)
anova(glys.m0_males, glys.m0_females)

par(mfrow=c(1,1), mar=c(3.2,3.2,.5,.5), mgp=c(2,.7,0))
relpot(glys.m0_males, interval = "delta", bty="l")

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - Lineaire dose-response analyse ----
df <- read_excel("~/Studie_data.xlsx")
df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Dosering)%>%
  arrange(Studie, Jaar, Dosering)%>%
  summarise(sum_Cases     = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  geom_point(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Ratio kankergevallen / dieren", 
       title="Ratio kankergevallen als functie van de dosering per studie")
df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Dosering)%>%
  arrange(Studie, Jaar, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  geom_point(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  facet_grid(Duur~Geslacht, scales="free")+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Ratio kankergevallen / dieren", 
       title="Ratio kankergevallen als functie van de dosering per studie",
       subtitle="Per geslacht en duur van de studie")
df%>%
  dplyr::select(Studie, Jaar, Soort, Dosering, Tumor, Geslacht, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Soort, Dosering)%>%
  arrange(Studie, Jaar, Duur, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases/sum_N,group=interaction(Studie,Geslacht), col=Geslacht))+
  facet_grid(Duur~Soort, scales="free")+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Ratio kankergevallen / dieren", 
       title="Ratio kankergevallen als functie van de dosering per studie",
       subtitle="Per geslacht, diersoort en duur van de studie")
df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Dosering)%>%
  arrange(Studie, Jaar, Dosering)%>%
  filter(Dosering>0)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  geom_point(aes(x=Dosering_log, y=sum_Cases/sum_N, group=Studie), alpha=0.7)+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Ratio kankergevallen / dieren", 
       title="Ratio kankergevallen als functie van de dosering per studie",
       subtitle="Resultaten bij nul-dosering zijn weggelaten")+
  theme_bw()
df%>%
  dplyr::select(Studie, Jaar, Soort, Dosering, Tumor, Geslacht, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Soort, Dosering)%>%
  arrange(Studie, Jaar, Duur, Dosering)%>%
  filter(Dosering>0)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases/sum_N, group=interaction(Studie,Geslacht), col=Geslacht))+
  facet_grid(Duur~Soort, scales="free")+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Ratio kankergevallen / dieren", 
       title="Ratio kankergevallen als functie van de dosering per studie",
       subtitle="Per geslacht, diersoort en duur van de studie waarbij nul-dosering is weggelaten")

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - Lineair Mixed Model
df <- read_excel("~/Studie_data.xlsx")
df_lmer<-df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur, Soort)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Dosering, Soort)%>%
  arrange(Studie, Jaar, Geslacht, Dosering, Soort)%>%
  summarise(sum_Cases      = sum(Cases), 
            Sum_N          = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))

df_lmer%>%
  ggplot()+
  geom_line(aes(x=Dosering_log, y=sum_Cases, group=Studie))

fit<-lme4::lmer(sum_Cases~Dosering_log*Geslacht + Duur + Soort + (1|Studie), data=df_lmer)
summary(fit)
sjPlot::tab_model(fit)
plot(fit)
sjPlot::plot_model(fit, type = "pred", terms = c("Dosering_log", "Geslacht", "Duur"))+
  theme_bw()
sjPlot::plot_model(fit, type = "pred", terms = c("Dosering_log", "Geslacht", "Soort"))+
  theme_bw()
sjPlot::plot_model(fit, type = "pred", 
                   terms = c("Dosering_log", "Geslacht", "Duur", "Soort"))


fit<-lme4::lmer(sum_Cases~splines::ns(Dosering_log,3)*Geslacht + Duur + Soort + (1|Studie), data=df_lmer)
summary(fit)
plot(fit)
sjPlot::tab_model(fit)
sjPlot::plot_model(fit, type = "pred", terms = c("Dosering_log [all]", "Geslacht", "Duur", "Soort"))

df_lmer_nozero<-df%>%
  dplyr::select(Studie, Jaar, Soort, Dosering, Tumor, Geslacht, N, Cases, Duur)%>%
  group_by(Studie, Jaar, Duur, Geslacht, Soort, Dosering)%>%
  arrange(Studie, Jaar, Duur, Dosering, Soort)%>%
  filter(Dosering>0)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N         = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))
fit<-lme4::lmer(sum_Cases~Dosering_log*Geslacht + Duur + Soort + (1|Studie), data=df_lmer_nozero)
summary(fit)
sjPlot::tab_model(fit)
plot(fit)
sjPlot::plot_model(fit, type = "pred", terms = c("Dosering_log", "Geslacht", "Duur", "Soort"))


fit<-lme4::lmer(sum_Cases~splines::ns(Dosering_log,3)*Geslacht + Duur + Soort + (1|Studie), data=df_lmer_nozero)
summary(fit)
plot(fit)
sjPlot::tab_model(fit)
sjPlot::plot_model(fit, type = "pred", terms = c("Dosering_log [all]", "Geslacht", "Duur", "Soort"))

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - BINOMIAAL MODEL----
df <- read_excel("~/Studie_data.xlsx")
df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases)%>%
  group_by(Studie, Dosering, Geslacht)%>%
  filter(Dosering == 0) %>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  ungroup()%>%
  rowwise()%>%
  mutate(prop  = (prop.test(Cases,N)[[4]][1])*100,
         low   = (prop.test(Cases,N)$conf.int[1])*100, 
         high  = (prop.test(Cases,N)$conf.int[2])*100) %>%
  ungroup()%>%
  ggplot(aes(y=Studie, x=(Cases/N)*100, col=Geslacht, group=Geslacht))+
  geom_point(position=position_dodge(width = 0.5))+
  geom_pointrange(aes(xmin=low, 
                      xmax=high),position=position_dodge(width = 0.5))+
  theme_bw()+
  labs(x="Proportie kankergevallen bij nul-dosering", 
       y="Studie", 
       title="Proportie totaal aantal kankergevallen",
       subtitle="Deler is het totaal aantal mogelijker observaties")+
  theme(legend.position = "bottom")

df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases)%>%
  group_by(Studie, Dosering, Geslacht)%>%
  filter(Dosering == 0) %>%
  summarise(N      = sum(N), 
            Cases  = sum(N)/n())%>%
  ungroup()%>%
  rowwise()%>%
  mutate(prop  = (prop.test(Cases,N)[[4]][1])*100,
         low   = (prop.test(Cases,N)$conf.int[1])*100, 
         high  = (prop.test(Cases,N)$conf.int[2])*100) %>%
  ungroup()%>%
  ggplot(aes(y=Studie, x=(Cases/N)*100, col=Geslacht, group=Geslacht))+
  geom_point(position=position_dodge(width = 0.5))+
  geom_pointrange(aes(xmin=low, 
                      xmax=high),position=position_dodge(width = 0.5))+
  theme_bw()+
  labs(x="Proportie kankergevallen bij nul-dosering", 
       y="Studie", 
       title="Proportie totaal aantal kankergevallen",
       subtitle="Deler is het gemiddelde aantal mogelijker observaties")+
  theme(legend.position = "bottom")

df_prop<-df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases, Duur, Soort)%>%
  group_by(Studie, Dosering, Geslacht, Duur, Soort)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur)) %>%
  mutate(prop=(Cases/N)*100)%>%
  ungroup()%>%
  mutate(ID = row_number())


fit0<-lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + Geslacht + Duur + Soort + (1|Studie), 
                 data = df_prop, family = binomial)
sjPlot::tab_model(fit0)
fit1<-lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + Geslacht + Duur + Soort + (1|Studie) + (1|ID), 
                 data = df_prop, family = binomial)
sjPlot::tab_model(fit1)
fit2<-lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + Duur + Soort + (1 + Geslacht|Studie) + (1|ID), 
                  data = df_prop, family = binomial)
sjPlot::tab_model(fit2)
AIC(fit0, fit1, fit2)

summary(fit1)
plot(fit1)
sjPlot::tab_model(fit1)
sjPlot::plot_model(fit1, type = "pred", 
                   terms=c("Dosering_log[all]", "Geslacht", "Duur", "Soort"))
df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")

fit<-lme4::glmer(cbind(Cases, N-Cases) ~ splines::ns(Dosering_log,3) + Geslacht + Duur + Soort + (1|Studie) + (1|ID), 
                 data = df_prop, family = binomial)
summary(fit)
plot(fit)
sjPlot::tab_model(fit)
sjPlot::plot_model(fit, type = "pred", 
                   terms=c("Dosering_log[all]", "Geslacht", "Duur", "Soort"))
df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")

df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred_cond=exp(.fitted)*100, 
         pred_marg=exp(.fixed)*100)%>%
  ggplot()+
  geom_point(aes(x=Dosering, y=prop))+
  geom_line(aes(x=Dosering, y=pred_cond, col="Random Effects"))+
  geom_line(aes(x=Dosering, y=pred_marg, col="Marginal Effects"))+
  facet_grid(Geslacht~"")+
  scale_x_log10()+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_manual(name = "Model", 
                     values = c("Random Effects" = "#D93840", 
                                "Marginal Effects"= "#445599")) +
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een Mixed-Model met Binomiaal verdeling", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter")

df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  facet_grid(~Geslacht)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")

df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  facet_grid(~Duur)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")

df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  facet_grid(Geslacht~Duur)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")


fit<-lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log*Geslacht + Duur + Soort + (1|Studie) + (1|ID), 
                  data = df_prop, family = binomial)
sjPlot::tab_model(fit)
summary(fit)
sjPlot::plot_model(fit, type = "pred", 
                   terms=c("Dosering_log[all]", "Geslacht", "Duur", "Soort"))
df_prop%>%
  cbind(., broom.mixed::augment(fit)[7:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, N, Cases, prop, .fitted, .resid, .fixed)%>%
  mutate(pred=exp(.fitted)*100)%>%
  ggplot(aes(x=prop, y=pred))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een mixed-model logistisch model", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")


#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - BINOMIAAL MODEL- replicatie logistisch model Portier ----
df <- read_excel("~/Studie_data.xlsx")
df_prop<-df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases, Soort, Tumor)%>%
  group_by(Studie, Dosering, Geslacht, Soort, Tumor)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  mutate(prop=(Cases/N)*100)%>%
  ungroup()%>%
  mutate(ID = row_number())

fit<-df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)
sjPlot::tab_model(fit) 

fit<-df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  filter(Studie!="Wood_a")%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)
sjPlot::tab_model(fit) 

df_prop%>%
  filter(   Soort=="CD-1" & 
            Geslacht =="Man" & 
            Tumor=="Kidney Adenomas" & 
            Studie=="Wood_a")

fit<-df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)
summary(fit)
sjPlot::tab_model(fit) 

fit<-df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)
summary(fit)
sjPlot::tab_model(fit) 

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  ggplot()+
  geom_point(aes(x=Dosering_log, y=prop, group=Studie, col=Studie))+
  geom_line(aes(x=Dosering_log, y=prop, group=Studie, col=Studie))+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Proprotie kankergevallen per geobserveerd aantal dieren", 
       title="Proportie Kidney Adenomas voor mannelijke CD-1 Muizen", 
       subtitle="De data voor tabel 3 uit de studie van Portier")+
  theme(legend.position = "bottom")


df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Carcinomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Carcinomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Kidney Adenomas and Carcinomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Man" & Tumor=="Hemangiosarcomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Vrouw" & Tumor=="Hemangiomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="CD-1" & Geslacht =="Vrouw" & Tumor=="Malignant Lymphomas")%>%
  mutate(Cases = if_else(Studie=="Wood_a", 2, Cases))%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()


df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Man" & Tumor=="Hepatocellular Adenomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Man" & Tumor=="Kidney Adenomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Man" & Tumor=="Skin Keratoacanthomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Man" & Tumor=="Skin Basal Cell Tumors")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Vrouw" & Tumor=="Adrenal Cortical Carcinoma")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()

df_prop%>%
  filter(Soort=="Sprague-Dawley" & Geslacht =="Man" & Tumor=="Skin Basal Cell Tumors")%>%
  ggplot()+
  geom_point(aes(x=Dosering_log, y=prop, group=Studie, col=Studie))+
  geom_line(aes(x=Dosering_log, y=prop, group=Studie, col=Studie))+
  theme_bw()+
  labs(x="Dosering op de log schaal", 
       y="Proprotie kankergevallen per geobserveerd aantal dieren", 
       title="Proportie Skin Basal Cell Tumors voor mannelijke Sprague-Dawley ratten", 
       subtitle="De data voor tabel 4 uit de studie van Portier")+
  theme(legend.position = "bottom")



df_prop%>%
  filter(Soort=="Wistar" & Geslacht =="Man" & Tumor=="Hepatocellular Adenomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Wistar" & Geslacht =="Man" & Tumor=="Hepatocellular Adenomas and Carcinomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()
df_prop%>%
  filter(Soort=="Wistar" & Geslacht =="Man" & Tumor=="Skin Keratoacanthomas")%>%
  mutate(prop=(Cases/N)*100)%>%
  lme4::glmer(cbind(Cases, N-Cases) ~ Dosering_log + (1|Studie), 
              data = ., family = binomial)%>%
  summary()













#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - POISSON MODEL----
df <- read_excel("~/Studie_data.xlsx")
df%>%
  ggplot(aes(x=Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen")

df%>%summarize(mean = mean(Cases, na.rm=TRUE))


df_poiss<-df%>%
  dplyr::select(Studie, Dosering, Cases, N, Duur, Geslacht, Soort)%>%
  group_by(Studie, Dosering, Geslacht, Soort)%>%
  arrange(Studie, Dosering)%>%
  mutate(sum_Cases      = sum(Cases), 
         sum_N          = sum(N),
         mean_N         = sum(N)/n())%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur)) %>%
  ungroup()%>%
  mutate(ID = row_number())

fit<-lme4::glmer(sum_Cases ~ Dosering_log*Geslacht + Duur + Soort+ 
                   offset(log(sum_N))+
                   (1|Studie) + (1|ID), 
                 data = df_poiss, family = poisson)
summary(fit)
plot(fit)
sjPlot::tab_model(fit)

df_poiss%>%
  cbind(., broom.mixed::augment(fit4)[9:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, sum_Cases, .fitted, .resid, .fixed)%>%
  mutate(pred_cond=exp(.fitted))%>%
  ggplot(aes(x=sum_Cases, y=pred_cond))+
  geom_abline(intercept = 0, slope=1, lty=2, col="black")+
  geom_point(aes(col=.resid),size=3)+
  geom_point(shape = 1,size = 3,colour = "black")+
  scale_colour_viridis_c(option="turbo")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een Mixed-Model met Poisson verdeling", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter", 
       col="Afwijking")
df_poiss%>%
  cbind(., broom.mixed::augment(fit4)[9:18])%>%
  dplyr::select(Studie, Dosering, Geslacht, Duur, sum_Cases, .fitted, .resid, .fixed)%>%
  mutate(pred_cond=exp(.fitted), 
         pred_marg=exp(.fixed))%>%
  ggplot()+
  geom_point(aes(x=Dosering, y=sum_Cases))+
  geom_line(aes(x=Dosering, y=pred_cond, col="Random Effects"))+
  geom_line(aes(x=Dosering, y=pred_marg, col="Marginal Effects"))+
  facet_grid(~Geslacht)+
  scale_x_log10()+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_manual(name = "Model", 
                     values = c("Random Effects" = "#D93840", 
                                "Marginal Effects"= "#445599")) +
  labs(x="Geobserveerde proportie kankergevallen", 
       y="Voorspelde prorportie kankergevallen", 
       title="Beoordeling van een Mixed-Model met Poisson verdeling", 
       subtitle="Hoe dichter bij de diagonale lijn hoe beter")


sjPlot::plot_model(fit, type = "pred", 
                   terms=c("Dosering_log[all]", "Geslacht", "Duur", "Soort"))+
  theme(legend.position = "bottom")


newdata<-with(df_poiss, expand.grid(Dosering_log=unique(Dosering_log), 
                                    Geslacht=unique(Geslacht),
                                    Duur=unique(Duur), 
                                    Soort=unique(Soort), 
                                    sum_N=unique(sum_N), 
                                    Studie=unique(Studie), 
                                    ID=unique(ID)))
predict_expand_grid<-predict(fit,newdata)



summary(df_poiss$Dosering)
summary(df_poiss$sum_N)

Geslacht<-rep("Man",8)
Soort<-rep(c("CD-1", "Wistar","Sprague-Dawley","Swiss ALbino"), each=2, length.out=8)
Dosering<-rep(c(0,685), each=1, length.out=8)
Duur<-rep(18,8)
sum_N<-rep(408,8)
nd<-data.frame(Geslacht, Soort, Dosering, sum_N, Duur)%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur), 
         sum_N=log(sum_N))%>%
  dplyr::rename("offset(log(sum_N))" = "sum_N")%>%
  dplyr::select(-Dosering)%>%
  ungroup()%>%
  mutate(ID = row_number())%>%
  mutate(Studie="test");nd
predict(fit,nd, type="response")







#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - NEGATIVE BINOMIAL----
df <- read_excel("~/Studie_data.xlsx")
df_nb<-df%>%
  dplyr::select(Studie, Dosering, Cases, N, Duur, Geslacht, Soort)%>%
  group_by(Studie, Dosering, Geslacht, Soort)%>%
  arrange(Studie, Dosering)%>%
  mutate(sum_Cases      = sum(Cases), 
         sum_N          = sum(N),
         mean_N         = sum(N)/n())%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur)) %>%
  ungroup()%>%
  mutate(ID = row_number())

fit<-lme4::glmer.nb(sum_Cases ~ Dosering_log*Geslacht + Duur + Soort+ 
                   offset(log(sum_N))+
                   (1|Studie), 
                 data = df_nb)
summary(fit)
plot(fit)
sjPlot::plot_model(fit, type="diag")
plot(fit, resid(.) ~ Dosering_log)
simulationOutput<-DHARMa::simulateResiduals(fittedModel=fit, n=500, refit=T)
dev.off()
plot(simulationOutput) 
par(mfrow=c(1,2))
DHARMa::testZeroInflation(simulationOutput) 
DHARMa::testDispersion(simulationOutput)

dev.off()
par(mfrow=c(3,2))
DHARMa::plotResiduals(simulationOutput, form=df_nb$Dosering_log)
DHARMa::plotResiduals(simulationOutput, form=df_nb$Geslacht)
DHARMa::plotResiduals(simulationOutput, form=df_nb$Duur)
DHARMa::plotResiduals(simulationOutput, form=df_nb$Soort)
DHARMa::plotResiduals(simulationOutput, form=df_nb$Studie)
DHARMa::plotResiduals(simulationOutput, form=df_nb$sum_N)

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - ZERO-INFLATED & HURDLE MODELS POISSON----
df <- read_excel("~/Studie_data.xlsx")
df_nb<-df%>%
  dplyr::select(Studie, Dosering, Cases, N, Duur, Geslacht, Soort)%>%
  group_by(Studie, Dosering, Geslacht, Soort)%>%
  arrange(Studie, Dosering)%>%
  mutate(sum_Cases      = sum(Cases), 
         sum_N          = sum(N),
         mean_N         = sum(N)/n())%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur), 
         Studie=as.factor(Studie)) %>%
  ungroup()%>%
  mutate(ID = as.factor(row_number()))

fit.p.ADMB.0<- glmmADMB::glmmadmb(sum_Cases ~
                                    1 + (1|Studie) +
                                    offset(log(sum_N)),
                                  data=df_nb,
                                  corStruct="full",
                                  zeroInflation=FALSE,
                                  family="poisson", 
                                  link="log")
fit.p.ADMB.1<- glmmADMB::glmmadmb(sum_Cases ~
                                  Dosering_log + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                corStruct="full",
                                zeroInflation=FALSE,
                                family="poisson", 
                                link="log")
fit.p.ADMB.2<- glmmADMB::glmmadmb(sum_Cases ~
                                  Dosering_log + Geslacht+ (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                corStruct="full",
                                zeroInflation=FALSE,
                                family="poisson", 
                                link="log")
fit.p.ADMB.3<- glmmADMB::glmmadmb(sum_Cases ~
                                  Dosering_log*Geslacht+(1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                corStruct="full",
                                zeroInflation=FALSE,
                                family="poisson", 
                                link="log")
fit.p.ADMB.4<- glmmADMB::glmmadmb(sum_Cases ~
                                    Dosering_log*Soort +
                                    offset(log(sum_N)),
                                  data=df_nb,
                                  corStruct="full",
                                  zeroInflation=FALSE,
                                  family="poisson", 
                                  link="log")
anova(fit.p.ADMB.0, fit.p.ADMB.1, fit.p.ADMB.2, fit.p.ADMB.3)

fit.p.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                1 + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                1 + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~1,
                              family=poisson)
fit.hnp.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                  1 + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.0,fit.zip.TMB.0,fit.hnp.TMB.0)


fit.p.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=poisson)
fit.hnp.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.1,fit.zip.TMB.1,fit.hnp.TMB.1)
summary(fit.hnp.TMB.1)


fit.p.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=poisson)
fit.hnp.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.2,fit.zip.TMB.2,fit.hnp.TMB.2)


fit.p.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + Soort + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=poisson)
fit.hnp.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.3,fit.zip.TMB.3,fit.hnp.TMB.3)


fit.p.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + Soort + Duur + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + Soort + Duur + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=poisson)
fit.hnp.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + Soort + Duur + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.4,fit.zip.TMB.4,fit.hnp.TMB.4)

fit.p.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log*Geslacht + Soort + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=poisson)
fit.zip.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log*Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=poisson)
fit.hnp.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log*Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=poisson)
anova(fit.p.TMB.5,fit.zip.TMB.5,fit.hnp.TMB.5)
anova(fit.hnp.TMB.0,fit.hnp.TMB.1,fit.hnp.TMB.2,fit.hnp.TMB.3,fit.hnp.TMB.4,fit.hnp.TMB.5)
summary(fit.hnp.TMB.5)
summary(fit.hnp.TMB.3)
summary(fit.hnp.TMB.2)
summary(fit.hnp.TMB.1)

sjPlot::plot_models(fit.hnp.TMB.1,
                    fit.hnp.TMB.2,
                    fit.hnp.TMB.3, 
                    fit.hnp.TMB.5,
                    axis.labels = rlist::list.reverse(c("Intercept",
                    "Dosering (log schaal)", "Geslacht", "Sprague-Dawley vs CD-1", 
                    "Swiss Albino vs CD-1", "Wistar vs CD-1", "Dosering * Geslacht")),
                    legend.title ="Modellen",
                    m.labels=c("Poisson Hurdle Model 1", 
                               "Poisson Hurdle Model 2",
                               "Poisson Hurdle Model 3",
                               "Poisson Hurdle Model 5"),
                    show.p = FALSE,
                    spacing=0.8,
                    p.shape = TRUE,
                    show.intercept = TRUE,
                    auto.label = FALSE,
                    wrap.title = 75,
                    vline.color = "black",
                    title="Vergelijking van vijf Hurdle modellen voor het beoordelen van de relatie tussen tumor en dosering glyfosaat"
                    #,grid = TRUE
                    ) + theme_bw()


#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - ZERO-INFLATED NEGATIVE BINOMIAL----
df <- read_excel("~/Studie_data.xlsx")
df_nb<-df%>%
  dplyr::select(Studie, Dosering, Cases, N, Duur, Geslacht, Soort)%>%
  group_by(Studie, Dosering, Geslacht, Soort)%>%
  arrange(Studie, Dosering)%>%
  mutate(sum_Cases      = sum(Cases), 
         sum_N          = sum(N),
         mean_N         = sum(N)/n())%>%
  mutate(Dosering_log = log10(Dosering+0.00001), 
         Duur = as.factor(Duur), 
         Studie=as.factor(Studie)) %>%
  ungroup()%>%
  mutate(ID = as.factor(row_number()))

fit.nb.ADMB.0<- glmmADMB::glmmadmb(sum_Cases ~
                                    1 + (1|Studie),
                                  data=df_nb,
                                  corStruct="full",
                                  zeroInflation=FALSE,
                                  family="nbinom", 
                                  link="log")

anova(fit.nb.ADMB.0, fit.nb.ADMB.1, fit.nb.ADMB.2, fit.nb.ADMB.3) # Non of the models run, not even the first one

fit.nb.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                1 + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                  1 + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.0<-glmmTMB::glmmTMB(sum_Cases ~
                                  1 + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.nb.TMB.0,fit.zinb.TMB.0,fit.hnnb.TMB.0)


fit.nb.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.1<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.nb.TMB.1,fit.zinb.TMB.1,fit.hnnb.TMB.1)
summary(fit.hnnb.TMB.1)


fit.nb.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.2<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.p.TMB.2,fit.zip.TMB.2,fit.hnp.TMB.2)


fit.nb.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + Soort + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.3<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.p.TMB.3,fit.zip.TMB.3,fit.hnp.TMB.3)


fit.nb.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log + Geslacht + Soort + Duur + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log + Geslacht + Soort + Duur + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.4<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log +  + Geslacht + Soort + Duur + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.p.TMB.4,fit.zip.TMB.4,fit.hnp.TMB.4)

fit.nb.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                Dosering_log*Geslacht + Soort + (1|Studie) +
                                offset(log(sum_N)),
                              data=df_nb,
                              ziformula=~0,
                              family=glmmTMB::nbinom2)
fit.zinb.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log*Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~1,
                                family=glmmTMB::nbinom2)
fit.hnnb.TMB.5<-glmmTMB::glmmTMB(sum_Cases ~
                                  Dosering_log*Geslacht + Soort + (1|Studie) +
                                  offset(log(sum_N)),
                                data=df_nb,
                                ziformula=~.,
                                family=glmmTMB::nbinom2)
anova(fit.nb.TMB.5,fit.zinb.TMB.5,fit.hnnb.TMB.5)
anova(fit.hnnb.TMB.0,fit.hnnb.TMB.1,fit.hnnb.TMB.2,fit.hnnb.TMB.3,fit.hnnb.TMB.4, fit.hnnb.TMB.5)
summary(fit.hnnb.TMB.5)
summary(fit.hnnb.TMB.3)
summary(fit.hnnb.TMB.2)
summary(fit.hnnb.TMB.1)

sjPlot::plot_models(fit.hnnb.TMB.1,
                    fit.hnnb.TMB.2,
                    fit.hnnb.TMB.3, 
                    fit.hnnb.TMB.5,
                    axis.labels = rlist::list.reverse(c("Intercept",
                                                        "Dosering (log schaal)", "Geslacht", "Sprague-Dawley vs CD-1", 
                                                        "Swiss Albino vs CD-1", "Wistar vs CD-1", "Dosering * Geslacht")),
                    legend.title ="Modellen",
                    m.labels=c("NB Hurdle Model 1", 
                               "NB Hurdle Model 2",
                               "NB Hurdle Model 3",
                               "NB Hurdle Model 5"),
                    show.p = FALSE,
                    spacing=0.8,
                    p.shape = TRUE,
                    show.intercept = TRUE,
                    auto.label = FALSE,
                    wrap.title = 75,
                    vline.color = "black",
                    title="Vergelijking van vijf Hurdle modellen voor het beoordelen van de relatie tussen tumor en dosering glyfosaat"
                    #,grid = TRUE
) + theme_bw()#+theme(legend.position = "bottom")

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - BAYESIAANSE ANALYSE  - Een simpele regressie ----
df <- read_excel("~/Studie_data.xlsx")
df_lmer<-df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur, Soort)%>%
  group_by(Studie, Geslacht, Dosering)%>%
  arrange(Studie, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            )%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ungroup()

fit_reml <- lmer(sum_Cases~Dosering_log + (1|Studie), data=df_lmer); summary(fit_reml)
fit_mcmc<-
  brms::brm(sum_Cases~Dosering_log + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
prior_draws <- tibble(b_Dosering_log = prior_draws(fit_mcmc, variable = "b_Dosering_log")$b)
post_draws <- as_tibble(as_draws_df(fit_mcmc))
draws <- bind_rows(prior = prior_draws, posterior = post_draws, .id = "dist")%>%
  dplyr::select(dist, b_Dosering_log)
lik_draws<-data.frame(lik = rnorm(201, fit_reml@beta[2], 0.5126))
ggplot()+
  stat_slab(data=draws, aes(x = b_Dosering_log, fill = dist), alpha = 0.5)+
  geom_density(data=lik_draws, aes(x=lik, fill="likelihood"),color = "transparent", alpha = 0.5)+
  scale_fill_manual(name = "", values = c("#4477AA", "#228833", "#CCBB44"))+
  theme_linedraw()+
  labs(x="Dosering log", 
       y="Frequentieverdeling", 
       title="Frequentieverdeling van Dosering (log schaal)",
       subtitle="voor de prior, posterior en likelihood")

fit_mcmc<-
  brms::brm(sum_Cases~Dosering_log + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(normal(0.95, 0.1),    class = b,  coef=Dosering_log),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
prior_draws <- tibble(b_Dosering_log = prior_draws(fit_mcmc, variable = "b_Dosering_log")$b)
post_draws <- as_tibble(as_draws_df(fit_mcmc))
draws <- bind_rows(prior = prior_draws, posterior = post_draws, .id = "dist")%>%
  dplyr::select(dist, b_Dosering_log)
lik_draws<-data.frame(lik = rnorm(201, fit_reml@beta[2], 0.5126))
ggplot()+
  stat_slab(data=draws, aes(x = b_Dosering_log, fill = dist), alpha = 0.5)+
  geom_density(data=lik_draws, aes(x=lik, fill="likelihood"),color = "transparent", alpha = 0.5)+
  scale_fill_manual(name = "", values = c("#4477AA", "#228833", "#CCBB44"))+
  theme_linedraw()+
  labs(x="Dosering log", 
       y="Frequentieverdeling", 
       title="Frequentieverdeling van Dosering (log schaal)",
       subtitle="voor de prior, posterior en likelihood")

fit_reml <- lmer(sum_Cases~Dosering_log*Geslacht + (1|Studie), data=df_lmer); summary(fit_reml)
fit_mcmc<-
  brms::brm(sum_Cases~Dosering_log*Geslacht + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log),
              prior(uniform(-5,5),    class = b,  coef=GeslachtVrouw),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log:GeslachtVrouw),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
summary(fit_mcmc)
pp_check(fit_mcmc, ndraws = 500)
plot(fit_mcmc)
ce<-conditional_effects(fit_mcmc, surface = TRUE)
pl_ce<-plot(ce, theme=theme_bw())
pl_ce

df_lmer %>%
  group_by(Studie, Geslacht) %>%
  data_grid(Dosering_log = seq_range(Dosering_log, n = 101)) %>%
  add_epred_draws(fit_mcmc, ndraws = 100) %>%
  ggplot(aes(x = Dosering_log, y = sum_Cases, color = Geslacht)) +
  geom_line(aes(y = .epred, group = paste(Studie,Geslacht,.draw)), alpha = .1) +
  geom_point(data = df_lmer, show.legend = FALSE) +
  theme_bw()+
  facet_wrap(~Studie, ncol=3, scales="free")+
  theme(legend.position = "bottom")+
  labs(x="Dosering (log schaal)", 
       y="Cumulatief aantal kankergevallen",
       title="Data en uitkomsten van een Bayesiaans model",
       subtitle="per studie,per dosering en per geslacht", 
       col="Geslacht", 
       caption="Bolletjes zijn datapunten; lijnen zijn de voorspelde regressielijnen op basis van het model")
ggsave("BayesianGauss.png", width = 35, height = 25, units = "cm", dpi=300)


plot(density(predict(fit_mcmc)[,1]-df_lmer$sum_Cases))
mean(predict(fit_mcmc)[,1]-df_lmer$sum_Cases)
sd(predict(fit_mcmc)[,1]-df_lmer$sum_Cases)
var(predict(fit_mcmc)[,1]-df_lmer$sum_Cases)

pp_check(fit_mcmc, type = "error_hist", ndraws = 11)
pp_check(fit_mcmc, type = "scatter_avg", ndraws = 100)
pp_check(fit_mcmc, type = "stat_2d")
pp_check(fit_mcmc, type = "loo_pit")

post <- posterior_samples(fit_mcmc)
n_lines <- 150
df_lmer %>% 
  ggplot(aes(x = Dosering_log, y = sum_Cases)) +
  geom_abline(intercept = post[1:n_lines, 1], 
              slope     = post[1:n_lines, 2],
              color = "grey50", size = 1/4, alpha = .3) +
  geom_point(shape = 1) +
  labs(subtitle = eval(substitute(paste(n_lines, 
                                        "voorspellingen op basis van Bayesiaans model"))),
       x = "Aantal kankergevallen",
       y = "Dosering (log schaal)", 
       title="Relatie tussen Dosering (op de log schaal) en het aantal kanker gevallen", 
       caption="Lijnen komen uit Bayesiaans model, bolletjes zijn geobserveerde data") +
  theme_bw()

df_lmer %>% 
  ggplot(aes(x = Dosering_log, y = sum_Cases)) +
  geom_abline(intercept = post[1:n_lines, 1] + post[1:150, 5]%>%purrr::map(\(x) rnorm(1,0, x))%>%unlist(), 
              slope     = post[1:n_lines, 2],
              color = "grey50", size = 1/4, alpha = .3) +
  geom_point(shape = 1) +
  labs(subtitle = eval(substitute(paste(n_lines, 
                                        "voorspellingen op basis van Bayesiaans model"))),
       x = "Aantal kankergevallen",
       y = "Dosering (log schaal)", 
       title="Relatie tussen Dosering (op de log schaal) en het aantal kanker gevallen", 
       caption="Lijnen komen uit Bayesiaans model, bolletjes zijn geobserveerde data") +
  theme_bw()

colnames(post)
df_lmer %>%
  ggplot(aes(Dosering_log, y = sum_Cases, group=Geslacht)) +
  geom_abline(intercept = post[1:n_lines, 1] + (post[1:n_lines, 3]*0) + post[1:150, 5]%>%purrr::map(\(x) rnorm(1,0, x))%>%unlist(), 
              slope     = post[1:n_lines, 2] + post[1:n_lines, 2]*(post[1:n_lines, 4]*0), 
              size = 1/4, 
              alpha = .2, 
              col="red") +
  geom_abline(intercept = mean(post[1:n_lines, 1]) + mean((post[1:n_lines, 3]*0)), 
              slope     = mean(post[1:n_lines, 2]) + mean(post[1:n_lines, 2]*(post[1:n_lines, 4]*0)), 
              size = 2, 
              alpha = 1,
              col="red") +
  geom_abline(intercept = post[1:n_lines, 1] + (post[1:n_lines, 3]*1) + post[1:150, 5]%>%purrr::map(\(x) rnorm(1,0, x))%>%unlist(), 
              slope     = post[1:n_lines, 2] + post[1:n_lines, 2]*(post[1:n_lines, 4]*1), 
              size = 1/4, 
              alpha = .2, 
              col="blue") +
  geom_abline(intercept = mean(post[1:n_lines, 1]) + mean((post[1:n_lines, 3]*1)), 
              slope     = mean(post[1:n_lines, 2]) + mean(post[1:n_lines, 2]*(post[1:n_lines, 4]*1)), 
              size = 2, 
              alpha = 1, 
              col="blue") +
  geom_point(shape = 1) +
  labs(subtitle = eval(substitute(paste(n_lines, 
                                        "voorspellingen voor vrouwen (blauw) en mannen (rood)"))),
       x = "Aantal kankergevallen",
       y = "Dosering (log schaal)",
       title="Relatie tussen Dosering (op de log schaal) en het aantal kanker gevallen", 
       caption="Lijnen komen uit Bayesiaans model, bolletjes zijn geobserveerde data") +
  theme_bw()

df_lmer %>%
  add_epred_draws(fit_mcmc, 
                  ndraws=50) %>%
  ggplot(aes(x = Dosering_log, 
             y = sum_Cases, 
             color = as.factor(Studie), 
             group=Studie)) +
  geom_line(aes(y = .epred, group = paste(Studie, .draw)), alpha = 0.25, show.legend = FALSE) +
  geom_line(data = df_lmer, color="black") +
  geom_point(data = df_lmer, color="black")+
  theme_bw()+
  facet_grid(~Geslacht)
df_lmer %>%
  add_epred_draws(fit_mcmc, 
                  ndraws=50) %>%
  ggplot(aes(x = Dosering_log, 
             y = sum_Cases, 
             color = as.factor(Geslacht), 
             group=Geslacht)) +
  geom_line(aes(y = .epred, group = paste(Geslacht, Studie, .draw)), alpha = 0.25, show.legend = FALSE) +
  geom_line(data = df_lmer, color="black") +
  geom_point(data = df_lmer, color="black")+
  facet_wrap(~Studie, scales="free", ncol=3)+
  theme_bw()+
  labs(x="Dosering (log schaal)", 
       y="Totaal aantal kankergevallen", 
       title="Totaal aantal kankergevallen als functie van glyfosaat dosering per geslacht", 
       caption="Gekleurde lijnen zijn voorspellingen afkomstig uit Bayesiaans model, zwarte lijnen zijn geobserveerde gegevens")

data.frame(Male=post$b_Dosering_log, 
           Female=post$b_Dosering_log+post$b_GeslachtVrouw, 
           ID=seq(1,dim(post)[1],1))%>%
  pivot_longer(!ID, names_to = "Geslacht")%>% 
  ggplot(aes(x = value, y = 0, group=Geslacht, fill=Geslacht)) +
  stat_histinterval(point_interval = mode_hdi, .width = .95, slab_color = "grey",
                    breaks = 40, slab_size = .2, outline_bars = T) +
  stat_histinterval(point_interval = mean_qi, 
                    .width = .95,
                    point_color = "green",
                    slab_type = "pdf",
                    slab_size = .2,
                    alpha=0.5, show.legend = FALSE) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "De posterior verdeling van Dosering per Geslacht",
       subtitle = "Mode and 85% HDI interval (grey) next to mean and 95% confidence interval (seagreen)",
       x = expression(beta[1]~(slope))) +
  theme_bw()+
  theme(legend.position = "bottom")


fit_mcmc_1<-
  brms::brm(sum_Cases~1 + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
fit_mcmc_2<-
  brms::brm(sum_Cases~Dosering_log + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
fit_mcmc_3<-
  brms::brm(sum_Cases~Geslacht + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=GeslachtVrouw),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
fit_mcmc_4<-
  brms::brm(sum_Cases~Dosering_log+Geslacht + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log),
              prior(uniform(-5,5),    class = b,  coef=GeslachtVrouw),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
fit_mcmc_5<-
  brms::brm(sum_Cases~Dosering_log*Geslacht + (1|Studie), data=df_lmer,
            prior = c(
              prior(uniform(-5,5),    class = Intercept),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log),
              prior(uniform(-5,5),    class = b,  coef=GeslachtVrouw),
              prior(uniform(-5,5),    class = b,  coef=Dosering_log:GeslachtVrouw),
              prior(gamma(2,2),     class = sigma), 
              prior(gamma(2,2),     class = sd, group=Studie)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)

comparison <- bayesfactor_models(fit_mcmc_5,
                                 fit_mcmc_4, 
                                 fit_mcmc_3,
                                 fit_mcmc_2, 
                                 denominator = fit_mcmc_1)
result<-as.matrix(comparison);result


fit_mcmc_2



## Evidence for NULL model vs FULL model 
BF_Dosering_0 <-brms::hypothesis(fit_mcmc_2,
                             "Dosering_log = 0", 
                             robust=TRUE, 
                             alpha=0.05)
BF_Dosering_less <-brms::hypothesis(fit_mcmc_2,
                                 "Dosering_log < 0", 
                                 robust=TRUE, 
                                 alpha=0.05)
BF_Dosering_more <-brms::hypothesis(fit_mcmc_2,
                                    "Dosering_log > 0", 
                                    robust=TRUE, 
                                    alpha=0.05)

#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - BAYESIAANSE ANALYSE  - Bayesiaanse analyse van het hurdle model ----
df <- read_excel("~/Data/Studie_data.xlsx")
df_lmer<-df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur, Soort)%>%
  group_by(Studie, Geslacht, Dosering)%>%
  arrange(Studie, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ungroup()

g1<-df%>%
  ggplot(aes(x=Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen")
g2<-df_lmer%>%
  ggplot(aes(x=sum_Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van totaal aantal kankergevallen")
gridExtra::grid.arrange(g1,g2)


# Set some global options
CHAINS <- 4
ITER <- 40000
WARMUP <- 3000
CORES <-6
BAYES_SEED <- 1234

mcmc_hurdle_0 <- brm(
  bf(sum_Cases ~ 1,
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
summary(mcmc_hurdle_0)
pp_check(mcmc_hurdle_0, ndraws = 500)
pp_check(mcmc_hurdle_0, type = "error_hist", ndraws = 11)
pp_check(mcmc_hurdle_0, type = "scatter_avg", ndraws = 100)
pp_check(mcmc_hurdle_0, type = "stat_2d")
pp_check(mcmc_hurdle_0, type = "loo_pit")

hu_intercept <- tidy(mcmc_hurdle_0) %>%
  pull(estimate)%>%
  last()
plogis(hu_intercept) # probability of zero's 

df_lmer%>%
  ungroup()%>%
  mutate(is_zero = ifelse(sum_Cases == 0, 1, 0))%>%
  count(is_zero) %>% 
  mutate(prop = n / sum(n))


df_lmer2<-df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Duur, Soort)%>%
  group_by(Studie, Geslacht, Soort, Dosering)%>%
  arrange(Studie, Geslacht, Soort, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  mutate(Dosering_log = log10(Dosering+0.00001))%>%
  ungroup()
g3<-df_lmer2%>%
  ggplot(aes(x=sum_Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van totaal aantal kankergevallen")
gridExtra::grid.arrange(g1,g2, g3)

mcmc_hurdle_1 <- brm(
  bf(sum_Cases ~ 1 + (1|Studie),
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_2 <- brm(
  bf(sum_Cases ~ Dosering_log + (1|Studie),
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_3 <- brm(
  bf(sum_Cases ~ Geslacht + (1|Studie),
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_4 <- brm(
  bf(sum_Cases ~ Dosering_log + Geslacht + (1|Studie),
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_5 <- brm(
  bf(sum_Cases ~ Dosering_log*Geslacht + (1|Studie),
     hu ~ 1),
  data = df_lmer,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_6 <- brm(
  bf(sum_Cases ~ Dosering_log + Geslacht + Soort + (1|Studie),
     hu ~ 1),
  data = df_lmer2,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)
mcmc_hurdle_7 <- brm(
  bf(sum_Cases ~ Dosering_log*Geslacht + Soort + (1|Studie),
     hu ~ 1),
  data = df_lmer2,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)

comparison <- bayesfactor_models(mcmc_hurdle_7,
                                 mcmc_hurdle_6,
                                 mcmc_hurdle_5,
                                 mcmc_hurdle_4, 
                                 mcmc_hurdle_3,
                                 mcmc_hurdle_2, 
                                 denominator = mcmc_hurdle_1)
result<-as.matrix(comparison);result

mcmc_hurdle_6 <- brm(
  bf(sum_Cases ~ Dosering_log + Geslacht + Soort + (1|Studie),
     hu ~ 1),
  data = df_lmer2,
  family = hurdle_negbinomial,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, cores=CORES,
  silent = 2)

BF_Dosering_0 <-brms::hypothesis(mcmc_hurdle_6,
                                 "Dosering_log = 0", 
                                 robust=TRUE, 
                                 alpha=0.05)
BF_Dosering_less <-brms::hypothesis(mcmc_hurdle_6,
                                    "Dosering_log < 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_more <-brms::hypothesis(mcmc_hurdle_6,
                                    "Dosering_log > 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_0;BF_Dosering_less;BF_Dosering_more

BF_Dosering_0_Studie<-brms::hypothesis(mcmc_hurdle_6,
                                 "Dosering_log = 0", 
                                 group="Studie",
                                 robust=TRUE, 
                                 scope = "coef",
                                 alpha=0.05)
BF_Dosering_less_Studie <-brms::hypothesis(mcmc_hurdle_6,
                                    "Dosering_log < 0", 
                                    group="Studie",
                                    robust=TRUE, 
                                    scope = "coef", 
                                    alpha=0.05)
BF_Dosering_more_Studie <-brms::hypothesis(mcmc_hurdle_6,
                                    "Dosering_log > 0", 
                                    group="Studie",
                                    robust=TRUE, 
                                    scope = "coef",
                                    alpha=0.05)
BF_Dosering_0_Studie;BF_Dosering_less_Studie;BF_Dosering_more_Studie

brms::hypothesis(mcmc_hurdle_6,
                 "Dosering_log = 0",
                 group="Studie",
                 robust=TRUE, 
                 scope = "coef", 
                 alpha=0.05)
brms::hypothesis(mcmc_hurdle_6,
                 "Dosering_log = 0",
                 group="Studie",
                 robust=TRUE, 
                 scope = "coef", 
                 alpha=0.05)
brms::hypothesis(mcmc_hurdle_6,
                 "Dosering_log = 0",
                 group="Studie",
                 robust=TRUE, 
                 scope = "coef", 
                 alpha=0.05)

pp_check(mcmc_hurdle_6, ndraws=500)

df_lmer2 %>%
  add_epred_draws(mcmc_hurdle_6, 
                  ndraws=50) %>%
  ggplot(aes(x = Dosering, 
             y = sum_Cases,  
             group=interaction(Studie, Geslacht), 
             col=Geslacht)) +
  geom_line(aes(y = .epred, group = paste(Geslacht, Soort, Studie, .draw)), alpha = 0.25, show.legend = FALSE) +
  geom_line(data = df_lmer2, color="black") +
  geom_point(data = df_lmer2, color="black")+
  theme_bw()+
  facet_wrap(~Studie, ncol=3, scales = "free")+
  labs(x="Dosering", 
       y="Totaal aantal tumoren", 
       title="Relatie tussen dosering en totaal aantal tumoren", 
       subtitle="per geslacht en soort (Studie)", 
       caption="Gekleurde lijnen zijn voorspellingen uit het Bayesiaanse model, zwarte lijnen en bollen zijn geobserveerde waarden")


#### GLYFOSAAT: DOSE-RESPONSE ANALYSES - BAYESIAANSE ANALYSE - Modelleren van de verandering: vóóraf vs. áchteraf ----
df <- read_excel("~/Studie_data.xlsx")

df%>%
  dplyr::select(Dosering, Cases)%>%
  group_by(Dosering)%>%
  summarize(sum=sum(Cases))%>%
  ggplot(., aes(x=Dosering, y=sum, group=1))+
  geom_point()+
  geom_line()+
  theme_bw()

df%>%
  dplyr::select(Dosering, Cases)%>%
  filter(Dosering==0)%>%
  ggplot(aes(x=Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen voor de nuldosering")

df%>%
  dplyr::select(Dosering, Cases)%>%
  filter(Dosering==0)%>%
  summarise(mean=mean(Cases), 
            sd=sd(Cases), 
            max=sum(Cases), 
            n=n())

p1<-bayesrules::plot_normal(3.93,6.88)
p2<-bayesrules::plot_gamma_poisson(3.93/4,0.25,806,205, posterior = FALSE, likelihood = FALSE)
p3<-bayesrules::plot_gamma(3.93/4,0.25)
gridExtra::grid.arrange(p1,p2,p3)+theme_bw()

df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases, Soort)%>%
  filter(Dosering==0)%>%
  group_by(Studie, Geslacht, Soort)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  ungroup()%>%
  ggplot()+
  geom_density(aes(x=sum_Cases, fill=Geslacht), alpha=0.5)+
  facet_wrap(~Soort, ncol=2, scales="free")+
  theme_bw()+
  labs(x="Totaal aantal kankergevallen", 
       y="Frequentieverdeling", 
       title="Frequentieverdeling van het totaal aantal kankergevallen per geslacht en soort", 
       subtitle="voor de nul-meting", 
       caption="Te weinig data voor Swiss-Albino om een frequentieverdeling te maken")

df%>%
  dplyr::select(Dosering, Soort)%>%
  filter(Soort=="Swiss Albino")%>%
  table()
  

df%>%
  dplyr::select(Dosering, Cases)%>%
  filter(Dosering>0)%>%
  ggplot(aes(x=Cases))+
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "grey", alpha=0.4)+
  geom_density(color = "firebrick", lty=2, size=1)  +
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen bij glyfosaat")

df%>%
  dplyr::select(Dosering, Cases)%>%
  filter(Dosering>0)%>%
  summarise(mean=mean(Cases), 
            sd=sd(Cases), 
            max=sum(Cases), 
            n=n())

df%>%
  dplyr::select(Dosering, Cases)%>%
  mutate(Class = if_else(Dosering==0, "Nul-dosering", "Dosering"))%>%
  ggplot(aes(x=Cases, group=Class, fill=Class, col=Class))+
  geom_histogram(aes(y = ..density.., fill=Class), alpha=0.4)+
  theme_bw()+
  labs(x="Gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen bij wel of geen dosering")+
  theme(legend.position = "bottom")


df%>%
  dplyr::select(Studie, Cases, N, Dosering)%>%
  mutate(Class = if_else(Dosering==0, "Nul-dosering", "Dosering"))%>%
  dplyr::select(-Dosering)%>%
  group_by(Studie, Class)%>%
  summarise(sum_Cases   = sum(Cases), 
         sum_N          = sum(N),
         mean_N         = sum(N)/n(), 
         ratio          = sum_Cases/sum_N)%>%
  ggplot(aes(x=ratio, group=Class, fill=Class, col=Class))+
  geom_histogram(aes(y = ..density.., fill=Class), alpha=0.4)+
  theme_bw()+
  labs(x="Ratio gerapporteerde aantal kankergevallen", 
       y="Verdeling van waarden", 
       title="Verdeling van aantal kankergevallen bij wel of geen dosering", 
       subtitle = "y-waarde is een ratio: totaal aantal kankergevallen /totaal aantal observaties")+
  theme(legend.position = "bottom")


df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases, Soort)%>%
  filter(Dosering>0)%>%
  group_by(Studie, Geslacht, Soort, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  ungroup()%>%
  ggplot()+
  geom_density(aes(x=sum_Cases, fill=Geslacht), alpha=0.5)+
  facet_wrap(~Soort, ncol=2, scales="free")+
  theme_bw()+
  labs(x="Totaal aantal kankergevallen", 
       y="Frequentieverdeling", 
       title="Frequentieverdeling van het totaal aantal kankergevallen per geslacht en soort", 
       subtitle="voor de glyfosaat metingen")

df%>%
  dplyr::select(Studie, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Pre", "Post"))%>%
  group_by(Studie, Group)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n())%>%
  group_by(Group)%>%
  summarise(Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)


df_binom_group<-
  df%>%
  dplyr::select(Studie, Soort, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Studie, Group)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n())%>%
  group_by(Studie, Group)%>%
  summarise(Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N))#%>%
  #print(n=26)

df_binom_group

brms::get_prior(Cases | trials(N)~Group + (1|Studie), 
                family=binomial,data=df_binom_group)

mcmc_binom_0<-
  brms::brm(Cases | trials(N) ~ 1 + (1|Studie), 
            family=binomial, data=df_binom_group,
            #prior = c(
            #  prior(uniform(-5,5),    class = Intercept),
            #  prior(gamma(2,2),       class = sd)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_1<-
  brms::brm(Cases | trials(N) ~ Group + (1|Studie), 
            family=binomial, data=df_binom_group,
            #prior = c(
            #  prior(uniform(-5,5),    class = Intercept),
            #  prior(uniform(-5,5),    class = b),
            #  prior(gamma(2,2),       class = sd)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
comparison <- bayesfactor_models(mcmc_binom_1,
                                 denominator = mcmc_binom_0)
result<-as.matrix(comparison);result

summary(mcmc_binom_1)
plot(mcmc_binom_1)
pp_check(mcmc_binom_1, ndraws = 500)
pp_check(mcmc_binom_1, type = "stat_2d")
pp_check(mcmc_binom_1, type = "loo_pit")
conditional_effects(mcmc_binom_1)

stanplot(mcmc_binom_1, type = "areas",prob = 0.95)
exp(fixef(mcmc_binom_1)[,-2])

brms_binom_emmean<-emmeans (mcmc_binom_1,  ~ Group)
brms_binom_contrast<-contrast(brms_binom_emmean, "pairwise")
brms_binom_emmean;brms_binom_contrast

mcmc_binom_1 %>%
  recover_types(df_binom_group) %>%
  emmeans::emmeans( ~ Group, type = "response") %>%
  gather_emmeans_draws() %>%
  mutate(prob = plogis(.value))%>%
  ggplot(aes(x = prob, 
             y = Group)) +
  stat_halfeye(alpha=0.5)+
  theme_bw()+
  labs(x="Kans op kanker", 
       title="Kans op kanker vs kans op geen kanker voor de behandeling en de controle", 
       subtitle="Binomiaal model")

mcmc_binom_1 %>%
  recover_types(df_binom_group) %>%
  emmeans::emmeans( ~ Group) %>%
  emmeans::contrast(method = "revpairwise", type="response") %>%
  tidybayes::gather_emmeans_draws()%>%
  ggplot(., aes(x = exp(.value),
             y = contrast)) +
  stat_halfeye(alpha=0.5)+
  geom_vline(xintercept = 1, lty=2, col="black")+
  geom_vline(aes(xintercept = mean(exp(.value))), lty=2, col="red")+
  theme_bw()+
  labs(x="Odds Ratio", 
       title="Kans op kanker vs kans op geen kanker voor de behandeling en de controle", 
       subtitle="Binomiaal model")

BF_Dosering_0 <-brms::hypothesis(mcmc_binom_1,
                                 "GroupTreatment = 0", 
                                 robust=TRUE, 
                                 alpha=0.05)
BF_Dosering_less <-brms::hypothesis(mcmc_binom_1,
                                    "GroupTreatment < 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_more <-brms::hypothesis(mcmc_binom_1,
                                    "GroupTreatment > 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_0;BF_Dosering_less;BF_Dosering_more




df_binom_all<-
  df%>%
  dplyr::select(Studie, Soort, Geslacht, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Studie, Group, Soort, Geslacht)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n(),
            Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N));
df_binom_all%>%print(n=52)



mcmc_binom_0<-
  brms::brm(Cases | trials(N) ~ 1 + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_1<-
  brms::brm(Cases | trials(N) ~ Group + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_2<-
  brms::brm(Cases | trials(N) ~ Geslacht + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_3<-
  brms::brm(Cases | trials(N) ~ Soort + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_4<-
  brms::brm(Cases | trials(N) ~ Group + Geslacht + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_5<-
  brms::brm(Cases | trials(N) ~ Group + Geslacht + Soort + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_6<-
  brms::brm(Cases | trials(N) ~ Group *Geslacht + Soort + (1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
mcmc_binom_7<-
  brms::brm(Cases | trials(N) ~ Group * Soort + Geslacht +(1|Studie), 
            family=binomial, data=df_binom_all,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
comparison <- bayesfactor_models(mcmc_binom_7,
                                 mcmc_binom_6,
                                 mcmc_binom_5,
                                 mcmc_binom_4,
                                 mcmc_binom_3, 
                                 mcmc_binom_2, 
                                 mcmc_binom_1,
                                 denominator = mcmc_binom_0)
result<-as.matrix(comparison);result


summary(mcmc_binom_7)
plot(mcmc_binom_7)
pp_check(mcmc_binom_7, ndraws = 500)
pp_check(mcmc_binom_7, type = "stat_2d")
pp_check(mcmc_binom_7, type = "loo_pit")
conditional_effects(mcmc_binom_7)

brms_binom_emmean<-emmeans (mcmc_binom_7,  ~ Group | Soort)
brms_binom_contrast<-contrast(brms_binom_emmean, "pairwise")
brms_binom_emmean;brms_binom_contrast

mcmc_binom_7 %>%
  recover_types(df_binom_all) %>%
  emmeans::emmeans( ~ Group | Soort, type = "response") %>%
  gather_emmeans_draws() %>%
  mutate(prob = plogis(.value))%>%
  ggplot(aes(x = prob, 
             y = Soort, 
             fill=Group)) +
  stat_halfeye(alpha=0.5)+
  theme_bw()+
  xlim(0,0.4)+
  labs(x="Kans op kanker", 
       title="Kans op kanker vs kans op geen kanker voor de behandeling en de controle", 
       subtitle="Per soort - binomiaal model")

mcmc_binom_7 %>%
  recover_types(df_binom_all) %>%
  emmeans::emmeans( ~ Group | Soort) %>%
  emmeans::contrast(method = "revpairwise", type="response") %>%
  tidybayes::gather_emmeans_draws()%>%
  mutate(.OR=mean(exp(.value)))%>%
  ggplot(., aes(x = exp(.value),
                y = Soort,
                )) +
  stat_halfeye(alpha=0.5)+
  geom_vline(xintercept = 1, lty=2, col="black")+
  #geom_vline(aes(xintercept = .OR), lty=2, col="red")+
  theme_bw()+
  xlim(0,4)+
  labs(x="Odds Ratio", 
       title="Kans op kanker vs kans op geen kanker voor de behandeling en de controle", 
       subtitle="Per soort - binomiaal model")

BF_Dosering_0 <-brms::hypothesis(mcmc_binom_7,
                                 "GroupTreatment = 0", 
                                 robust=TRUE, 
                                 alpha=0.05)
BF_Dosering_0_SA<-brms::hypothesis(mcmc_binom_7,
                 "GroupTreatment:SoortSwissAlbino = 0", 
                 robust=TRUE,
                 alpha=0.05)
BF_Dosering_less <-brms::hypothesis(mcmc_binom_7,
                                    "GroupTreatment < 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_less_SA<- brms::hypothesis(mcmc_binom_7,
                 "GroupTreatment:SoortSwissAlbino < 0", 
                 robust=TRUE, 
                 alpha=0.05)
BF_Dosering_more <-brms::hypothesis(mcmc_binom_7,
                                    "GroupTreatment > 0", 
                                    robust=TRUE, 
                                    alpha=0.05)
BF_Dosering_more_SA<-brms::hypothesis(mcmc_binom_7,
                 "GroupTreatment:SoortSwissAlbino > 0", 
                 robust=TRUE, 
                 alpha=0.05)
BF_Dosering_0;BF_Dosering_less;BF_Dosering_more
BF_Dosering_0_SA;BF_Dosering_less_SA;BF_Dosering_more_SA




plogis(-2.78 + 0.47) = 0.09
plogis(-2.78 + 0.47 + 0.09 + 0.52) =0.154


df%>%
  dplyr::select(Studie, Soort, Geslacht, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Studie, Group, Geslacht, Soort)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n(),
            Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N))%>%
  filter(Soort=="Swiss Albino")

df_binom_all%>%
  filter(Soort=="Swiss Albino")%>%
  ggplot(aes(x=Group, y=ratio_sum, col=Geslacht, group=Geslacht))+
  geom_point()+
  geom_line()+
  theme_bw()

df_binom_all %>%
  add_epred_draws(mcmc_binom_7, 
                  ndraws=200) %>%
  filter(Soort=="Swiss Albino")%>%
  mutate(ratio_pred = .epred/N)%>%
  ggplot(aes(x = Group, 
             y = ratio_sum, 
             col=Geslacht, 
             group=Geslacht)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  geom_line(aes(y = ratio_pred,  group = paste(Geslacht,Studie, .draw)), alpha = 0.25, show.legend = FALSE) +
  facet_grid(~Geslacht)+
  labs(x="Groep", 
       y="Kans op kanker", 
       title="Kans op kanker voor Swiss-Albino ratten",
       subtitle="Per Geslacht", 
       caption="Dunne lijnen zijn voorspellingen afkomstig uit Bayesiaans model, dikke lijnen plus bollen zijn geobserveerde ratios")

df_binom_all%>%
  filter(Soort=="Swiss Albino")%>%
  ggplot(aes(x=Group, y=ratio_sum, col=Geslacht, group=Geslacht))+
  geom_point()+
  geom_line()+
  theme_bw()

df_binom_group2<-
  df%>%
  dplyr::select(Studie, Soort, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Group)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n())%>%
  group_by(Group)%>%
  summarise(Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N))
df_binom_group2


df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Soort)%>%
  filter(Soort=="Swiss Albino")%>%
  group_by(Studie, Geslacht, Dosering)%>%
  arrange(Studie, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  ungroup()%>%
  ggplot(aes(x=Dosering, y=sum_Cases/sum_N, col=Geslacht))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Dosering", 
       y="Ratio kankergevallen", 
       col="Geslacht", 
       title="Kans op kanker voor Swiss Albino ratten", 
       subtitle="per Geslacht", 
       caption="gebaseerd op de studie van Kumar")

df%>%
  dplyr::select(Studie, Dosering, Geslacht)%>%
  group_by(Studie, Geslacht)%>%
  summarise(max_dose = max(Dosering))%>%
  ggplot()+
  geom_bar(aes(x=Studie,y=max_dose, fill=Geslacht), stat="identity", position="dodge")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="Studie", 
       y="Max dosis glyfosaat", 
       title="Max dosis glyfosaat per studie en geslacht")
  
df%>%
  dplyr::select(Studie, Jaar, Geslacht, Dosering, Tumor, N, Cases, Soort)%>%
  filter(Studie== "Kumar" | Studie=="Knezevich and Hogan" | Studie=="Stout and Ruecker" | Studie=="Takahashi")%>%
  group_by(Studie, Geslacht, Dosering)%>%
  arrange(Studie, Geslacht, Dosering)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N))%>%
  ungroup()%>%
  ggplot(aes(x=Dosering, y=sum_Cases/sum_N, col=Studie))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_grid(~Geslacht)+
  theme(legend.position = "bottom")+
  labs(x="Dosering", 
       y="Prpportie kankergevallen", 
       title="Proportie totaal aantal kankergevallen ",
       subtitle="per studie, geslacht en dosering")


df%>%
  dplyr::select(Studie, Geslacht, Dosering, N, Cases)%>%
  group_by(Studie, Dosering, Geslacht)%>%
  summarise(N      = sum(N), 
            Cases  = sum(Cases))%>%
  ungroup()%>%
  rowwise()%>%
  mutate(prop  = (prop.test(Cases,N)[[4]][1])*100,
         low   = (prop.test(Cases,N)$conf.int[1])*100, 
         high  = (prop.test(Cases,N)$conf.int[2])*100) %>%
  ungroup()%>%
  filter(Studie== "Kumar" | Studie=="Knezevich and Hogan" | Studie=="Stout and Ruecker" | Studie=="Takahashi")%>%
  ggplot(aes(x=Dosering, y=(Cases/N)*100, col=Studie, group=Studie, fill=Studie))+
  geom_point(position=position_dodge(width = 0.5))+
  geom_line(position=position_dodge(width = 0.5))+
  geom_pointrange(aes(ymin=low, 
                      ymax=high),position=position_dodge(width = 0.5))+
  geom_ribbon(aes(ymin=low, 
  ymax=high),position=position_dodge(width = 0.5), alpha=0.2)+
  theme_bw()+
  facet_wrap(~Geslacht, ncol=1, scales='free')+
  labs(x="Dosering", 
       y="Prpportie kankergevallen", 
       title="Proportie totaal aantal kankergevallen ",
       subtitle="per studie, geslacht en dosering")+
  theme(legend.position = "bottom")

df_binom_group<-
  df%>%
  dplyr::select(Studie, Soort, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Studie, Group)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n())%>%
  group_by(Studie, Group)%>%
  summarise(Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N))#%>%
df_binom_group<-
  df%>%
  dplyr::select(Studie, Soort, Dosering, Tumor, N, Cases)%>%
  mutate(Group = if_else(Dosering==0, "Control", "Treatment"))%>%
  group_by(Studie, Group)%>%
  summarise(sum_Cases      = sum(Cases), 
            sum_N          = sum(N), 
            mean_N         = sum(N)/n())%>%
  group_by(Studie, Group)%>%
  summarise(Cases = sum(sum_Cases), 
            N = sum(sum_N), 
            N_mean = sum(mean_N), 
            ratio_sum = Cases/N, 
            ratio_mean = Cases/N_mean)%>%
  mutate(N=as.integer(N))
mcmc_binom_0<-
  brms::brm(Cases | trials(N) ~ 1 + (1|Studie), 
            family=binomial, data=df_binom_group,
            #prior = c(
            #prior(beta(1,15),    class = Intercept),
            #  prior(gamma(2,2),       class = sd)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
plogis(fixef(mcmc_binom_0)[1]) = 0.079

mcmc_binom_group_beta<-
  brms::brm(Cases | trials(N) ~ Group + (1|Studie), 
            family=binomial, data=df_binom_group,
            prior = c(
              #prior(beta(1,1),    class = Intercept),
              prior(beta(1,15),    class = b),
              prior(gamma(2,2),    class = sd)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
prior_draws_beta <- tibble(b_GroupTreatment = prior_draws(mcmc_binom_group_beta, 
                                                          variable = "b_GroupTreatment")$b)%>%
  mutate(Distribution="Beta(1,15)")
post_draws_beta <- as_tibble(as_draws_df(mcmc_binom_group_beta))
draws_beta <- bind_rows(prior = prior_draws_beta, 
                   posterior = post_draws_beta, 
                   .id = "dist")%>%
  dplyr::select(dist, b_GroupTreatment)
g1<-ggplot()+
  stat_slab(data=draws_beta, aes(x = b_GroupTreatment, fill = dist), alpha = 0.5, show.legend = FALSE)+
  scale_fill_manual(name = "", values = c("#4477AA", "#228833", "#CCBB44"))+
  theme_linedraw()+
  geom_vline(aes(xintercept = mean(draws_beta$b_GroupTreatment[draws_beta$dist=="posterior"])),
                 col="#4477AA", lty=2)+
  xlim(-0.5,0.5)+
  labs(x="Effect van behandeling", 
       y="Frequentieverdeling", 
       title="Frequentieverdeling voor het effect van behandeling",
       subtitle="voor de prior en posterior", 
       caption="prior behandeling is Beta (1,15)")

mcmc_binom_group_normal<-
  brms::brm(Cases | trials(N) ~ Group + (1|Studie), 
            family=binomial, data=df_binom_group,
            prior = c(
              prior(normal(-0.5,0.2),    class = b),
              prior(gamma(2,2),       class = sd)),
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            chains=4, 
            cores=6,
            iter = 40000,
            warmup = 3000)
prior_draws_normal <- tibble(b_GroupTreatment = prior_draws(mcmc_binom_group_normal, 
                                                            variable = "b_GroupTreatment")$b)%>%
  mutate(Distribution="Normal(-0.5,1)")
post_draws_normal <- as_tibble(as_draws_df(mcmc_binom_group_normal))
draws_normal <- bind_rows(prior = prior_draws_normal, 
                   posterior = post_draws_normal, 
                   .id = "dist")%>%
  dplyr::select(dist, b_GroupTreatment)
g2<-ggplot()+
  stat_slab(data=draws_normal, aes(x = b_GroupTreatment, fill = dist), alpha = 0.5)+
  scale_fill_manual(name = "", values = c("#4477AA", "#228833", "#CCBB44"))+
  theme_linedraw()+
  geom_vline(aes(xintercept = mean(draws_normal$b_GroupTreatment[draws_normal$dist=="posterior"])),
             col="#4477AA", lty=2)+
  xlim(-0.5,0.5)+
  theme(legend.position = "bottom")+
  labs(x="Effect van behandeling", 
       y="Frequentieverdeling", 
       #title="Frequentieverdeling voor het effect van behandeling",
       #subtitle="voor de prior en posterior",
       caption="prior behandeling is Normal (0.5,1)")
gridExtra::grid.arrange(g1,g2,ncol=1)










