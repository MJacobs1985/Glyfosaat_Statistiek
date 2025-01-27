
set.seed(42);
wdata = data.frame(
  Geslacht = factor(rep(c("Vrouw", "Man"), each=1000)),
  Lengte = c(rnorm(1000, 170.6, 6.3), rnorm(1000, 184, 7)))
mean_m=round(mean(wdata$Lengte[wdata$Geslacht =="Man"]),0)
sd_m = sd(wdata$Lengte[wdata$Geslacht =="Man"])
min_m = round(min(wdata$Lengte[wdata$Geslacht =="Man"]),0)
max_m = round(max(wdata$Lengte[wdata$Geslacht =="Man"]),0)
mean_v=round(mean(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
sd_v = sd(wdata$Lengte[wdata$Geslacht =="Vrouw"])
min_v = round(min(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
max_v = round(max(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
x_m <- seq(min_m,max_m,length.out = N)
y_m <- dnorm(x_m, mean_m, sd_m)
df_m <- data.frame(x=x_m, y=y_m)
upper_m = round(qnorm(0.975, mean_m, sd_m))
lower_m = round(qnorm(0.025, mean_m, sd_m))
x_v <- seq(min_v,max_v,length.out = N)
y_v <- dnorm(x_v, mean_v, sd_v)
df_v <- data.frame(x=x_v, y=y_v)
x_m <- seq(min_m, max_m, .001)
y_m <- dnorm(x_m, mean_m, sd_m)
df_m <- data.frame("x" = x_m, "y" = y_m)
x_v <- seq(min_v, max_v, .001)
y_v <- dnorm(x_v, mean_v, sd_v)
df_v <- data.frame("x" = x_v, "y" = y_v)
upper_v = round(qnorm(0.975, mean_v, sd_v))
lower_v = round(qnorm(0.025, mean_v, sd_v))
ggplot(df_m, aes(x,y, color="Mannen")) +
  stat_function(fun = ~dnorm(.x, mean(wdata$Lengte[wdata$Geslacht =="Man"]), sd(wdata$Lengte[wdata$Geslacht =="Man"])),
                xlim = c(min(wdata$Lengte[wdata$Geslacht =="Vrouw"]), upper_m),
                geom = "area",
                fill='salmon', show.legend = FALSE)+
  geom_line(size=1) + 
  geom_line(data=df_v, aes(color="Vrouwen"),size=1) +
  scale_color_manual("Geslacht", 
                     values= c("Mannen" = "salmon","Vrouwen" = "#00BFC4"))+
  geom_vline(xintercept = mean_v, col="blue", linetype="dotted") + 
  geom_vline(xintercept = mean_m, col="red", linetype="dotted")  + 
  
  stat_function(fun = ~dnorm(.x, mean(wdata$Lengte[wdata$Geslacht =="Vrouw"]), sd(wdata$Lengte[wdata$Geslacht =="Man"])),
                xlim = c(min(wdata$Lengte[wdata$Geslacht =="Vrouw"]), mean1),
                geom = "area",
                fill='#00BFC4')+
  geom_vline(xintercept = alfa, linetype="dotted") + 
  geom_vline(xintercept = mean1, col="blue", linetype="dotted") + 
  geom_vline(xintercept = mean2, col="red", linetype="dotted") +
  scale_color_manual("Geslacht", 
                     values= c("Mannen" = "blue","Vrouwen" = "red"))


geom_polygon(aes(color=NULL), data=poly, fill="red", alpha=I(4/10),
             show_guide=F) +
  geom_vline(xintercept = alfa, linetype="dotted") + 
  geom_vline(xintercept = mean1, col="blue", linetype="dotted") + 
  geom_vline(xintercept = mean2, col="red", linetype="dotted") + 
  theme_bw()+
  labs(title=paste("Visualizing Effect Sizes 
      (Cohen's d = ",ES,"; U3 = ",u3,")", sep="")) +
  scale_color_manual("Geslacht", 
                     values= c("Mannen" = "blue","Vrouwen" = "red")) +
  ylab(NULL) + xlab(NULL)















m1 <- 184     
sd1 <- 7   
m2 <-  170.6 
sd2 <- 6.3    
z_crit <- qnorm(1-(0.05/2), m1, sd1)
min1 <- m1-sd1*4
max1 <- m1+sd1*4
min2 <- m2-sd2*4
max2 <- m2+sd2*4          
x <- seq(min(min1,min2), max(max1, max2), .01)
y1 <- dnorm(x, m1, sd1)
df1 <- data.frame("x" = x, "y" = y1)
y2 <- dnorm(x, m2, sd2)
df2 <- data.frame("x" = x, "y" = y2)
y.poly <- pmin(y1,y2)
poly1 <- data.frame(x=x, y=y.poly)
poly1 <- poly1[poly1$x >= z_crit, ] 
poly1<-rbind(poly1, c(z_crit, 0))  # add lower-left corner
poly2 <- df2
poly2 <- poly2[poly2$x <= z_crit,] 
poly2<-rbind(poly2, c(z_crit, 0))  # add lower-left corner
poly3 <- df2
poly3 <- poly3[poly3$x >= z_crit,] 
poly3 <-rbind(poly3, c(z_crit, 0))  # add lower-left corner
poly1$id <- 3 # alpha, give it the highest number to make it the top layer
poly2$id <- 2 # beta
poly3$id <- 1 # power; 1 - beta
poly <- rbind(poly1, poly2, poly3)
poly$id <- factor(poly$id,  labels=c("power","beta","alpha"))
ggplot(poly, aes(x,y, fill=id, group=id)) +
  geom_polygon(show_guide=F, alpha=I(8/10)) +
  geom_line(data=df1, aes(x,y, color="H0", group=NULL, fill=NULL), size=1.5, show_guide=F) + 
  geom_line(data=df2, aes(color="HA", group=NULL, fill=NULL),size=1.5, show_guide=F) +
  geom_vline(xintercept = z_crit, size=1, linetype="dashed") +
  scale_color_manual("Group", 
                     values= c("HA" = "#981e0b","H0" = "black")) +
  scale_fill_manual("test", values= c("alpha" = "#0d6374","beta" = "#be805e","power"="#7cecee")) +
  annotate("segment", x=0.1, y=0.045, xend=1.3, yend=0.01, arrow = arrow(length = unit(0.3, "cm")), size=1) +
  annotate("text", label="beta", x=0, y=0.05, parse=T, size=8) +
  annotate("segment", x=4, y=0.043, xend=3.4, yend=0.01, arrow = arrow(length = unit(0.3, "cm")), size=1) +
  annotate("text", label="frac(alpha,2)", x=4.2, y=0.05, parse=T, size=8) +
  annotate("segment", x=6, y=0.2, xend=4.5, yend=0.15, arrow = arrow(length = unit(0.3, "cm")), size=1) +
  annotate("text", label="1-beta", x=6.1, y=0.21, parse=T, size=8) +
  annotate("text", label="H[0]", x=m1, y=0.28, parse=T, size=8) +
  annotate("text", label="H[a]", x=m2, y=0.28, parse=T, size=8) +
  ggtitle("Statistical Power Plots, Textbook-style") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill="#f9f0ea"),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=22))





















set.seed(42);
m<-rnorm(1000, 184, 7)
df1<-data.frame(m)
v<-rnorm(1000, 170.6, 6.3)
df2<-data.frame(v)
df<-cbind(df1,df2)%>%
  mutate(diff = m-v)
mean=round(mean(df$diff),0)
sd = sd(df$diff)
min = round(min(df$diff),0)
max = round(max(df$diff),0)
upper = round(qnorm(0.975, mean, sd))
lower = round(qnorm(0.025, mean, sd))
ggplot(df)+
  geom_density(aes(x=diff))+
  geom_vline(xintercept = mean, color = 'black', lty=2) +
  geom_vline(xintercept = t_test_result$conf.int[1], color = 'red', lty=2) +
  geom_vline(xintercept = t_test_result$conf.int[2], color = 'red', lty=2) +
  geom_vline(xintercept = 0, color = 'blue', lty=2)





set.seed(42);
m<-rnorm(1000, 184, 7)
df1<-data.frame(m)
v<-rnorm(1000, 170.6, 6.3)
df2<-data.frame(v)
df<-cbind(df1,df2)%>%
  mutate(diff = m-v)
mean=round(mean(df$diff),0)
sd = sd(df$diff)
min = round(min(df$diff), 0)
max = round(max(df$diff),0)
N=100
sample=0
upper = round(qnorm(0.975, 200, 7))
lower = round(qnorm(0.025, 200, 7))
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
       subtitle="Frequentie van mannen groter of gelijk aan 184 cm")
hist(wdata$Lengte[wdata$Geslacht =="Man"])
t.test(wdata$Lengte[wdata$Geslacht =="Man"], 
       mu = 184)
qt(p = .025, df = 999)
qt(p = .975, df = 999)









mean_m=round(mean(wdata$Lengte[wdata$Geslacht =="Man"]),0)
sd_m = sd(wdata$Lengte[wdata$Geslacht =="Man"])
min_m = round(min(wdata$Lengte[wdata$Geslacht =="Man"]),0)
max_m = round(max(wdata$Lengte[wdata$Geslacht =="Man"]),0)
mean_v=round(mean(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
sd_v = sd(wdata$Lengte[wdata$Geslacht =="Vrouw"])
min_v = round(min(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
max_v = round(max(wdata$Lengte[wdata$Geslacht =="Vrouw"]),0)
N=100
x_m <- seq(min_m,max_m,length.out = N)
y_m <- dnorm(x_m, mean_m, sd_m)
df_m <- data.frame(x=x_m, y=y_m)
area_m <- round(1-(pnorm(sample,mean_m,sm)),3)
x_v <- seq(min_v,max_v,length.out = N)
y_v <- dnorm(x_v, mean_v, sd_v)
df_v <- data.frame(x=x_v, y=y_v)
area_v <- round(1-(pnorm(sample,mean_v,sd_v)),3)
upper_m = round(qnorm(0.975, mean_m, sd_m))
lower_m = round(qnorm(0.025, mean_m, sd_m))
upper_v = round(qnorm(0.975, mean_v, sd_v))
lower_v = round(qnorm(0.025, mean_v, sd_v))
ggplot()+
  geom_line(data=df_m, aes(x=x_m, y=y_m), col="") + 
  geom_line(data=df_v, aes(x=x_v, y=y_v)) 
stat_function(fun = ~dnorm(.x, mean_m, sd_m),
              xlim = c(upper_m, max_m),
              geom = "area",
              fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean_m, sd_m),
                xlim = c(lower_m, min_m),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean_v, sd_v),
                xlim = c(upper_v, max_v),
                geom = "area",
                fill='salmon') +
  stat_function(fun = ~dnorm(.x, mean_v, sd_v),
                xlim = c(lower_v, min_v),
                geom = "area",
                fill='salmon') +
  geom_line(data=df_m, aes(x=x_m, y=y_m)) + 
  geom_line(data=df_v, aes(x=x_v, y=y_v)) +
  geom_vline(xintercept = mean_m, color = 'black', lty=2) +
  geom_vline(xintercept = mean_v, color = 'black', lty=2) +
  geom_vline(xintercept = lower_m, color = 'red', lty=2) +
  geom_vline(xintercept = upper_m, color = 'red', lty=2) +
  geom_vline(xintercept = lower_v, color = 'red', lty=2) +
  geom_vline(xintercept = upper_v, color = 'red', lty=2) 

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
       subtitle="Frequentie van mannen groter of gelijk aan 184 cm")







