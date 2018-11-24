#We'll load in our 'Tips' Dataset and rename.
tips <- read.csv("C:/Git/Tools for Data Analysis/tips (1).csv")

#These are the main 2 packages we'll use for our analysis.
library("ggplot2")

library("tidyverse")

#Just to get a feel for the data, we'll look at different variables we'll
#be using as well as the first 10 observations.

names(tips)

class(tips$day)

class(tips$totbill)

head(tips$size, n = 10)

#Since they've added this 'smoker' variable, I want to see how the smokers
#stack up against non-smokers when it comes to tip amount. We'll also use
#sex to further distiguish within our plot.

tipdensity <- tips %>% 
  ggplot(aes(x = tip, fill = sex)) + 
  geom_density(alpha = 0.5)+
  facet_wrap(~smoker)

#Below gives us an idea of what the average tip amount looks like between
#male and female smokers vs. male and female non-smokers. The percentage
#breaks down the proportion between the 4 groups. Male non-smokers on average
#tipped the most.

tips %>% 
  select(smoker,sex, tip) %>% 
  group_by(smoker, sex) %>% 
  summarize(avg.tip = mean(tip),
                n = n()) %>% 
  ungroup() %>% 
  mutate(percent = (n/sum(n)*100))



#The output below says that the larger your party size, the higher your average tip amount
#would be. There is a significant increase in the average tip amount from party
#size of 1 person vs. 2 people. I'm assuming that 2 people are possibly on a dates,
#and therefore causing tipper to give a sinificant amount for the person they're
#on a data with. A party size of 4 people might also mean they're on double dates,
#which caused the tippers to give a signigicantly higher amount than a party
#of 3 or 5 people. The 5th person might be the odd ball in the group and tipped
#a smaller amount than the rest of the group.

tips %>% 
  select(size, tip) %>% 
  group_by(size) %>% 
  summarise(n = n(),
            averagetip = mean(tip)) %>% 
  arrange(size)

#The time variable shows that only 28% of buisness was conducted during the day time. This
#variable is a bit misleading because we aren't sure exacly how many hours would fall under the
#day and night time factors. There could be equal amount of hours that fall under both
#categories. It's likely that more patrons are visiting the restaurant at night. But we can't
#difinitively make that assumption.

tips %>% 
   select(time) %>% 
   group_by(time) %>% 
   summarize(n = n()) %>% 
   mutate(percent = (n/sum(n)*100))


#This graph is skewed to the right. We can try and fit this graph with 
#an ex-gaussian density plot.

tips %>% 
  ggplot(aes(x = tip)) + 
  geom_density()

dexg <- function(x, mu, sigma, tau){
  return((1/tau)*exp((sigma^2/(2*tau^2))-(x-mu)/tau)*pnorm((x-mu)/sigma-(sigma/tau)))
}

nll.exg <- function(data, par){
  return(-sum(log(dexg(data,
                       mu = par[1],
                       sigma = par[2],
                       tau = par[3]))))
}

optim(par = c(0,0.1, 0.1), fn = nll.exg, data = tips$tip)

x = seq(-1,10,0.01)
plot(density(tips$tip), 
     ylim = c(0, 0.50),
     main = "Model of tip distribution vs. Actual",
     xlab = "tip")
lines(x, dexg(x,  mu = 1.589, sigma = 0.427, tau = 1.409), lty=2)







