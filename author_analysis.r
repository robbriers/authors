# analysis of changes in number of co-authors over time, based on my publication history

# load the libraries needed, we will use viridis to colour the plot symbols
library(ggplot2)
library(viridis)

# read in the data direct from the Github repo
url<-'https://raw.githubusercontent.com/robbriers/authors/master/coauthors.csv'
auths<-read.csv(url, header=TRUE)

# plot the data to look at the trend
p1 <- ggplot(auths, aes(x = year, y = authors, colour = authors)) +
  geom_point(size=1.5) +
  scale_color_viridis(end = 0.80) +
  labs(title = "Change in number of co-authors over time",
       x = "year of publication",
       y = "number of co-authors") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  theme(legend.position="none")

p1


# glm analysis of pattern of change; count data so use poisson family
auth.m1<-glm(authors~year, data=auths, family="poisson")
summary(auth.m1)

# so how many co-authors will I have in 2050?
exp(predict(auth.m1, data.frame(year=c(2050))))