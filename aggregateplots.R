library(ggplot2)
library(readxl)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(car)
library(PMCMRplus)
library(ggsignif)

#load dataset
data <- read_excel("C:/Users/Alice/Desktop/Alice/Postdoc 2024-2026/Research/2 Settling and Aggregation Experiments/Methods/8 Making Plots/AggregateNumber.xlsx")
head(data)
data$SizeClass <- factor(data$SizeClass, levels = c("1-2","2-4","4-8","8-16","> 16"))

#aggregate number
data %>%
  ggplot(aes(x=SizeClass,y=Number,group=Treatment,color=Treatment)) +
  geom_line() +
  geom_point(size=3) +
  theme_ipsum(base_size=22) +
  ggtitle("Aggregate Number") + xlab("Size Class (mm)") + ylab("Number") +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size=25)) +
  scale_color_viridis(discrete = TRUE) +
  geom_errorbar(aes(x=SizeClass,ymin=Number-SD,ymax=Number+SD),width=0.1,color="black",size=0.01)

#aggregate size
data2 <- read_excel("C:/Users/Alice/Desktop/Alice/Postdoc 2024-2026/Research/2 Settling and Aggregation Experiments/Methods/8 Making Plots/AggregateSize.xlsx")
head(data2)

data2 %>%
  ggplot(aes(x=Treatment,y=dv,fill=Treatment)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) + 
  geom_jitter(color="black",size=0.4,alpha=0.9) + 
  theme_ipsum(base_size=22) +
  ggtitle("Aggregate Size") + xlab("Treatment") + ylab("log(Equivalent Spherical Diameter[mm])") +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size=25)) +
  geom_signif(comparisons=list(c("LDPE (-)","Control"),c("LDPE (+)","Control"),c("PVC","Control")),
              map_signif_level = TRUE, textsize = 6) + 
  scale_y_continuous(trans="log10")

#ANOVA
result <- leveneTest(dv ~ Treatment,data2) #test for homogeneity of variance, p < 0.05
print(result)
p <- data2 %>% 
  ggplot(aes(x=dv,fill=Treatment)) +
  geom_histogram(position = "identity",alpha=0.5) +
  scale_fill_viridis(discrete = TRUE) + 
  theme_ipsum()
p #not really normal either

#convert character into factor to avoid "all group levels must be finite" error
data2$Treatment <- as.factor(data2$Treatment)

kruskal.test(dv ~ Treatment, data = data2) #at least one pair is significant as p < 0.05
ans <- kwManyOneDunnTest(dv ~ Treatment, data = data2) #nonparametric equivalent of Tukey HSD
summary(ans) #LDPE neg & control, LDPE pos & control, PVC & control
