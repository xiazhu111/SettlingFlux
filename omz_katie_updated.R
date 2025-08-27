#Vertical distribution of microplastics in ENTP OMZ
#helpful resource: https://www.theanalysisfactor.com/r-tutorial-5/

library(ggplot2)
library(readxl)

install.packages("viridis")
library(viridis)

install.packages("dplyr")
library(dplyr)

install.packages("scales")
library(scales)

install.packages("gridExtra")
library(gridExtra)

install.packages("cowplot")
library(cowplot)

install.packages("ggpubr")
library(ggpubr)

#flip the chart 
data <- read_excel("C:/Users/katie/OneDrive/Documents/OMZ Data 2025/DepthProfiles.xlsx")
data$station <- as.factor(data$station) #convert continuous into categorical variable
#plot
ggplot(data=data, aes(x=conc,y=depth)) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6"),
                     values=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")) +
  scale_y_reverse(limits=c(1500,0)) +
  labs(x = "Concentration (#/L)",y = "Depth (m)") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        plot.title=element_text(size=25),
        strip.text.x=element_text(size=20)) +
  ggtitle("Microplastic Concentration by Station")

#logged
ggplot(data=data, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6"),
                     values=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")) +
  scale_y_reverse(limits=c(10,0)) +
  scale_x_discrete(limits=c(1.75,3)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        plot.title=element_text(size=25),
        legend.title = element_text(size=20),
        strip.text.x=element_text(size=20)) +
    ggtitle("Microplastic Logged Concentration by Station")


# remove individual axes labels
uncorrected1 <- uncorrected1 + labs(x = "", y = "")
uncorrected2 <- uncorrected2 + labs(x = "", y = "")
uncorrected3 <- uncorrected3 + labs(x = "", y = "")
uncorrected4 <- uncorrected4 + labs(x = "", y = "")
uncorrected5 <- uncorrected5 + labs(x = "", y = "")
uncorrected6 <- uncorrected6 + labs(x = "", y = "")

# arrange in one plot
figure <- ggarrange(uncorrected1, uncorrected2, uncorrected3, uncorrected4, uncorrected5, uncorrected6,
                    common.legend = TRUE, legend = "right", #no legend, plot title
                    align = "hv", # align both horizontally and vertically
                    ncol = 3, nrow = 2)


# common x and y axis labels
annotate_figure(figure,
                left = text_grob("log(Depth[m])", rot = 90, vjust = 1),
                bottom = text_grob("log(Concentration[#/L])"))

#getting R^2 value for equation, log(conc) = b*log(depth) + a

###############################################################################
# corrected data

corrected_data <- read_excel("C:/Users/katie/OneDrive/Documents/OMZ Data 2025/CorrectedDepthProfiles.xlsx")
corrected_data$station <- as.factor(corrected_data$station) #convert continuous into categorical variable
#plot
ggplot(data=corrected_data, aes(x=conc,y=depth)) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6"),
                     values=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")) +
  scale_y_reverse(limits=c(1500,0)) +
  labs(x = "Concentration (#/L)",y = "Depth (m)") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        plot.title=element_text(size=25),
        strip.text.x=element_text(size=20)) +
  ggtitle("Microplastic Concentration by Station")

#logged
ggplot(data=corrected_data, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6"),
                     values=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")) +
  scale_y_reverse(limits=c(10,0)) +
  scale_x_discrete(limits=c(1.5,3)) +
  geom_smooth(method=lm,se=F, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        plot.title=element_text(size=25),
        legend.title = element_text(size=20),
        strip.text.x=element_text(size=20)) +
  ggtitle("Microplastic Logged Concentration by Station")

#microplastic vs depth plot, for fun
ggplot(data=corrected_data, aes(x=log(depth),y=log(conc))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_manual(breaks = c("1", "2", "3", "4", "5", "6"),
                     values=c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")) +
  scale_y_discrete(limits=c(1.75,3)) +
  scale_x_discrete(limits=c(2,5,8)) +
  #xlim(0,10) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Depth[m])",y = "log(Concentration[#/L])") +
  theme_bw() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        plot.title=element_text(size=25),
        legend.title = element_text(size=20),
        strip.text.x=element_text(size=20)) +
  ggtitle("Microplastic Logged Concentration by Station")


#getting R^2 value for equation, log(conc) = b*log(depth) + a
#station 1 corrected r2
m <- lm(log(conc[1:8])~log(depth[1:8]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station2 corrected r2
m <- lm(log(conc[9:16])~log(depth[9:16]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station3 corrected r2
m <- lm(log(conc[17:24])~log(depth[17:24]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station4 corrected r2
m <- lm(log(conc[25:31])~log(depth[25:31]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station5 corrected r2
m <- lm(log(conc[32:38])~log(depth[32:38]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station6 corrected r2
m <- lm(log(conc[39:45])~log(depth[39:45]),corrected_data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

###### Calculate p value for each station corrected data

#Station 1 corrected p value
anova_m1 <- aov(log(conc[1:8]) ~ log(depth[1:8]), data = corrected_data)
summary(anova_m1)
p_value <- summary(anova_m1)[[1]][["Pr(>F)"]][1]
print(p_value)

#Station 2 corrected p value
anova_m2 <- aov(log(conc[9:16]) ~ log(depth[9:16]), data = corrected_data)
summary(anova_m2)
p_value2 <- summary(anova_m2)[[1]][["Pr(>F)"]][1]
print(p_value2)

#Station 3 corrected p value
anova_m3 <- aov(log(conc[17:24]) ~ log(depth[17:24]), data = corrected_data)
summary(anova_m3)
p_value3 <- summary(anova_m3)[[1]][["Pr(>F)"]][1]
print(p_value3)

#Station 4 corrected p value
anova_m4 <- aov(log(conc[25:31]) ~ log(depth[25:31]), data = corrected_data)
summary(anova_m4)
p_value4 <- summary(anova_m4)[[1]][["Pr(>F)"]][1]
print(p_value4)

#Station 5 corrected p value 
anova_m5 <- aov(log(conc[32:38]) ~ log(depth[32:38]), data = corrected_data)
summary(anova_m5)
p_value5 <- summary(anova_m5)[[1]][["Pr(>F)"]][1]
print(p_value5)

#Station 6 corrected p value
anova_m6 <- aov(log(conc[39:45]) ~ log(depth[39:45]), data = corrected_data)
summary(anova_m6)
p_value6 <- summary(anova_m6)[[1]][["Pr(>F)"]][1]
print(p_value6)

#############################################################################

# pie charts corrected

install.packages("ggrepel")
library(ggrepel)

#station 1 corrected

pie1 <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "green", "light blue", "pink", "purple", "red", "yellow"),
  value = c(2, 4, 17, 2, 4, 1, 13, 1, 2, 2, 2)
)
sum1 <- 2 + 4 + 17 + 2 + 4 + 1 + 13 + 1 + 2 + 2 + 2

ggplot(pie1, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  ggtitle("Station 1") +
  #theme_void() + # Remove default ggplot2 theme elements
  #theme_minimal() +
  geom_text(aes(x=1.55, label=paste0(round(value/sum1*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "brown"="sienna", "clear"="azure2", "dark blue"="blue4", "gray"="azure4", "green"="seagreen", "light blue"="lightskyblue1", "pink"="pink", "purple"="darkorchid1", "red"="firebrick1", "yellow"="gold1"))

#station 2 corrected

pie2 <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "light blue", "orange", "pink", "white", "yellow"),
  value = c(8, 1, 11, 4, 9, 2, 3, 1, 1, 1)
)

sum2 <- 8 + 1 + 11 + 4 + 9 + 2 + 3 + 1 + 1 + 1

ggplot(pie2, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 2") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum2*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "brown"="sienna", "clear"="azure2", "dark blue"="blue4", "gray"="azure4", "light blue"="lightskyblue1", "pink"="pink", "purple"="darko", "yellow"="gold1", "orange"="orange", "white"="white"))

#station 3 corrected

pie3 <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "light blue", "pink", "red", "yellow"),
  value = c(2, 8, 22, 1, 6, 2, 2, 1, 2)
)

sum3 <- 2 + 8 + 22 + 1 + 6 + 2 + 2 + 1 + 2

ggplot(pie3, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 3") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum3*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "brown"="sienna", "clear"="azure2", "dark blue"="blue4", "gray"="azure4", "light blue"="lightskyblue1", "pink"="pink", "yellow"="gold1", "red"="firebrick1"))

#station 4 corrected

pie4 <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "light blue", "gray", "pink", "red", "silver", "yellow"),
  value = c(2, 4, 22, 3, 5, 6, 1, 1, 1, 4)
)

sum4 <- 2 + 4 + 22 + 3 + 5 + 6 + 1 + 1 + 1 + 4

ggplot(pie4, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 4") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum4*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "brown"="sienna", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "pink"="pink", "yellow"="gold1", "red"="firebrick1", "silver"="gray60"))

#station 5

pie5 <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "light blue", "gray", "pink", "red", "yellow"),
  value = c(3, 3, 44, 6, 2, 3, 1, 2, 2)
)

sum5 <- 3 + 3 + 44 + 6 + 2 + 3 + 1 + 2 + 2

ggplot(pie5, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 5") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum5*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "brown"="sienna", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "pink"="pink", "yellow"="gold1", "red"="firebrick1"))

#station 6

pie6 <- data.frame(
  color = c("black", "clear", "dark blue", "light blue", "gray", "red", "white", "yellow"),
  value = c(8, 13, 3, 5, 4, 3, 2, 1)
)

sum6 <- 8 + 13 + 3 + 5 + 4 + 3 + 2 + 1

ggplot(pie6, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 6") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum6*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white"))

# for legend

pie7 <- data.frame(
  color = c("black", "white", "gray", "clear", "silver", "brown", "dark blue", "light blue", "pink", "red", "yellow", "orange", "green", "purple"),
  value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)
ggplot(pie7, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 7") +
  theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"),
                    labels = c("Black", "Brown", "Clear", "Dark blue", "Gray", "Green", "Light blue", "Orange", "Pink", "Purple", "Red", "Silver", "White", "Yellow")
  )

######################################################

#Color by depth pie charts

# Surface
piecolor_surface <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "green", "light blue", "orange", "pink", 'red', "white"),
  value = c(5, 5, 18, 1, 9, 1, 6, 1, 1, 1, 1)
)

sumsurface <- 5 + 5 + 18 + 1 + 9 + 1 + 6 + 1 + 1 + 0 + 1 + 0 + 1 + 0

ggplot(piecolor_surface, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Surface") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumsurface*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Chlorophyll max
piecolor_chlorophyll <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "light blue", "pink", 'red', "yellow"),
  value = c(1, 9, 19, 3, 6, 2, 2, 3, 1)
)

sumchlorophyllmax <- 1 + 9 + 19 + 3 + 6 + 0 + 2 + 0 + 2 + 0 + 3 + 0 + 0 + 1

ggplot(piecolor_chlorophyll, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Chlorophyll Max") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumchlorophyllmax*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Upper oxycline
piecolor_upperoxycline <- data.frame(
  color = c("black", "clear", "dark blue", "gray", "light blue", "pink", 'red', "yellow"),
  value = c(4, 18, 2, 9, 2, 1, 1, 1)
)

sumupperoxycline <- 4 + 0 + 18 + 2 + 9 + 0 + 2 + 0 + 1 + 0 + 1 + 0 + 0 + 1

ggplot(piecolor_upperoxycline, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Upper Oxycline") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumupperoxycline*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Upper Oxygen Core
piecolor_uppercore <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "light blue", 'red', "yellow"),
  value = c(6, 1, 19, 1, 3, 3, 1, 1)
)

sumuppercore <- 6 + 1 + 19 + 1 + 3 + 0 + 3 + 0 + 0 + 0 + 1 + 0 + 0 + 1

ggplot(piecolor_uppercore, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Upper Oxygen Core") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumuppercore*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# 2nd Chlorophyll Max
piecolor_2ndchlorophyll <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "orange", "purple", "yellow"),
  value = c(2, 2, 10, 1, 1, 1, 2)
)

sum2ndchlorophyll <- 2 + 2 + 10 + 1 + 0 + 0 + 0 + 1 + 0 + 1 + 0 + 0 + 0 + 2

ggplot(piecolor_2ndchlorophyll, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("2nd Chlorphyll Max") +
  geom_text(aes(x=1.55, label=paste0(round(value/sum2ndchlorophyll*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Oxygen minimum
piecolor_oxygenminimum <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "gray", "light blue", 'red', "silver", "white", "yellow"),
  value = c(3, 2, 16, 3, 4, 5, 1, 1, 1, 4)
)

sumoxygenminimum <- 3 + 2 + 16 + 3 + 4 + 0 + 5 + 0 + 0 + 0 + 1 + 1 + 1 + 4

ggplot(piecolor_oxygenminimum, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Oxygen Minimum") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumoxygenminimum*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Lower Oxygen Core
piecolor_lowercore <- data.frame(
  color = c("clear", "dark blue", "gray", "light blue", "orange", "pink", "purple", 'red', "white", "yellow"),
  value = c(16, 5, 1, 3, 1, 1, 1, 1, 1, 2)
)

sumlowercore <- 0 + 0 + 16 + 5 + 1 + 0 + 3 + 1 + 1 + 1 + 1 + 0 + 1 + 2

ggplot(piecolor_lowercore, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Lower Oxygen Core") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumlowercore*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

# Deep
piecolor_deep <- data.frame(
  color = c("black", "brown", "clear", "dark blue", "light blue", "pink", 'red', "yellow"),
  value = c(4, 1, 13, 3, 9, 1, 1, 1)
)

sumdeep <- 4 + 1 + 13 + 3 + 0 + 0 + 9 + 0 + 1 + 0 + 1 + 0 + 0 + 1

ggplot(piecolor_deep, aes(x = "", y = value, fill = color)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Deep") +
  geom_text(aes(x=1.55, label=paste0(round(value/sumdeep*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Color") +
  scale_fill_manual(values = c("black"="black", "clear"="azure2", "dark blue"="blue4", "gray"="gray30", "light blue"="lightskyblue1", "yellow"="gold1", "red"="firebrick1", "white"="white", "pink" = "pink", "orange"="orange", "purple" = "purple", "silver"="gray60","brown"="sienna", "green"="seagreen"))

#############################################

#Morphology by Station Pie Charts

# Station 1 
piemorph_s1 <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(18, 17, 15)
)

summorph_s1 <- 18 + 17 + 15

ggplot(piemorph_s1, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 1") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s1*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Station 2
piemorph_s2 <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(13, 2, 4, 15)
)

summorph_s2 <- 13 + 2 + 4 + 15

ggplot(piemorph_s2, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 2") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s2*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))


# Station 3
piemorph_s3 <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(5, 1, 13, 28)
)

summorph_s3 <- 5 + 1 + 13 + 28

ggplot(piemorph_s3, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 3") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s3*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Station 4
piemorph_s4 <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(14, 1, 12, 22)
)

summorph_s4 <- 14 + 1 + 12 + 22

ggplot(piemorph_s4, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 4") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s4*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))


# Station 5
piemorph_s5 <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(13, 19, 34)
)

summorph_s5 <- 13 + 19 + 34

ggplot(piemorph_s5, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 5") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s5*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Station 6
piemorph_s6 <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(12, 10, 17)
)

summorph_s6 <- 12 + 10 + 17

ggplot(piemorph_s6, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Station 6") +
  geom_text(aes(x=1.6, label=paste0(round(value/summorph_s6*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))


# Morphology by depth pie charts

# Surface 
piemorph_surface <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(8, 10, 31)
)

summorph_surface <- 8 + 10 + 31

ggplot(piemorph_surface, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Surface") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_surface*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

#chlorophyll max
piemorph_chlorophyllmax <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(12, 20, 14)
)

summorph_chlorophyll <- 12 + 20 + 14

ggplot(piemorph_chlorophyllmax, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Chlorophyll Max") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_chlorophyll*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Upper Oxycline
piemorph_upperoxycline <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(13, 0, 9, 16)
)

summorph_upperoxycline <- 13 + 9 + 16

ggplot(piemorph_upperoxycline, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Upper Oxycline") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_upperoxycline*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Upper Oxygen Core
piemorph_uppercore <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(8, 0, 7, 20)
)

summorph_uppercore <- 8 + 7 + 20

ggplot(piemorph_uppercore, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Upper Oxygen Core") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_uppercore*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# 2nd Chlorophyll max
piemorph_2ndchlorophyll <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(2, 2, 8, 7)
)

summorph_2ndchlorophyll <- 2 + 2 + 8 + 7

ggplot(piemorph_2ndchlorophyll, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("2nd Chlorophyll Max") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_2ndchlorophyll*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"),
                    labels = c("Fiber", "Fiber bundle", "Film", "Fragment"))

# Oxygen minimum
piemorph_oxygenminimum <- data.frame(
  morphology = c("fiber", "fiber bundle", "film", "fragment"),
  value = c(10, 2, 8, 20)
)

summorph_oxygenminimum <- 10 + 2 + 8 +20

ggplot(piemorph_oxygenminimum, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Oxygen Minimum") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_oxygenminimum*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Lower Oxygen Core
piemorph_lowercore <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(10, 7, 15)
)

summorph_lowercore <- 10 + 7 + 15

ggplot(piemorph_lowercore, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Lower Oxygen Core") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_lowercore*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))

# Deep
piemorph_deep <- data.frame(
  morphology = c("fiber", "film", "fragment"),
  value = c(12, 6, 15)
)

summorph_deep <- 12 + 6 + 15
ggplot(piemorph_deep, aes(x = "", y = value, fill = morphology)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Deep") +
  geom_text(aes(x=1.55, label=paste0(round(value/summorph_deep*100), "%")),
            position = position_stack(vjust=0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  #theme_void() + # Remove default ggplot2 theme elements
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(legend.title = element_text(size=10)) +
  labs(fill = "Morphology") +
  scale_fill_manual(values = c("fiber"="#440154FF", "fiber bundle"="#FDE725FF", "film"="#7AD151FF", "fragment"="#414487FF"))




