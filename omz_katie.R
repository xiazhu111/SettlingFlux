#Vertical distribution of microplastics in ENTP OMZ
#helpful resource: https://www.theanalysisfactor.com/r-tutorial-5/

library(ggplot2)
library(readxl)

install.packages("viridis")
library(viridis)

install.packages("scales")
library(scales)

#flip the chart 
data <- read_excel("C:/Users/katie/OneDrive/Documents/OMZ Data 2025/DepthProfiles.xlsx")
data$station <- as.factor(data$station) #convert continuous into categorical variable
#plot
ggplot(data=data, aes(x=conc,y=depth)) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_y_reverse(limits=c(1500,0)) +
  labs(x = "Concentration (#/L)",y = "Depth (m)") +
  theme_bw() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#logged
ggplot(data=data, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 1 uncorrected plots
data1_uncorrected <- data[1:8,]
ggplot(data=data1_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 2 uncorrected plots
data2_uncorrected <- data[9:16,]
ggplot(data=data2_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 3 uncorrected plots
data3_uncorrected <- data[17:24,]
ggplot(data=data3_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 4 uncorrected plots
data4_uncorrected <- data[25:31,]
ggplot(data=data4_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 5 uncorrected plots
data5_uncorrected <- data[32:38,]
ggplot(data=data5_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 6 uncorrected plots
data6_uncorrected <- data[39:45,]
ggplot(data=data6_uncorrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#getting R^2 value for equation, log(conc) = b*log(depth) + a
#station 1 uncorrected r2
m <- lm(log(conc[1:8])~log(depth[1:8]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station2 uncorrected r2
m <- lm(log(conc[9:16])~log(depth[9:16]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station3 uncorrected r2
m <- lm(log(conc[17:24])~log(depth[17:24]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station4 uncorrected r2
m <- lm(log(conc[25:31])~log(depth[25:31]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station5 uncorrected r2
m <- lm(log(conc[32:38])~log(depth[32:38]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

#station6 uncorrected r2
m <- lm(log(conc[39:45])~log(depth[39:45]),data)
a = format(unname(coef(m)[1]),digits=2)
b = format(unname(coef(m)[2]),digits=2)
r2 = format(summary(m)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth), r2 = ", r2)

###############################################################################
# corrected data

corrected_data <- read_excel("C:/Users/katie/OneDrive/Documents/OMZ Data 2025/CorrectedDepthProfiles.xlsx")
corrected_data$station <- as.factor(corrected_data$station) #convert continuous into categorical variable
#plot
ggplot(data=corrected_data, aes(x=conc,y=depth)) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_y_reverse(limits=c(1500,0)) +
  labs(x = "Concentration (#/L)",y = "Depth (m)") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#logged
ggplot(data=corrected_data, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 1 corrected plots
data1_corrected <- corrected_data[1:8,]
ggplot(data=data1_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 2 corrected plots
data2_corrected <- corrected_data[9:16,]
ggplot(data=data2_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 3 corrected plots
data3_corrected <- corrected_data[17:24,]
ggplot(data=data3_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 4 corrected plots
data4_corrected <- corrected_data[25:31,]
ggplot(data=data4_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 5 corrected plots
data5_corrected <- corrected_data[32:38,]
ggplot(data=data5_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#station 6 corrected plots
data6_corrected <- corrected_data[39:45,]
ggplot(data=data6_corrected, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station), size=3) + 
  facet_grid(~station) + 
  scale_color_viridis(discrete = TRUE) +
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

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