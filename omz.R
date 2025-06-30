#Vertical distribution of microplastics in ENTP OMZ
#helpful resource: https://www.theanalysisfactor.com/r-tutorial-5/

library(ggplot2)
library(readxl)
#practice fitting an exponential decay function or power law function to data
#in the form of y = ab^x
depth <- c(10,54,200,400,500,700,1200) #y-axis #Station 1 hypothetical
conc <- c(18,13,7,10,10,10,5) #x-axis #Station 1 hypothetical
depth2 <- c(10,32,100,300,500,800,1200) #Station 2 hypothetical
conc2 <- c(15,13,10,10,8,7,8) #Station 2 hypothetical

#to get the actual regression equations
exponential.model <- lm(log(conc)~depth)
summary(exponential.model)
exponential.model2 <- lm(log(conc2)~depth2)
summary(exponential.model2)

#log(conc) = -0.0007637*depth + 2.6083427, Station 1 hypothetical
#log(conc2) = -0.0004984*depth2 + 2.4927611, Station 2 hypothetical

#plot the exponential function with the original points
depthvalues <- seq(0,2000,0.1) #0 m to 2000 m, by 0.1 m increments
predict.exponential <- exp(predict(exponential.model,list(depth=depthvalues)))
predict.exponential2 <- exp(predict(exponential.model2,list(depth=depthvalues)))
plot(depth,conc,pch=16)
lines(depthvalues,predict.exponential,lwd=2,col="red",xlab="depth (m)",ylab="concentration (#/L)")

#flip the chart 
data <- read_excel("C:/Users/Alice/Desktop/Alice/Postdoc 2024-2026/Research/1 Microplastics in OMZ, Eastern Tropical North Pacific/Data Analysis/Coding, Plot Making Practice/HypotheticalDepthProfilesforR.xlsx")
my.formula = y ~ log(x)
data$station <- as.factor(data$station) #convert continuous into categorical variable
#plot
ggplot(data=data, aes(x=conc,y=depth)) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_y_reverse(limits=c(1500,0)) +
  #geom_smooth(formula=my.formula,method="lm",se=FALSE,color="black") + #T,level=0.95,color="black")+
  labs(x = "Concentration (#/L)",y = "Depth (m)") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#logged
ggplot(data=data, aes(x=log(conc),y=log(depth))) + 
  geom_point(aes(color=station),size=3) + 
  facet_grid(~station) + 
  scale_y_reverse(limits=c(10,0)) +
  geom_smooth(method=lm,se=T, level=0.95,color="black") +
  labs(x = "log(Concentration[#/L])",y = "log(Depth[m])") +
  theme_bw() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=25),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20))

#getting R^2 value for equation, log(conc) = b*log(depth) + a
stn1_hypothetical <- lm(log(conc)~log(depth),data)
a = format(unname(coef(stn1_hypothetical)[1]),digits=2)
b = format(unname(coef(stn1_hypothetical)[2]),digits=2)
r2 = format(summary(stn1_hypothetical)$r.squared,digits=3)
paste("log(conc) = ",a, "+ ", b, "*log(depth)")
