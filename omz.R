#Vertical distribution of microplastics in ENTP OMZ
#helpful resource: https://www.theanalysisfactor.com/r-tutorial-5/

library(ggplot2)
#practice fitting an exponential decay function or power law function to data
#in the form of y = ab^x
depth <- c(10,54,200,400,500,700,1200) #y-axis #Station 1 hypothetical
conc <- c(18,13,7,10,10,10,5) #x-axis #Station 1 hypothetical
depth2 <- c(10,32,100,300,500,800,1200) #Station 2 hypothetical
conc2 <- c(15,13,10,10,8,7,8) #Station 2 hypothetical

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
