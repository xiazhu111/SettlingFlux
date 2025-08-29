Ploug10sinkorbit = function(x1,y1,T1,pf) { #enter 1/rotation rate (s) here 
  #e.g. Ploug10sinkorbit(c(1,2,3,4),c(1,2,3,4),0.083,1)
  
  # file: Ploug10sinkorbit.R
  # created by: Dieter.Wolf-Gladrow@awi.de
  #    8/2016 version 1.0
  # This software is provided 'as is' without warranty of
  # any kind. But it's mine, so you can't sell it.
  #
  # purpose: estimate sinking speed from orbit ('orbit analysis')
  #
  # related publication: 
  #    Helle Ploug, Anja Terbrueggen, Anna Kaufmann,
  #    Dieter Wolf-Gladrow, Uta Passow, A novel method to measure 
  #    particle sinking velocity in vitro, and its comparison to three 
  #    other in vitro methods, Limnology & Oceanography Methods,
  #    8, 386-393, 2010. DOI 10:4319/lom.2010.8.386
  #
  # input:
  #   x1   array of x-coordinates of particles (mm) *for a single aggregate b/c only
  #   y1   array of y-coordinates of particles (mm) *one sinking velocity is outputted
  #        remarks: 
  #          1. Origin of coordinate system at rotation
  #             axis of the roller tank; x = horizontal;
  #             y = vertical 
  #          2. x1 and y1 arrays must have the same length
  #   T1   rotation period (s)
  #   pf   plot-flag: p = 0 <-> no plot; 
  #                   p = 1 <-> plot data, orbit, optimal values
  #
  # output: 
  #   out  array with sinking speed (out(1)), orbit centre 
  #        coordinates xc and yc (out(2) and out(3)), and
  #        orbit radius r (out(4))
  #   out(1) sinking speed in m/d
  #   out(2) orbit centre coordinate xc in mm
  #   out(3) orbit centre coordinate yc in mm
  #   out(4) orbit radius in mm
  #
  # original MATLAB code created by:
  # Dieter Wolf-Gladrow (Dieter.Wolf-Gladrow@awi.de) 1 December 2009
  # This software is provided "as is" without warranty of any kind. But
  # it's mine, so you can't sell it.
  #
  # --------------------------------------------------------------------
  L = length(x1); M = length(y1)
  #if (L != M) {
  #print('Error: number of x and y coordinates not equal'); 
  #  stop()}
  # rough estimate: 
  # center coordinates (x_c,y_c) = mean over (x_k, y_k), 
  # radius = mean over [(x_k,y_k) - (x_c, y_c)]^2
  xc1 = mean(x1); yc1 = mean(y1)
  r1 = mean(sqrt( (x1-xc1)^2 + (y1-yc1)^2 ))
  # range of centre coordinates: 
  dx = 0.1; dy = dx # 0.1 mm increments in x and y, explore the space of answers until best fit is found 
  xca1 = xc1-0.8*r1; xca2 = xc1+0.8*r1 #assume max error of +/-80% mean radius in our estimate of center position
  yca1 = yc1-0.8*r1; yca2 = yc1+0.8*r1
  xca = seq(xca1,xca2,dx); Lx = length(xca)
  yca = seq(yca1,yca2,dy); Ly = length(yca)
  # calculate variances of radii for various center coordinates
  # and search minimum of these variances
  smin = 1e20; nxmin = 1; nymin = 1; ra=numeric(L)
  LxLy=Lx*Ly; arr1=numeric(LxLy)
  rvar=matrix(data=arr1,nrow=Lx,ncol=Ly)
  rma=matrix(data=arr1,nrow=Lx,ncol=Ly)
  for(nx in 1:Lx) {xc = xca[nx];
  for(ny in 1:Ly) {yc = yca[ny];
  for(n in 1:L) ra[n] = sqrt((x1[n]-xc)^2+(y1[n]-yc)^2)
  rmean=mean(ra); rvar[nx,ny]=sum((ra-rmean)^2); 
  rma[nx,ny]=rmean;
  if (rvar[nx,ny] < smin) {smin = rvar[nx,ny];
  nxmin = nx; nymin = ny}}}
  xc1 = xca[nxmin]; yc1 = yca[nymin]; r1 = rma[nxmin,nymin]
  wsmms = xc1/T1*2*pi    # (mm/s) sinking speed
  wsmd = wsmms*3.6*24    # (m/d)  sinking speed
  out = c(wsmd,xc1,yc1,r1)
  # plot
  if (pf == 1) {
  p = seq(0,7,0.1);   # angle phi
  xcy = r1*cos(p)+xc1; ycy = r1*sin(p)+yc1;
  ax1 = xc1-r1*1.2; ax2 = xc1+r1*1.2;
  ay1 = yc1-r1*1.2; ay2 = yc1+r1*1.2;
  a1 = min(xcy); a2 = max(xcy);
  b1 = min(ycy); b2 = max(ycy);
  c1 = max((a2-a1),(b2-b1));
  d1 = a1-c1/10; d2 = d1+c1*1.2;
  e1 = b1-c1/10; e2 = e1+c1*1.2;
  # jpeg('SinkOrbit160808.jpeg',width=16,height=12,units='cm',quality=100,res=300)
  # plot(x,y,type='l',lwd=3,col='blue',xlab='x',ylab='y',las=1,xaxs='i',yaxs='i')
  # dev.off()
  plot(x1,y1,type='p',col='red',xlab='x (mm)',ylab='y (mm)',las=1,
       xlim=c(d1,d2),ylim=c(e1,e2))
  lines(xcy,ycy,col='blue')
  points(xc1,yc1,col='black',lty=23)
       # xcy,ycy,'b',xc1,yc1,'k+','LineWidth',2)
  # xt = mean(xcy)-0.3*(max(xcy)-min(xcy));
  # text(xt,b1+c1*0.55,['x_b (mm) = ',num2str(xc1,3)], ...
  #     'Color',[0 0 1],'Fontsize',16)
  # text(xt,b1+c1*0.4,['y_b (mm) = ',num2str(yc1,3)], ...
  #     'Color',[0 0 1],'Fontsize',16)
  # text(xt,b1+c1*0.25,['r (mm) = ',num2str(r1,2)], ...
  #     'Color',[0 0 1],'Fontsize',16)
  # title(['w_s (m d^{-1}) = ',num2str(wsmd,3)], ...
  #      'Color',[0 0 1],'Fontsize',16)
  }
  return(out)
}

#Week 1 rpm = 1.89, Week 2 rpm = 1.47, Week 3 rpm = 1.466

#Particle 1, > 16 mm, length = 29.155 mm, Week 1 LDPE Tank
#x1 = array (mm), y2 = array (mm), period (s) = 60/rpm, 0 = no plot
Ploug10sinkorbit(data$x1,data$y1,60/1.89,1)
#Particle 2, 8-16 mm, length = 11.001 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x2),na.omit(data$y2),60/1.89,1)
#Particle 3, 8-16 mm, length = 11.668 mm, Week 1 LDPE Tank, positively buoyant
Ploug10sinkorbit(na.omit(data$x3),na.omit(data$y3),60/1.89,1)
#Particle 4, 4-8 mm, length = 5.926 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x4),na.omit(data$y4),60/1.89,1)
#Particle 5, 2-4 mm, length = 2.719 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x5),na.omit(data$y5),60/1.89,1)
#Particle 6, 2-4 mm, length = 2.445 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x6),na.omit(data$y6),60/1.89,1)
#Particle 7, 2-4 mm, length = 2.284 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x7),na.omit(data$y7),60/1.89,1)
#Particle 8, 1-2 mm, length = 0.977 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x8),na.omit(data$y8),60/1.89,1)
#Particle 9, 2-4 mm, length = 3.184 mm, Week 1 LDPE Tank, positively buoyant
Ploug10sinkorbit(na.omit(data$x9),na.omit(data$y9),60/1.89,1)
#Particle 10, 4-8 mm, length = 5.728 mm, Week 1 LDPE Tank, positively buoyant
Ploug10sinkorbit(na.omit(data$x10),na.omit(data$y10),60/1.89,1)
#Particle 11, 2-4 mm, length = 2.247 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x11),na.omit(data$y11),60/1.89,1)
#Particle 12, 1-2 mm, length = 1.122 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x12),na.omit(data$y12),60/1.89,1)
#Particle 13, 1-2 mm, length = 1.285 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x13),na.omit(data$y13),60/1.89,1)
#Particle 14, 4-8 mm, length = 6.332 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x14),na.omit(data$y14),60/1.89,1)
#Particle 15, 4-8 mm, length = 4.585 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x15),na.omit(data$y15),60/1.89,1)
#Particle 16, 4-8 mm, length = 4.388 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x16),na.omit(data$y16),60/1.89,1)
#Particle 17, 1-2 mm, length = 1.442 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x17),na.omit(data$y17),60/1.89,1)
#Particle 18, 1-2 mm, length = 1.904 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x18),na.omit(data$y18),60/1.89,1)
#Particle 19, 8-16 mm, length = 9.199 mm, Week 1 LDPE Tank
Ploug10sinkorbit(na.omit(data$x19),na.omit(data$y19),60/1.89,1)
#Particle 20, 4-8 mm, length = 5.247 mm, Week 1 LDPE Tank, positively buoyant
Ploug10sinkorbit(na.omit(data$x20),na.omit(data$y20),60/1.89,1)
#Particle 21, 4-8 mm, length = 6.866 mm, Week 1 LDPE Tank, positively buoyant
Ploug10sinkorbit(na.omit(data$x21),na.omit(data$y21),60/1.89,1)
#Particle 22, 4-8 mm, length = 4.164 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x22),na.omit(data$y22),60/1.89,1)
#Particle 23, 2-4 mm, length = 3.175 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x23),na.omit(data$y23),60/1.89,1)
#Particle 24, 2-4 mm, length = 2.597 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x24),na.omit(data$y24),60/1.89,1)
#Particle 25, 1-2 mm, length = 1.961 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x25),na.omit(data$y25),60/1.89,1)
#Particle 26, 2-4 mm, length = 2.511 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x26),na.omit(data$y26),60/1.89,1)
#Particle 27, 4-8 mm, length = 4.984 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x27),na.omit(data$y27),60/1.89,1)
#Particle 28, 1-2 mm, length = 1.567 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x28),na.omit(data$y28),60/1.89,1)
#Particle 29, 2-4 mm, length = 2.25 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x29),na.omit(data$y29),60/1.89,1)
#Particle 30, 2-4 mm, length = 2.35 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x30),na.omit(data$y30),60/1.89,1)
library(readxl)
data <- read_excel("C:/Users/Alice/Desktop/Alice/Postdoc 2024-2026/Research/2 Settling and Aggregation Experiments/Methods/7 Data Analysis/Aggregate Properties Workbooks - Results/sinkingvelocities.xlsx")
#Particle 31, 1-2 mm, length = 1.447 mm, Week 1 Control Tank
Ploug10sinkorbit(na.omit(data$x31),na.omit(data$y31),60/1.89,1)
