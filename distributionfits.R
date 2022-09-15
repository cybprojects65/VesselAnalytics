speeddistr<-dataVessel_bb_fishing_vessels$speed#test$speed
speeddistr<-speeddistr[which(speeddistr<5 & speeddistr>1)]

library(MASS)
fit_params <- fitdistr(speeddistr,"lognormal")
x <- seq(1,5,length=700)
it <- dlnorm(x, fit_params$estimate['meanlog'], fit_params$estimate['sdlog'])
upper_thr = exp(fit_params$estimate['meanlog']+1.96*fit_params$estimate['sdlog'])
lower_thr = exp(fit_params$estimate['meanlog']-1.96*fit_params$estimate['sdlog'])
cat(lower_thr,"<speed<",upper_thr,"\n")
fit_params2 <- fitdistr(speeddistr,"normal")
it2 <- dnorm(x, fit_params2$estimate['mean'], fit_params2$estimate['sd'])
cat(fit_params2$estimate['mean']-1.96*fit_params2$estimate['sd'],"<speed<",fit_params2$estimate['mean']+1.96*fit_params2$estimate['sd'],"\n")

fit_params3 <- fitdistr(speeddistr,"logistic")
it3 <- dlogis(x, location=fit_params3$estimate['location'], scale=fit_params3$estimate['scale'])
sigma<-as.numeric(fit_params3$estimate['scale']*pi/sqrt(3))
upper_thr = (fit_params3$estimate['location']+1.96*sigma)
lower_thr = (fit_params3$estimate['location']-1.96*sigma)
cat(lower_thr,"<speed_logit<",upper_thr)

plot(density(speeddistr))
lines(x,it,type='l',col='yellow')
lines(x,it2,type='l',col='blue')
lines(x,it3,type='l',col='red')


p = st_sfc(st_point(c(test[1,]$x,test[1,]$y)))
st_distance(st_point(c(test[1,]$x,test[1,]$y)), st_point(c(test[1,]$x+1,test[1,]$y)))

dists_lonlat <- pointDistance(c(test[1,]$x,test[1,]$y),c(test[1,]$x+1,test[1,]$y), lonlat = TRUE) #specify for lonlat
pointDistance(c(test[1,]$x,test[1,]$y),c(test[1,]$x,test[1,]$y+1), lonlat = TRUE) #specify for lonlat


hdistr<-dataVessel_reconstructed$timediff_hours_estimated
#hdistr<-hdistr[which(hdistr>10 & hdistr<20)]
hdistr<-hdistr[which(hdistr>10 & hdistr<30)]
dens<-density(hdistr)
max<-dens$x[which(dens$y == max(dens$y))]
plot(density(hdistr))


