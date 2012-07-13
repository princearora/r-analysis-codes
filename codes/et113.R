source('~/R/library/spindle_func.R')

#reading csv from ghetto
filenames = paste(GhettoFolder(459), list.files(GhettoFolder(459))[1], sep = "")
filenames
e = read.csv(filenames)
e[,3]<-e[,3]*1000
d = read.csv("/home/deploy/ghetto-data/shared/queries/442/csv_442_Wattage_imgears-E-141.csv")

#newdata<-et113[,1]
#newdata<-append(newdata,et106[,1],length(newdata))
newdata<-e[,3]
plot(newdata,type="l")
SpindleAnomaly(newdata, split_diff=500, diff_iter = 2, sampling_rate_Hz = 10, type = "test")



plot(newdata,type="l",ylab="Et106 RPM based",xlab="Deci-Seconds",ylim=c(2000,5000))
diff_nd = (abs(diff(newdata)))
plot(diff_nd)
chk = newdata[abs(diff(diff_nd))<500]
plot(chk)
roll_sd_chk = rollapply(chk,10,sd,align="right") 
plot(roll_sd_chk)
roll_sd2chk = rollmedian(roll_sd_chk,501, na.pad = FALSE, align = "right")
plot(roll_sd2chk,type='l',ylab="Et106 RPM based",ylim=c(0,250))
abline(h=65, col = 'red')


####for cycle data
filt1= butfilt(diff_nd,type="HP",fl=0.95,deltat=0.1)
chk2 = diff_nd - filt1
plot(chk2,type='l')
chk = filt1[abs(diff(filt1))<500]
plot(chk)
roll_sd_chk = rollapply(chk,10,sd,align="right") 
plot(roll_sd_chk)
roll_sd2chk = rollmedian(roll_sd_chk,101, na.pad = FALSE, align = "right")
plot(roll_sd2chk,type='l',ylab="Et113 3-cycles",xlab="Deci-Seconds")
abline(h=65, col = 'red')


#NEW Algo
#for load data
newd = data.frame(e[,1],e[,3])
colname(newd) = c("time","obs")
as.POSIXct(newd[,1], origin = '1970-01-01', tz = 'Kolkata')
events0 <- clust(newd[1:3000,], u = 35, tim.cond = 8/365, clust.max = TRUE)
par(mfrow=c(2,2))
mrlplot(events0[, "obs"])
abline(v = 160, col = "green")
diplot(events0)
abline(v = 160, col = "green")
tcplot(events0[, "obs"], which = 1)
abline(v = 160, col = "green")
tcplot(events0[, "obs"], which = 2)
abline(v = 160, col = "green")
par(mfrow = c(2, 2))
plot(newd[,1],type="l")
summary(newd)