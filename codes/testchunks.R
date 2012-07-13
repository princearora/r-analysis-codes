test <- vibration[,6]
plot(ts(test[1:2000]))
acf(test,100)
plot(ts(test[1:5000]))
filt1= butfilt(test,type="BR",fh=0.2)
acf(filt1,100)
plot(ts(filt1[645000:730000]))

test3 <- test_3[10000:30000,2]
acf(test3,100)


test_22 <- tc1000[,6]
filt3= butfilt(test_22,type="BR",fh=0.25)
plot(ts(filt3[1000:400000]))
acf(test_22,100000)


#trying out a functional relation & also part recognition1
test_reg= vibration[,5]
plot(ts(test_reg[1:50000]))
t_mean=mean(test_reg)
#test_reg= test_reg - t_mean
library(zoo)
x=c(test_reg)
roll_m=rollmean(x,20)
indic=c(0);
sum_l-0.0;
sum_r=0.0;
for (i in 1:(length(x)-30))
{
  sum_l=sum_r;
  sum_r=0.0;
  for(j in 1:30)
  {
    sum_r=0.0+sum_r+x[i+j]
  }
  #sum_r=99.991
  if (abs(sum_l-sum_r) > 0.015)
  {indic<-c(indic,i);}
  else{};
  i=i+30;
}
plot(ts(test_reg))
for (i in 1:length(indic)){abline(v=indic[i],col='blue')}
#plot(ts(test_reg[1:2000]))
test_reg= abs(test_reg)
plot(ts(test_reg))
for (i in 1:length(indic)){abline(v=indic[i],col='blue')}
t_min = min (test_reg)
test_reg= test_reg - t_min
plot(ts(test_reg))
fuc_srpm=15000*test_reg*(t_mean+(test_reg*1000))
plot(ts(fuc_srpm[1:50000]))
########################################################


#part-extraction try2

test_reg= tc50[,6]
plot(ts(test_reg[1:50000]))
t_mean=mean(test_reg)
#test_reg= test_reg - t_mean
library(zoo)
x=c(test_reg)
roll_m=rollmean(x,200)
roll_sd = rollapply(x,200,sd,align="right") 
indic=c(0);
sum_lm=sum_lsd=0.0;
sum_rm=sum_rsd=0.0;
for (i in 1:(length(x)-400))
{
  sum_lm=sum_rm;
  sum_lsd=sum_rsd;
  sum_rm=0.0;
  sum_lsd=0.0;
  for(j in 1:200)
  {
    sum_rsd=0.0+sum_rsd+roll_sd[i+j]
    sum_rm=0.0+sum_rm+roll_sd[i+j]
  }
  #sum_r=99.991
  if (abs(sum_lsd-sum_rsd) > 0.0007 && abs(sum_lm-sum_rm) > 0.002)
  {indic<-c(indic,i);}
  else{};
  i=i+5;
}
plot(ts(test_reg))
for (i in 1:(length(indic)-1)){if(indic[i+1] > indic[i]+10){abline(v=indic[i],col='blue')}}
plot(ts(roll_sd))
for (i in 1:(length(indic)-1)){if(indic[i+1] > indic[i]+10){abline(v=indic[i+1],col='blue')}}



#part-extraction testing for other data

test_reg= tc1000[,6]
plot(ts(test_reg[1:50000]))
t_mean=mean(test_reg)
#test_reg= test_reg - t_mean
library(zoo)
x=c(test_reg)
roll_m=rollmean(x,1000)
roll_sd = rollapply(x,1000,sd,align="right") 
indic=c(0);
sum_lm=sum_lsd=0.0;
sum_rm=sum_rsd=0.0;
for (i in 1:(length(x)-2000))
{
  sum_lm=sum_rm;
  sum_lsd=sum_rsd;
  sum_rm=0.0;
  sum_lsd=0.0;
  for(j in 1:1000)
  {
    sum_rsd=0.0+sum_rsd+roll_sd[i+j]
    sum_rm=0.0+sum_rm+roll_sd[i+j]
  }
  #sum_r=99.991
  if (abs(sum_lsd-sum_rsd) > 0.001 && abs(sum_lm-sum_rm) > 0.003)
  {indic<-c(indic,i);}
  else{};
  i=i+5;
}
plot(ts(test_reg))
for (i in 1:(length(indic)-1)){if(indic[i+1] > indic[i]+10){abline(v=indic[i],col='blue')}}
#plot(ts(roll_sd))
#for (i in 1:(length(indic)-1)){if(indic[i+1] > indic[i]+10){abline(v=indic[i+1],col='blue')}}