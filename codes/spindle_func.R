GhettoFolder <- function(queryid)
{
  foldername = paste("/home/deploy/ghetto-data/shared/queries/", queryid, "/", sep = "")
}

#function to find spindle anomaly using energy data 
SpindleAnomaly <- function(test_data, split_diff=500,diff_iter = 2,
                          thresh=-1, sampling_rate_Hz = 10, type = "cycle")
{
   diff_nd<-test_data
   for (i in 1:diff_iter)
    {diff_nd <- abs(diff(diff_nd))}
  if (type == "cycle")    
  {   filt1<-butfilt(diff_nd,type="HP",fl=0.95,deltat=0.1)
      spike_rem<-filt1[abs(diff(filt1))<split_diff]
  }
  else
  {
    spike_rem<-test_data[abs(diff(diff_nd))<split_diff]
  }
  roll_sd<-rollapply(spike_rem,10,sd,align="right") 
  roll_med<-rollmedian(roll_sd,101, na.pad = FALSE, align = "right")
  cl <- kmeans(roll_med, 2)
  plot(roll_med, col = cl$cluster)
  points(cl$centers, col = 1:2, pch = 8, cex=2)
  plot(roll_med,type='l')
  if (thresh==-1){
   thresh = abs(diff(cl$centers))}
  abline(h=thresh, col = 'red')
  print("The difference between two centers is")
  print(abs(diff(cl$centers)))
  thresh<-thresh+mean(abs(test_data[diff(test_data)<500]))
  print(thresh)
  plot(abs(diff(newdata)),type="l")
  abline(h=thresh, col = 'red')
}