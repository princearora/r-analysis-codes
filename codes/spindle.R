##Faulty Spindle Energy Analysis Type1##

#setting up variables and taking data
faulty = et028[,2]
non_faulty = et043[,2]
complete <- faulty
complete <- append(complete,non_faulty,4182)
plot(complete, type = 'l')
diff_faulty = (abs(diff(faulty)))
diff_non_faulty = (abs(diff(non_faulty)))

#removing spikes
c = faulty[diff_faulty<500]
d = non_faulty[abs(diff(diff_non_faulty))<300]
d = d[abs(diff(diff(d)))<500]
e <-c
e <-append(e,d)
plot(c)
plot(d)

#applying rolling sd
roll_sd = rollapply(e,10,sd,align="right") 
plot(roll_sd)
roll_sd2 = rollmedian(roll_sd, 1001, na.pad = FALSE, align = "right")
plot(roll_sd2,type='l')
abline(h=65, col = 'red')

#using ggplot for a better plot
roll_sd2.df = data.frame(index=c(1:7023),value=roll_sd2)
p = ggplot(roll_sd2.df)+geom_line(aes(x=index,y=value))+ xlab("Number of Samples")+  ylab("Rolling Mean (Index)")
p = p + geom_hline(aes(yintercept=62), colour="#990000", linetype="dashed")
p = p + geom_rect(data=roll_sd2.df,aes(xmin=950, xmax=2300, ymin=62, ymax=+Inf), fill='pink', alpha=0.01)
p = p + geom_vline(aes(xintercept=950), colour="#330000", linetype="solid") + geom_vline(aes(xintercept=2300), colour="#330000", linetype="solid")
p

#finding if faulty or not
result = c(0)
for (i in 100:(length(e)-100))
{
  if (sd(e[i:(i+100)])>80) {result[i]=1;} else  {}
  i<-i+100
}  

plot(result)
sd(c)
sd(d)


#applying filters o smoothen
filt1= butfilt(roll_sd,type="LP",fh=0.01)
plot(ts(filt1),type='l')

#trying out kmeans
for (i in 1:8032)
 { if (i<4140) assign[i]=1 else assign[i]=2}
comple <-data.frame(e,assign)
colnames(comple) = c("value", "group")
library(cluster) 
clusplot(comple, comple$group, color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)
plotcluster(comple, comple$group)

fit <- kmeans(comple, 2)

