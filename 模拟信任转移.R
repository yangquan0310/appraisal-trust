nter=50#试次
change_time=floor(nter/3)
show=500
Appraise=3000
diffcute=3000
study=100
model=2000
stat_trans_trust<-matrix(
  c(0.7,0.299,0.001,#规则学习
    0.01,0.70,0.29,#代理评估
    0.01,0.19,0.80),#代理
  nrow=3,byrow = T
)

stat_trans_nontrust<-matrix(
  c(0.7,0.299,0.001,#规则学习
    0.349,0.65,0.001,#代理评估
    0.01,0.19,0.80),#代理
  nrow=3,byrow = T
)

init_stat_pdf<-c(1,0,0)



mean_rt<-function(stat,i){
  dplyr::case_when(
    stat==1~max(diffcute-i*study,model),
    stat==2~show+Appraise,
    stat==3~show+100
  )
}
last_stat_pdf=init_stat_pdf
rt=matrix(NA,ncol = 2,nrow = nter)
stat=matrix(NA,ncol=3,nrow=nter)
study_t=0
for (i in 1:nter){
  gaibian=ifelse(i>=change_time,T,F)
  stat_trans=dplyr::case_when(
    !gaibian~stat_trans_trust,
    gaibian~stat_trans_nontrust
    )
  curr_stat_pdf=c(crossprod(last_stat_pdf,stat_trans))
  stat[i,]=curr_stat_pdf
  curr_stat=which(curr_stat_pdf==max(curr_stat_pdf))[1]
  rt[i,1]=curr_stat
  m=max(mean_rt(curr_stat,study_t),show)
  rt[i,2]=rnorm(1,m,sd=50)
  last_stat_pdf=curr_stat_pdf
  study_t=ifelse(curr_stat==1,study_t+1,study_t)
  print(rt[i,1])
}
plot(rt[,2],ylab = "rt")
abline(h=500,lty = 3)
abline(v=change_time,lty = 4)
text(x=1,y=show+100,"show")
text(x=change_time-2,y=diffcute,"change")

