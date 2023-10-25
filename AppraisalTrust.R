library(R6)
AppraisalTrust<-R6Class(
  "AppraisalTrust",
  public = list(
    nter=NULL,
    change=NULL,
    show=NULL,
    appraise=NULL,
    diffcute =NULL,
    study = NULL,
    model =NULL,
    init_stat_pdf =NULL,
    stat_trans_trust=NULL,
    stat_trans_nontrust=NULL,
    rt=NULL,
    stat=NULL,
    initialize = function(nter, change, show, appraise,diffcute,study,model,init_stat_pdf,stat_trans_trust,stat_trans_nontrust) {
      self$nter = nter
      self$change=change
      private$change_time=floor(self$nter*self$change)
      self$show = show
      self$appraise = appraise
      self$diffcute = diffcute
      self$study = study
      self$model=model
      self$init_stat_pdf=init_stat_pdf
      self$stat_trans_trust =stat_trans_trust 
      self$stat_trans_nontrust=stat_trans_nontrust
      private$last_stat_pdf=init_stat_pdf
      private$stat_now=which(init_stat_pdf==max(init_stat_pdf))[1]
      self$rt=matrix(NA,ncol = 2,nrow = self$nter)
      self$stat=matrix(NA,ncol=3,nrow = self$nter)
    },
    simulate=function(){
      for (i in 1:self$nter){
        gaibian=ifelse(i>=private$change_time,T,F)
        stat_trans=dplyr::case_when(
          !gaibian~self$stat_trans_trust,
          gaibian~self$stat_trans_nontrust
        )
        curr_stat_pdf=c(crossprod(private$last_stat_pdf,stat_trans))
        self$stat[i,]=curr_stat_pdf
        private$stat_now=which(curr_stat_pdf==max(curr_stat_pdf))[1]
        self$rt[i,1]=private$stat_now
        m=max(private$mean_rt(private$study_t,self$diffcute,self$study,self$model,self$show,self$appraise),self$show)
        self$rt[i,2]=rnorm(1,m,sd=50)
        private$last_stat_pdf=curr_stat_pdf
        private$study_t=ifelse(private$stat_now==1,private$study_t+1,private$study_t)
      }
    },
    plot=function(){
      plot(self$rt[,2],ylab = "rt")
      abline(h=500,lty = 3)
      abline(v=private$change_time,lty = 4)
      text(x=1,y=self$show+100,"show")
      text(x=private$change_time-2,y=self$diffcute,"change")
    }
  ),
  private=list(
    change_time=NULL,
    study_t=0,
    last_stat_pdf=NULL,
    stat_now=NULL,
    rt_now=NULL,
    mean_rt=function(i,diffcute,study,model,show,Appraise){
      private$rt_now=dplyr::case_when(
        private$stat_now==1~max(diffcute-i*study,model),
        private$stat_now==2~show+Appraise,
        private$stat_now==3~show+100
      )
    }
  )
)
if (interactive()) {
  test=AppraisalTrust$new(
    nter=50,
    change=1/3,
    show=500,
    appraise=3000,
    diffcute=3000,
    study=100,
    model=2000,
    init_stat_pdf=c(1,0,0),
    stat_trans_trust=matrix(
    c(0.7,0.299,0.001,#规则学习
      0.01,0.70,0.29,#代理评估
      0.01,0.19,0.80),#代理
    nrow=3,byrow = T
    ),
    stat_trans_nontrust=matrix(
    c(0.7,0.299,0.001,#规则学习
      0.349,0.65,0.001,#代理评估
      0.01,0.19,0.80),#代理
    nrow=3,byrow = T
    )
  )
  test$simulate()
  test$plot()
}
