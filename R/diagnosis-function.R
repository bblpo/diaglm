#' @title test whether linear model assumptions have been violated
#' @description  test for unusual data, nonlinearity, non-normality, non-constant error variance, auto-correlation
#' @param  x an object, which is a result from a linear model (lm)
#' @param  file is the name of text file you want to create (e.g.,'mydiagnosis.txt'). It is iniated as 'result.txt'
#' Note that the file will be saved at the working directory
#' data(data)
#' mod<-lm(data$tolerance_scale~data$sg_participation+data$virtual_si+as.numeric(data$EDUC)+as.numeric(data$yrbrn)+data$IVRACE2+data$IVRACE3+data$gndr+as.numeric(data$lrscale))
#' diaglm(mod,data$tolerance_scale)
#' @export
#' @return The statistical tests results, outlierTest, durbinWatsonTest, VIF, and gvlma



diaglm<-function(x,file='result.txt'){

### for unusal data ###########
  plot(density(rstudent(x)))
  dev.copy(png,'density.png') ## save the chart in the same directory
  dev.off()

  influencePlot(x,main="influencePlot") #test influence
  dev.copy(png,'influence.png')
  dev.off()

  qqPlot(x,main="qqPlot for outlier and normality of residual") # test outlier
  dev.copy(png,'qq.png')
  dev.off()

  leveragePlots(x,main = "leveragePlots for outlier") # test outlier
  dev.copy(png,'leverage.png')
  dev.off()

   avPlots(x) # check far left and far right observation as unusual
  dev.copy(png,'avPlot.png')
  dev.off()

### check nonlinerity #########

  #ceresPlots(x,main="ceresPlots for nonlinerity")
  residualPlots(x,main="residualPlot for nonlinearity (no curve, no systematic feature)")
  dev.copy(png,'residualPlot.png')
  dev.off()

### check non-normality #########
  sresid <- studres(x)
  hist(sresid, freq=FALSE,
       main="Distribution of Studentized Residuals to check non-normality")
  xfit<-seq(min(sresid),max(sresid),length=40)
  yfit<-dnorm(xfit)
  lines(xfit, yfit)
  dev.copy(png,'studenResid.png')
  dev.off()

### check non-constant error variance ########
  #ncvTest(x,main="ncvTest for non-consistent error variance")
  spreadLevelPlot(x,main="spreadLevelPlot for non-consistent error variance")
  dev.copy(png,'spread.png')
  dev.off()

### examine the overall model fit
  marginalModelPlots(x,main="marginalModelPlots")
  dev.copy(png,'marginal.png')
  dev.off()

### save the test results in a file named based on your second parameter, y #####

  cat("\n","\n","The outlier test shows","\n",file=file,append = TRUE)
  out<-capture.output(outlierTest(x))
  cat(out,file=file,append=TRUE,fill=2,sep = "\n")

  cat("\n","\n","The durbinWatson tests if p value is <.05 for autocorrelation. If so, use robust regression","\n",file=file, append = TRUE)
   out<-capture.output(durbinWatsonTest(x))
  cat(out,file=file,append=TRUE,fill=3,sep = "\n")

  cat("\n","\n","The collinearity test if VIF>4","\n",file=file, append = TRUE)
  out<-capture.output(vif(x))
  cat(out,file=file,append=TRUE,sep = "\n")

  cat("\n","\n","The linear model assumptions test shows","\n",file=file, append = TRUE)
  out<-capture.output(gvlma(x))
  cat(out,file=file,append=TRUE,sep = "\n")



  ### Since CrPlot cannot be used for interactive term, put it last here:
  crPlots(x,main="crPlots for nonlinearity and need for transformation: see if two lines are straight and merge")
  dev.copy(png,'crPlot.png')
  dev.off()

### display the test results on the screen ########

  a<-outlierTest(x)
  b<-durbinWatsonTest(x) ## check autocorrelation
  c<-vif(x)
  # check violations of LM assumptions
  d<-gvlma(x)

  all<-list("outlierTest"=a,"Durbin Watson Test"=b,
            "collinearityTest-VIF"=c,"check violations of LM assumptions"=d)
  return(all)

}

