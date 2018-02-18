#' @title find the best transformatio of explanatory variables in multiple regression
#' @description  tranaform the X variables in multiple regression
#' @param  mod a formula like y~X1+X2 for the linear model
#' @param control the control variable(s) are those in the model that you don't want to transform, use the form of ~x3+x4
#' @param data the dataset you will run on
#' @param file the default is'transformX.txt'
#' Note that the file will be saved at the working directory
#' @examples
#' data(data)
#' transformX(tolerance_scale~as.numeric(EDUC)+as.numeric(lrscale),~factor(gndr),data)
#' @export
#' @return The statistical tests results based on the Maximum likelihood

transformX<-function (mod,control,d,file='transformX.txt'){
  c<-get("control",environment())
  o<-boxTidwell(mod,c,data=d,na.action=na.exclude)
  cat("\n","\n","The best transformation of X shows","\n",file=file, append = TRUE)
  out<-capture.output(o)
  cat(out,file=file,append=TRUE,sep = "\n")
  return(o)
}

