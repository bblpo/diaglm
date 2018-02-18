#' @title find the best transformatio of explanatory variables in multiple regression
#' @description  tranaform the X variables. Different from transformX because here all variables are considered for transformation
#' @param  mod a formula like y~X1+X2 for the linear model
#' @param data the dataset you will run on
#' @param file the default is'transformX.txt'
#' Note that the file will be saved at the working directory
#' @examples
#' data(data)
#' transformX.nocontrol(tolerance_scale~as.numeric(EDUC)+as.numeric(lrscale),data)
#' @export
#' @return The statistical tests results based on the Maximum likelihood

transformX.nocontrol<-function (mod,d,file='transformX.txt'){
  o<-boxTidwell(mod,data=d,na.action=na.exclude)
  cat("\n","\n","The best transformation of X shows","\n",file=file, append = TRUE)
  out<-capture.output(o)
  cat(out,file=file,append=TRUE,sep = "\n")
  return(o)
}
