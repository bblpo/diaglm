#' @title find the best transformatio of response variable, and then run the linear model based on the best transformation
#' @description  tranaform the y and rerun the linear model
#' @param  x an lm object, which is a result from a linear model (lm)
#' @param y the response variable you would like to transform
#' @param  file the name of text file you want to create (e.g.,'mydiagnosis.txt'). It is iniated as 'result.txt'
#' Note that the file will be saved at the working directory
#' @examples
#' data(data)
#' mod<-lm(data$tolerance_scale~data$sg_participation+data$virtual_si+as.numeric(data$EDUC)+as.numeric(data$yrbrn)+data$IVRACE2+data$IVRACE3+data$gndr+as.numeric(data$lrscale))
#' transformY(mod,data$tolerance_scale)
#' @export
#' @return The statistical tests results, outlierTest, durbinWatsonTest, VIF, and gvlma
#' @return y1round a transformed y vector in global environment


transformY<-function(x,y,file='tranformResult.txt'){

p1<-powerTransform(x) ## p1 is a statistical test to find the best power function for the response variable

y1round<<-bcPower(as.numeric(y),coef(p1,round=TRUE)) # we need to make y1round global
d2<-update(x,y1round~.)  ## rerun the regression based on the new transformed response variable
qqPlot(d2) # display a qqPlot for the new residual analysis

## create output for the statistical test
cat("\n","\n","The power transformation test shows","\n",file=file,append = TRUE)
out<-capture.output(summary(p1))
cat(out,file=file,append=TRUE,fill=2,sep = "\n")

## create output for the regression based on the transformed response variable
cat("\n","\n","The new regression based on the transformed response variable","\n",file=file,append = TRUE)
out<-capture.output(summary(d2))
cat(out,file=file,append=TRUE,fill=2,sep = "\n")

# perform the diagnosis for the regression based on the transformed response variable
diaglm(d2,file=file)
}

## If this transformation of response variable doesn't improve much the final fit, you may need to check the explanatory variables.
## Especially, crPlot should reveal whether a particular explanatory variable should be transformed.
