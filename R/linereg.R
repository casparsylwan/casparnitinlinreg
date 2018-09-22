


#' Title
#'
#' @field dep matrix.
#' @field indep matrix.
#' @field betahat matrix.
#' @field predicted matrix.
#' @field residuals matrix.
#' @field inv_sqr_dep matrix.
#' @field deg_fre numeric.
#' @field sigmasq numeric.
#' @field var_betas numeric.
#' @field t_values numeric.
#' @field p_values numeric.
#' @field data ANY.
#'
#' @return
#' @export linreg
#' @exportClass linreg
#'
#' @examples
linreg <- setRefClass("linreg",
                      fields = list(dep = "numeric", indep = "matrix", betahat = "matrix",
                                    predicted = "matrix", residuals = "matrix",inv_sqr_dep = "matrix",
                                    deg_fre = "numeric", sigmasq="numeric", var_betas = "numeric",
                                    t_values = "numeric",p_values = "numeric" ,data="data.frame",
                                    formula="formula", dataname="character"),
                      methods = list(
                        coef = function(){
                          "Method coef"

                          sqr_dep<-t(indep)%*%indep #creates the square matrix

                          inv_sqr_dep<<-solve(sqr_dep) # Creates an inverse matrix to the square matrix

                          t_indep_dep<-t(indep)%*%dep #creates a matrix that is solvebal

                          betahat <<- inv_sqr_dep%*%t_indep_dep # Creates betahat for the regression

                        },
                        pred = function(){
                          "Method for prediction"
                          predicted <<- indep%*%betahat
                        },
                        resid = function(){residuals<<- predicted - dep},
                        dfre = function(){ deg_fre<<- nrow(indep) - ncol(indep)  },
                        sigma= function(){   sig <- (t(residuals)%*%residuals)/deg_fre
                        sigmasq<<- as.numeric(sig)
                        },
                        vbetas=function(){  var <-sigmasq*inv_sqr_dep
                        var_betas <<- sqrt( abs(colSums(var)) ) # Sums the variance for each colum.
                        },
                        tvalue=function(){
                          "method t-value"
                          t_values <<- as.numeric( betahat/ var_betas )
                        p_values <<- pt(t_values, deg_fre)},
                        plot  = function(){
                          "plots the residuals"
                          df<-data.frame(predicted, residuals)
                        ggplot(data = df) + aes(x =predicted, y=residuals, group=2) + geom_point()+geom_abline(intercept = 0, slope = 0 ) },
                        summary=function(){





                        },
                        print=function(){
                                  form<-format(formula)
                                  base::print(paste0("formula = ",form, ", data = ", dataname))
                                                                           },
                        initialize = function(form, data){
                          "Formula for reggression"

                          dataname<<-deparse(substitute(data))
                          indep<<-model.matrix(form,data)
                          dep<<-data[,all.vars(form)[1]]



                          formula<<-form

                        coef();pred();resid();dfre();sigma();vbetas();tvalue()






                        }

                      ) )







