#' Calculates annualized growth rates.
#' From RobR
#'
#' @param begin Value at the beginning of the period.
#' @param end Value at the end of the period.
#' @param n Number of years from beginning to end
#' @keywords growth rate, annualized
#' @examples  ann.gr(4300,7950, 10)
#' @export


ann.gr<- function(begin,end, n){
  gr=(((end/begin)^(1/n))-1)*100
  return(gr)
}

#' Categorize a continuous age variable into common categories.
#'
#' @param data Data Frame with the age variable to categorize.
#' @param agevar Name of the age variable.
#' @param groups Categorization Scheme: census, census65,  five, or ten.
#' @param factor Return a factor or not.  Defaults to TRUE
#' @keywords age, recode
#' @return A factor, unless otherwise specified, with recoded \code{agevar}.
#' @examples
#' age.cat()


age.cat= function(data, agevar, groups="census", factor=TRUE){
  require(car, quietly=TRUE)
   gl=list(
    census65=list(
      recode="0:4=1; 5:17=2; 18:24=3;  25:34=4; 35:44=5; 45:54=6; 55:64=7; 65:115=8",
      labels=c("0 to 4" ,"5 to 17", "18 to 24", "25 to 34", "35 to 44","45 to 54","55 to 64", "65 and Over"),
      levels=8),
    census=list(
      recode="0:4=1; 5:17=2; 18:24=3;  25:34=4; 35:44=5; 45:54=6; 55:64=7; 65:74=8; 75:84=9; 85:115=10",
      labels=c("0 to 4" ,"5 to 17", "18 to 24", "25 to 34", "35 to 44","45 to 54","55 to 64", "65 to 74", "75 to 84", "85 and Over"),
      levels=10),
    five=list(
      recode="0:4=1;5:9=2;10:14=3;15:19=4;20:24=5;25:29=6;30:34=7;35:39=8;40:44=9;45:49=10;
            50:54=11;55:59=12;60:64=13;65:69=14;70:74=15;75:79=16;80:84=17;85:115=18",
      labels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34",
               "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59", "60 to 64", "65 to 69",
               "70 to 74", "75 to 79","80 to 84", "85 and Over"),
      levels=18),
    ten=list(
      recode="0:9=1;10:19=2;20:29=3;30:39=4;40:49=5;50:59=6;60:69=7;70:79=8;80:89=9;90:115=10",
      labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39","40 to 49","50 to 59", "60 to 69",
               "70 to 79","80 to 89", "90 and Over"),
      levels=10))
  grp=gl[[groups]]
  v=recode(data[[agevar]], grp$recode)
  f=ordered(v,levels=1:grp$levels, labels=grp$labels)
  if (factor==TRUE) {
 return(f)
  } else {
    return(v)
  }
}

#' Categorize a continuous education variable into common categories.
#'
#' @param data Data Frame with the education variable to categorize.
#' @param edvar Name of the education variable.
#' @param groups Categorization Scheme: more, basic, higher
#' @param factor Return a factor or not.  Defaults to TRUE
#' @keywords education, recode
#' @return A factor, unless otherwise specified, with recoded \code{edvar}.
#' @examples
#' ed.cat()

ed.cat= function(data, edvar, groups="basic",factor=TRUE){
  require(car, quietly=TRUE)
  gl=list(
    more=list(
      recode="0:15=1;16:24=2",
      labels=c("Less than High School", "High School or higher"),
      levels=2),
    basic= list(
      recode="0:15=1;16:17=2;18:19=3; 20=4; 21:24=5",
      labels=c("Less than High School", "High School or GED", "Some College",
               "Associate's Degree", "Bachelor's or higher"),
      levels=5),
    higher=list(
      recode="0:15=1;16:17=2;18:19=3; 20=4; 21=5; 22=6; 23:24=7",
      labels=c("Less than High School", "High School or GED", "Some College",
               "Associate's Degree","Bachelor's Degree", "Master's Degree",
               "Graduate or Professional Degree"),
      levels=7))
  grp=gl[[groups]]
  v=recode(data[[edvar]], grp$recode)
  f=ordered(v,levels=1:grp$levels, labels=grp$labels)
  if(factor==TRUE){
    return(f)
  } else{
    return(v)
  }
}

#' Estimate Model Migration Schedules using Non-linear Least Squares
#'
#' @param data Data frame with places in columns with an ID for age.
#' @param model Picks 7, 9, or 11 parameter models
#' @param ages Number of age categories, use the maximum age.
#' @param profiles Number of profiles/areas to fit.
#' @keywords initital Optional. Initital parameter values for the model.
#' @return A factor, unless otherwise specified, with recoded \code{agevar}.
#' @examples
#' age.cat()
#'


modelmig <- function(data,model,ages,profiles,initial=NULL)
{
  data=t(data)
  beta=list(model7=c(a1=.075, alpha1=.03, a2=.06, alpha2=.1, mu2=20, lambda2=.4, c=.003),
            model9=c(a1=.075, alpha1=.03, a2=.06, alpha2=.1, mu2=20, lambda2=.3, a3=.75,lambda3=.145, c=.003),
            model11=c(a1=.075, alpha1=.03, a2=.06, alpha2=.1, mu2=20, lambda2=.35, a3=.85,lambda3=.155, c=.003))
  beta0=ifelse(initial==NULL, beta[[model]],initial)
  lb=0
  control=nls.control(maxiter=100000000, tol=.000000000001, minFactor=0.000000001)
  param=mat.or.vec(profiles, length(beta0))
  sched=mat.or.vec(profiles, (ages+1))
  fitting=mat.or.vec(profiles,2)
  model7=y~a1*exp(-alpha1*age) + a2*exp(-alpha2*(age-mu2) - exp(-lambda2*(age-mu2)))+c
  model9=y~a1*exp(-alpha1*age) + a2*exp(-alpha2*(age-mu2) - exp(-lambda2*(age-mu2)))+a3*exp(lambda3*age)+c
  model11=y~a1*exp(-alpha1*age) + a2*exp(-alpha2*(age-mu2) - exp(-lambda2*(age-mu2)))+a3*exp(lambda3*age)+c
  models=list(model7=model7, model9=model9, model11=model11)

  for (i in 1:profiles)
  {
    age=rep(0:ages)
    y=as.vector(data[,i+1])
    fit<-nls(models[[model]] , start=beta0, control=control, algorithm="port", lower=lb)
    param[i,]=coef(fit)
    sched[i,]=predict(fit, age)
    sst=sum(y-mean(y))^2
    ssr=sum(residuals(fit)-mean(residuals(fit)))^2
    fitting[i,2]=1-(ssr/sst)
    fitting[i,1]=(100*(1/(length(age)-1)))*sum(abs((sched[i,]-y))/y)
  }

  write.table(param, "parameter.est.7param.csv", sep=",")
  write.table(sched, "predicted.profiles.7param.csv", sep=",")
  write.table(fitting, "fit.stats.7param.csv", sep=",")
  return(list(param, sched, fitting))
}



