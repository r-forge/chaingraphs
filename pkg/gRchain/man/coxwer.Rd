% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/coxwer.R
\name{coxwer}
\alias{coxwer}
\title{Fitting Chain Graph Models}
\usage{
coxwer(fmla, data, var.frame, vartype, automatch = FALSE, pen,
  signif = 0.01, contrasts = c("contr.treatment", "contr.treatment"),
  scalepredictors = c("no", "center", "scale", "normalize"), silent = FALSE,
  restr = NULL, adjfile = NULL, ...)
}
\arguments{
\item{fmla}{A formula listing the block structure and the variables in each block. It needs be of the form Block1Vars ~ Block2Vars ~ Block3Vars and the variables per block are separated by +. So a formula y1+y2 ~ x1+x2 ~ z1~z2 refers to the model wehere y1 and y2 are in block 1 and are purely dependent variables, x1 and x2 are in block 2 and are intermediate (i.e. predictors for block 1 and dependents for block 3) and z1 and z3 are in block 3 and are purely explanatory. Internally, the var.frame is built from the formula and the vartypes is automatically detected (but that is crude).}

\item{data}{the data frame containing the variables in var.frame or formula}

\item{var.frame}{If no formula is given a variable frame with the following structure: First column are the variable names, second column are the variable types (one of categorical, ordinal, continuous, binary, count, odcount (overdispersed count), gamma or invgaussian, see also \code{\link{prep_coxwer}}), third column are the block which are labeled from left to right increasingly, so the left most block is 1 and then 2 and so forth; the block with purely dependent variable is block 1 the block with purely explanatory variables is the right most block. For an example see \code{\link{cmc_prep}}. If both formula and var.frame are given, var.frame takes precedence.}

\item{vartype}{(optional) the types of variables in data and formula; if formula is given but vartype is not, the function attempts to detect the type of variable and will select one of ordinal, categorical and continuous. The order of types in vartype is expected to be the order of variables in formula (from left to right) or in var.frame from top to bottom}

\item{automatch}{flag whether the function should match the variable type to the data object (TRUE if so); will override user specifications in var.frame; defaults to FALSE}

\item{pen}{the penalty applied for the numbers of parameters to the information criterion for variable selection; defaults to log(n) (BIC); 2 would be AIC}

\item{signif}{the significance level used in the tests for inclusion of higher order effects; defaults to 0.01}

\item{contrasts}{which contrasts to use for ordinal and nominal predictors; defaults to treatment contrasts all around}

\item{scalepredictors}{whtehr and how to scale the predictors (no=no scaling, center=mean centering, scale=standardization (mean=0 and var=1) and normalize=scaling to [0,1])}

\item{silent}{flags whether output should be printed (if TRUE, no output; the default)}

\item{restr}{a matrix of the same size as the adjacency matrix in the model so as to supress specific dependencies fitted in the algorithm (0 means not fitted)}

\item{adjfile}{if TRUE the adjacency matrix is written to a file}

\item{...}{additional arguments (not used currently)}
}
\value{
an object of class cw which contains a list of fitted models, the variable frame and the fitted adjacency matrix
}
\description{
A function to fit a block recursive chain graph model in R following the outlines of Cox & Wermuth 1996 and Caputo et al 1997; some adaptions made by us
}
\examples{
data(cmc)
#this is a 5 block model with age being
#purely explanatory (block 5) and nrChild and
#contraceptive being purely the targets (block 1)

#Using a formula and autdetection for models
#vartype is automatically detected which is crude for non-factors
res_cmc2<-coxwer(contraceptive + nrChild ~                          #block 1 - purely targets
                 mediaExp ~                                         #block 2
                 solIndex ~                                         #block 3
                 wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ #block 4
                 age,                                               #block 5 - purely explanatory
                 data=cmc)

print(res_cmc2) #Prints adjacency matrix
summary(res_cmc2,target="nrChild") #model path to "contraceptive" as the target variable
predict(res_cmc2,target="nrChild") #predict method

\dontrun{
#Using a formula and specifying the vartype
#so the algorithm can use better models for the metric/continuous variables
#(here Poisson model for nrChild instead of OLS)
res_cmc3<-coxwer(contraceptive + nrChild ~
                 mediaExp ~
                 solIndex ~
                 wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~
                 age,
                 vartype=c("cate","count","bin","ord","bin","bin","ord","ord","ord","metric"),
                data=cmc)
summary(res_cmc3,target="nrChild")

#Using a var.frame
#cmc_prep<-prep_coxwer(cmc)
data(cmc_prep)
cmc_prep
res_cmc <- coxwer(var.frame=cmc_prep, data=cmc)
}
}
\seealso{
\code{\link{prep_coxwer}} \code{\link{cmc_prep}}
}

