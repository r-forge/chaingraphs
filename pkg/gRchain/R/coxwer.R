#' Fitting wrapper for coxwer
#'
#' @param fmla an R formula
#' @param data the data frame to take the variables from
#' @param type what is the type of the target variable
#' @param currfamily current family for the glm
#' @param ... further arguents passed to the fitting functions
#' 
#' @return a fitted model of either class multinom (categorical type) or polr (ordinal type) or glm.nb (count type) or glm (for type being metric or positive or so)
#' @import MASS nnet
fitfunc <- function(fmla, data=data, type, currfamily=currfamily,...)
  {
     if (type == "categorical")
       {
            mod <- nnet::multinom(fmla,data=data,trace=FALSE,Hess=TRUE,MaxNWts = dim(data)[1]+1,model=TRUE,...)
       } else if (type == "ordinal")
         {
            mod <- MASS::polr(fmla,data=data,Hess=TRUE,...)
         } else if (type == "odcount")
           {
            mod <- MASS::glm.nb(fmla,data=data,...) 
         } else
           {
            mod <- stats::glm(fmla,data=data,family=currfamily,...)
        }       
   mod
   }


#' Fitting Chain Graph Models
#'
#' A function to fit a block recursive chain graph model in R following the outlines of Cox & Wermuth 1996 and Caputo et al 1997; some adaptions made by us
#'
#'
#' @param fmla A formula listing the block structure and the variables in each block. It needs be of the form Block1Vars ~ Block2Vars ~ Block3Vars and the variables per block are separated by +. So a formula y1+y2 ~ x1+x2 ~ z1~z2 refers to the model wehere y1 and y2 are in block 1 and are purely dependent variables, x1 and x2 are in block 2 and are intermediate (i.e. predictors for block 1 and dependents for block 3) and z1 and z3 are in block 3 and are purely explanatory. Internally, the var.frame is built from the formula and the vartypes is automatically detected (but that is crude).
#' @param data the data frame containing the variables in var.frame or formula 
#' @param var.frame If no formula is given a variable frame with the following structure: First column are the variable names, second column are the variable types (one of categorical, ordinal, continuous, binary, count, odcount (overdispersed count), gamma or invgaussian, see also \code{\link{prep_coxwer}}), third column are the block which are labeled from left to right increasingly, so the left most block is 1 and then 2 and so forth; the block with purely dependent variable is block 1 the block with purely explanatory variables is the right most block. For an example see \code{\link{cmc_prep}}. If both formula and var.frame are given, var.frame takes precedence. 
#' @param vartype (optional) the types of variables in data and formula; if formula is given but vartype is not, the function attempts to detect the type of variable and will select one of ordinal, categorical and continuous. The order of types in vartype is expected to be the order of variables in formula (from left to right) or in var.frame from top to bottom 
#' @param automatch flag whether the function should match the variable type to the data object (TRUE if so); will override user specifications in var.frame; defaults to FALSE
#' @param pen the penalty applied to the information criterion for variable selection; defaults to #parameters*log(n) (BIC); -2 would be AIC
#' @param signif the significance level used in the tests for inclusion of higher order effects; defaults to 0.01
#' @param contrasts which contrasts to use for ordinal and nominal predictors; defaults to treatment contrasts all around 
#' @param scalepredictors whtehr and how to scale the predictors (no=no scaling, center=mean centering, scale=standardization (mean=0 and var=1) and normalize=scaling to [0,1])
#' @param silent flags whether output should be printed (if TRUE, no output; the default)
#' @param restr a matrix of the same size as the adjacency matrix in the model so as to supress specific dependencies fitted in the algorithm (0 means not fitted)
#' @param adjfile if TRUE the adjacency matrix is written to a file
#' @param ... additional arguments (not used currently)
#' 
#' @return an object of class cw which contains a list of fitted models, the variable frame and the fitted adjacency matrix
#'
#' @examples
#' data(cmc)
#' #this is a 5 block model with age being
#' #purely explanatory (block 5) and nRchild and
#' #contraceptive being purely the targets (block 1)
#'
#' #Using a var.frame
#' data(cmc_prep)
#' cmc_prep
#' res_cmc <- coxwer(var.frame=cmc_prep, data=cmc)
#' print(res_cmc) #Prints adjacency matrix
#' summary(res_cmc,target="contraceptive") #model path to "contraceptive" as the target variable
#'
#' #Using a formula and autdetection for models
#' res_cmc2<-coxwer(contraceptive + nrChild ~ mediaExp ~ solIndex ~ wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ age, data=cmc) #vartype is automatically detected which is crude for non-factors
#' 
#' #Using a formula and specifying the vartype
#' #so the algorithm can use better models for the metric/continuous variables
#' res_cmc3<-coxwer(contraceptive + nrChild ~ mediaExp ~ solIndex ~ wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ age, vartype=c("cate","count","bin","ord","bin","bin","ord","ord","ord","metric"), data=cmc)
#' 
#' @export
#' @seealso \code{\link{prep_coxwer}} \code{\link{cmc_prep}}
#' 
coxwer<-function(fmla, data, var.frame, vartype, automatch=FALSE, pen, signif=0.01, contrasts=c("contr.treatment", "contr.treatment"), scalepredictors=c("no","center","scale","normalize"),silent=FALSE, restr=NULL, adjfile=NULL,...){
#means that variable type is automatically detected from var.type
    
#FIXME: Implement the correction as suggested by Stat Med. 1991 May;10(5):697-709.The effects of transformations and preliminary tests for non-linearity in regression. Grambsch PM, O'Brien PC
#FIXME: Use association within blocks rather than regression and inverse regression within block?  
#FIXME: Overall model fit index
#FIXME: Add Survreg and beta etc.  
#FIXME: add AIC instead of p vals?
#FIXME: output such that we have CIs attached
#FIXME: no $ reference but [[""]]
#FIXME: perhaps use a bigger pval as a false positive is less of a problem if we later do stepAIC
#FIXME: Add variable selection by L1 regularization next to stepAIC
#FIXME: Make var.frame$type such that match.arg also works with factors
#FIXME: Make var.frame$type such that automatic conversion to character 
#FIXME check Kathis patch wegen restrictions (or better yet, make it proper)
  
#require(MASS)
#require(nnet)
#stopifnot(require(MASS))

findtype <- function(name,data)
    {
    #crude matching of variable type based on the appearance in the data frame
        #unordered factor or character vector with > 2levels -> categorical
        #unordered factor or charcter vector with 2 levels -> binary
        #ordered factor -> ordinal
        #else metric
    vari <- data[[name]]
    type <- "metric"
    if(is.character(vari) & length(unique(vari))>2) type <- "categorical"
    if(is.character(vari) & length(unique(vari))<3) type <- "binary"
    if(is.factor(vari) & nlevels(vari)>2) type <- "categorical"
    if(is.factor(vari) & nlevels(vari)<3) type <- "binary"
    if(is.ordered(vari)) type <- "ordinal"
#    if(is.integer(vari) & !any(vari)<1) type <- "odcount"
    type           
    }
    
parse_cwformula <- function(fmla,data)
    {
     #parse the formula object that must look like
     #block1var1 + block1var2 + ... ~  block2var1 + block2var2 + ... ~ block3var1 + block3var2 ...
     #into a var.frame data.frame   
     f1 <- Reduce(paste, deparse(fmla))
     tmp1 <- unlist(strsplit(f1,"~",fixed=TRUE))
     nrblocks <-1:length(tmp1)
     tmp1 <- gsub(" ","",tmp1)
     tmp2 <- strsplit(tmp1,"+",fixed=TRUE)
     varnames <- unlist(tmp2) 
     blcklength <- unlist(lapply(tmp2,length))
     blocks <- rep(nrblocks,times=blcklength)
     #automatically matches the type
     vartype <- sapply(varnames,function(x) findtype(x,data))
     var.frame <- data.frame(type=vartype,block=blocks,stringsAsFactors=FALSE)
     rownames(var.frame) <- varnames
     var.frame
    }

if(missing(var.frame) && missing(fmla)) stop("One of 'fmla' or 'var.frame' must be given.") 

#If formula is given but no var.frame then parse formula; if vartype is given use that too 
if(!missing(fmla) && missing(var.frame))
    {
        var.frame <- parse_cwformula(fmla,data)
        #If vartype is given override the autodetect
        if(!missing(vartype)) var.frame$type <- vartype
    }

#If var.frame is given and vartype is given override the type variable in var.frame
if(!missing(var.frame) && !missing(vartype)) var.frame$type <- vartype

#Else if var.frame is given and no vartype use only varframe

var.frame <- na.omit(var.frame)
vf <- var.frame
nblocks <- max(var.frame$block)
data<-data[,rownames(var.frame)]

var.frame$type <- unlist(lapply(as.character(var.frame$type), function(i) match.arg(i, c("binary", "categorical", "continuous", "count", "ordinal","odcount","gamma","invgaussian","factor","metric"))))

if (isTRUE(automatch)) { 
facs<-grep(c("binary|categorical|factor"),var.frame$type)    
for (i in facs) data[,i]<-factor(data[,i])
ords<-grep(c("ordinal"),var.frame$type) 
for (i in ords) data[,i]<-factor(data[,i],ordered=TRUE)
}

#Scaling predictors
if (missing(scalepredictors)) scalepredictors <- "no"

if(!isTRUE("no"%in%scalepredictors))
    {
     metri<-unlist(lapply(data, !is.factor))

     normalize <- function(data){
        return (apply(data, function(x) (x-min(x))/(max(x)-min(x))))
     }

#FIXME: check this     
data[,metri] <- switch(scalepredictors,
         no = data ,
         center= scale(data[,metri]),
         scale = scale(data[,metri]),
         norm = normalize(data[,metri])
)
}

# adjacency matrix
k <- sum(var.frame$block>0)
A <- matrix(0, k, k)
vars<-rownames(var.frame[var.frame$block>0,])
dimnames(A) <- list(vars, vars)

modList <- NULL

#FIXME: insert some sanity checks
#Check whether any variable is not specified as a family type for metric variables 
if(any(c("metric","continuous") %in% var.frame[["type"]]) && !isTRUE(silent)) warning(paste("Some models for continuous/metric variables are not further specified. Ordinary least squares estimation is used by default. You can specify a different model type with the 'vartype' argument."))

# initially increment working block numbers
var.frame$wblock <- var.frame$block + 1

#set contrasts
oldcont <- options()$contrasts
options(contrasts = contrasts)

#### loop blocks
for (blocks in 1:nblocks){

     var.frame$wblock <- var.frame$wblock - 1
     curr.targ.set <- rownames(var.frame)[var.frame$wblock==1]

#### loop targets
     for (curr.target.name in curr.targ.set){

          if (length(curr.targ.set)==1 && blocks==nblocks) break  # stop if only 1 purely explanatory var

          # specification of model family and test statistics according to target type

          curr.type <- var.frame[curr.target.name,"type"]

          if(curr.type=="continuous" | curr.type=="metric" )
            {
              #warning(paste("Model for continuous/metric variable",curr.target.name,"is not specified. Ordinary least squares estimation is used by default."))
              curr.type <- "gaussian"
            }          

          if (curr.type == "binary"){
              curr.mod <- "binomial logit"
              currfamily <- "binomial"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "count"){
              curr.mod <- "poisson loglinear"
              currfamily <- "poisson"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "gamma"){
              curr.mod <- "gamma generalized linear"
              currfamily <- "Gamma"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "invgaussian"){
              curr.mod <- "inverse gaussian generalized linear"
              currfamily <- "Gamma"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "gaussian"){
              curr.mod <- "ordinary least squares"
              currfamily <- "gaussian"
              curr.test <- "F"
              curr.p <- "Pr(F)"
          } else if (curr.type == "categorical"){ 
              curr.mod <- "multinomial logit"
              currfamily <- "multinomial logit"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "ordinal"){    
              curr.mod <- "proportional odds logit"
              currfamily <- "proportional odds logit"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          } else if (curr.type == "odcount"){
              curr.mod <- "negative binomial"
              currfamily <- "negbin"
              curr.test <- "Chisq"
              curr.p <- "Pr(Chisq)"
          }
          
          if(!silent) {
            cat("TARGET: ",curr.target.name," (",curr.mod," model)","\n",sep="")
            flush.console()
          }
          
          y<-data[,curr.target.name] # current dependent var

          ### CHANGE KG: restriction matrix to surpress specific dependencies to be fitted in the algorithmn
          # current predictors in dat
          # remove predictors not wanted due to restrMat
          if(is.null(restr))
             curr.pred<-var.frame$wblock > 0 ### 0
          else
             curr.pred<-na.omit(colnames(restr)[(restr[,curr.target.name]>0) & (var.frame$wblock > 0)]  )

          dat<-data[,curr.pred, drop=FALSE]
          dat<-dat[,-grep(curr.target.name,names(dat)),drop=FALSE]

          if(missing(pen)) pen <- log(nrow(dat))  
          ## current model selection

          # set up model formula for linear terms
          f<-"y~."

          # set up model formula for quadratic terms
          f.qall <- NULL

          # logical vector with TRUE for factors
          isfac<-unlist(lapply(dat, is.factor))
         
          # logical vector with TRUE for continuous variables
          ismetr<-!isfac
          # construct formula for quadratic terms for continuous predictors
          #RH used the construction var1+poly(var1,2)[,2] Instead of poly(var1,2)[,1]+poly(var1,2)[,2] (full orthogonal) or var1+I(var1) (non orthogonal). I think that it is quite clever. poly(x,2)[,2] is orthogonal to the degree 0, so it's estimation is independent of the reference; as we want to get rid of it orthoganility there is welcome. But we don't want x to be orthogonal to the baseline, so no use of poly(x,2)[,2] as well.   
          if (any(ismetr)){
            metr.varnames <- paste("I(poly(",names(dat)[ismetr],",2)[,2])",sep="")
            f.qmetr <- paste("+",paste(metr.varnames,sep="",collapse="+"),sep="")
          } else {
              f.qmetr <- ""
          }

          #FIXME: Here RH wanted to calcuate quadratic effects for ordered factors; DOES NOT WORK! Possible remedy: orth poly contrasts and use the quadratic from there; I don't think that is clever. Current solution: Default treats them as dummies. The user can supply contr.poly if she wants, but there is no variable selection for the higher order polynomials. I included this because it may help with model checking for the user, but I'd avoid using these contrasts.     
          
          f.qord <- ""

          f.qall <- paste(f.qmetr,f.qord,sep="")  

          # fit maineffects model
          mod <- fitfunc(formula(f),data=dat,type=curr.type,currfamily=currfamily)

          # check for non-linearities
          if (f.qall != ""){
            qtframe<-addterm(mod,scope=formula(paste("~.",f.qall,collapse="")),test=curr.test,data=dat) 
            qterms<-rownames(qtframe[qtframe[[curr.p]]<signif,])[-1] # first term is NA
            qterms<-gsub("\\[, 2\\]","",qterms)# replace significant 2nd-order effects with whole polynomial
          } else {
            qterms <- NULL
          }

          # formula for quadratic terms in model after step 1
          if (length(qterms)>0) {
            f.qt <- paste("+",paste(qterms,sep="",collapse="+"),sep="")
          } else {
            f.qt <- ""   
          }

          # check for interactions
          if (length(attr(mod$terms,"term.labels")) < 2){
             iterms <- NULL # no interactions for less than 2 predictors
          } else {
             iframe<-addterm(mod,scope=~.^2,test=curr.test) 
             # remove terms with df=0
             iframe<-iframe[iframe[["Df"]]>0,]
             iterms<-rownames(iframe[iframe[[curr.p]]<signif,])[-1] # first term is NA
          }

          # formula for interaction terms in model after step 1
          if (length(iterms)>0) {
            f.it <- paste("+",paste(iterms,sep="",collapse="+"),sep="")
          } else {
            f.it <- ""   
          }

          f.step2 <- formula(paste(f, f.qt, f.it))
            
          model <- fitfunc(f.step2,data=dat,type=curr.type,currfamily=currfamily)

          resback<-stepAIC(model, k=pen,trace=0)#, trace=AICtrace)

          ## re-enter interactions for remaining variables
          rbterms<-attr(resback$terms,"term.labels")   
          rborder<-attr(resback$terms,"order")   
          rbterms<-rbterms[rborder==1]


          rbterms <- gsub("I\\(poly\\(","",rbterms)
          rbterms <- gsub(",.*","",rbterms)
          rbterms <- unique(rbterms)

          if (length(rbterms)>0){
              f.ia <- paste("~ . +(",paste(rbterms,sep="",collapse="+"),")^2",sep="")
          } else {
              f.ia <- "~ ." 
          }
          
          ## check for remaining interactions
          mod2 <- update(resback,f.ia)
          mod3<-stepAIC(mod2, k=pen, trace=0)# trace=AICtrace)

          ## re-enter quadratic terms for remaining variables
          m3terms<-attr(mod3$terms,"term.labels")
          m3order<-attr(mod3$terms,"order")
          m3terms<-m3terms[m3order==1]

          # remove already defined quadratic terms
          rem.qt <- grep("^I",m3terms)
          if (length(rem.qt)>0) m3terms<-m3terms[-rem.qt]
          # remove factors from m3terms
          m3facs<-sapply(data[m3terms],is.factor)
          m3terms<-m3terms[!m3facs]


          ## check for remaining quadratic terms

          if (length(m3terms)>0){
              m4qt <- paste("I(poly(",m3terms,",2)[,2])",sep="")
              f.m4qt <-  paste("~ . +(",paste(m4qt,sep="",collapse="+"),")",sep="")
          } else {
              f.m4qt <- "~ ."
          }

          mod4 <- update(mod3, f.m4qt)
          ## modfinal is final model for current target
          modfinal<-stepAIC(mod4, k=pen,trace=0 )#, trace=AICtrace)

          # output list
          modListElement <- list(list(model=modfinal)) #FIXME: TR add summary() instead of print()?
          names(modListElement)<-curr.target.name
          modList<-c(modList,modListElement)

          ## extract final model terms 
          mfterms <- attr(modfinal$terms,"term.labels")
          mforder <- attr(modfinal$terms,"order")  
          mfterms<-mfterms[mforder==1]

          mfterms <- gsub("I\\(poly\\(","",mfterms)
          mfterms <- gsub(",.*","",mfterms)
          mfterms <- unique(mfterms)

          ## entry into adjacency matrix
          A[mfterms,curr.target.name] <- 1
     }
}
if (!is.null(adjfile)) write.table(A, file=adjfile)
cwobj <- list(modList=modList, vframe=vf, AdjMat=A)
class(cwobj)<-"cw"
#options(contrasts = oldcont)
cwobj
}

######### Methods 

#' S3 print method (prints adjacency matrix)
#' @param x object of class cw
#' @param ... Further arguments (no effect)
#' @export
print.cw<-function(x,...){
  A<-x$AdjMat
  nums<-sprintf("%2i", 1:nrow(A))
  rownames(A) <- paste(nums,rownames(A))
  colnames(A) <- nums#1:nrow(A)
  cat("Adjacency Matrix:\n\n")
  print(A)
}

#' Function to extract the adjacency matrix form a cw object
#' @param x object of class cw
#' 
#' @export
adjmatrix<-function(x){
  A<-x$A
  nums<-sprintf("%2i", 1:nrow(A))
  rownames(A) <- paste(nums,rownames(A))
  colnames(A) <- nums#1:nrow(A)
  A
}

#' S3 summary method
#' @param object a cw object
#' @param target summary for which target variable, "all" gives all models? Defaults to NULL which gives nothing models
#' @param vframe flags whether variable frame should be summarized, defaults to FALSE
#' @param adj flags whether adjacency matrix should be summarized, defaults to FALSE
#' @param ... further args
#' @export
summary.cw <- function(object, target=NULL, vframe=FALSE, adj=FALSE, ...){  
  if(!is.null(target)){
    if(target=="all") target <- rownames(object$vframe)
    for (i in target){
      if (is.numeric(i)) var<-names(object$modList[i]) else var<-i
      cat("---------- Summary for dependent variable:", var, "----------\n")
    #  if (inherits(obj$modList[[i]]$model,"multinom")) {    #CHANGE: TR added multinom
    #    print(summary(obj$modList[[i]]$model))
    #  } else {
      print(summary(object$modList[[i]]$model,Wald.ratios=TRUE))
    #}
    }
  }
  if(vframe){
      cat("---------- Variable Frame --------------------------\n\n")
      print(object$vframe)
  }
  if(adj){
      print(object)
  }
}


#' S3 predict method
#' @param object a cw object
#' @param target Which target variable to predict? Defaults to NULL which predicts all
#' @param newdata new data frame 
#' @param type what type of predictions are wanted (if missing it is "")
#' @param silent Should info be printed (FALSE means yes)
#' @param ... additional arguents passed to predict for the model classes 
predict.cw <- function(object, target, newdata, type , silent=FALSE, ...){
  if (missing(target)) target <- seq(1,length(object$modList),by=1)
  if (is.numeric(target)) target <- unlist(lapply(target,function(i) names(object$modList[i])))
  out <- list()
  if (missing(type)) type <- character(length(target))
    for (i in target) {
    if(!silent)  cat("---------- Predicting dependent variable:", i, "----------\n")
      if(isTRUE(all.equal(type[grep(i,target,fixed=TRUE)],""))) {
        if (inherits(object$modList[[i]]$model,"glm")) {
          type[grep(i,target,fixed=TRUE)] <- "response"
          } else {
                  type[grep(i,target,fixed=TRUE)] <- "class"
                 }
      }
      if(missing(newdata)) {
        out[[i]] <- predict(object$modList[[i]]$model,type=type[grep(i,target,fixed=TRUE)]) 
      } else { 
        out[[i]] <- predict(object$modList[[i]]$model,newdata=newdata,type=type[grep(i,target,fixed=TRUE)])
      }
    }
  out
}

#' Function to interactively prepare a variable frame for coxwer
#' @param data a data frame
#' @return a variable frame
#' @export
prep_coxwer <- function(data){
   # generate variable description
   var.frame<-data.frame(type=rep("cont",ncol(data)),block=rep(NA,ncol(data)),stringsAsFactors = FALSE)
   var.frame$block<-as.numeric(var.frame$block)
   rownames(var.frame) <- names(data)
   var.frame<-fix(var.frame)
   var.frame
}

#'S3 plot method;  Interactively plot the coxwer object 
#' @param x an object of class cw
#' @param ... further arguments to tkplot
#'
#' @import igraph 
#' @export
#interactive plot
plot.cw<-function(x,...){
   AA<-x$A
   g<-igraph::graph.adjacency(AA)
   # labels fuer Punkte
   # vnames<-substr(colnames(AA),2,999) # Variablennamen ohne "p"  #CHANGE: comment 
   vnames<-colnames(AA)                
   V(g)$name<-vnames                  # einfuegen der labels in die grafik definition
   igraph::tkplot(g,vertex.label=V(g)$name,...)   # plot der grafik g mit labels
}

#' Write the coxwer adjacency matrix plot to a file
#' @param obj an object of class cw
#' @param file filename
#' @param ... further arguments to write.graph
#' @import igraph
#' 
#' @export
#' 
#interactive plot
write_cw<-function(obj,file,...){
   AA<-obj$A
   g<-igraph::graph.adjacency(AA)
   # labels fuer Punkte
   vnames<-colnames(AA)                
   V(g)$name<-vnames                  # einfuegen der labels in die grafik definition
   igraph::write.graph(g,file,"pajek",...) # plot der grafik g mit labels
}

#FIXME: non interactive plot
#leverage the igraph facilities


