## ----echo=FALSE----------------------------------------------------------
library(gRchain)
data(cmc_prep)

## ------------------------------------------------------------------------
cmc_prep

## ----eval=FALSE----------------------------------------------------------
#  cmc_prep <- prep_coxwer(cmc)

## ----eval=FALSE----------------------------------------------------------
#  data(cmc)
#  rescmc <- coxwer(var.frame=cmc_prep, data=cmc)

## ----eval=FALSE----------------------------------------------------------
#  data(cmc)
#  rescmc <- coxwer(contraceptive + nrChild ~			    #Block 1
#                   mediaExp ~    	 	    			    #Block 2
#                   solIndex ~ 		    			    #Block 3
#                   wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ #Block 4
#                   age, 	   	      	      		  	    #Block 5
#                   data=cmc)

## ----warning=FALSE-------------------------------------------------------
data(cmc)
rescmc <- coxwer(contraceptive + nrChild ~ mediaExp ~ solIndex ~ 
                 wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ age, 
                 data=cmc)

## ------------------------------------------------------------------------
print(rescmc) 

## ------------------------------------------------------------------------
summary(rescmc,target=c("contraceptive","nrChild"))

## ----warning=FALSE-------------------------------------------------------
data(cmc)
rescmc <- coxwer(contraceptive + nrChild ~ mediaExp ~ solIndex ~ 
                 wifeRel + wifeWork + husbOcc + wifeEdu + husbEdu ~ age, 
                 vartype=c("cate","count","bin","ord","bin","bin",
                           "ord","ord","ord","metric"), 
                 data=cmc)

## ------------------------------------------------------------------------
summary(rescmc,target=c("contraceptive","nrChild"))

## ----eval=FALSE----------------------------------------------------------
#  plot(rescmc)

