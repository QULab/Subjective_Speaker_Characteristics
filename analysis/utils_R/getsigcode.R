
## returns significance code given the p-value

getsigcode <- function(pval){
  
  
  if (pval<1) sigcode<-""
  if (pval<0.1) sigcode<-"."
  if (pval<0.05) sigcode<-"*"
  if (pval<0.01) sigcode<-"**"
  if (pval<0.001) sigcode<-"***"
  if (pval<0.0001) sigcode<-"****"
  
  return(sigcode)
  
}