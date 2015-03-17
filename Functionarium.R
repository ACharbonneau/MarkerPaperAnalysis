## This is the functionarium. The functions live here.

ApproxBayesFac <- function(x) {
	x <- x[!is.na(x)]
	max.probs <- max(x)
	new.probs <- x - max.probs
	trans.probs <- exp(new.probs)
	BayesProb <- trans.probs/sum(trans.probs)
	return(BayesProb)
}


#Calculates a proportion of two things (percentage)
CalcProp <- function(numerator, denominator){
	return(length(na.omit(numerator))/length(na.omit(denominator)))
}

#Tell me the standard error!
CalcSE <- function(x){
	x <- x[!is.na(x)]
	sd(x)/sqrt(length(x))
}

SEM <- function(x){
  sd(x)/sqrt(length(x))
}

#Standard Error of a proportion
SEP <- function( x, n ){
	B <- sqrt( ( x * (1-x) ) / (n - 1) )
}

ConditionNumber <- function(model.x=model.object){
	x <- model.matrix(model.x)
    eigen.x <- eigen(t(x)%*%x)
    sqrt(max(eigen.x$values)/min(eigen.x$values))
}

length2 <- function(x){
	x <- x[!is.na(x)]
	length(x)
}

sum2 <- function(x){
	x <- x[!is.na(x)]
	sum(x)
}

mean2 <- function(x){
	x <- x[!is.na(x)]
	mean(x)
}
var2 <- function(x){
	x <- x[!is.na(x)]
	var(x)
}
sd2 <- function(x){
  x <- x[!is.na(x)]
  sd(x)
}

# univariate Rsquared and partial Rsquared calculators

Rsq <- function( model ){
	fitted.variance <- var(model$fitted)
	total.variance	<- var(model$fitted) + var(model$resid)
	fitted.variance / total.variance
}

PRsq <- function( model ){
	residual.variance <- var(model$resid)
	variables <- attr(terms(model), "term.labels")
		model.length <- length(variables)
		variable.name <- rep(NA, model.length )
		partial.Rsq <- rep(NA, model.length )
		univariate.Model.Rsq <- rep(NA, model.length )
			
	for (i in 1:model.length){
		variable.name[i] <- variables[i]
		drop <- parse( text=variables[i] )
		new.formula <- as.formula( paste( ".~.-", variables[i], sep=""))
		new.model <- update(model, new.formula )
		partial.Rsq[i] <- (var(new.model$resid) - residual.variance)/ var(new.model$resid)
		
		new.formula.univariate <- as.formula( paste( ".~", variables[i], sep=""))
		univariate.model <- update(model, new.formula.univariate)
		univariate.Model.Rsq[i] <- summary(univariate.model)$r.sq
		}
	
	R2 <- Rsq( model )
	adj.R2 <- summary(model)$adj.r
	
	partials <- data.frame(partial.Rsq, univariate.Model.Rsq )
	row.names(partials) <- variable.name
	
	list(FullModelRsquared=R2, FullModelAdjustedR2 = adj.R2, partials=partials	)
}