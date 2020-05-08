computeBIC <- function(X, y, muh, betah, k, c = 10) {
	s <- sum(abs(betah) > 0)
	n <- dim(X)[1]
	p <- dim(X)[2]
	Qn <- sum((y - muh - X %*% betah) ^ 2) / n
	df <- k + p
	
	bic <- log(Qn) + c * log(log(n + p)) * log(n) / n * df
	
	return(bic)
}
