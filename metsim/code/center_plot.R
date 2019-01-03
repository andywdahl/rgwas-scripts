library(Hmisc)
scale01	<- function(X)
	apply( X, 2, function(x)
{
	x	<- x - min(x,na.rm=T)
	x	<- x / max(x,na.rm=T)
	x
})

center_plot	<- function( quantvars, binvars, pmat,
	col.phen=1,
	leg=1:ncol(pmat),
	cols=1:ncol(pmat),
	qcol=1,
	bcol=1,
	...
){
	layout( rbind( 1, 2:4 ), widths=c(5,1.8,3.3) )

	P		<- ncol(quantvars)
	quantvars	<- scale( quantvars )

	centroids_q	<- t(sapply( 1:K, function(k) colMeans(
		( pmat[,k] %o% rep( 1, ncol(quantvars) ) ) * quantvars/mean(pmat[,k]),
	na.rm=T )))
	centroids_b	<- t(sapply( 1:K, function(k) colMeans(
		( pmat[,k] %o% rep( 1, ncol(binvars) ) ) * binvars/mean(pmat[,k]),
	na.rm=T )))*100
	colnames(centroids_b)	<- NULL
	rownames(centroids_b)	<- NULL

	if( K == 3 ){
	} else {
		cols	<- 1+1:K
	}

	######
	par( mar=c( 10, 10, 3, 1 ) )
	plot( c(1-.2,P+.2), c(-2.3,2.7), type='n', ylab='', xlab='', main='', axes=F )
	mtext( side=2, line=4.2, cex=2.1 , 'Cluster Means\nand 90% IQR' )
	abline(h=0,col='lightgrey',lwd=4,lty=3)
	axis(2,cex.axis=1.5)
	mtext( side=1, at=1:P, text=colnames(quantvars), line=5.0, cex=2.1, col=qcol )
	for( k in 1:K )
		for( p in 1:P )
	try({
		x0	<- seq(-1,1,len=K)[k] * ifelse( K == 2, .15, .25 ) + p
		qs	<- wtd.quantile( quantvars[,p], weights=pmat[,k], probs=c(.1,.9), na.rm=T )
		points( x0		, centroids_q[k,p]	, col=cols[k], pch=16, cex=7.1 )
		lines(	rep( x0, 2 )	, qs				, col=cols[k], lwd=2.9, lty=1 )
		lines(	x0+c(-.05,.05), qs[c(1,1)], col=cols[k], lwd=2.9, lty=1 )
		lines(	x0+c(-.05,.05), qs[c(2,2)], col=cols[k], lwd=2.9, lty=1 )
	})
	mtext( at=.8, line=-2.5, letters[1], cex=5 )

	######
	if( missing( binvars ) ) next
	par( mar=c( 8, 10, 1, 1 ) )
	mp	<- barplot( centroids_b, beside=T, col=cols, main='', xlab='', ylab='', axes=F, ylim=c(0,102) )
	mtext( side=2, line=4.2, cex=2.1 , 'Cluster\nPrevalences (%)' )
	axis(2,at=0:5/5*100, cex.axis=1.5)
	ats	<- colMeans( matrix( mp, K, ncol(binvars) ) )
	mtext( side=1, at=ats, text=colnames(binvars), line=5.0, cex=2.1, col=bcol )
	abline(h=0,col='lightgrey',lwd=4,lty=3)
	abline(h=100,col='lightgrey',lwd=4,lty=3)
	abline(h=50,col='lightgrey',lwd=4,lty=3)
	mtext( at=1.9, line=-5, letters[2], cex=5 )
	
	######
	par( mar=rep(1,4) )
	pie( colMeans( pmat ),col=cols,lab='',cex=.7)
	mtext( at=-.8, line=-5, letters[3], cex=5 )

	######
	par( mar=rep(0,4) )
	plot.new()
	#legend( 'center', bty='n', fill=c('grey67',cols), c( 'Homogeneous', 'High T2D, High CHD', 'High T2D, Low CHD', 'Low T2D, Low CHD' ), cex=4.5 )
	legend( 'center', bty='n', col=c(bcol[1],bcol[length(bcol)],NA,cols), c( 'Covariates', 'Traits', '', 'High T2D, High CHD', 'High T2D, Low CHD', 'Low T2D, Low CHD' ), cex=4.5, pch=c(16,16,NA,16,16,16), pt.cex=rep(6,6) )

}
