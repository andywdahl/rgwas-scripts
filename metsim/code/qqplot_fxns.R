panel.qqconf<-function(n, conf.points=n,conf.alpha=.05 ) {
	mpts<-matrix(nrow=conf.points*2, ncol=2)
	for(i in seq(from=1, to=conf.points)) {
		mpts[i,1]<- -log10((i-.5)/n)
		mpts[i,2]<- -log10(qbeta(1-conf.alpha/2, i, n-i))
		mpts[conf.points*2+1-i,1]<- -log10((i-.5)/n)
		mpts[conf.points*2+1-i,2]<- -log10(qbeta(conf.alpha/2, i, n-i))
	}
	lines(x=mpts[,1],y=mpts[,2])
}


qqplot_fxn	<- function( y, lims=range(y,na.rm=T), main='', add.ylab=F ){

	plot( lims, lims, type='n', main=main, xlab='', ylab='', cex.axis=1.3, cex.main=3.5 )

	mtext( side=1, line=4.1, cex=1.3, expression( -log[10](p[null]) ) )
	if( add.ylab )
	mtext( side=2, line=4.1, cex=1.3, expression( -log[10](p[obs]) ) )


	abline( a=0, b=1, col='lightgrey', lty=3 )

	qqline_fxn(y,col='grey',pts=F)
	panel.qqconf(n=length(y))

}

qqline_fxn	<- function(y,col,pts=T,ymax=8,x=NULL){
	ny	<- length( y )
	if( is.null(x) )
		x		<- sort(-log10( 1:ny/(1+ny) ))

	if( !pts ){
		y		<- y[ x > .3 ]
		x		<- x[ x > .3 ]
	}

	pchs	<- rep( 16, length(y) )
	if( any( y > ymax ) ){
		pchs[y>ymax]	<- 15
		y		[y>ymax]	<- ymax
	}

	if( pts ){
		points(	x, y , col=col, pch=16, cex=2.5 )
		lines(	x, y , col=col, lwd=3 )
	} else {
		points( x, y , col=col, pch=16, cex=.7 )
	}
}

count_signif	<- function( X, tol=-2, nreturn=5, trans=F ){
	#if( trans ) X	<- t(X)
	#print( dim( X ) )
	#x	<- rowSums( X > tol )
	#sort( x, dec=T )[1:nreturn]
	#x
	rowSums( X > tol )
}
