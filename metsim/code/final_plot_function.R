wf	<- function(pvec){
	if( any( is.na( pvec ) ) )
		pvec[is.na( pvec )]	<- Inf
	sapply( pvec, function(xx) paste0( rep( '*', sum( xx < c( .01, .001, .0001 ) ) ), collapse="" ) )
}

final_plot_fxn	<- function( out, xnames, K, cols, left=TRUE, hpvals,
	ylab='',
	renorm=T, resign=F,
	Gnorm=F, G=NULL,
	rpar=ifelse( left, 0, .8 ),
	betas, ses, pvals
 ){ 
 
	if( missing( betas ) ){
		betas	<- t(sapply( out, function(out.i) out.i$coef[paste0('gxe',1:K),'Estimate'] ))
		ses	<- t(sapply( out, function(out.i) out.i$coef[paste0('gxe',1:K),'Std. Error'] ))
		pvals	<-   sapply( out, function(out.i) out.i$pvals )
	}
	pvals_num	<- pvals

	S	<- nrow(betas)
	if( renorm ){
		renorms	<- apply( betas, 1, function(x) round( max( log10( abs(x) ) ) ) )
		cat( 'Renorm:', renorms, '\n' )
		betas	<- betas * matrix( 10^-renorms, S, K, byrow=F )
		ses	<- ses * matrix( 10^-renorms, S, K, byrow=F )
	} else if( Gnorm ){
		renorms	<- apply( G, 2, function(g.s) sd(g.s) )
		cat( 'Renorm:', renorms, '\n' )
		betas	<- betas * matrix( renorms, S, K, byrow=F )
		ses	<- ses * matrix( renorms, S, K, byrow=F )
	}
	#ymax	<- max(abs(betas))*1.8
	ymax	<- max(abs(betas),na.rm=T)*1.8
	if( resign )
		betas	<- t(apply( betas, 1, function(x) x*sign(x[1]) ))
	pvals	<- apply( pvals, 1:2, function(x) sprintf( '%5.3f', x ) )
	if( any( pvals == '0.000' ) )
		pvals[ pvals == '0.000' ]	<- '<.001'

	#ymax	<- max(abs(c(betas + 2*ses, betas - 2*ses )))
	lpar	<- ifelse( left, 5, 2 )

	#ylab	<- ifelse( left, 'Asthma Subtype\nLog Odds Ratios', '' )
	par( mar=c(0,lpar,1,1) )
	plot( c( 2.5, S*3+.5 ), ymax*c(-1,1), type='n', axes=F, xlab='', ylab=ylab, cex.lab=1.4, xlim=c( 2.5, S*3+.5 ) )
	box()
	abline( h=0, lty=2, col='lightgrey', lwd=1.5 )
	axis(2,cex.axis=.9)

	for( s in 1:S )
		for( k in 1:K )
	{
		offset	<- seq(-.5,.5,len=K)[k]
		points( s*3+offset		, betas[s,k]										, col=cols[k],pch=16, cex=2 )
		lines( rep( s*3+offset, 2 )	, betas[s,k]+c(-2,2)*ses[s,k]		, col=cols[k], lwd=1, lty=1 )
		lines( s*3+offset+c(-.1,.1) , betas[s,k]+rep(2,2)*ses[s,k]	, col=cols[k], lwd=1, lty=1 )
		lines( s*3+offset+c(-.1,.1) , betas[s,k]+rep(-2,2)*ses[s,k]	, col=cols[k], lwd=1, lty=1 )
	}

	lpar	<- ifelse( left, 4.5, 1.8 )

	par( mar=c(0,lpar,1,rpar) )
	plot( c( 2.5, S*3+.5 ), 0:1, type='n', axes=F, xlab='', ylab='' )

	if( left ){
		mtext( side=2, las=2, at=.99, line=.5						, cex=1.11, 'Hom. p' )
		mtext( side=2, las=2, at=.82, line=.5						, cex=1.11, 'Het. p' )
		mtext( side=2, las=2, at=.65, line=.5						, cex=1.11, 'Global p' )
	}
	#mtext( side=2, las=2, at=.77, line=.5, cex=1.50, 'Het. p\n(K=3)' )
	#text( 3*1:S, rep(.8,S), cex=1.21, lab=sapply( pvals, function(x) sprintf( '%5.3f', x ) )  )
	#text( 3*1:S, rep(.2,S), cex=1.31, lab=sapply(xnames,function(x) sprintf('%18s',x) ), srt=90 )

	#text( 3*1:S, rep(.99,S), lab=pvals[1,]	, cex=1.15  )
	#text( 3*1:S, rep(.82,S), lab=pvals[2,]	, cex=1.15  )
	#text( 3*1:S, rep(.65,S), lab=pvals[3,]	, cex=1.15  )
	#text( 3*1:S, rep(.14,S), cex=1.31, lab=sapply(xnames,function(x) sprintf('%18s',x) ), srt=90 )

	text( 3*1:S, rep(.99,S), lab=wf( pvals_num[1,])	, cex=1.85  )
	text( 3*1:S, rep(.82,S), lab=wf( pvals_num[2,])	, cex=1.85  )
	text( 3*1:S, rep(.65,S), lab=wf( pvals_num[3,])	, cex=1.85  )
	text( 3*1:S, rep(.14,S), cex=1.31, lab=sapply(xnames,function(x) sprintf('%18s',x) ), srt=90 )

}


mylines	<- function(x,y,col,points=F,trunc=T,pt.cex=2.0){
	if( trunc ){
	pchs	<- sapply( y, function(y.i) ifelse( y.i <= 0 | y.i > 1, 17, 16 ) )
	y			<- sapply( y, function(y.i) ifelse( y.i < 0, 0, y.i ) )
	y			<- sapply( y, function(y.i) ifelse( y.i > 1, 1, y.i ) )
	} else {
	pchs	<- sapply( y, function(y.i) 16 )
	}
	if( points ){
		points( x	 , y, col=col, lwd=1, lty=1, pch=pchs, cex=pt.cex )
	} else {
		lines( x	 , y, col=col, lwd=1, lty=1 )
	}
}
