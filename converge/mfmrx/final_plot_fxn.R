final_plot_fxn	<- function( pdfname, pvals, numpvals, betas, ses, S, K, pvec, tols=c(5e-8,.05/5,5e-8), rsids ){

	numpvals<- numpvals	[,c(4,5,1,2,3)]
	pvals		<- pvals		[,c(4,5,1,2,3)]
	betas		<- betas		[c(4,5,1,2,3),]
	ses			<- ses			[c(4,5,1,2,3),]
	rsids		<- rsids		[c(4,5,1,2,3)]

	pdf( pdfname, width=11, height=3.5 )

	layout( cbind( c( 1, 1, 1, 2 ), 3:6 ), widths=c(10,2.1), heights=c(.8,2.9,0.3,2.8) )
	par( mar=c(0,7,0,0) )

	ymax	<- max(abs(c(betas + 2*ses, betas - 2*ses )))*.98
	plot( c( 2.5, S*3+.5 ), ymax*c(-1,.9), type='n', axes=F, xlab='', ylab='Subtype-Specific\nOdds Ratios', cex.lab=1.8 )
	#box()
	mtext( line=-3.1, side=3, at=3, 'c', cex=2.1 )
	abline( h=0, lty=2, col='lightgrey', lwd=1.5 )
	axis(2)

	for( s in 1:S )
		for( k in 1:K )
	{
		offset	<- c(-.3,.3)[k]
		points( s*3+offset					, betas[s,k]										, col=k, pch=16, cex=2 )
		lines( rep( s*3+offset, 2 )	, betas[s,k]+c(-2,2)	*ses[s,k]	, col=k, lwd=1, lty=1 )
		lines( s*3+offset+c(-.1,.1) , betas[s,k]+rep(2,2)	*ses[s,k]	, col=k, lwd=1, lty=1 )
		lines( s*3+offset+c(-.1,.1) , betas[s,k]+rep(-2,2)*ses[s,k]	, col=k, lwd=1, lty=1 )
	}

	par( mar=c(0,7,2,0) )
	plot( c( 2.5, S*3+.5 ), 0:1, type='n', axes=F, xlab='', ylab='' )
	fontfxn	<- function( nump, tol )
		sapply( nump, function(pp) ifelse( pp < tol, 2, 1 ) )
	text( 3*1:S, rep(.975,S), cex=1.35 , lab=pvals['hom',]			, font=fontfxn(numpvals['hom',]			, tols[1] )	)
	text( 3*1:S, rep(.775,S), cex=1.35 , lab=pvals['het',]			, font=fontfxn(numpvals['het',]			, tols[2] )	)
	text( 3*1:S, rep(.575,S), cex=1.35 , lab=pvals['hom+het',]	, font=fontfxn(numpvals['hom+het',]	, tols[3] )	)
	text( 3*1:S, rep(.25,S)	, cex=1.51 , lab=rsids																													)#, srt=90 )

	mtext( side=2, las=2, at=.975, line=.6					, cex=1.10, 'Hom. p' )
	mtext( side=2, las=2, at=.775, line=.6					, cex=1.10, 'Het. p' )
	mtext( side=2, las=2, at=.575, line=.6					, cex=1.10, 'Overall p' )

	par( mar=c(0,1,2,0) )
	plot.new()
	mtext( line=-1.1, side=3, at=.05, 'd', cex=2.1 )

	par( mar=c(0,1,0,0) )
	pie(pvec,col=1:K,lab='')

	plot.new()
	plot.new()
	legend( 'center', bty='n', fill=1:2, leg=c( 'No stress', 'Stress' ), cex=2.0 )

	dev.off()

}
