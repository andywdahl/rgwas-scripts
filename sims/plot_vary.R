rm( list=ls() )

meths	= c( 'mfmr_fast', 'mvgmmq', 'cca-Y'	, 'oracle')
cols	= c(  1					, 2				,  4		 	,	3				)
leg		= c( 'MFMR'			, 'GMM'		, 'CCA'		,	'Oracle')

dirs	<- c( 'vary_K', 'vary_Pbin', 'vary_sig2E', 'vary_sig2hom', 'vary_sig2het', 'ge_corr'  )
dirnames	<- expression( 'K', 'B', h[z]^2, h[hom]^2, h[het]^2, rho[GE] )
names( dirnames	)	<- dirs

ge_corrs<- seq( .05, .95, len=7 )
sig2Es	<- c( 0, .02, .04, .07, .1, .15, .2 )
sig2hets<- c( 0, .0004, .002, .004, .02, .04, .2 )
sig2homs<- c( 0, .0004, .002, .004, .02, .04, .2 )

locxticks	<- list(
	1:4,
	c( 1, 3, 5, 10, 20 ),
	c( 0, .01, .03, .1, .3 ),
	c( 0, .01, .03, .1, .3 ),
	c( 0, .01, .03, .1, .3 ),
	ge_corrs
)
names( locxticks )	<- dirs


plotfxn	<- function( ys, ylim=0:1, xnames, ylab, ats, labs, ytext=.95 ){
	
		x	<- 1:(dim(ys)[2])
	
		plot( range(x), ylim, type='n', axes=F, ylab='', main='', xlab='' )
		text( sum(c(.975,.025)*range(x)), ytext, lab=letters[6*ifelse(zzz=='Het',0,1) + which(dir==dirs)], cex=3 )

			axis( 1, cex.axis=1.4, padj=.5, at=x      		, lab=locxticks[[dir]] )
			mtext( side=1, line=6.0, cex=1.9 , dirnames[[dir]] )
		if( dir %in% dirs[c(1,4)] ){
			axis( 2, cex.axis=1.2, padj=-.3	, at=ats			, lab=labs )
			mtext( side=2, line=3.8, cex=1.4 , ylab )
		}

		for( meth in rev(meths) )
			for( sig2.i in 1:2  )
		{
			if( zzz != 'Het' ){
				ys1	<- log10(ys[meth,,sig2.i])
			} else {
				ys1	<- ys[meth,,sig2.i]
			}
			lines(	x, ys1, col=cols[which( meths==meth )], lty=sig2.i, lwd=ifelse( meth %in% c( 'oracle' ), 5, 3 ) )
			points(	x, ys1, col=cols[which( meths==meth )], pch=16, cex=3 )
		}
}

pdf( paste0( '~/figs/mfmr/SFig4.pdf' ), width=14, height=6 )
layout( cbind( 10, matrix(c(1:3,7:9,4:6),3,3,byrow=T) ), widths=c(1.2,6,6,6), heights=c(6,0.2,6) )

for( zzz in c( 'Het', 'Hom', 'Null' ) ){ #, 'K=1'

par( mar=c(8,.5,.5,3.5) )
for( dir in dirs ){

	load( paste0( dir, '/Rdata/out.Rdata' ) )
	if( zzz == 'Het' ){
		ylim	<- 0:1
		ylab	<- 'True Positive Rate'
		ats	<- labs	<- c(0,.25,.5,.75,1)
	} else {
		ylab	<- 'False Positive Rate'
		ylim	<- c(-2.2,0)
		ats	<- c(.01	,.05	,.2		,1)
		labs<- c('.01','.05','.2'	,'1' )
		ats	<- log10(ats)
	}

	plotfxn(pvals[zzz,'quant',meths,,],xnames=allxnames[[dir]], ylim=ylim, ylab=ylab, ats, labs, ytext=ifelse( zzz == 'Het', .95, log10(.75) ) )
	rm( pvals )

}

par( mar=rep(0,4) )
for( ii in 1:4 )
	plot.new()

}
dev.off()
