rm( list=ls() )
load( '../Rdata/setup.Rdata' )
load( 'Rdata/out.Rdata' )
x			<- log10( Ns )

meths	<- c( 'mfmr_fast'	, 'mvgmmq', 'cca-Y')#, 'oracle, 'allgmmq'		
leg		<- c( 'MFMR'			, 'GMM'		, 'CCA'	 )#, 'Oracle, 'GMM+'			
cols	<- c( 1						, 2				, 4			 )#, 3			, 'rosybrown1'
names(cols)	<- meths

print(cbind( nruns[meths,,1],nruns[meths,,2]))

pdf( '~/figs/mfmr/SFig7a.pdf', width=23, height=5.9 )
layout( cbind( 1, 2+rbind( 1:4, 9:12, 5:8 ), 2 ), widths=c( 1.9, rep( 6, 4 ), 3.9 ), heights=c( 1.0, 5.3, 1.8 ) )

plot.new() ##### left

##### right
par( mar=c( 10, 0, 10, 0 ) )
plot.new()
legend( 'center', fill=cols, leg=leg, cex=4, border=F, bty='n' )#, horiz=T )

##### top
par( mar=rep(0,4) )
for( zzz in 1:4 ){
	plot.new()
	#text( .5, .5, cex=3.1, lab=c( 'Trait -> Tested Covariate', 'Trait -> Untested Covariate', 'Covariate -> Tested Trait', 'Covariate -> Untested Trait' )[zzz] )
	text( .5, .5, cex=3.1, lab=c(
		'Quant. Traits, Null SNPs',
		'Binary Traits, Null SNPs',
		'Quant. Traits, Hom. SNPs',
		'Binary Traits, Hom. SNPs'
	)[zzz] )
}

##### bottom
par( mar=rep(0,4) )
par( mar=c( 1.0, 2, 1.0, 2.0 )/2 )
for( zzz in 1:4 ){
	plot(range(x)+c(-.1,.1),0:1,type='n',axes=F,ylab='',xlab='')
	text( mean(range(x)), .25, cex=3.5, lab='Sample Size (N)' )
	axis( 1, cex.axis=2.4,  at=x, line=-9.9, lab=F )
	axis( 1, cex.axis=2.4,  at=x, line=- 9.7, lab=c( '1,000', '3,000', '10,000', '30,000' ), padj=.9, tick=F )
}

##### meat
par( mar=c( 1.0, 1, 1.0, 1.0 )/2 )
for( zzz in 1:4 ){

	if( zzz == 1 ){
		allys	<- pvals['Null','quant',,,]
	} else if( zzz == 2 ){
		allys	<- pvals['Null','bin',,,]
	} else if( zzz == 3 ){
		allys	<- pvals['Hom','quant',,,]
	} else if( zzz == 4 ){
		allys	<- pvals['Hom','bin',,,]
	}

	ylim	<- c( -2.2, 0 )
	ats	<- c(.01	,.05	,.2		,1)
	ats	<- log10(ats)
	labs<- c('.01','.05','.2'	,'1' )
	allys	<- log10(allys)

	plot( range(x)+c(-.1,.1), ylim, type='n', axes=F, ylab='', main='', xlab='' )
	box()
	legend( 'topleft', bty='n', cex=4, leg=letters[zzz], adj=c(2.5,-.3) )

	abline( a=-2	, b=0, col='lightgrey', lty=3, lwd=8 )
	if( zzz %in% 1 ){
		axis( 2, cex.axis=2.7, padj=-.3	, at=ats			, lab=labs )
		mtext( side=2, line=6.2, cex=2.3 , 'False Positive Rate' )
	}

	for( meth in rev(meths) )
		for( sig2.i in 1:2 )
	try({
		lines(	x, allys[meth,,sig2.i], col=cols[meth], lty=c(1,2)[sig2.i], lwd=ifelse( meth %in% c( 'oracle++' ), 7, 3 ) )
		points(	x, allys[meth,,sig2.i], col=cols[meth], pch=18-sig2.i, cex=6 )
	})

}
dev.off()
