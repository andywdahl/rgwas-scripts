rm( list=ls() )
load( 'Rdata/setup.Rdata' )
load( 'Rdata/out.Rdata' )
x			<- log10( Ns )

pdf( '~/figs/mfmr/SFig8_bottom.pdf', width=23, height=5.9 )
layout( cbind( rbind(
	c( 1, 3, 4, 5, 2, 6 ),
	c( 7, 14+c(1:3,5,4) ),
	8+c( 1, 3, 4, 5, 2, 6 )
),8), widths=c( 1.7, rep( 6, 3 ), 2.4, 6, 4.8 ), heights=c( 1.0, 5.3, 1.8 ) )

for( xxx in c("med") ){
	print(cbind( nruns[xxx,meths,,1],nruns[xxx,meths,,2]))

##### top
par( mar=rep(0,4) )
plot.new()
plot.new()
for( zzz in 1:4 ){
	plot.new()
	text( .5, .5, cex=4.7, lab=c( 'Null SNPs', 'Hom SNPs (K=1)', 'Hom SNPs', 'Het SNPs' )[zzz] )
}
plot.new() ##### left

##### right
par( mar=c( 10, 0, 10, 0 ) )
plot.new()
legend( 'center', fill=cols, leg=leg, cex=4, border=F, bty='n' )#, horiz=T )

##### bottom
par( mar=rep(0,4) )
plot.new()
plot.new()
par( mar=c( 1.0, 2, 1.0, 2.0 )/2 )
for( zzz in 1:4 ){
	plot(range(x)+c(-.1,.1),0:1,type='n',axes=F,ylab='',xlab='')
	text( mean(range(x)), .25, cex=3.5, lab=expression( sigma[pop]^2 ) )
	axis( 1, cex.axis=2.4,  at=x, line=-9.9, lab=F )
	axis( 1, cex.axis=2.4,  at=x, line=- 9.7, lab=c( '.3%', '1%', '3%', '10%' ), padj=.9, tick=F )
}
##### meat
par( mar=c( 1.0, 1, 1.0, 1.0 )/2 )
	for( zzz in 1:4 )
{
	allys	<- pvals[xxx,zzz,'quant',meths,,]
	if( zzz != 4 ){
		ylim	<- c( -2.2, 0 )
		ats	<- c(.01	,.05	,.2		,1)
		ats	<- log10(ats)
		labs<- c('.01','.05','.2'	,'1' )
		allys	<- log10(allys)
	} else {
		ylim	<- c( 0, 1 )
		ats		<- c(0,.25,.5	,.75,1)
		labs	<- ats
	}

	plot( range(x)+c(-.1,.1), ylim, type='n', axes=F, ylab='', main='', xlab='' )
	box()

	abline( a=-2	, b=0, col='lightgrey', lty=3, lwd=8 )
	if( zzz %in% c(1,4) ){
		axis( 2, cex.axis=2.7, padj=-.3	, at=ats			, lab=labs )
		mtext( side=2, line=6.2, cex=2.3 , ifelse( zzz == 1, 'False Positive Rate', 'True Positive Rate' ) )
	}

	for( meth in rev(meths) )
		for( sig2.i in 1:2 )
	try({
		lines(	x, allys[meth,,sig2.i], col=cols[meth], lty=c(1,2)[sig2.i], lwd=ifelse( meth %in% c( 'oracle++' ), 7, 3 ) )
		points(	x, allys[meth,,sig2.i], col=cols[meth], pch=18-sig2.i, cex=6 )
	})
}
plot.new()

}
dev.off()
