rm( list=ls() )
library(phenix)
load( 'Rdata/out.Rdata' )
load( 'Rdata/setup.Rdata' )

labs	<- c( '1,000', '3,000', '10,000', '30,000' )
x			<- log10(Ns)

meths	<- c( 'mfmr_fast'	, 'mvgmmq', 'cca-Y', 'oracle'	)
cols	<- c(  1					,	2				,	4			 ,	3				)
ltys	<- c(  1					,	1				,	1		 	 ,	1				)
names	<- c( 'MFMR'			, 'GMM'		, 'CCA'	 , 'Oracle' )
names(cols)	<- meths
names(ltys)	<- meths


pdf( paste0( '~/figs/mfmr/final_times.pdf' ), width=8, height=3.6 )
layout( matrix( 1:2, 1, 2 ), width=c(8,2.6) )

par( mar=c(4.5,5,1,1) )

ys	<- apply( log10( times ), 2:3, mean, na.rm=T ) # avg over scenarios and iterations

plot( range(x), range(c(ys[meths,],1.5),na.rm=T), type='n', axes=F, ylab="Runtime (m)", main='', xlab='', cex.lab=1.6 )
box()
axis( 1, cex.axis=1, at=x, lab=labs )
axis( 2, cex.axis=1, at=seq(-2,2), lab=10^seq(-2,2) )
for( xx in seq(-2,2) )
	abline( h=xx, col='lightgrey', lwd=1, lty=3 )
mtext( side=1, line=3, cex=1.4 , 'Sample Size (N)' )

for( meth in rev(meths) )
	lines( x, ys[meth,], col=cols[meth], lty=ltys[meth], lwd=4 )

par( mar=c(3,0,1,0) )
plot.new()
legend( 'center', names, col=cols, lty=ltys, cex=1.3, bty='n', lwd=4 )

dev.off()
