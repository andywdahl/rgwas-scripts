rm( list=ls() )
library(gxemm)
load( '../Rdata/setup.Rdata' )
K0	<- 3

source( '../code/final_plot_function.R' )

h2s			<- array( NA, dim=c( np, 4 ), dimnames=list( phens, c( 'g', 'iid', 'hom', 'het' ) ) )
h2.ses	<- array( NA, dim=c( np, 4 ), dimnames=list( phens, c( 'g', 'iid', 'hom', 'het' ) ) )
allps		<- array( NA, dim=c( np, 2 ), dimnames=list( phens, c('hom','iid') ) )
times 	<- array( NA, dim=c( 2, np ), dimnames=list( c('hom','iid'), phens ) )
for( pp in sample(np) )
try({

	load( savefile1	<- paste0( 'Rdata/hom/' , phens[pp], '.Rdata' ) )
	times	['hom',pp]	<- runtime_hom
	h2s		[pp,'g']	<- out_hom$h2
	h2.ses[pp,'g']	<- sqrt( out_hom$h2Covmat[1] )
	allps[pp,'hom']	<- pchisq( (h2s[pp,'g']/h2.ses[pp,'g'])^2, df=1, lower.tail=F )

	load( savefile2	<- paste0( 'Rdata/iid/' , phens[pp], '.Rdata' ) )
	times['iid',pp]	<- runtime_het
	h2s		[pp,c('hom','het')]	<- out_iid$h2   
	h2.ses[pp,c('hom','het')]	<- sqrt(diag(out_iid$h2Covmat))


	h2s		[pp,'iid']	<- sum(out_iid$h2)
	h2.ses[pp,'iid']	<- sqrt(sum(out_iid$h2Covmat))

	allps [pp,'iid']	<- pchisq( 2*(out_iid$ll - out_hom$ll), df=1, lower.tail=F )

})

pdf( paste0( '../figs/GxEMM_K_', K0, '.pdf' ), width=15, height=5.5 )
layout( matrix(1:2,2,1,byrow=F), heights=c(6.9,4.5) )

par( mar=c(0,6,.5,0) )

plot( c( 1, np ), c(-.3,1.02), type='n', axes=F, xlab='', ylab='Variance Explained', cex.lab=1.6, bty='n' )
axis(2,cex.axis=.75,at=c(-(1:2)/4,0:4/4),lab=paste0( c( -(1:2)/4, 0:4/4 )*100, '%' ),tick=F)
abline( h=0, lty=1, col=1, lwd=1.5 )
abline( h=1, lty=1, col=1, lwd=1.5 )
for( x in c( -(1:2)/4, 1:3/4 ) )
abline( h=x, lty=3, col='lightgrey', lwd=1.5 )


cols	<- c( 1, 'purple', 'grey', '#FFBB00' )

legend( 'topleft', fill=cols, leg=expression( h[g]^2, h['iid']^2, h['hom']^2, h['het']^2 ), bty='n', cex=1.4, y.inter=1.1, horiz=T )

off	<- seq( -.25, .25, len=4 )
for( pp in 1:np )
	for( j in 1:4 )
{
	mylines( pp						+off[j],h2s[pp,j]												,trunc=F,col=cols[j], points=T,pt.cex=3)
	mylines( rep( pp, 2 )	+off[j],h2s[pp,j]+c(-2,2)  *h2.ses[pp,j],trunc=F,col=cols[j])
	mylines( pp+c(-.1,.1)	+off[j],h2s[pp,j]+rep(2,2) *h2.ses[pp,j],trunc=F,col=cols[j])
	mylines( pp+c(-.1,.1)	+off[j],h2s[pp,j]+rep(-2,2)*h2.ses[pp,j],trunc=F,col=cols[j])
}

par( mar=c(0,6,.5,0) )
plot(c(1,np),0:1,type='n',axes=F,bty='n',xlab='',ylab='')

xs  <- c( .8, .6 )
mtext( side=2, las=2, at=xs[1], line=.3, cex=1.09, 'GREML p:'	, col=1 )
mtext( side=2, las=2, at=xs[2], line=.3, cex=1.09, 'GxEMM p:'	, col=1 )
for( xx in c(1,2) ){
	pvals  <- c( rep('',3), as.character( format( allps[-(1:3),xx], digits=2, scientific=T ) ) )
	text( 1:np, rep(xs[xx],np), cex=1.11, lab=pvals, col=1, font=1+( allps[,xx] < .01/(nrow(allps)-3) ) )
}
text( 1:np, rep(.1,np), cex=1.01, lab=sapply(xnames,function(x) sprintf('%18s',x) ), srt=90 )
dev.off()
