rm( list=ls() )
load( '../Rdata/setup.Rdata' )

K	<- 3
cols	<- c( '#FB6542', '#3F681C', '#375E97' )

load( 'Rdata/hit_barplots.Rdata' )

het_hits	<- data.frame(
	snps=c(	     'rs7138803'	, 'rs7528419'	, 'rs780094', 'rs10401969' ),
	phens=c(     'Insulin'		, 'HDL'				, 'NMR PC 3', 'NMR PC 6'	 ),
	phens_full=c('B_P_ins0'   , 'B_S_hdlc'  , 'NMR_PC_3', 'NMR_PC_6'	 )
)

yats	<- c( -.5, -.7, -.9 )*1.2

pdf( '../figs/forest.pdf', width=11, height=2.75 )
layout( matrix( 1:6, 1, 6, byrow=T ), widths=c(1.2,rep(4,4),4) )
par( mar=c(9.1,0,4,0) )

plot.new()
mtext( side=2,                    line=-4.5, cex=.99, 'Subtype Specific\nSNP Effects' )
mtext( side=2, las=2, at=yats[1], line=-5.5, cex=.82, 'Hom. p' )
mtext( side=2, las=2, at=yats[2], line=-5.5, cex=.82, 'Het. p' )
mtext( side=2, las=2, at=yats[3], line=-5.5, cex=.82, 'Global p' )

par( mar=c(9.1,1,4,1) )
for( xx in 1:4 ){

	betas	<- summary(out[[xx]]$fit)$coef[paste0('gxe',1:K),'Estimate'	 ]
	ses		<- summary(out[[xx]]$fit)$coef[paste0('gxe',1:K),'Std. Error']

	beta0	<- summary(out_hom[[xx]])$coef['g','Estimate'	 ]
	se0		<- summary(out_hom[[xx]])$coef['g','Std. Error']

	npvals	<- sapply( out[[xx]]$pvals, function(x) x )

	pvals	<- sapply( npvals, function(x) as.character( format( x, scientific=T, digits=4 ) ) )

	yrange<- range( c( betas + 2*ses, betas - 2*ses,-.1,.1 ), na.rm=T )

	plot( yrange, 0:1, type='n', axes=F, xlab='', ylab='', main='' )
	mtext( side=3, line=1.9, cex=0.9, paste0( het_hits$snps[xx], ' -> ', het_hits$phens[xx] ) )
	abline( v=0, lty=3, col='red', lwd=2.5 )
	axis(1,cex.axis=.75)

	for( k in 1:K ){
		yval	<- rev( 1:K/2/K )[k]
		points( betas[k]									, yval						,col=cols[k], pch=16, cex=3.2 )
		lines(  betas[k]+c(-2,2)*  ses[k]	, rep( yval, 2 )	,col=cols[k], lwd=1.2, lty=1 )
		lines(  betas[k]+rep(2,2)* ses[k]	, yval+c(-.06,.06),col=cols[k], lwd=1.2, lty=1 )
		lines(  betas[k]+rep(-2,2)*ses[k]	, yval+c(-.06,.06),col=cols[k], lwd=1.2, lty=1 )
	}

	yval	<- .9
	points( beta0								, yval						,col='grey67', pch=16, cex=3.2 )
	lines(  beta0+c(-2,2)*  se0	, rep( yval, 2 )	,col='grey67', lwd=1.2, lty=1 )
	lines(  beta0+rep(2,2)* se0	, yval+c(-.06,.06),col='grey67', lwd=1.2, lty=1 )
	lines(  beta0+rep(-2,2)*se0	, yval+c(-.06,.06),col='grey67', lwd=1.2, lty=1 )

	mtext( side=2, las=2, at=yats[1], line=-10.5, cex=.82, text=pvals[1], font=ifelse( npvals[1] < 5e-8, 2, 1 ))
	mtext( side=2, las=2, at=yats[2], line=-10.5, cex=.82, text=pvals[2], font=ifelse( npvals[2] < 1e-3, 2, 1 ))
	mtext( side=2, las=2, at=yats[3], line=-10.5, cex=.82, text=pvals[3], font=ifelse( npvals[3] < 5e-8, 2, 1 ))

}

par( mar=rep(0,4) )
plot.new()
legend( 'center', bty='n', col=c('grey67',cols), c( 'Homogeneous', 'High T2D, High CHD', 'High T2D, Low CHD', 'Low T2D, Low CHD' ), cex=1.5, pch=rep(16,4), pt.cex=2.3 )

dev.off()
