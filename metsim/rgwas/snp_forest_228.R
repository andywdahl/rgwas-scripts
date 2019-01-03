rm( list=ls() )
load( '../Rdata/setup.Rdata' )
load( '../parse_data/parsed_data/snpids.Rdata' )
load( '../parse_data/parsed_data/allsnps.Rdata' )
load( '../parse_data/parsed_data/softI.Rdata' )
Y	<- cbind( Yb, Y )

K	<- 3
load( paste0( 'Rdata/', type,'_', K, '_cand.Rdata' ) )
cols	<- c( '#FB6542', '#3F681C', '#375E97' )

pdf( '../figs/forest_228.pdf', width=13, height=2.7 )
layout( matrix( 1:7, 1, 7, byrow=T ), widths=c(1.6,rep(4,6)) )
par( mar=c(9.1,0,4,0) )

snpset	<- c( is, js )
candsnps<- allsnps1[snpset]
S	<- length(c( is, js ))

counter	<- 0
yats	<- c( -.5, -.7, -.9 )*1.2

par( mar=c(9.1,1,4,1) )
for( ii in 1:S )
	for( pp in 1:ncol(Y) )
{

	outi	<- out[[pp]][[ii]]
	outi_hom	<- out_hom[[pp]][[ii]]
	if( outi$pvals['Het'] > .05/S ) next

	if( counter %% 6 == 0 ){
		plot.new()
		mtext( side=2,                 line=-4.5, cex=.99, 'Subtype Specific\nSNP Effects' )
		mtext( side=2, las=2, at=yats[1], line=-5.5, cex=.82, 'Hom. p' )
		mtext( side=2, las=2, at=yats[2], line=-5.5, cex=.82, 'Het. p' )
		mtext( side=2, las=2, at=yats[3], line=-5.5, cex=.82, 'Global p' )
	}
	counter	<- counter+1

	betas	<- outi$coef[paste0('gxe',1:K),'Estimate'	 ]
	ses		<- outi$coef[paste0('gxe',1:K),'Std. Error']

	print( outi_hom['g','Estimate'	 ] )
	beta0	<- outi_hom['g','Estimate'	 ]
	se0		<- outi_hom['g','Std. Error']

	npvals	<- sapply( outi$pvals, function(x) x )
	#npvals[1]<- summary(out_hom[[xx]])$coef['g',4]
	#npvals[2]<- summary(out_hom[[xx]])$coef['g',4]

	pvals	<- sapply( npvals, function(x) as.character( format( x, scientific=T, digits=3 ) ) )

	yrange<- range( c( betas + 2*ses, betas - 2*ses,-.1,.1 ), na.rm=T )

	plot( yrange, 0:1, type='n', axes=F, xlab='', ylab='', main='' )
	mtext( side=3, line=0.9, cex=0.7, paste0( candsnps[ii], ' ->\n', colnames(Y)[pp] ) )
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

	ymax	<- 1.4
	mtext( side=2, las=2, at=yats[1], line=-10.5, cex=.82, text=pvals[1], font=ifelse( npvals[1] < 5e-8, 2, 1 ))
	mtext( side=2, las=2, at=yats[2], line=-10.5, cex=.82, text=pvals[2], font=ifelse( npvals[2] < 1e-3, 2, 1 ))
	mtext( side=2, las=2, at=yats[3], line=-10.5, cex=.82, text=pvals[3], font=ifelse( npvals[3] < 5e-8, 2, 1 ))

}

par( mar=rep(0,4) )
plot.new()
legend( 'center', bty='n', col=c('grey67',cols), c( 'Homogeneous', 'High T2D, High CHD', 'High T2D, Low CHD', 'Low T2D, Low CHD' ), cex=1.5, pch=rep(16,4), pt.cex=2.3 )
dev.off()
