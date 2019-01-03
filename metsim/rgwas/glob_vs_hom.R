rm( list=ls() )
load('../Rdata/setup.Rdata' )
load('../parse_data/parsed_data/snpids.Rdata' )
load('../parse_data/parsed_data/allsnps.Rdata' )
source( '../code/plot_fxns.R' )

#"T2D",			# het has one Inf, otherwise all null
#"preT2D",		# one shared hit,  otherwise all null
#"chd", "B_P_ins0", "B_WHR", # no hits hom or het
#"NMR_PC_5",	#gc = 1.64 
phens1	<- c( "B_P_gl0", "B_BMI", "B_S_ldlc", "B_S_hdlc", "B_S_tottg", "NMR_PC_1", "NMR_PC_2", "NMR_PC_3", "NMR_PC_4", "NMR_PC_6")
names1	<- c( 'Glucose', 'BMI', 'LDL', 'HDL', 'Triglyc.', paste0( 'NMR PC ', c(1:4,6) ) )
names(names1)	<- phens1

tau		<- -log10( 5e-8 )
K			<- 3
load( paste0( 'compiled_Rdata/', K, '_Glob.Rdata' ) )
ally	<- allout
rm( allout )

load( paste0( 'compiled_Rdata/', 1, '.Rdata' ) )
allx	<- allout
rm( allout )

jpeg( paste0( '../figs/glob_vs_hom_', K, '.jpeg' ), width=(6+1/6)*400, height=(2+2/6)*400 )
layout(
	rbind(
		cbind( 1:2+11, matrix(c(1:11,11),2,6,byrow=T) ),
		1:7+2+11
	),
	widths=c(1.0,rep(6,6)),
	heights=c(rep(6,2),1.0)
)

par( mar=c(3,2,5,2) )
for( phen in phens1 ){

	x	<- allx[phen,]
	y	<- ally[phen,]

	lam_gcy	<- median(qchisq(1-10^-y,1),na.rm=T)/qchisq(0.5,1)
	lam_gcx	<- median(qchisq(1-10^-x,1),na.rm=T)/qchisq(0.5,1)

	ymax	<- max( apply( cbind(x,y), 1, max ), na.rm=T )
	ymax	<- min( ymax, 20 )
	lims	<- c(0,max(c(12,ymax)))

	plot( lims, lims, type='n', main=names1[phen], xlab='', ylab='', cex.axis=1.3, cex.main=3.5 )
	abline( col=4, lty=3, a=0, b=1 )
	lines(  col=4, lty=3, c(0,tau,tau), c(tau,tau,0) )

	lam_gcx1	<- format( lam_gcx, nsmall=2, dig=1 )
	lam_gcy1	<- format( lam_gcy, nsmall=2, dig=1 )
	text( 10.5, 1.5, substitute(paste(lambda[gc] == nn), list(nn=lam_gcx1)), col=2, cex=2.8 )
	text( 2.5 , 11 , substitute(paste(lambda[gc] == nn), list(nn=lam_gcy1)), col=3, cex=2.8 )

	pchs	<- rep( 16, length(x) )
	pchs[ x > 20 | y > 20 ]	<- 15
	x[ x > 20 ]	<- 20
	y[ y > 20 ]	<- 20

	points( x, y, col=1, cex=.6, pch=16 )

	subpoints	<- function( subset, pchs=rep( 16, length(x) ), col )
		if( length(subset) > 0 ) points( x[subset], y[subset], col=col, cex=2.4, pch=pchs[subset] )
	subpoints( which( x > tau & y < tau )	, pchs=pchs, col=2 )
	subpoints( which( x < tau & y > tau )	, pchs=pchs, col=3 )
	subpoints( which( x > tau & y > tau )	, pchs=pchs, col=1 )

	points( x[is], y[is], col='tan'	, pch=16, cex=1.2 )
	points( x[js], y[js], col=5			, pch=16, cex=1.2 )
	rm( x, y )

}
par(mar=c(0,0,0,0))
plot.new()
legend( 'center', fill=c(3,2,1,5,'tan'), legend=c('Only GxE Signif','Only Hom Signif', 'No Signif Change', 'CHD GWAS SNPs', 'T2D GWAS SNPs' ), cex=5, bty='n' )

for( aa in 1:2 ){
	plot.new()
	mtext( side=2, line=-4.5, cex=2.6, expression( -log[10](p[GxE-GWAS]) ) )
}
plot.new()
for( aa in 1:4 ){
	plot.new()
	mtext( side=1, line=-2.1, cex=2.6, expression( -log[10](p[GWAS]) ) )
}

dev.off()
