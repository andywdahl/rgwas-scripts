rm( list=ls() )
library(rgwas)
library(BEDMatrix)
load( '../Rdata/setup.Rdata' )
load( '../parse_data/parsed_data/snpids.Rdata' )

for( type in 'pc6' )
	for( K in 3 )
{

	savefile	<-	paste0( 'Rdata/', type,'_', K, '_cand.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_cand.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	sink(	sinkfile )

	## load pmat
	load( paste0( '../mfmrx/Rdata/'		, type,'_', K, '.Rdata' ) )
	pmat	<- out$pmat
	rm( out )

	## load G/Y/Yb
	load( '../parse_data/parsed_data/softI.Rdata' )
	Y	<- cbind( Yb, Y )
	B	<- ncol(Yb)
	rm(Yb)

	Gxpmat		<- t(sapply( 1:nrow(G), function(i) G[i,] %x% pmat[i,] ))
	X   <- cbind( pmat[,-K], Gxpmat )

	## load SNPs
	Gsnp	<- BEDMatrix( '/u/home/j/joelmeff/project-eeskin/geno/autosomes.U05' )[,c( is, js )]
	rownames(Gsnp)	<- sapply( rownames(Gsnp), function(x) strsplit( x, '_' )[[1]][2] )
	sub	<- intersect( rownames(G), rownames(Gsnp) )

	out_hom	<- lapply( 1:ncol(Y), function(pp)
		lapply( 1:ncol(Gsnp), function(ii){
			if( ii == 1 ) print( pp )
			g			<- Gsnp[sub,ii]
			y			<- Y[,pp]
			sub1	<- which( !is.na( g ) )
			y				<- y			[sub1]
			G				<- G			[sub1,]
			g				<- g			[sub1]
			fit		<- lm( y ~ G + g )
			summary( fit )$coef
		})
	)
	out	<- lapply( 1:ncol(Y), function(pp)
		lapply( 1:ncol(Gsnp), function(ii){
			if( ii == 1 ) print( pp )
			g			<- Gsnp[sub,ii]
			sub1	<- which( !is.na(g) )
			tmp	<- interxn_test( X=X[sub1,], y=Y[sub1,pp], g=g[sub1], pmat=pmat[sub1,-K,drop=F], bin=(pp<=B) )
			list( pvals=tmp$pvals, coef=summary( tmp$fit )$coef )
		})
	)

	save( out_hom, out, file=savefile )
	rm( out, G )
	print(warnings())
	sink()
}
