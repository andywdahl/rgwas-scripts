rm( list=ls() )
library( mfmr )
library( parallel )
library(BEDMatrix)
load( '../Rdata/setup.Rdata' )

K   	<- as.numeric( commandArgs(TRUE)[[1]] )
batch	<- as.numeric( commandArgs(TRUE)[[2]] )

if( file.exists( paste0( 'compiled_Rdata/', K, '.Rdata' ) ) ) stop()

types	<- 'pc6'
for( type in types ){

	savefile	<-	paste0( 'Rdata/', type,'_', K, '_', batch, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_', batch, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	sink(	sinkfile )

	## load pmat
	load( paste0( '../mfmrx/Rdata/'		, type,'_', K, '.Rdata' ) )
	pmat	<- out$pmat
	rm( out )

	## load G/Y/Yb
	load( paste0( '../parse_data/parsed_data/'	, type, '.Rdata' )  )

	## load SNPs
	dat	<- BEDMatrix( '/u/home/j/joelmeff/project-eeskin/geno/autosomes.U05' ) # 6263 x 665478
	rownames(dat)	<- sapply( rownames(dat), function(x) strsplit( x, '_' )[[1]][2] )
	sub	<- intersect( rownames(G), rownames(dat) )

	Gxpmat		<- t(sapply( 1:nrow(G), function(i) G[i,] %x% pmat[i,] ))
	nullmat   <- cbind( pmat[,-K], Gxpmat )

	dat			<- dat[,intersect( (batch-1)*1e3+1:1e3, 1:ncol(dat) )]

	allY	<- cbind(Yb,Y)
	out	<- lapply( 1:ncol(allY), function(pp)
		sapply( 1:1e3, function(ii)
	{

		if( ii > ncol(dat) ){ x	<- rep( NA, 3); names(x) <- c( 'Hom', 'Het', 'Global' ); return( x ) }
		print( ii )
		g	<- dat[sub,ii]
		if( mean(g,na.rm=T) < .01 | mean(g,na.rm=T) > .99 ){ x	<- rep( NA, 3); names(x) <- c( 'Hom', 'Het', 'Global' ); return( x ) }

		y			<- allY[,pp]
		if( pp == 1 )
			y[ which( y==0 & Yb[,'preT2D']==1 ) ] <- NA

		sub1	<- which( !is.na( g ) )
		nullmat	<- nullmat[sub1,]
		y				<- y			[sub1]
		g				<- g			[sub1]
		pmat		<- pmat		[sub1,]

		if( length( g ) == 0 ){
			print( 'huh' )
			x	<- rep( NA, 3); names(x) <- c( 'Hom', 'Het', 'Global' ); return( x )
		}

		gxe_test_new( nullmat=nullmat, y=y, g=g, pmat=pmat[,-K,drop=F], bin=(pp<=ncol(Yb)) )$pvals
	}))

	save( out, file=savefile )
	rm( out, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()
}
