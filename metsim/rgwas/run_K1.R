rm( list=ls() )
library( mfmr )
library( parallel )
library(BEDMatrix)
load( '../Rdata/setup.Rdata' )

K   	<- 1
batch	<- as.numeric( commandArgs(TRUE)[[1]] )

if( file.exists( paste0( 'compiled_Rdata/', K, '.Rdata' ) ) ) stop()

types	<- 'pc6'
for( type in types ){

	savefile	<-	paste0( 'Rdata/', type,'_', K, '_', batch, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_', batch, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	sink(	sinkfile )

	## load G/Y/Yb
	load( paste0( '../parse_data/parsed_data/'	, type, '.Rdata' )  )

	## load SNPs
	dat	<- BEDMatrix( '/u/home/j/joelmeff/project-eeskin/geno/autosomes.U05' ) # 6263 x 665478
	rownames(dat)	<- sapply( rownames(dat), function(x) strsplit( x, '_' )[[1]][2] )
	sub	<- intersect( rownames(G), rownames(dat) )

	dat	<- dat[,intersect( (batch-1)*1e3+1:1e3, 1:ncol(dat) )]

	allY	<- cbind(Yb,Y)
	out	<- lapply( 1:ncol(allY), function(pp)
		sapply( 1:1e3, function(ii)
	{

		if( ii > ncol(dat) ) return(NA)
		print( ii )
		g	<- dat[sub,ii]

		bin	<- (pp<=ncol(Yb))
		y			<- allY[,pp]

		if( pp == 1 )
			y[ which( y==0 & Yb[,'preT2D']==1 ) ] <- NA
		#if( pp == 2 )
		#	y[ which( Yb[,'T2D']==1 ) ] <- 2

		sub1	<- which( !is.na( g ) )
		G				<- G			[sub1,]
		y				<- y			[sub1]
		g				<- g			[sub1]

		nullfit	<- glm( y ~ G		, family=ifelse( bin, 'binomial', 'gaussian' ) )
		gfit		<- glm( y ~ G + g, family=ifelse( bin, 'binomial', 'gaussian' ) )
		-log10( anova( nullfit, gfit	, test='Chisq')$Pr[2] )

	}))

	save( out, file=savefile )
	rm( out, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()

}
