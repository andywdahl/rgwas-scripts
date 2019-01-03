rm( list=ls() )
library(mixtools)

types	<- 'pc6'
for( type in types )
	for( K in 2:5 )
{
	print( type )

	savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	print( type )
	sink(	sinkfile )

	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
	#Ymat	<- scale( cbind( Yb, Y, G ) )
	print( dim( Yb ) )
	print( dim( Y ) )
	Ymat	<- scale( cbind( Yb, Y ) )
	print( dim( Ymat ) )
	print( svd( Ymat )$d )
	Ymat	<- scale( Y )

	runtime	<- system.time({
		pmat  <- mvnormalmixEM( Ymat, k=K, maxit=1e4 )$posterior
	})[3]

	save( pmat, runtime, file=savefile )
	rm( pmat, runtime, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()
}
