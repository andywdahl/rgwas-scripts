rm( list=ls() )
load( '../parse_data/parsed_data/softI.Rdata' )
P0		<- length( Ynames )
Y0		<- Y[,1:P0]
Ynmr	<- Y[,-(1:P0)]
np1		<- ncol(Ynmr)
phens1<- colnames(Ynmr)

load( '../Rdata/setup.Rdata' )

type	<- 'pc6'
for( K in 1:4 )
	for( mode in c( '', '_Glob' ) )
try({

	bigfile	<- paste0( 'compiled_Rdata/', K, mode, '.Rdata' )
	if( file.exists( bigfile ) ) next

	allout	<- matrix( NA, np, 666 * 1e3 )
	rownames( allout )	<- phens

	for( batch in 1:666 ){
		savefile	<-	paste0( 'Rdata/', type,'_', K, '_', batch, '.Rdata' )
		if( !file.exists( savefile ) ){
			print( savefile )
			stop()
		}
		load( savefile )
		for( pp in 1:nrow(allout) ){
			if( K > 1 ){
				if( mode != '_Glob' ){
					ps	<- -log10( out[[pp]]['Het',] )
				} else {
					ps	<- -log10( out[[pp]]['Global',] )
				}
			} else {
				if( mode == '' ){
					ps	<- out[[pp]]
				} else {
					next
				}
			}
			allout[ pp, (batch-1)*1e3+1:1e3 ]	<- ps
		}
		rm( out, ps )
	}
	save( allout, file=bigfile )
	rm( allout )
})
