rm( list=ls() )
load( '../Rdata/setup.Rdata' )

type	<- 'pc6'
for( K in 1:5 )
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
		for( pp in 1:np ){
			if( K > 1 ){
				if( mode == '' ){
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
