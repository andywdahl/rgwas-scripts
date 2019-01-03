rm( list=ls() )
load( '../Rdata/setup.Rdata' )
Kmax      <- 4
for( suff in c( '', '_Glob' ) ){

	savefile	<- paste0( 'Rdata/lambda_gcs', suff, '.Rdata' )
	if( file.exists( savefile ) ){
		load( savefile )
	} else {
		all_lambdas <-  matrix( NA, Kmax, np )
		colnames(all_lambdas) <- phens
		for( K in 1:Kmax )try({

			if( K == 1 ){
				load( paste0( 'compiled_Rdata/', K, '.Rdata' ) )
			} else {
				load( paste0( 'compiled_Rdata/', K, suff, '.Rdata' ) )
			}

			for( pp in 1:np ){
				print( pp )
				x       <- allout[pp,]
				lam_gc  <- median(qchisq(1-10^-x,1),na.rm=T)/qchisq(0.5,1)
				all_lambdas[K,pp]  <- lam_gc
				rm( lam_gc, x )
			}
			rm( allout )

		})
		save( all_lambdas, file=savefile )
	}
	print( round( all_lambdas, 2 ) )
	rm( all_lambdas )
}
