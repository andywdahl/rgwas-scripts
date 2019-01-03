rm( list=ls() )
library( rgwas )

types	<- 'pc6'
for( type in types )
	for( K in 2:8 )
		for( suff in c( '', '_raw', '_mfmrx', '_dropT2D' ) )
try({

	if( suff == '_mfmrx' ){
		load( paste0( '../mfmrx/Rdata/', type,'_', K, '.Rdata' ) )
	} else {
		load( paste0( 'Rdata/', type,'_', K, '.Rdata' ) )
	}
	pmat_all  <- out$pmat
	rm( out )

	savefile <- paste0( 'Rdata/', type,'_', K, '_test', suff, '.Rdata' )
	if( file.exists( savefile ) ) next

	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
	allY	<- cbind(Yb,Y)

	g	<- G[,'B_statin',drop=F]
	G	<- G[,-which(colnames(G)=='B_statin'),drop=F]
	if( ! suff %in% c( '_raw', '_dropT2D' ) )
		G	<- cbind( G, Yb[,'T2D'] )
	#suff = '_raw' exactly coincides with ../mfmr/, except at T2D which was manually retweaked

	if( suff == '_dropT2D' ){
		not2d	<- which( Yb[,'T2D'] == 0 )
		Yb		<- Yb  [not2d,]
		allY	<- allY[not2d,]
		pmat_all<- pmat_all[not2d,]
		G   	<- G   [not2d,]
		g   	<- g   [not2d]
	}

	Gxpmat    <- t(sapply( 1:nrow(G), function(i) G[i,] %x% pmat_all[i,] ))
	pmat			<- pmat_all[,-K,drop=F]
	X					<- cbind( pmat, Gxpmat )

	out_test  <- lapply( 1:ncol(allY), function(ii){
		y	<- allY[,ii]
		if( ii == 1 )
			y[Yb[,'preT2D']==0]	<- NA
		out1	<- interxn_test( X=X, y=y, g=g, pmat=pmat, bin=(ii<=ncol(Yb)) )
		list( pvals=out1$pvals, coef=summary(out1$fit)$coef, pvec=out1$pvec )
	})
	names(out_test)	<- colnames(allY)

	save( out_test, file=savefile )
	rm( out_test, allY, Yb, Y, G, g, pmat, pmat_all, Gxpmat )
	print(warnings())
	print('Done')
})
