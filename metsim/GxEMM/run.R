rm( list=ls() )
library(gxemm)
load( '../Rdata/setup.Rdata' )
ldak_loc	<- '../code/ldak5.linux '

set.seed( round(as.numeric(Sys.time())) + 47 )
for( pp in 1:(np+235) )
try({

	suff	<- ifelse( pp > np, pp-np, phens[pp] )

	savefile1	<- paste0( 'Rdata/hom/' , suff, '.Rdata' )
	savefile2	<- paste0( 'Rdata/iid/' , suff, '.Rdata' )
	sinkfile	<- paste0( 'Rout/'      , suff, '.Rout' )
	if( file.exists( savefile2 ) | file.exists( sinkfile ) ) next
	print( sinkfile )
	sink( sinkfile )

	## load pmat
	load( '../mfmrx/Rdata/pc6_3.Rdata' )
	pmat	<- out$pmat
	rm( out )

	## load K
	load( '../parse_data/parsed_data/K.Rdata' )
	K	<- K*nrow(K)/sum(diag(K))

	## load G/Y/Yb
	if( pp <= np ){ 
		load( '../parse_data/parsed_data/pc6.Rdata' )
		y			<- cbind( Yb+1, Y )[,pp]
		tsub	<- which( Yb[,'T2D']==1 | Yb[,'preT2D']==0 )
		tsub1	<- which( Yb[,'T2D']==1 )
	} else {
		load( '../parse_data/parsed_data/softI.Rdata' )
		y			<- Y[,pp-np]
	}
	rm(Y,Yb)

	y			<- scale(y)
	X			<- cbind( pmat[,-3], t(sapply( 1:nrow(G), function(i) G[i,] %x% pmat[i,] )) )

	# diabetes based exclusions
	if( pp == 1 ){
		y		<- y [tsub]
		X		<- X [tsub,]
		K 	<- K [tsub,tsub]
		pmat<- pmat [tsub,]
	} else if( pp == 2 ){
		y		<- y   [-tsub1]
		X		<- X   [-tsub1,]
		K 	<- K   [-tsub1,-tsub1]
		pmat<- pmat[-tsub1,]
	}

	if( ! file.exists( savefile1 ) ){
		runtime_hom	<- system.time({
			out_hom	<- GxEMM( y=y, X=X, Z=pmat, K=K, ldak_loc=ldak_loc, bin=F, gtype='hom'  , etype='hom' )
		})[3]
		save( runtime_hom, out_hom, file=savefile1 )
	}

	if( ! file.exists( savefile2 ) ){
		runtime_het	<- system.time({
			out_iid	<- GxEMM( y=y, X=X, Z=pmat, K=K, ldak_loc=ldak_loc, bin=F, gtype='iid'  , etype='hom' )
		})[3]
		save( runtime_het, out_iid, file=savefile2 )
	}

	print(warnings())
	print('Done')
	sink()
	rm( pmat, y, X, K )
})
