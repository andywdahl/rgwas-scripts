rm( list=ls() )
load( '../Rdata/setup.Rdata' )
source( '../code/center_plot.R' )

Kmax	<- 3
types	<- 'pc6'
for( type in types )
		for( K in 3:Kmax )
tryCatch({

	savefile<- paste0( 'Rdata/', type,'_', K, '.Rdata' )
	if( ! file.exists( savefile ) )	next
	load( savefile )
	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
	print( paste( type, K ) )

	statin	<- G[,'B_statin']
	qmat	<- cbind( G[,c('B_smoke','B_totalcw','Age')], Y )
	colnames(qmat)	<- c('Smoke', 'Alcohol',  'Age', 'Glucose', 'Insulin', 'BMI', 'LDL', 'HDL', 'Triglyc.', 'WHR', paste0( 'NMR\nPC ', 1:6 ) )

	bmat	<- cbind( scale01( G[,c('B_statin', 'B_betabloc', 'B_diuretic')] ), Yb )
	colnames(bmat)	<- c( 'Statin', 'Beta Block', 'Diuretic', 'T2D', 'preT2D', 'CHD' )

	locsub	<- 1:nrow(qmat)
	locp		<- colnames(bmat)

	pdf( paste0( '../figs/mfmrx.pdf' ), width=24, height=10.1 )
	center_plot( qmat[locsub,], bmat[locsub,locp], out$pmat[locsub,], col.phen='BMI', bcol=c( rep( 'lightsteelblue4', 3 ), rep( 1, 3 ) ), qcol=c( rep( 'lightsteelblue4', 3 ), rep( 1, 13 ) ), cols=c( '#FB6542', '#3F681C', '#375E97' ) )
	dev.off()

},error=function(e)print(e))
