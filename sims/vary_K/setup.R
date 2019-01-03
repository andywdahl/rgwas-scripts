rm( list=ls() )
Ks			<- 1:4
types		<- c( 'N1e3','N3e3','base','N3e4')
names(Ks) <- types

sig2homs	<- c( .04, .004 )
sig2hets	<- c( .004, .04 )

S			<- 12
P			<- 30
P_bin	<- 3

maxit			<- 300
methods		<- c( 'mfmr_fast', 'cca-Y', 'oracle', 'mvgmmq' )
mfmrmeth	<- methods[1]

Me	<- length( methods )

meths	= c( 'mfmr_fast'	, 'cca-Y'	, 'oracle', 'mvgmmq'	)
cols	= c(  1			,  4		 	,	 3			, 2					)
leg		= c( 'MFMR'	, 'CCA'		,	'Oracle', 'GMM'			)


names( cols )	<- meths

save.image( 'Rdata/setup.Rdata' )
