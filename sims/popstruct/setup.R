rm( list=ls() )
Ns			<- c( 1e3		,3e3		,1e4		,3e4		)#3e2		,
types		<- c( 'N1e3'	,'N3e3'	,'base','N3e4')#'N3e2',
names(Ns) <- types

sig2list			<- c( 3e-3, .01, .03, .10 )
names(sig2list) <- types

sig2Es				<- c( .01		, .05		, .1		 )
poptypes			<- c( 'low'	, 'med'	, 'high' )
names(sig2Es) <- poptypes

sig2homs	<- c( .04, .004 )
sig2hets	<- c( .004, .04 )

S			<- 12
P			<- 30
P_bin	<- 3

maxit			<- 300
methods		<- c(
	'mfmr_pc',# 'mfmr_pc20',
	'cca-Y',
	'oracle+', 'oracle++', 'oracle',#'oracle2+',
	#'mvgmm', 'mvgmm+',
	'mvgmmq'	, 'mvgmmq+'
)
mfmrmeth	<- methods[1]

Me	<- length( methods )

meths	= c( 'mfmr_pc', 'cca-Y'	, 'oracle++', 'mvgmmq', 'mvgmmq+'	) #, 'oracle+'	, 'mvgmm'	, 'mvgmm+'	, 'mvgmmq'	, 'mvgmmq+', 'mfmr_pc20'	
cols	= c(  1				,  4		 	,	 3				, 2				, 'orange'	) #, 'seagreen1','salmon'	, 2					, 'tan'			, 'tan4'	 ,  'grey'				
leg		= c( 'MFMR'		, 'CCA'		,	'Oracle'	, 'GMM'		, 'GMM-PC'	)	#, 'Oracle'		,	'GMM'		, 'GMM-PC'	, 'GMMQ'		, 'GMMQ-PC', 'MFMRx20'		


names( cols )	<- meths

save.image( 'Rdata/setup.Rdata' )
source( 'plot.R' )
