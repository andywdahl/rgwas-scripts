rm( list=ls() )
Ns			<- c( 1e3		,3e3		,1e4		,3e4	 )
types		<- c( 'N1e3','N3e3' ,'base' ,'N3e4')
names(Ns) <- types

sig2homs	<- c( .04, .004 )
sig2hets	<- c( .004, .04 )

S			<- 12
P			<- 30
P_bin	<- 3

maxit			<- 300

plotmeth<-
methods	<- c(
	'mfmr_fast', 'mfmr+',
	'geno_pc_disc', 'geno_pc', 'pheno_pc',
	'cca-Y', 'oracle',
	'allgmmq',
	'mvgmmq'
)
mfmrmeth<- methods[1:2]
Me			<- length( methods )

dirs	<- c( 'base', 'no_asc', 'no_asc_nohet', 'nohet' )

save.image( 'Rdata/setup.Rdata' )
