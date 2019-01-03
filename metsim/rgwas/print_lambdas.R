rm( list=ls() )
load( '../Rdata/setup.Rdata' )
Kmax      <- 4

out	<- matrix( NA, np, 1+2+2*3 )
rownames(out) <- phens
colnames(out) <- c('hom','het','glob', paste0( 'K_', c( 2,2, 3,3, 4,4 ), rep( c('Het', 'Glob'), each=3 ) ) )
rm( all_lambdas )

load( 'Rdata/lambda_gcs.Rdata' )
out[,'hom']	<- all_lambdas[1,]
out[,'het']	<- all_lambdas[3,]
rm( all_lambdas )

load( 'Rdata/lambda_gcs_Glob.Rdata' )
out[,'glob']<- all_lambdas[3,]
rm( all_lambdas )


load( '../rgwas_naive/Rdata/lambda_gcs.Rdata' )
out[,2+1:3*2]	<- t(all_lambdas[2:4,])
rm( all_lambdas )

load( '../rgwas_naive/Rdata/lambda_gcs_Glob.Rdata' )
out[,3+1:3*2]	<- t(all_lambdas[2:4,])
rm( all_lambdas )

format( out, nsmall=2, digits=2 )
