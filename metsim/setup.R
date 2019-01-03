rm( list=ls() )
#types	<- c( 'pc6', 'cvp1', 'cvp2' )
types	<- c( 'pc6' )
#'biom2', 'cvp'
#, 'all_nmr', 'prediab', 't2d', 'newnmr3' )
#, 'nmr', 'big_nmr'

n.folds		<- 10
n.scores	<- 2

if( file.exists( 'parse_data/parsed_data/G_all.Rdata' ) ){
	load( 'parse_data/parsed_data/G_all.Rdata' )
	rm(G_snp)
}

load( paste0( 'parse_data/parsed_data/', 'pc6', '.Rdata' )  )
##YbYnames<- c( colnames(Yb), colnames(Y) )
##YbYnames
#YbYnames<- c(
#	'T2D', 'preT2D', 'CHD',
#	'Gl0\n(glucose?)', 'Ins0\n(insulin?)', 'BMI',
#	'Total CW(?)',
#	#l'Total C\n(cholesterol?)',
#	'LDL', 'HDL', 'TG', 'WHR', 'Height'
#)
#YbYnames
##rm(G,Y,Yb)

Gnames	<- colnames(G)
#Ynames	<- c( "B_P_gl0", "B_P_ins0", 'B_BMI', "B_totalcw", "B_S_ldlc", "B_S_hdlc", "B_S_tottg" ) #"DI", "B_GLTOL5_6", 'B_weight', 'B_waist', 'B_hip'
#Y1names	<- c( "B_P_gl0", "B_P_ins0", 'B_BMI', "B_totalcw", "B_S_ldlc", "B_S_hdlc", "B_S_tottg", 'B_WHR' )

#save( types, n.folds, n.scores, snpnames, Y1names, Ynames, Gnames, YbYnames, file='Rdata/setup.Rdata' )
#save( types, n.folds, n.scores, Y1names, Ynames, Gnames, YbYnames, file='Rdata/setup.Rdata' )
#save( types, n.folds, file='Rdata/setup.Rdata' )

type	<- 'pc6'
load( paste0( 'parse_data/parsed_data/', type, '.Rdata' )  )
phens	<- c( colnames(Yb), colnames(Y) )
np		<- ncol(Yb)+ncol(Y)
rm( Yb, Y, G )

heartphens	<- c( "chd","B_BMI","B_S_ldlc","B_S_hdlc","B_S_tottg","B_WHR","NMR_PC_1","NMR_PC_2","NMR_PC_3","NMR_PC_4","NMR_PC_5","NMR_PC_6")
diabphens		<- c( "T2D","preT2D","B_P_gl0","B_P_ins0","B_BMI","B_WHR","NMR_PC_1","NMR_PC_2","NMR_PC_3","NMR_PC_4","NMR_PC_5","NMR_PC_6")

xnames	<- c( 'T2D', 'PreT2D', 'CHD', 'Glucose', 'Insulin', 'BMI', 'LDL', 'HDL', 'Triglyc.', 'WHR', paste0( 'NMR PC ', 1:6 ) )
#xnames	<- xnames[-1]
#xnames	<- phens

save.image( 'Rdata/setup.Rdata' )
