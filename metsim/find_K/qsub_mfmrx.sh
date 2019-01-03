#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=2G  
#$ -pe shared 10  ## processes=cores
#$ -l time=10:20:00       #-- runtime limit (this requests 24 hours)
 
## these lines will be printed to the output file for the job
uname -a
date

/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 2
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 3
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 4
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 5
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 6
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 7
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 8
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 9
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./rerun_mfmr.R 10 10
