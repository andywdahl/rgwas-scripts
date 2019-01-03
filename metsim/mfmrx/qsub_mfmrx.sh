#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=2G  
#$ -pe shared 10  ## processes=cores
#$ -l time=1:20:00       #-- runtime limit (this requests 24 hours)
 
## these lines will be printed to the output file for the job
uname -a
date

/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run_mfmr.R 10 2
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run_mfmr.R 10 3
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run_mfmr.R 10 4
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run_mfmr.R 10 5
