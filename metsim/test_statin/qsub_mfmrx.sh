#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=.5G  
#$ -pe shared 8  ## processes=cores
#$ -l time=19:20:00       #-- runtime limit (this requests 24 hours)
 
## these lines will be printed to the output file for the job
uname -a
date

/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 8 2
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 8 3
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 8 4
