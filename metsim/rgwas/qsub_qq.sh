#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=3G  
#$ -l time=22:20:00       #-- runtime limit (this requests 24 hours)
#$ -t 1-666													#-- remove first '#' to specify the number of
 
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run_K1.R ${SGE_TASK_ID}
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run.R 2 ${SGE_TASK_ID}
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run.R 3 ${SGE_TASK_ID}
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run.R 4 ${SGE_TASK_ID}
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run.R 5 ${SGE_TASK_ID}
