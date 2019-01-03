first I run rgwas split across K an SNP blocks with:
qsub_qq.sh -> run_K1.R, run.R


then I compile the output, including calculating gc:
make_lambdas.R
compile_qq.R


then I find significant het SNPs with K=3
fdr_candsnps.R


then I rerun these hits in order to get the full regression output (which I don't store for all SNPs/K/phenos from the rgwas stage)
rerun_het_hits.R
and then I plot the ouptut in Figure 4:
snp_forest.R


I also produce the GxE GWAS results from the global test, giving Table 1:
GxEWAS_tables.R


Supp Figs:
glob_vs_hom.R

Run candidate SNPs for all NMR traits
qsub_cand.sh -> run_cands.R, snp_forest_228.R
