

check <- function(){
	
	evo <- read.table(gzfile("evo.txt"),header=T)
	grt <- read.table(gzfile("grt.txt"),header=T)
	
	cat("EvoSuite: ",score(evo,5.08),"\n")
	cat("GRT: ",score(grt,1.29),"\n")

	
	evo <- read.table(gzfile("7_evo.txt"),header=T)
	grt <- read.table(gzfile("7_grt.txt"),header=T)
	
	cat("\nonly on 7 projects\n")
	
	cat("EvoSuite: ",score(evo,5.08),"\n")
	cat("GRT: ",score(grt,1.29),"\n")
}

score <- function(dt,t_prep){
	
	ins = 1 * (sum(dt$InstrCov) / 100)
	bra = 2 * (sum(dt$BranchCov) / 100)
	mut = 4 * (sum(dt$MutationCov) / 100)
	
	cov = ins + bra + mut
	      
	pen = (t_prep + sum(dt$t_gen) + sum(dt$t_exec)) / 60
	
	score = cov  - pen
	return(score)
}