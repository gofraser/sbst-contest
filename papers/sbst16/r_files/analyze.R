#tool,benchmark,class,run,preparationTime,generationTime,executionTime,testcaseNumber,
#uncompilableNumber,brokenTests,failTests,linesTotal,linesCovered,linesCoverageRatio,
#conditionsTotal,conditionsCovered,conditionsCoverageRatio,mutantsTotal,mutantsCovered,mutantsCoverageRatio,
#mutantsKilled,mutantsKillRatio,mutantsAlive,timeBudget,totalTestClasses


FILE = "../data/single_transcript.csv";

mainTable <- function(){

	dt <- read.csv(FILE,header=T)

	TABLE = paste("../mainTable.tex",sep="")
	unlink(TABLE)
	sink(TABLE, append=TRUE, split=TRUE)

	cat("\\begin{tabular}{ l rrrr rrrr}\\toprule","\n")
	cat(" Class &  \\multicolumn{4}{c}{Branch Coverage}  &  \\multicolumn{4}{c}{Fault Detection} \\\\ \n")
	cat(" & 60s & 120s & 240s & 480s & 60s & 120s & 240s & 480s \\\\ \n" )
	cat("\\midrule","\n")

	classes = sort(unique(dt$class))
	times = sort(unique(dt$timeBudget))

	for(class in classes){

		mask = dt$tool=="evosuite" & dt$class==class

		cat(class)

		for(t in times){
			cov = dt$conditionsCoverageRatio[mask & dt$timeBudget==t]
			cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
		}

		for(t in times){
			fails = dt$failTests[mask & dt$timeBudget==t]
			ratio = sum(fails>0) / length(fails)
			cat(" & ", paste(formatC(100*mean(ratio),digits=1,format="f"),"\\%",sep=""))
		}
		cat("\\\\ \n")
	}

	cat("\\bottomrule","\n")
	cat("\\end{tabular}","\n")

	sink()
}
