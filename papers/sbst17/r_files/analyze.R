#tool,benchmark,class,run,preparationTime,generationTime,executionTime,testcaseNumber,
#uncompilableNumber,brokenTests,failTests,linesTotal,linesCovered,linesCoverageRatio,
#conditionsTotal,conditionsCovered,conditionsCoverageRatio,mutantsTotal,mutantsCovered,mutantsCoverageRatio,
#mutantsKilled,mutantsKillRatio,mutantsAlive,timeBudget,totalTestClasses

cols = c("character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
FILE = "../data/single_transcript.csv";

mainTable <- function(){

	dt <- read.csv(FILE,sep=",",dec=".",header=T) #,colClasses=cols)

	TABLE = paste("../mainTable.tex",sep="")
	unlink(TABLE)
	sink(TABLE, append=TRUE, split=TRUE)

	cat("\\begin{tabular}{ l rrrr rrrr}\\toprule","\n")
	cat(" Benchmark &  \\multicolumn{4}{c}{Branch Coverage}  &  \\multicolumn{4}{c}{Fault Detection} \\\\ \n")
	cat(" & 60s & 120s & 240s & 480s & 60s & 120s & 240s & 480s \\\\ \n" )
	cat("\\midrule","\n")

	classes = sort(unique(dt$class))
	times = sort(unique(dt$timeBudget))
	benchmarks = sort(unique(dt$benchmark))

        for(benchmark in benchmarks) {
	#for(class in classes){

		mask = dt$tool=="evosuite" & dt$benchmark==benchmark

                cat(benchmark)

		#cat(class)

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


coverageTable <- function(){

	dt <- read.csv(FILE,header=T)

	TABLE = paste("../coverageTable.tex",sep="")
	unlink(TABLE)
	sink(TABLE, append=TRUE, split=TRUE)

	cat("\\begin{tabular}{ ll rrrr rrrr}\\toprule","\n")
	cat(" Defects4J ID & Class &  \\multicolumn{4}{c}{Line Coverage}  &  \\multicolumn{4}{c}{Branch Coverage} \\\\ \n")
	cat(" & & 60s & 120s & 240s & 480s & 60s & 120s & 240s & 480s \\\\ \n" )
	cat("\\midrule","\n")

	classes = sort(unique(dt$class))
	times = sort(unique(dt$timeBudget))
	benchmarks = sort(unique(dt$benchmark))
        
        for(benchmark in benchmarks) {
	#for(class in classes){

		mask = dt$tool=="evosuite" & dt$benchmark==benchmark

                cat(benchmark)

		class = as.character(dt$class[mask & dt$timeBudget==60][1])
                cat(" & ", class)

                for(t in times){
			cov = dt$linesCoverageRatio[mask & dt$timeBudget==t]
			cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
		}

		for(t in times){
			cov = dt$conditionsCoverageRatio[mask & dt$timeBudget==t]
			cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
		}
		cat("\\\\ \n")
	}
	cat("\\midrule","\n")
        cat("Average & ")
        for(t in times){
            cov = dt$linesCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t]
            cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
        }
        
        for(t in times){
            cov = dt$conditionsCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t]
            cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
        }

        cat("\\\\ \n")
	cat("\\bottomrule","\n")
	cat("\\end{tabular}","\n")

	sink()

}


faultTable <- function(){

	dt <- read.csv(FILE,header=T)

	TABLE = paste("../faultTable.tex",sep="")
	unlink(TABLE)
	sink(TABLE, append=TRUE, split=TRUE)

	cat("\\begin{tabular}{ ll rrrr rrrr}\\toprule","\n")
	cat(" Defects4J ID & Class &  \\multicolumn{4}{c}{Mutation Score}  &  \\multicolumn{4}{c}{Fault Detection} \\\\ \n")
	cat(" & & 60s & 120s & 240s & 480s & 60s & 120s & 240s & 480s \\\\ \n" )
	cat("\\midrule","\n")

	classes = sort(unique(dt$class))
	times = sort(unique(dt$timeBudget))
	benchmarks = sort(unique(dt$benchmark))

        for(benchmark in benchmarks) {
	#for(class in classes){

		mask = dt$tool=="evosuite" & dt$benchmark==benchmark

                cat(benchmark)

		class = as.character(dt$class[mask & dt$timeBudget==60][1])
                cat(" & ", class)

                for(t in times){
			cov = dt$mutantsKillRatio[mask & dt$timeBudget==t]
			cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
		}

        	for(t in times){
			fails = dt$failTests[mask & dt$timeBudget==t]
			ratio = sum(fails>0) / length(fails)
			cat(" & ", paste(formatC(100*mean(ratio),digits=1,format="f"),"\\%",sep=""))
		}
		cat("\\\\ \n")
	}
	cat("\\midrule","\n")
        cat("Average & ")
        for(t in times){
            cov = dt$mutantsKillRatio[dt$tool=="evosuite" & dt$timeBudget==t]
            cat(" & ", paste(formatC(mean(cov),digits=1,format="f"),"\\%",sep=""))
        }

        for(t in times){
            fails = dt$failTests[dt$tool=="evosuite" & dt$timeBudget==t]
            ratio = sum(fails>0) / length(fails)
            cat(" & ", paste(formatC(100*mean(ratio),digits=1,format="f"),"\\%",sep=""))
        }

        cat("\\\\ \n")
	cat("\\bottomrule","\n")
	cat("\\end{tabular}","\n")

	sink()
}

getsuts <- function(){

	dt <- read.csv(FILE,header=T)
        times = sort(unique(dt$timeBudget))
	benchmarks = sort(unique(dt$benchmark))

        for(benchmark in benchmarks) {
            for(t in times){

                mask = dt$benchmark==benchmark & dt$timeBudget==t

                cov_evo = mean(dt$linesCoverageRatio[mask & dt$tool=="evosuite"])
                cov_rdp = mean(dt$linesCoverageRatio[mask & dt$tool=="randoop"])
                cov_jte = mean(dt$linesCoverageRatio[mask & dt$tool=="jtexpert"])
                cov_t3  = mean(dt$linesCoverageRatio[mask & dt$tool=="t3"])
                if(cov_evo < cov_t3) {
                    cat(benchmark, ", Time = ", t, ", ", paste(formatC(cov_evo,digits=1,format="f"),"\\%",sep=""), " vs. ", paste(formatC(cov_t3,digits=1,format="f"),"\\%",sep=""), "\n")
                }
            }

        }
 }


getresults <- function(){

        dt <- read.csv(FILE,header=T)
        times = sort(unique(dt$timeBudget))
        benchmarks = sort(unique(dt$benchmark))

        for(benchmark in benchmarks) {
            for(t in times){

                mask = dt$benchmark==benchmark & dt$timeBudget==t

                cov_evo = mean(dt$failTests[mask & dt$tool=="evosuite"])
                cov_rdp = mean(dt$failTests[mask & dt$tool=="randoop"])
                cov_jte = mean(dt$failTests[mask & dt$tool=="jtexpert"])
                cov_t3  = mean(dt$failTests[mask & dt$tool=="t3"])
                cat(benchmark, ", Time = ", t, ", ", paste(formatC(cov_evo,digits=1,format="f"),"\\%",sep=""),
                ",", paste(formatC(cov_rdp,digits=1,format="f"),"\\%",sep=""),
                ",", paste(formatC(cov_jte,digits=1,format="f"),"\\%",sep=""),
                ",",
                paste(formatC(cov_t3,digits=1,format="f"),"\\%",sep=""),
                "\n")
            }

        }
 }