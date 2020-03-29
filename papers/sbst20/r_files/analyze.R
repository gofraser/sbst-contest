#tool,benchmark,class,run,preparationTime,generationTime,executionTime,testcaseNumber,
#uncompilableNumber,brokenTests,failTests,linesTotal,linesCovered,linesCoverageRatio,
#conditionsTotal,conditionsCovered,conditionsCoverageRatio,mutantsTotal,mutantsCovered,
#mutantsCoverageRatio,mutantsKilled,mutantsKillRatio,mutantsAlive,timeBudget,totalTestClasses

library(stringr)

FILE <- "../data/single_transcript_without_question_mark.csv"

benchmarks <- sort(unique(read.csv("../data/benchmarks.txt", header=T)$benchmark))

mainTable <- function(){

  dt <- read.csv(FILE,sep=",",dec=".",header=T) #,colClasses=cols)

  TABLE = paste("../mainTable.tex",sep="")
  unlink(TABLE)
  sink(TABLE, append=TRUE, split=TRUE)

  cat("\\begin{tabular}{ ll rr rr rr}\\toprule","\n")
  cat(" \\multirow{2}{1in}{Benchmark} & \\multirow{2}{1in}{Java Class} &  \\multicolumn{2}{c}{Line Coverage} &  \\multicolumn{2}{c}{Branch Coverage} &  \\multicolumn{2}{c}{Mutation Score}\\\\\\cmidrule(lr){3-4}\\cmidrule(lr){5-6} \n")
  cat(" & & 60s & 180s & 60s & 180s & 60s & 180s \\\\ \n" )
  cat("\\midrule","\n")

  times <- c(60, 180) # sort(unique(dt$timeBudget))
  classes <- sort(unique(dt$class))

  for(benchmark in benchmarks) {
  #for(class in classes){

    mask = dt$tool=="evosuite" & dt$benchmark==benchmark & dt$preparationTime != "?"

    cat(benchmark, " & ", as.character(unique(dt$class[ dt$benchmark==benchmark ])))

    #cat(class)

    for(t in times){
      cov <-  as.numeric(as.character(dt$linesCoverageRatio[mask & dt$timeBudget==t]))
      cat(" & ", getColouredCell(mean(cov)),sep="")
    }
    
    for(t in times){
      cov <-  as.numeric(as.character(dt$conditionsCoverageRatio[mask & dt$timeBudget==t]))
      cat(" & ", getColouredCell(mean(cov)),sep="")
    }
    for(t in times){
      score <- as.numeric(as.character(dt$mutantsKillRatio[mask & dt$timeBudget==t]))
      cat(" & ", getColouredCell(mean(score)),sep="")
    }

    cat("\\\\ \n")
  }
  cat("\\midrule","\n")
  cat("Average & ")
  for(t in times){
    cov <- as.numeric(as.character(dt$linesCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t]))
    cat(" & ", getColouredCell(mean(cov, na.rm = TRUE)))
  }
  
  for(t in times){
    cov <- as.numeric(as.character(dt$conditionsCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t]))
    cat(" & ", getColouredCell(mean(cov, na.rm = TRUE)))
  }

  for(t in times){
    score <- as.numeric(as.character(dt$mutantsKillRatio[dt$tool=="evosuite" & dt$timeBudget==t]))
    cat(" & ", getColouredCell(mean(score, na.rm = TRUE)))
  }

  cat("\\\\ \n")

  cat("\\bottomrule","\n")
  cat("\\end{tabular}","\n")

  sink()
}
t <- function(){

	dt <- read.csv(FILE,sep=",",dec=".",header=T)
	dt.man <- read.csv(FILE.MANUAL,sep=",",dec=".",header=T)
	evo.cov <- c()
	evo.mut <- c()
	for(benchmark in benchmarks) {
		if (length(dt.man[dt.man$benchmark==benchmark,"conditionsCoverageRatio"])==0) {
			cat(benchmark, " does not exist\n")
			next
		}
		cat(benchmark, " exists\n")
		evo.mask = dt$tool=="evosuite" & dt$benchmark==benchmark
		evo.cov <- c(mean(dt$conditionsCoverageRatio[evo.mask]), evo.cov)
		evo.mut <- c(mean(dt$mutantsKillRatio[evo.mask]), evo.mut)
		cat(evo.cov, "\n")
	}
}

MacroFile <- function(){

  dt <- read.csv(FILE,sep=",",dec=".",header=T)
  macro.file <- "../macros.tex"
  unlink(macro.file)
  sink(macro.file, append=TRUE, split=TRUE)

  classes <- sort(unique(dt$class))
  times <- sort(unique(dt$timeBudget))

  tools <- sort(unique(dt$tool))

  for(benchmark in benchmarks) {
    mask <- dt$tool=="evosuite" & dt$benchmark==benchmark

    for(t in times){
      cov <- dt$conditionsCoverageRatio[mask & dt$timeBudget==t]
      score <- dt$mutantsKillRatio[mask & dt$timeBudget==t]
      #mean(score))
    }
  }
  ## Coverage and mutation scores per time budget
  printComment(macro.file, "Coverage and Mutation per time budget")
  i <- 1
  for(t in times){
    printComment(macro.file, concat(toupper(letters[i]),concat("=",t)))

    cov <- mean(as.numeric(as.character(dt$linesCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t])), na.rm = TRUE)
    printMacro(macro.file, concat("AvgCov",toupper(letters[i])), asPercent(cov))
    
    cov <- mean(as.numeric(as.character(dt$conditionsCoverageRatio[dt$tool=="evosuite" & dt$timeBudget==t])), na.rm = TRUE)
    printMacro(macro.file, concat("AvgCov",toupper(letters[i])), asPercent(cov))

    score <- mean(as.numeric(as.character(dt$mutantsKillRatio[dt$tool=="evosuite" & dt$timeBudget==t])), na.rm = TRUE)
    printMacro(macro.file, concat("AvgMut",toupper(letters[i])), asPercent(score))
    i <- i+1
  }
  ## Flakiness per tool
  printComment(macro.file, "EvoSuite vs Tool(s)")
  for(t in tools){
    flaky <- mean(as.numeric(as.character(dt$brokenTests[dt$tool==t])), na.rm = TRUE)
    printMacro(macro.file, concat("Flaky",toolname(t)), formatC(flaky,digits=1,format="f"))
  }
  ## EvoSuite vs Manual
#  printComment(macro.file, "EvoSuite vs Manual")
#  dt.man <- read.csv(FILE.MANUAL,sep=",",dec=".",header=T)
#  evo.cov <- c()
#  evo.mut <- c()

#  # REDISSON not available for manual
#  for(benchmark in benchmarks) {
#    if (length(dt.man[dt.man$benchmark==benchmark,"conditionsCoverageRatio"])==0) {
#    	next
#    }
#    evo.mask = dt$tool=="evosuite" & dt$benchmark==benchmark
#    evo.cov <- c(mean(as.numeric(as.character(dt$conditionsCoverageRatio[evo.mask])), na.rm = TRUE), evo.cov)
#    evo.mut <- c(mean(as.numeric(as.character(dt$mutantsKillRatio[evo.mask])), na.rm = TRUE), evo.mut)
#  }
#  man.cov <- dt.man$conditionsCoverageRatio
#  man.mut <- dt.man$mutantsKillRatio
#  printMacro(macro.file, "AvgCovEvosuite", asPercent(mean(evo.cov, na.rm = TRUE)))
#  printMacro(macro.file, "AvgMutEvosuite", asPercent(mean(evo.mut, na.rm = TRUE)))
#  printMacro(macro.file, "AvgCovManual", asPercent(mean(man.cov, na.rm = TRUE)))
#  printMacro(macro.file, "AvgMutManual", asPercent(mean(man.mut, na.rm = TRUE)))
#  covCmp <- measureA(evo.cov, man.cov)
#  mutCmp <- measureA(evo.mut, man.mut)
#  printMacro(macro.file, "EvoManCovA", covCmp$A)
#  printMacro(macro.file, "EvoManCovPV", pvalue(covCmp$pv))
#  printMacro(macro.file, "EvoManMutA", mutCmp$A)
#  printMacro(macro.file, "EvoManMutPV", pvalue(mutCmp$pv))

  sink()
}

asPercent <- function(value){
  return(paste(formatC(value,digits=1,format="f"),"\\%",sep=""))
}

concat <- function(a, b){
  return(paste(a, b, sep=""))
}

printMacro <- function(macros.file, name, value){
  cat("\\newcommand{\\",name,"}{{",value,"}\\xspace} \n", file=macros.file, sep="", append=T)
}

printComment <- function(macros.file, comment){
  cat("%",comment,"\n", file=macros.file, append=T)
}

getresults <- function(){

  dt <- read.csv(FILE,header=T)
  times = sort(unique(dt$timeBudget))

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

getColouredCell <- function( value, suffix = "\\%") {
	if (is.nan(value) | is.na(value)) {
		cell <- paste("\\cellcolor{light-gray} \\textcolor{black}", "{-}",sep="")
	} else {
  	if(value == 0) {
    	cell <- paste("\\cellcolor{light-gray} \\textcolor{black}", "{",formatC(value,digits=1,format="f"), suffix, "}",sep="")
  	} else {
    	cell <- paste(formatC(value,digits=1,format="f"), suffix, sep="")
  	}
	}
  return(cell)
}

toolname <- function(x) {
  x <- str_replace(x,"3","three")
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " "))
}

measureA <- function(a, b) {
  res <- list()
  if(length(a)==0 & length(b)==0){
    res$A <- 0.5
    res$pv <- 1
    return(res)
  } else if(length(a)==0){
    ## motivation is that we have no data for "a" but we do for "b".
    ## maybe the process generating "a" always fail (eg out of memory)
    res$A <- 0
    res$pv <- .001
    return(res)
  } else if(length(b)==0){
    res$A <- 1
    res$pv <- .001
    return(res)
  }

  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])

  m = length(a)
  n = length(b)
  A = (r1/m - (m+1)/2)/n


  stat = FALSE
  w = wilcox.test(b, a, exact=FALSE)
  pv = w$p.value
  if (!is.nan(pv) & pv <= 0.05)
    stat = TRUE

  A = floor(A*100)/100

  res$A <- A
  res$pv <- pv
  return(res)
}

pvalue <- function(pv){
  if (!is.nan(pv) & pv <= 0.001)
    return("<0.001")
  else
    return(paste("{",formatC(pv,digits=3,format="f"),"}",sep=""))
}

mainTable()
