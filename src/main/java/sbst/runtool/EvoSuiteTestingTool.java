package sbst.runtool;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.evosuite.EvoSuite;

public class EvoSuiteTestingTool implements ITestingTool {

	private EvoSuite evosuite = null;
	
	private String targetClassPath ="";
	
	@Override
	public List<File> getExtraClassPath() {
		File evoJar = new File("lib"+File.separator+"evosuite-master-0.1.1-SNAPSHOT-jar-minimal.jar");
		if(!evoJar.exists()){
			System.err.println("Wrong EvoSuite jar setting, jar is not at: "+evoJar.getAbsolutePath());
			return new ArrayList<File>();
		}
		List<File> extra = new ArrayList<File>();
		extra.add(evoJar);
		return extra;
	}

	@Override
	public void initialize(File src, File bin, List<File> classPath) {
		StringBuilder sb = new StringBuilder();
		// Including the source may include other dependency jars
		// sb.append(src.getPath());
		// sb.append(File.pathSeparator);
		sb.append(bin.getPath());
		for(File dependency : classPath) {
			sb.append(File.pathSeparator);
			sb.append(dependency.getPath());
		}
		targetClassPath = sb.toString();
		evosuite = new EvoSuite();
	}

	@Override
	public void run(String cName) {
		if(evosuite == null) {
			System.err.println("EvoSuite has not been initialized with the target classpath!");
			return;
		}
		
		//OUTPUT_DIR  report_dir  test_dir
		
		List<String> commands = new ArrayList<String>();
		commands.addAll(Arrays.asList(new String[] {
		        "-class",
		        cName,
		        "-Duse_deprecated=true",
//		        "-Dreplace_calls=false",
		        "-Dshow_progress=false", 
//		        "-Dstopping_condition=MaxTime",
		        "-Dstopping_condition=TimeDelta",
//		        "-criterion","weakmutation",
		        "-criterion","archivebranch:archiveline:archivemutation",
		        "-Dcompositional_fitness=true",
		        "-Dassertion_strategy=all",
		        "-Dmax_mutants=-1",  
		        "-Dstop_zero=false",
		        "-Dtest_comments=false", 
		        "-mem", "1200",
		        "-Dsecondary_objectives=totallength",
		        "-Dminimize=true",
		        "-Dsearch_budget=120",
		        "-Dglobal_timeout=600", 
		        "-Dminimization_timeout=240",
		        "-Dassertion_timeout=120",
		        "-projectCP="+targetClassPath,
		        "-Dtest_dir=temp/testcases",
		        "-Dtest_scaffolding=false"
//		        "-Dbranch_statement=true"
		        // "-Duse_separate_classloader=true"
		        // "-Dlogback.configurationFile=sbst_logback.xml"  NOTE: cannot be set for client, as not among parameters, but should be fine*/
		        }));
		String[] command = new String[commands.size()];
		commands.toArray(command);
		evosuite.parseCommandLine(command);
	}

}
