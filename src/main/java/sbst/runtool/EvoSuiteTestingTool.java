package sbst.runtool;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.evosuite.EvoSuite;

public class EvoSuiteTestingTool implements ITestingTool {

	private EvoSuite evosuite = null;
	
	private String targetClassPath ="";
	
	public List<File> getExtraClassPath() {
        List<File> extra = new ArrayList<File>();
		File evoJar = new File("lib"+File.separator+"evosuite-master-1.0.3-SNAPSHOT.jar");
		if(!evoJar.exists()){
			System.err.println("Wrong EvoSuite jar setting, jar is not at: "+evoJar.getAbsolutePath());
		} else {
            extra.add(evoJar);
        }

		return extra;
	}

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

	public void run(String cName, long timeBudget) {
		if(evosuite == null) {
			System.err.println("EvoSuite has not been initialized with the target classpath!");
			return;
		}
		
		//OUTPUT_DIR  report_dir  test_dir

        // TODO: What shall we use?
		long searchTime   = timeBudget / 2;
		long minTimeout   = timeBudget / 3;
		long assTimeout   = timeBudget / 3;
		long checkTimeout = timeBudget / 3;

		List<String> commands = new ArrayList<String>();
		commands.addAll(Arrays.asList(
		        "-class",
		        cName,
		        "-Duse_deprecated=true",
//		        "-Dreplace_calls=false",
		        "-Dshow_progress=false", 
		        "-Dstopping_condition=MaxTime",
//		        "-Dstopping_condition=TimeDelta",
//		        "-criterion","weakmutation",
//		        "-criterion","archivebranch:archiveline:archivemutation",
//		        "-Dcompositional_fitness=true",
//		        "-Dtest_factory=ARCHIVE",
		        "-Dassertion_strategy=all",
//		        "-Dmax_mutants=-1",
//		        "-Dstop_zero=false",
		        "-Dtest_comments=false", 
		        "-mem", "1200",
//		        "-Dsecondary_objectives=totallength",
		        "-Dminimize=true",
		        "-Dinline=false",
//		        "-Dsandbox_mode=IO",
		        "-Dcoverage=false",
		        "-Dsearch_budget="+searchTime,
		        "-Dglobal_timeout="+searchTime,
		        "-Dnew_statistics=false",
		        "-Dstatistics_backend=NONE",
		        "-Dminimization_timeout="+minTimeout,
		        "-Dassertion_timeout="+assTimeout,
		        "-Djunit_check_timeout="+checkTimeout,
		        "-projectCP="+targetClassPath,
		        "-Dtest_dir=temp/testcases",
		        "-Dtest_scaffolding=false",
                "-Dp_functional_mocking=0.8",
                "-Dfunctional_mocking_percent=0.5",
                "-Dp_reflection_on_private=0.5",
                "-Dreflection_start_percent=0.8"
//		        "-Dbranch_statement=true"
		        // "-Duse_separate_classloader=true"
		        // "-Dlogback.configurationFile=sbst_logback.xml"  NOTE: cannot be set for client, as not among parameters, but should be fine*/
		        ));
		String[] command = new String[commands.size()];
		commands.toArray(command);
		evosuite.parseCommandLine(command);
	}

}
