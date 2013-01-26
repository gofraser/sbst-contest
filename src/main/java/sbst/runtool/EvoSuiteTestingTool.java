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
		// TODO export evosuite.jar?
		return new ArrayList<File>();
	}

	@Override
	public void initialize(File src, File bin, List<File> classPath) {
		StringBuilder sb = new StringBuilder();
		sb.append(src.getPath());
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
		
		List<String> commands = new ArrayList<String>();
		commands.addAll(Arrays.asList(new String[] {
		        "-generateSuite",
		        "-class",
		        cName,
		        "-Dshow_progress=false", "-Dstopping_condition=MaxTime",
		        "-sandbox", "-Dtest_comments=false", "-cp="+targetClassPath }));
		String[] command = new String[commands.size()];
		commands.toArray(command);
		evosuite.parseCommandLine(command);
	}

}
