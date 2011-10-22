package cz.fit.cvut.paa;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import cz.cvut.felk.cig.jcop.algorithm.graphsearch.bfs.BreadthFirstSearch;
import cz.cvut.felk.cig.jcop.problem.bucket.Bucket;
import cz.cvut.felk.cig.jcop.result.render.SimpleRender;
import cz.cvut.felk.cig.jcop.solver.SimpleSolver;
import cz.cvut.felk.cig.jcop.solver.Solver;
import cz.cvut.felk.cig.jcop.solver.condition.IterationCondition;

public class Main {

	public static final String DATA_FILE = "data/bu.inst.dat";

	public static void main(String[] args) throws Exception {

		File f = new File(DATA_FILE);
		List<String> lines = getFileLines(f);

		for (String line : lines) {
			Bucket b = getProblemFromLine(line);
			runExperiment(b);
			break;
		}
	}

	public static void runExperiment(Bucket problem) {

		Solver bruteSolver = new SimpleSolver(new BreadthFirstSearch(), problem);
		// bruteSolver.addListener(new JFreeChartRender("Test"));
		bruteSolver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
		bruteSolver.run();
		bruteSolver.render();

		Solver heurSolver = new SimpleSolver(new BucketHeuristic(), problem);
		// bruteSolver.addListener(new JFreeChartRender("Test"));
		heurSolver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
		heurSolver.addStopCondition(new IterationCondition(20000));
		heurSolver.run();
		heurSolver.render();
	}

	public static Bucket getProblemFromLine(String line) {
		String[] parsedLine = line.split(" +");
		int num = Integer.valueOf(parsedLine[1]);
		int[] capacities = new int[num];
		int[] initConfig = new int[num];
		int[] endConfig = new int[num];

		for (int i = 0; i < num; i++) {
			capacities[i] = Integer.valueOf(parsedLine[i + 2]);
		}

		for (int i = 0; i < num; i++) {
			initConfig[i] = Integer.valueOf(parsedLine[i + 2 + num]);
		}

		for (int i = 0; i < num; i++) {
			endConfig[i] = Integer.valueOf(parsedLine[i + 2 + num + num]);
		}

		return new Bucket(capacities, initConfig, endConfig);
	}

	public static List<String> getFileLines(File file) throws Exception {

		List<String> lines = new ArrayList<String>();
		String line;
		BufferedReader br = new BufferedReader(new FileReader(file));

		while (true) {
			line = br.readLine();
			if (line == null) {
				break;
			}
			if (line.length() == 0) {
				continue;
			}
			lines.add(line);

		}

		br.close();

		return lines;
	}

}