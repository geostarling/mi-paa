package cz.fit.cvut.paa;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.felk.cig.jcop.algorithm.graphsearch.bfs.BreadthFirstSearch;
import cz.cvut.felk.cig.jcop.problem.bucket.Bucket;
import cz.cvut.felk.cig.jcop.result.ResultEntry;
import cz.cvut.felk.cig.jcop.result.render.SimpleRender;
import cz.cvut.felk.cig.jcop.solver.SimpleSolver;
import cz.cvut.felk.cig.jcop.solver.Solver;
import cz.cvut.felk.cig.jcop.solver.condition.IterationCondition;

public class Main {

	static class ResultTuple {
		int optimizeCounter;
		long resultLength;

		ResultTuple(int optimizeCounter, long resultLength) {
			this.optimizeCounter = optimizeCounter;
			this.resultLength = resultLength;
		}

		int getOptimizeCounter() {
			return this.optimizeCounter;
		}

		long getResultLength() {
			return this.resultLength;
		}
	}

	public static final String DATA_FILE = "data/bu.inst.dat";
	public static Map<String, Main.ResultTuple> bfsResults = new HashMap<String, Main.ResultTuple>();
	public static Map<String, Main.ResultTuple> heurManhattanResults = new HashMap<String, Main.ResultTuple>();
	public static Map<String, Main.ResultTuple> heurEuclideanResults = new HashMap<String, Main.ResultTuple>();

	public static void main(String[] args) throws Exception {

		File f = new File(DATA_FILE);
		List<String> lines = getFileLines(f);

		for (String line : lines) {
			Bucket b = getProblemFromLine(line);
			runExperiment(line.split(" ")[0], b);
		}

		System.out.println("BFS");
		System.out.println("id : optimizeCounter : resultLength");
		for (Map.Entry<String, Main.ResultTuple> entry : bfsResults.entrySet()) {
			System.out.println(entry.getKey() + " : "
					+ entry.getValue().optimizeCounter + " : "
					+ entry.getValue().getResultLength());
		}

		System.out.println("Manhattan");
		System.out.println("id : optimizeCounter : resultLength");
		for (Map.Entry<String, Main.ResultTuple> entry : heurManhattanResults.entrySet()) {
			System.out.println(entry.getKey() + " : "
					+ entry.getValue().optimizeCounter + " : "
					+ entry.getValue().getResultLength());
		}
		
		System.out.println("Euclid");
		System.out.println("id : optimizeCounter : resultLength");
		for (Map.Entry<String, Main.ResultTuple> entry : heurEuclideanResults.entrySet()) {
			System.out.println(entry.getKey() + " : "
					+ entry.getValue().optimizeCounter + " : "
					+ entry.getValue().getResultLength());
		}
	}

	public static void runExperiment(String id, Bucket problem) {

		Solver bruteSolver = new SimpleSolver(new BucketHeuristic(
				new NothingCounter()), problem);
		// bruteSolver.addListener(new JFreeChartRender("Test"));
		bruteSolver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
		bruteSolver.run();
		bruteSolver.render();

		Solver heurSolver = new SimpleSolver(new BucketHeuristic(
				new ManhattanCounter()), problem);
		// bruteSolver.addListener(new JFreeChartRender("Test"));
		heurSolver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
		heurSolver.addStopCondition(new IterationCondition(20000));
		heurSolver.run();
		heurSolver.render();

		
		Solver heur2Solver = new SimpleSolver(new BucketHeuristic(
				new EuclideanCounter()), problem);
		// bruteSolver.addListener(new JFreeChartRender("Test"));
		heur2Solver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
		heur2Solver.addStopCondition(new IterationCondition(20000));
		heur2Solver.run();
		heur2Solver.render();
		
		ResultEntry re = bruteSolver.getResult().getResultEntries().get(0);
		bfsResults.put(id, new Main.ResultTuple(re.getOptimizeCounter(), re
				.getBestConfiguration().getOperationHistory().getCounter()));
		re = heurSolver.getResult().getResultEntries().get(0);
		heurManhattanResults.put(id,
				new Main.ResultTuple(re.getOptimizeCounter(), re
						.getBestConfiguration().getOperationHistory()
						.getCounter()));
		re = heur2Solver.getResult().getResultEntries().get(0);
		heurEuclideanResults.put(id,
				new Main.ResultTuple(re.getOptimizeCounter(), re
						.getBestConfiguration().getOperationHistory()
						.getCounter()));
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