package cz.fit.cvut.paa;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import cz.cvut.felk.cig.jcop.algorithm.graphsearch.dfs.DepthFirstSearch;
import cz.cvut.felk.cig.jcop.problem.knapsack.Knapsack;
import cz.cvut.felk.cig.jcop.result.ResultEntry;
import cz.cvut.felk.cig.jcop.solver.SimpleSolver;
import cz.cvut.felk.cig.jcop.solver.Solver;

public class Main {

	private static final String[] DATA_FILES = { "data/knap_4.inst.dat",
			"data/knap_10.inst.dat", "data/knap_15.inst.dat",
			"data/knap_20.inst.dat", "data/knap_22.inst.dat",
			"data/knap_25.inst.dat", "data/knap_27.inst.dat",
			"data/knap_30.inst.dat", "data/knap_32.inst.dat",
			"data/knap_35.inst.dat", "data/knap_37.inst.dat",
			"data/knap_40.inst.dat" };

	private static final String[] SOLUTION_FILES = {
			"solutions/knap_4.sol.dat", "solutions/knap_10.sol.dat",
			"solutions/knap_15.sol.dat", "solutions/knap_20.sol.dat",
			"solutions/knap_22.sol.dat", "solutions/knap_25.sol.dat",
			"solutions/knap_27.sol.dat", "solutions/knap_30.sol.dat",
			"solutions/knap_32.sol.dat", "solutions/knap_35.sol.dat",
			"solutions/knap_37.sol.dat", "solutions/knap_40.sol.dat" };

	private static Map<Integer, Double> relErrorsMap = new HashMap<Integer, Double>();
	private static Map<Integer, Double> maxErrorsMap = new HashMap<Integer, Double>();
	private static Map<Integer, Double> bruteAvgTimeMap = new HashMap<Integer, Double>();
	private static Map<Integer, Double> pwhAvgTimeMap = new HashMap<Integer, Double>();

	public static void main(String[] args) throws IOException {
		int startIdx;
		int endIdx;
		// int startIdx = 0;
		// int endIdx = 4;
		// for (int i = startIdx; i < endIdx; i++) {
		// runExperiment(DATA_FILES[i], 9000 + 50 * i);
		// }

		startIdx = 4;
		endIdx = 12;
		for (int i = startIdx; i < endIdx; i++) {
			runExperiment(DATA_FILES[i], SOLUTION_FILES[i], 9000 + 50 * i);
		}

		System.out.println("Results:");
		System.out.println("Relative error Avg:");
		System.out.println("n    error [%]");
		for (Map.Entry<Integer, Double> entry : relErrorsMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

		System.out.println("Relative error Max:");
		System.out.println("n    error [%]");
		for (Map.Entry<Integer, Double> entry : maxErrorsMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

		System.out.println("BruteForce Times: ");
		System.out.println("n    time [ms]");
		for (Map.Entry<Integer, Double> entry : bruteAvgTimeMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

		System.out.println("PriceWeightHeuristic Times: ");
		System.out.println("n    time [ms]");
		for (Map.Entry<Integer, Double> entry : pwhAvgTimeMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

	}

	public static void runExperiment(String datafile, String solutionFile,
			int startId) throws IOException {

		double relErrorSum = 0.0;

		double pwhTimeSum = 0.0;
		double maxError = 0.0;
		int dimension = (new Knapsack(new File(datafile))).getDimension();

		Map<Integer, Integer> sol = getResults(new File(solutionFile));

		for (int id = startId; id < startId + 50; id++) {
			System.out.println("Processing: " + id);

			Solver pwhSolver = new SimpleSolver(new PriceWeightHeuristic(),
					new MyKnapsack(new File(datafile), Integer.toString(id)));
			pwhSolver.run();
			ResultEntry pwhEntry = pwhSolver.getResult().getResultEntries()
					.get(0);

			double pwhFitness = pwhEntry.getBestFitness();

			pwhTimeSum += pwhEntry.getStartTimestamp().getCpuTimeSpent(
					pwhEntry.getStopTimestamp());

			double bruteFitness = sol.get(Integer.valueOf(id)).doubleValue();
			double relError = ((bruteFitness - pwhFitness) / bruteFitness) * 100;
			if (relError > maxError) {
				maxError = relError;
			}
			relErrorSum += relError;

		}

		relErrorsMap.put(new Integer(dimension), relErrorSum / 50);
		pwhAvgTimeMap.put(new Integer(dimension), pwhTimeSum / 50);
		maxErrorsMap.put(new Integer(dimension), maxError);
	}

	public static Map<Integer, Integer> getResults(File f) throws IOException {
		Map<Integer, Integer> solutions = new HashMap<Integer, Integer>();
		BufferedReader br = new BufferedReader(new FileReader(f));
		String line;
		Integer fitness;
		while (true) {
			line = br.readLine();
			if (line == null) {
				break;
			}
			StringTokenizer st = new StringTokenizer(line);

			Integer id = Integer.valueOf(st.nextToken());
			st.nextToken();
			fitness = Integer.valueOf(st.nextToken());
			solutions.put(id, fitness);
		}

		br.close();
		return solutions;
	}

	public static void runExperiment(String datafile, int startId)
			throws IOException {

		double relErrorSum = 0.0;
		double bruteTimeSum = 0.0;
		double pwhTimeSum = 0.0;
		double maxError = 0.0;
		int dimension = (new Knapsack(new File(datafile))).getDimension();

		for (int id = startId; id < startId + 50; id++) {
			System.out.println("Processing: " + id);
			Solver bruteSolver = new SimpleSolver(new DepthFirstSearch(),
					new Knapsack(new File(datafile), Integer.toString(id)));
			// bruteSolver.addListener(new JFreeChartRender("Test"));
			// bruteSolver
			// .addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
			bruteSolver.run();
			// bruteSolver.render();

			Solver pwhSolver = new SimpleSolver(new PriceWeightHeuristic(),
					new MyKnapsack(new File(datafile), Integer.toString(id)));
			// pwhSolver.addRender(new
			// SimpleRender(SimpleRender.OUTPUT_STANDARD));
			pwhSolver.run();
			// pwhSolver.render();

			ResultEntry bruteEntry = bruteSolver.getResult().getResultEntries()
					.get(0);
			ResultEntry pwhEntry = pwhSolver.getResult().getResultEntries()
					.get(0);

			double bruteFitness = bruteEntry.getBestFitness();
			double pwhFitness = pwhEntry.getBestFitness();
			bruteTimeSum += bruteEntry.getStartTimestamp().getCpuTimeSpent(
					bruteEntry.getStopTimestamp());
			pwhTimeSum += pwhEntry.getStartTimestamp().getCpuTimeSpent(
					pwhEntry.getStopTimestamp());

			double relError = ((bruteFitness - pwhFitness) / bruteFitness) * 100;
			if (relError > maxError) {
				maxError = relError;
			}
			relErrorSum += relError;

		}

		relErrorsMap.put(new Integer(dimension), relErrorSum / 50);
		bruteAvgTimeMap.put(new Integer(dimension), bruteTimeSum / 50);
		pwhAvgTimeMap.put(new Integer(dimension), pwhTimeSum / 50);
		maxErrorsMap.put(new Integer(dimension), maxError);
	}
}
