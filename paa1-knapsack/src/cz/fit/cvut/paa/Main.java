package cz.fit.cvut.paa;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import cz.cvut.felk.cig.jcop.algorithm.graphsearch.bfs.BreadthFirstSearch;
import cz.cvut.felk.cig.jcop.problem.knapsack.Knapsack;
import cz.cvut.felk.cig.jcop.result.ResultEntry;
import cz.cvut.felk.cig.jcop.result.render.SimpleRender;
import cz.cvut.felk.cig.jcop.solver.SimpleSolver;
import cz.cvut.felk.cig.jcop.solver.Solver;

public class Main {

	private static final String[] DATA_FILES = { "data/knap_4.inst.dat",
			"data/knap_10.inst.dat", "data/knap_15.inst.dat",
			"data/knap_20.inst.dat", "data/knap_22.inst.dat",
			"data/knap_25.inst.dat", "data/knap_27.inst.dat",
			"data/knap_30.inst.dat", "data/knap_32.inst.dat",
			"data/knap_35.inst.dat", "data/knap_37.inst.dat",
			"data/knap_40.inst.dat"

	};

	private static Map<Integer, Double> relErrorsMap = new HashMap<Integer, Double>();
	private static Map<Integer, Double> bruteAvgTimeMap = new HashMap<Integer, Double>();
	private static Map<Integer, Double> pwhAvgTimeMap = new HashMap<Integer, Double>();

	public static void main(String[] args) throws IOException {
		int startId = 9000;
		for (String datafile : DATA_FILES) {
			runExperiment(datafile, startId);
			startId += 50;
		}

		System.out.println("Results:");
		System.out.println("Relative error:");
		System.out.println("n    error");
		for (Map.Entry<Integer, Double> entry : relErrorsMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

		System.out.println("BruteForce Times: ");
		System.out.println("n    time [ms]");
		for (Map.Entry<Integer, Double> entry : bruteAvgTimeMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

		System.out.println("PriceWeghtHeuristic Times: ");
		System.out.println("n    time [ms]");
		for (Map.Entry<Integer, Double> entry : pwhAvgTimeMap.entrySet()) {
			System.out.println(entry.getKey() + "    " + entry.getValue());
		}

	}

	public static void runExperiment(String datafile, int startId)
			throws IOException {

		double relErrorSum = 0.0;
		double bruteTimeSum = 0.0;
		double pwhTimeSum = 0.0;
		int dimension = (new Knapsack(new File(datafile))).getDimension();

		for (int id = startId; id < startId + 50; id++) {
			System.out.println("Processing: " + id);
			Solver bruteSolver = new SimpleSolver(new BreadthFirstSearch(),
					new Knapsack(new File(datafile), Integer.toString(id)));
			// bruteSolver.addListener(new JFreeChartRender("Test"));
			//bruteSolver
			//		.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
			bruteSolver.run();
			//bruteSolver.render();

			Solver pwhSolver = new SimpleSolver(new PriceWeightHeuristic(),
					new MyKnapsack(new File(datafile), Integer.toString(id)));
			//pwhSolver.addRender(new SimpleRender(SimpleRender.OUTPUT_STANDARD));
			pwhSolver.run();
			//pwhSolver.render();

			ResultEntry bruteEntry = bruteSolver.getResult().getResultEntries()
					.get(0);
			ResultEntry pwhEntry = pwhSolver.getResult().getResultEntries()
					.get(0);

			double bruteFitness = bruteEntry.getBestFitness();
			double pwhFitness = pwhEntry.getBestFitness();
			bruteTimeSum += bruteEntry.getStartTimestamp().getClockTimeSpent(
					bruteEntry.getStopTimestamp());
			pwhTimeSum += pwhEntry.getStartTimestamp().getClockTimeSpent(
					pwhEntry.getStopTimestamp());

			relErrorSum += (bruteFitness - pwhFitness) / bruteFitness;

		}

		relErrorsMap.put(new Integer(dimension), relErrorSum / 50);
		bruteAvgTimeMap.put(new Integer(dimension), bruteTimeSum / 50);
		pwhAvgTimeMap.put(new Integer(dimension), pwhTimeSum / 50);

	}
}
