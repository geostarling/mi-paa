package cz.fit.cvut.paa;

import java.util.Comparator;
import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

import cz.cvut.felk.cig.jcop.algorithm.BaseAlgorithm;
import cz.cvut.felk.cig.jcop.algorithm.CannotContinueException;
import cz.cvut.felk.cig.jcop.algorithm.InvalidProblemException;
import cz.cvut.felk.cig.jcop.problem.Configuration;
import cz.cvut.felk.cig.jcop.problem.ObjectiveProblem;
import cz.cvut.felk.cig.jcop.problem.OperationIterator;

public class BucketHeuristic extends BaseAlgorithm {

	private Queue<Configuration> queue;

	private Configuration destConfig;

	private DistanceCounter dc;

	private Set<Configuration> closedConfigs = new HashSet<Configuration>();
	

	public BucketHeuristic(DistanceCounter dc) {
		super();
		this.dc = dc;
		this.queue = new PriorityQueue<Configuration>(10,
				this.new ConfigurationComparator());
	}

	@Override
	public void init(ObjectiveProblem problem) throws InvalidProblemException {
		this.fitness = problem.getDefaultFitness();

		if (!problem.hasStartingConfiguration())
			throw new InvalidProblemException(
					"Graph Search algorithms requires StartingConfigurationProblem");
		Configuration start = problem.getStartingConfiguration();
		this.problem = problem;

		this.bestConfiguration = start;
		this.bestFitness = this.fitness.getValue(start);
		this.destConfig = problem.getDestinations().get(0);
		queue.add(start);
	}

	@Override
	public void optimize() throws CannotContinueException {

		Configuration currentConfig = queue.poll();
		//System.out.println("optim " + queue.size() + ", "
		//		+ closedConfigs.size() + ", "
		//		+ dc.countDistance(currentConfig, this.destConfig));

		if (currentConfig == null) {
			throw new CannotContinueException("Solution cannot be found!");
		}

		if (this.fitness.getValue(currentConfig) > this.bestFitness) {
			this.bestFitness = this.fitness.getValue(currentConfig);
			this.bestConfiguration = currentConfig;
		}

		if (currentConfig.equals(destConfig)) {
			throw new CannotContinueException("Solution found!");
		}

		if (closedConfigs.contains(currentConfig))
			System.out.println("What?");
		closedConfigs.add(currentConfig); // to cloased list

		OperationIterator opIt = this.problem
				.getOperationIterator(currentConfig);
		while (opIt.hasNext()) {
			Configuration nextConfig = opIt.next().execute(currentConfig);
			if (closedConfigs.contains(nextConfig)
					|| queue.contains(nextConfig)) {
				// System.out.println("closed or queue");
				continue;
			}
			queue.add(nextConfig);

		}
	}

	class ConfigurationComparator implements Comparator<Configuration> {

		@Override
		public int compare(Configuration conf1, Configuration conf2) {

			DistanceCounter dc = BucketHeuristic.this.dc;
			Configuration endConfig = BucketHeuristic.this.destConfig;

			double dist1 = dc.countDistance(conf1, endConfig);
			double dist2 = dc.countDistance(conf2, endConfig);
			return (int) (dist1 - dist2);
		}

	}

}
