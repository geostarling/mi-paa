package cz.fit.cvut.paa;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Queue;

import cz.cvut.felk.cig.jcop.algorithm.BaseAlgorithm;
import cz.cvut.felk.cig.jcop.algorithm.CannotContinueException;
import cz.cvut.felk.cig.jcop.algorithm.InvalidProblemException;
import cz.cvut.felk.cig.jcop.problem.Configuration;
import cz.cvut.felk.cig.jcop.problem.ObjectiveProblem;
import cz.cvut.felk.cig.jcop.problem.OperationIterator;

public class BucketHeuristic extends BaseAlgorithm {

	private Queue<BucketHeuristic.QueueItem> queue;

	private Configuration destConfig;

	private DistanceCounter dc;

	public BucketHeuristic(DistanceCounter dc) {
		super();
		this.dc = dc;
		this.queue = new PriorityQueue<BucketHeuristic.QueueItem>(0,
				this.new QueueItemComparator());
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
		queue.add(this.new QueueItem(start));
	}

	@Override
	public void optimize() throws CannotContinueException {

		
		QueueItem currentItem = queue.remove();
		// to cloased list
		OperationIterator opIt = this.problem.getOperationIterator(currentItem.getConfig());
		
	}

	class QueueItemComparator implements Comparator<BucketHeuristic.QueueItem> {

		@Override
		public int compare(QueueItem qi1, QueueItem qi2) {

			DistanceCounter dc = BucketHeuristic.this.dc;
			Configuration endConfig = BucketHeuristic.this.destConfig;

			double dist1 = dc.countDistance(qi1.getConfig(), endConfig);
			double dist2 = dc.countDistance(qi1.getConfig(), endConfig);
			return (int) (dist1 - dist2);
		}

	}

	class QueueItem {

		private Configuration config;

		QueueItem(Configuration config) {
			this.config = config;
		}

		public Configuration getConfig() {
			return this.config;
		}

		public boolean equals(Object o) {
			QueueItem qi = (QueueItem) o;
			return qi.getConfig().equals(this.config);
		}

	}

}
