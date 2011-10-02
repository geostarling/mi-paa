package cz.fit.cvut.paa;

import cz.cvut.felk.cig.jcop.algorithm.BaseAlgorithm;
import cz.cvut.felk.cig.jcop.algorithm.CannotContinueException;
import cz.cvut.felk.cig.jcop.algorithm.InvalidProblemException;
import cz.cvut.felk.cig.jcop.problem.Configuration;
import cz.cvut.felk.cig.jcop.problem.ObjectiveProblem;
import cz.cvut.felk.cig.jcop.problem.OperationIterator;

public class PriceWeightHeuristic extends BaseAlgorithm {

	// List<>

	@Override
	public void init(ObjectiveProblem problem) throws InvalidProblemException {
		if (!problem.hasStartingConfiguration())
			throw new InvalidProblemException(
					"Our algorithm requires StartingConfigurationProblem interface");
		this.problem = problem;
		this.fitness = problem.getDefaultFitness();
		Configuration start = problem.getStartingConfiguration();
		this.bestConfiguration = start;
		this.bestFitness = this.fitness.getValue(start);
	}

	@Override
	public void optimize() throws CannotContinueException {

		OperationIterator it = this.problem
				.getOperationIterator(this.bestConfiguration);

		if (!it.hasNext()) {
			throw new CannotContinueException(
					"Cannot continue, all items in ruksak");
		}

		Configuration configuration = it.next().execute(this.bestConfiguration);
		double fitness = this.fitness.getValue(configuration);

		if (fitness < this.bestFitness) {

return;
		}

		this.bestFitness = fitness;
		this.bestConfiguration = configuration;
	}

}
