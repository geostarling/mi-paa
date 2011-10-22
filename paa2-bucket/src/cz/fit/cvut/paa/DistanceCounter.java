package cz.fit.cvut.paa;

import cz.cvut.felk.cig.jcop.problem.Configuration;

public abstract class DistanceCounter {

	public double countDistance(Configuration start, Configuration end) {

		double dist = 0.0;

		for (int i = 0; i < start.size(); i++) {
			dist += doStuffWithCoordinateDiff(start.asList().get(i)
					- end.asList().get(i));
		}

		return doStuffWithSum(dist);
	}

	public abstract double doStuffWithCoordinateDiff(double diff);

	public abstract double doStuffWithSum(double sum);

}
