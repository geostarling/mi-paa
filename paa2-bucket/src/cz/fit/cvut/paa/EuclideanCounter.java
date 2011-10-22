package cz.fit.cvut.paa;

public class EuclideanCounter extends DistanceCounter {

	@Override
	public double doStuffWithCoordinateDiff(double diff) {
		return Math.pow(diff, 2);
	}

	@Override
	public double doStuffWithSum(double sum) {
		return Math.sqrt(sum);
	}

}
