package cz.fit.cvut.paa;

public class ManhattanCounter extends DistanceCounter {

	@Override
	public double doStuffWithCoordinateDiff(double diff) {
		return Math.abs(diff);
	}

	@Override
	public double doStuffWithSum(double sum) {
		return sum;
	}

}
