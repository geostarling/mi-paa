package cz.fit.cvut.paa;

public class NothingCounter extends DistanceCounter {

	
	@Override
	public double doStuffWithCoordinateDiff(double diff) {
		return 1;
	}

	@Override
	public double doStuffWithSum(double sum) {
		return 1;
	}
}
