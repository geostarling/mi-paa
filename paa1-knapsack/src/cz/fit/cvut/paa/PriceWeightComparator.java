package cz.fit.cvut.paa;

import java.util.Comparator;

import cz.cvut.felk.cig.jcop.problem.knapsack.KnapsackItem;

public class PriceWeightComparator implements Comparator<KnapsackItem> {

	@Override
	public int compare(KnapsackItem ki1, KnapsackItem ki2) {
		double ratio1 = (double) ki1.getPrice() / ki1.getWeight();
		double ratio2 = (double) ki2.getPrice() / ki2.getWeight();
		return ratio1 > ratio2 ? -1 : 1;
	}

}
