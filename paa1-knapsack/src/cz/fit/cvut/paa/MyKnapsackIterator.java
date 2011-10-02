package cz.fit.cvut.paa;

import java.util.List;
import java.util.NoSuchElementException;

import cz.cvut.felk.cig.jcop.problem.Configuration;
import cz.cvut.felk.cig.jcop.problem.Operation;
import cz.cvut.felk.cig.jcop.problem.knapsack.KnapsackItem;
import cz.cvut.felk.cig.jcop.problem.knapsack.KnapsackIterator;

public class MyKnapsackIterator extends KnapsackIterator {

	private MyKnapsack myProblem;

	public MyKnapsackIterator(Configuration configuration, MyKnapsack problem) {
		super(configuration, problem);
		this.myProblem = problem;
	}

	public boolean hasNext() {
		return myProblem.getOrderedItems().size() > 0;
	}

	public Operation next() throws NoSuchElementException {

		if (!myProblem.isOrdered())
			myProblem.sortItems();

		if (!hasNext()) {
			throw new NoSuchElementException("No more items to add");
		}

		List<KnapsackItem> items = myProblem.getOrderedItems();

		KnapsackItem bestItem = items.remove(0);

		return this.myProblem.getAddOperations().get(bestItem.getIndex());

	}

}
