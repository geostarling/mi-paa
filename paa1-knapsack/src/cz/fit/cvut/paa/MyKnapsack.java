package cz.fit.cvut.paa;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import cz.cvut.felk.cig.jcop.problem.Configuration;
import cz.cvut.felk.cig.jcop.problem.ProblemFormatException;
import cz.cvut.felk.cig.jcop.problem.knapsack.AddOperation;
import cz.cvut.felk.cig.jcop.problem.knapsack.Knapsack;
import cz.cvut.felk.cig.jcop.problem.knapsack.KnapsackItem;
import cz.cvut.felk.cig.jcop.problem.knapsack.KnapsackIterator;

public class MyKnapsack extends Knapsack {

	protected List<KnapsackItem> orderedItems;

	private boolean ordered = false;

	public MyKnapsack(File configFile) throws IOException,
			ProblemFormatException {
		super(configFile);

	}

	public MyKnapsack(File configFile, String id) throws IOException,
			ProblemFormatException {
		super(configFile, id);

	}

	protected void init(String line) throws ProblemFormatException {
		super.init(line);
		orderedItems = new ArrayList<KnapsackItem>(knapsackItems.size());

		for (KnapsackItem ki : knapsackItems) {
			orderedItems.add(ki);
		}
	}

	public KnapsackIterator getOperationIterator(Configuration configuration) {
		return new MyKnapsackIterator(configuration, this);
	}

	public List<KnapsackItem> getOrderedItems() {
		return orderedItems;
	}

	public List<AddOperation> getAddOperations() {
		return this.addOperations;
	}

	public boolean isOrdered() {
		return ordered;
	}

	public void sortItems() {
		Collections.sort(orderedItems, new PriceWeightComparator());
		ordered = true;
	}

}
