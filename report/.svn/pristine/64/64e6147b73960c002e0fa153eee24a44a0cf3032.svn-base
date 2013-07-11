/*
 * Chromatizer.h
 *
 *  Created on: Sep 29, 2010
 *      Author: watanabe
 */
#ifndef SOLVER_H_
#define SOLVER_H_

#include "utils.h"
#include "Graph.h"
#include <stack>
#include <ostream>

#define UNDEF_COLOR 0

class Solver {
public:

	Solver();
	~Solver();

	int Solve(Graph * graph);
	//int Solve2(Graph * graph);
	bool HasAllowedColoring(node_index_t node, color_index_t color) const;
	void ClearColorSpace(node_index_t starting_node);
	void PrintOutcome(std::ostream & os) const;

protected:
	struct NodeState {
		node_index_t node;
		color_index_t color;
		color_index_t maxColor;

		NodeState(node_index_t node_index, color_index_t color_index,
				color_index_t maxColor_index) :
			node(node_index), color(color_index), maxColor(maxColor_index) {
		}

	};

	std::vector<Solver::NodeState> m_stack;
	Graph * m_graph;
	std::vector<color_index_t> m_best_outcome;
	std::vector<color_index_t> m_colors;
};

#endif /* SOLVER_H_ */
