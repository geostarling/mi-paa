/*
 * Graph.h
 *
 *  Created on: Sep 28, 2010
 *      Author: watanabe
 */

#ifndef GRAPH_H_
#define GRAPH_H_

#include <vector>
#include <fstream>
#include "utils.h"

//using namespace std;

class Graph {

public:
	Graph();
	~Graph();
	void LoadFromFile(std::ifstream & file);
	void Printself(std::ostream& os) const;
	bool AdjacencyCheck(node_index_t node1, node_index_t node2) const;
	void PrintselfInColor(std::vector<int> coloring, std::ostream & os) const;

	unsigned int GetNodesCount() const {
		return m_nodes_count;
	}

	unsigned int GetNodesDegree() const {
		return m_nodes_degree;
	}

protected:
	std::vector<std::vector<bool> > * m_adj_matrix;
	unsigned int m_nodes_count;
	unsigned int m_nodes_degree;
};

#endif /* GRAPH_H_ */
