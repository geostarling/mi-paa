/*
 * Graph.cpp
 *
 *  Created on: Sep 28, 2010
 *      Author: watanabe
 */

#include "Graph.h"

#include <stdlib.h>
#include <string>

#include "mpi.h"

#include "logger/logger.hpp"
#include "ProcessInfo.h"

using namespace std;

Graph::Graph() {
	m_adj_matrix = NULL;
	m_nodes_count = 0;
	m_nodes_degree = 0;
}

Graph::~Graph() {
	if (m_adj_matrix != NULL) {
		delete[] m_adj_matrix;
	}
}

void Graph::LoadFromFile(ifstream & in_file) {
	m_nodes_count = 0;
	m_nodes_degree = 0;

	string line_string;
	int row_counter = 0;

	getline(in_file, line_string); //extracts nodes count from in file
	m_nodes_count = atoi(line_string.c_str());

	m_adj_matrix = new bool[m_nodes_count * m_nodes_count];
	while (true) {

		getline(in_file, line_string);

		if (in_file.eof()) {
			break;
		}

		if (line_string.length() != m_nodes_count) {
			//malformed input file
			exit(1);
		}

		for (unsigned int i = 0; i < line_string.length(); i++) {

			if (line_string[i] == '1') {
				m_adj_matrix[row_counter * m_nodes_count + i] = true;
			} else {
				m_adj_matrix[row_counter * m_nodes_count + i] = false;
			}
		}
		++row_counter;
	}

	//spocitani stupne uzlu (k)
	for (unsigned int j = 0; j < m_nodes_count; j++) {
		if (m_adj_matrix[j] == true) {
			++m_nodes_degree;
		}
	}
}

void Graph::LoadFromArray(bool * adjMatrix, int nodes_count) {
	DEBUG(*ProcessInfo::GetInfo()->mpLogger,"Loading graph from array!");
	m_nodes_count = nodes_count;
	m_adj_matrix = new bool[nodes_count*nodes_count];
	memcpy(m_adj_matrix, adjMatrix, nodes_count*nodes_count);

	//spocitani stupne uzlu (k)
	for (unsigned int j = 0; j < m_nodes_count; j++) {
		if (m_adj_matrix[j] == true) {
			++m_nodes_degree;
		}
	}
}

bool Graph::AdjacencyCheck(node_index_t node1, node_index_t node2) const {
	if (node1 >= m_nodes_count || node2 >= m_nodes_count) {
		return false; //todo sem vrazit vyhozeni vyjimky
	}
	return m_adj_matrix[node1 * m_nodes_count + node2];
}

void Graph::Printself(ostream& os) const {

	for (unsigned int i = 0; i < m_nodes_count; i++) {
		for (unsigned int j = 0; j < m_nodes_count; j++) {
			os << m_adj_matrix[i * m_nodes_count + j];
		}
		os << endl;
	}

}

void Graph::PrintselfInColor(vector<int> coloring, std::ostream & os) const {
	if (coloring.size() != m_nodes_count) {
		return; //todo throw up
	}

	for (unsigned int i = 0; i < m_nodes_count; i++) {
		for (unsigned int j = 0; j < m_nodes_count; j++) {
			if (i == j) {
				os << " " << coloring[i] << " ";
			} else {
				os << m_adj_matrix[i * m_nodes_count + j];
			}

		}
		os << endl;
	}

}
