/*
 * Graph.cpp
 *
 *  Created on: Sep 28, 2010
 *      Author: watanabe
 */

#include "Graph.h"

#include <stdlib.h>
#include <string>

using namespace std;

Graph::Graph() {
	m_adj_matrix = NULL;
	m_nodes_count = 0;
	m_nodes_degree = 0;
}

Graph::~Graph() {
	if (m_adj_matrix != NULL) {
		delete m_adj_matrix;
	}
}

void Graph::LoadFromFile(ifstream & in_file) {
	m_nodes_count = 0;
	m_nodes_degree = 0;

	string line_string;
	int row_counter = 0;

	getline(in_file, line_string); //extracts nodes count from in file
	m_nodes_count = atoi(line_string.c_str());

	m_adj_matrix = new vector<vector<bool> > (m_nodes_count);
	while (true) {

		getline(in_file, line_string);

		if (in_file.eof()) {
			break;
		}

		if (line_string.length() != m_nodes_count) {
			//malformed input file
			exit(1);
		}

		string::iterator it_str;
		for (it_str = line_string.begin(); it_str < line_string.end(); it_str++) {

			if (*it_str == '1') {
				m_adj_matrix->at(row_counter).push_back(true);
			} else {
				m_adj_matrix->at(row_counter).push_back(false);
			}
		}
		++row_counter;
	}

	//spocitani stupne uzlu (k)
	vector<bool>::iterator row_it;

	for (row_it = m_adj_matrix->at(0).begin(); row_it
			< m_adj_matrix->at(0).end(); ++row_it) {
		if (*row_it) {
			++m_nodes_degree;
		}
	}
}

bool Graph::AdjacencyCheck(node_index_t node1, node_index_t node2) const {
	if (node1 >= m_nodes_count || node2 >= m_nodes_count) {
		return false; //todo sem vrazit vyhozeni vyjimky
	}
	return (*m_adj_matrix)[node1][node2];
}

void Graph::Printself(ostream& os) const {
	vector<vector<bool> >::iterator row_it;
	vector<bool>::iterator col_it;

	for (row_it = m_adj_matrix->begin(); row_it < m_adj_matrix->end(); ++row_it) {
		for (col_it = (*row_it).begin(); col_it < (*row_it).end(); ++col_it) {
			os << *col_it;
		}

		os << endl;
	}
}

void Graph::PrintselfInColor(vector<int> coloring, std::ostream & os) const {
	if (coloring.size() != m_nodes_count) {
		return; //todo throw up
	}
	vector<vector<bool> >::iterator row_it;
	vector<bool>::iterator col_it;

	for (unsigned int i = 0; i < m_adj_matrix->size(); i++) {
		for (unsigned int j = 0; j < (*m_adj_matrix)[i].size(); j++) {
			if (i == j) {
				os << " " << coloring[i] << " ";
			} else {
				os << (*m_adj_matrix)[i][j];
			}
		}

		os << endl;
	}
}
