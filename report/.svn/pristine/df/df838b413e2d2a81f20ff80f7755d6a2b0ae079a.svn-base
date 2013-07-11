/*
 * Chromatizer.cpp
 *
 *  Created on: Sep 29, 2010
 *      Author: watanabe
 */

#include <iostream>
#include <stack>


#include "Solver.h"

#define LOWER_BOUND 2
//#define DEBUG

using namespace std;

Solver::Solver() {
}

Solver::~Solver() {
}

int Solver::Solve(Graph * graph) {



	m_graph = graph;

	//definujeme pole aktualniho obarveni grafu jako neobarvene
	m_colors.resize(graph->GetNodesCount(), UNDEF_COLOR);

	int optimal = graph->GetNodesDegree() + 1;

	cout << "nodes count is: " << graph->GetNodesCount() << endl;
	cout << "upper bound is: " << optimal << endl;

	m_stack.push_back(NodeState(0, 1, 1)); // obarvime uzel 0 barvou 1

#ifdef DEBUG
	cout << "PUSH: " << "NodeState: (0, 1, 1)" << endl;
#endif

	while (!m_stack.empty()) {

		NodeState cState = m_stack.back();
		m_stack.pop_back();
		// POPne vrchol zasobniku

		m_colors[cState.node] = cState.color; // obravi uzel v poli s aktualnim stavem obarveni
		ClearColorSpace(cState.node + 1); //vsechny uzly ktere jsou hloubeji v zasobniku odbarvi
#ifdef DEBUG
		cout << "POP " << "NodeState: (" << cState.node << ", " << cState.color
		<< ", " << cState.maxColor << ")" << endl;
#endif
		if (cState.node == graph->GetNodesCount() - 1) {//dosly uzly //zaznamenat optimum

			int chroma_num = cState.maxColor;
			if (chroma_num <= optimal) { //sem dat pro jistotu rovnitko aby se sejfovalo i to nejhorsi optimalni reseni rovny upper bound
				optimal = chroma_num;
				m_best_outcome = m_colors;
				/*for (int k = 0; k < m_colors.size(); k++) {
				 cout << m_colors[k];
				 }*/
#ifdef DEBUG
				cout << "Nalezeno nove optimalni reseni, chromatic number = "
				<< chroma_num << endl;
#endif
			}
#ifdef DEBUG
			cout << "Dosazeno dna DFS ve stavu: NodeState: (" << cState.node
			<< ", " << cState.color << ", " << cState.maxColor << ")"
			<< endl;
#endif
			continue;
		}

		if (cState.color >= optimal) { // neoptimalni stav //backtrack
#ifdef DEBUG
			cout << "Neperspektivni stav, provadim backtrack" << endl;
#endif
			continue;
		}

		// novy obarvovany uzel ma id o jedno vetsi nez jeho rodic
		node_index_t next_node = cState.node + 1;
		int newMaxColor = cState.maxColor;

		// pridavame barvy pouze do velikosti horni meze
		for (color_index_t color = optimal; color >= 1; color--) {
			newMaxColor = cState.maxColor;
			if (HasAllowedColoring(next_node, color)) {
				if (color > cState.maxColor) { //nove pridana barva zvysuje chromaticke cislo
					newMaxColor = color;
				}

				m_stack.push_back(NodeState(next_node, color, newMaxColor));
#ifdef DEBUG
				cout << "PUSH" << " NodeState: " << next_node << "," << color
				<< "," << newMaxColor << "," << endl;
#endif
			} else {
#ifdef DEBUG
				cout << "Coloring " << color << " is not allowed for node "
				<< next_node << endl;
#endif
			}
		}

	}


	return optimal;
}

void Solver::ClearColorSpace(node_index_t starting_node) {
	for (unsigned int i = starting_node; i < m_colors.size(); i++) {
		m_colors[i] = UNDEF_COLOR;
	}
}

bool Solver::HasAllowedColoring(node_index_t node, color_index_t color) const {

	for (unsigned int i = 0; i < m_colors.size(); i++) {
		if (i == node) {
			continue;
		}
		if (m_colors[i] == color && m_graph->AdjacencyCheck(node, i)) {
			return false;
		}
	}
	return true;
}

void Solver::PrintOutcome(std::ostream & os) const {
	m_graph->PrintselfInColor(m_best_outcome, os);
}
