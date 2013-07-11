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
#include <queue>
#include <ostream>
#include "logger/logger.hpp"
#include "ProcessInfo.h"

class Solver {
public:

	Solver();
	~Solver();

	void Solve();
	void PrintOutcome(std::ostream & os) const;
	void SetGraphInstance(Graph * graph);
	int GetChromaticNumber() const;

	struct NodeState {
		node_index_t node;
		color_index_t color;
		color_index_t maxColor;

		NodeState(node_index_t node_index = 0, color_index_t color_index = 0,
				color_index_t maxColor_index = 0) :
			node(node_index), color(color_index), maxColor(maxColor_index) {
		}

	};

protected:

	void Expand();

	void DistributeData();
	void RecvInitData();

	void ProcessIncomingMessages();
	void ProcessOutgoingMessages();

	bool IsAllowedColoring(node_index_t node, color_index_t color) const;
	void ClearColorSpace(node_index_t starting_node);

	int GetRightNeighbourRank() {
		return (mpProcInfo ->mRank + 1) % mpProcInfo ->mNumProcesses;
	}

	void IncNeighbour();
	NodeState * SplitStack(int * stack_size); // tuta metoda rozdeli stack


	enum SolverState {
		FINISHED, ACTIVE, PASSIVE, WAITING_FOR_WORK
	};

	enum OutgoingMsg {
		SOLUTION_FOUND, NO_WORK, FINISH, SEND_TOKEN
	};

	enum TokenType {
		TOKEN_WHITE = 5000, TOKEN_BLACK = 5001, TOKEN_NOTOKEN = 5002
	};

	enum ProcColor {
		PROC_WHITE = 2000, PROC_BLACK = 2001
	};

	ProcessInfo * mpProcInfo;
	char * mBuffer;
	int mBufferSize;

	SolverState mState;

	TokenType mToken;
	ProcColor mProcessColor;

	int mRefusedWorkReqs;
	std::queue<OutgoingMsg> mOutgoingQueue;
	int mNeighbour; //tato promenna ukazuje na cislo procesu kteremu bude zadano o praci

	std::vector<Solver::NodeState> m_stack;
	Graph * m_graph;
	std::vector<color_index_t> m_best_outcome;
	std::vector<color_index_t> m_colors;
	int mOptimal;

	double outgoingTime, incomingTime, noworkTime, finishTime, expandStartTime, expandFinishTime, userTime;
	int solves;
};

#endif /* SOLVER_H_ */
