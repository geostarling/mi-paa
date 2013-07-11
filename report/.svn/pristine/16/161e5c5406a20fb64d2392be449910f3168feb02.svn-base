/*
 * Chromatizer.cpp
 *
 *  Created on: Sep 29, 2010
 *      Author: watanabe
 */

#include <iostream>
#include <stack>
#include <math.h>
#include "mpi.h"
#include "logger/logger.hpp"

#include "Solver.h"

#define UNDEF_COLOR 0
#define BUFFER_MULTIPLIER 2

#define LOWER_BOUND 2
#define CHECK_MSG_AMOUNT  10

#define MSG_WORK_REQUEST 1000
#define MSG_WORK_SENT    1001
#define MSG_WORK_NOWORK  1002
#define MSG_TOKEN        1003
#define MSG_FINISH       1004
#define MSG_WORK_BCAST   1005
#define MSG_SOLUTION     1006

using namespace std;

Solver::Solver() {
	mpProcInfo = ProcessInfo::GetInfo();
	mNeighbour = (mpProcInfo->mRank + 1) % mpProcInfo->mNumProcesses;

	mOptimal = 0;
	m_graph = NULL;
	mBuffer = NULL;
	mBufferSize = 0;
	mToken = TOKEN_NOTOKEN;
	mProcessColor = PROC_WHITE;
	mRefusedWorkReqs = 0;
	solves=0;
}

Solver::~Solver() {
	if (m_graph != NULL) {
		delete m_graph;
	}
	if (mBuffer != NULL) {
		delete[] mBuffer;
	}

}

void Solver::Solve() {

	// INITIALIZE SOLVER
	if (mpProcInfo->mRank == ROOT_PROCESS_RANK) {

		m_colors.resize(m_graph->GetNodesCount(), UNDEF_COLOR);//definujeme pole aktualniho obarveni grafu jako neobarvene
		mOptimal = m_graph->GetNodesDegree() + 1;
		m_stack.push_back(NodeState(0, 1, 1)); // obarvime uzel 0 barvou 1
		TRACE(*(mpProcInfo->mpLogger), "PUSH: " << "NodeState: (0, 1, 1)");

		DistributeData();
	} else {
		RecvInitData();
	}

	DEBUG(*(mpProcInfo->mpLogger), "nodes count is " << m_graph->GetNodesCount() );
	DEBUG(*(mpProcInfo->mpLogger), "upper bound is:" << mOptimal);

	DEBUG(*(mpProcInfo->mpLogger), "Processes have been synchronized on barrier! Starting algorithm!");
	DEBUG(*(mpProcInfo->mpLogger), "============================================");

	int work_counter = 0;

	mState = ACTIVE;
	while (mState != FINISHED) {

		work_counter++;
		if ((work_counter % CHECK_MSG_AMOUNT) == 0) { // todo sem pridat podminku aby to 100x necekalo kdyz mu prijde job
			ProcessIncomingMessages();
		}

		if (!m_stack.empty()) {
			expandStartTime=MPI_Wtime();
			Expand();
			expandFinishTime=MPI_Wtime();
			userTime+=(expandFinishTime-expandStartTime);
			//INFO(*(mpProcInfo->mpLogger), "expand time "<< expandFinishTime-expandStartTime);
		}

		// dosly stavy a jeste necekame na novy work
		if (m_stack.empty() && mState == ACTIVE) {
			mOutgoingQueue.push(NO_WORK);
			mState = WAITING_FOR_WORK;
		}
		ProcessOutgoingMessages();

	}

}

void Solver::ProcessIncomingMessages() {
	MPI_Status status;
	int flag;
	while (true) {
		//tady zamontovat blokujici probe kdyz v pasiv stavu
		MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
		if (!flag) {
			break;
		}

		NodeState * splited_stack = NULL;
		int split_stack_size = 0;
		NodeState node_state = NULL;
		int position = 0;
		int new_optimal;
		const char * tok_str;
		//const char * state_str;

		//kdyz dojde prace umistit pozadavek do seznamu outgoinmessages, odeslat a hned v rte metode cekat na vysledek, nezprcovavat v incoming messages

		//prisla zprava, je treba ji obslouzit
		//v promenne status je tag (status.MPI_TAG), cislo odesilatele (status.MPI_SOURCE)
		//a pripadne cislo chyby (status.MPI_ERROR)
		switch (status.MPI_TAG) {

		//------------------------ MSG_WORK_REQUEST ------------------------
		case MSG_WORK_REQUEST: // zadost o praci, prijmout a dopovedet
			// zaslat rozdeleny zasobnik a nebo odmitnuti MSG_WORK_NOWORK

			//prijmu zpravu
			MPI_Recv(NULL, 0, MPI_CHAR, status.MPI_SOURCE, MSG_WORK_REQUEST,
					MPI_COMM_WORLD, &status);

			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE: tag = MSG_WORK_REQUEST, source = "<<status.MPI_SOURCE);

			//rozdelim zasobnik
			splited_stack = SplitStack(&split_stack_size);
			if (splited_stack == NULL) { //zasobnik prazdny //nelze odeslat praci

				DEBUG(*(mpProcInfo->mpLogger),
						"Work request received, but no work to share!");

				MPI_Send(NULL, 0, MPI_CHAR, status.MPI_SOURCE, MSG_WORK_NOWORK,
						MPI_COMM_WORLD);
				break;
			} else {
				position = 0;
				MPI_Pack(&split_stack_size, 1, MPI_INT, mBuffer, mBufferSize,
						&position, MPI_COMM_WORLD); //zabali pocet stavu ktery se budou odesilat
				for (int i = 0; i < split_stack_size; i++) {

					DEBUG(*(mpProcInfo->mpLogger),
							"Packing NodeState: node = " << splited_stack[i].node <<" color = "<< splited_stack[i].color << " max color = "<<splited_stack[i].maxColor);

					MPI_Pack(&(splited_stack[i].node), 1, MPI_INT, mBuffer,
							mBufferSize, &position, MPI_COMM_WORLD);
					MPI_Pack(&(splited_stack[i].color), 1, MPI_INT, mBuffer,
							mBufferSize, &position, MPI_COMM_WORLD);
					MPI_Pack(&(splited_stack[i].maxColor), 1, MPI_INT, mBuffer,
							mBufferSize, &position, MPI_COMM_WORLD);

				}

				MPI_Pack(m_colors.data(), splited_stack[0].node + 1, MPI_INT,
						mBuffer, mBufferSize, &position, MPI_COMM_WORLD);

				DEBUG(*(mpProcInfo->mpLogger), "Sending splitted stack to "<<status.MPI_SOURCE);

				MPI_Send(mBuffer, position, MPI_PACKED, status.MPI_SOURCE,
						MSG_WORK_SENT, MPI_COMM_WORLD);

				if (mpProcInfo->mRank > status.MPI_SOURCE) { //Pošle-li dárce Pi práci příjemci Pj a i > j, pak dárce Pi se stane černým.
					DEBUG(*(mpProcInfo->mpLogger), "Setting mProcessColor = PROC_BLACK");
					mProcessColor = PROC_BLACK;
					mToken = mToken != TOKEN_NOTOKEN ? TOKEN_BLACK
							: TOKEN_NOTOKEN;
				}
			}
			/*
			 * TODO call odeslani zasobniku dat do extra metody asi (dalo by se tam pripadne
			 * dat i totozne volani DistributeData()
			 */

			break;

			//------------------------ MSG_WORK_SENT ------------------------
		case MSG_WORK_SENT: // prisel rozdeleny zasobnik, prijmout
			MPI_Recv(mBuffer, mBufferSize, MPI_PACKED, status.MPI_SOURCE,
					MSG_WORK_SENT, MPI_COMM_WORLD, &status);

			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE: tag = MSG_WORK_SENT, source = "<<status.MPI_SOURCE);
			incomingTime=MPI_Wtime();
			INFO(*(mpProcInfo->mpLogger), "Time between job request and job come "<< incomingTime-outgoingTime);
			incomingTime=0;
			outgoingTime=0;
			// deserializovat a spustit vypocet

			position = 0;
			MPI_Unpack(mBuffer, mBufferSize, &position, &split_stack_size, 1,
					MPI_INT, MPI_COMM_WORLD);
			DEBUG(*(mpProcInfo->mpLogger), "Incoming job! Recieving "<<split_stack_size<<" states.");

			for (int i = 0; i < split_stack_size; i++) {
				MPI_Unpack(mBuffer, mBufferSize, &position, &(node_state.node),
						1, MPI_INT, MPI_COMM_WORLD);
				MPI_Unpack(mBuffer, mBufferSize, &position,
						&(node_state.color), 1, MPI_INT, MPI_COMM_WORLD);
				MPI_Unpack(mBuffer, mBufferSize, &position,
						&(node_state.maxColor), 1, MPI_INT, MPI_COMM_WORLD);

				DEBUG(*(mpProcInfo->mpLogger), "Inserting na dno stack node: node "<<node_state.node<<" color "<<node_state.color<<" max color: "<<node_state.maxColor);
				m_stack.insert(m_stack.begin(), node_state); // prijaty stav se uklada na dno zasobniku
			}
			MPI_Unpack(mBuffer, mBufferSize, &position, m_colors.data(),
					node_state.node + 1, MPI_INT, MPI_COMM_WORLD);
			ClearColorSpace(node_state.node);

			DEBUG(*(mpProcInfo->mpLogger), "mState = ACTIVE");
			mState = ACTIVE;
			mRefusedWorkReqs = 0;
			/*
			 * TODO to same jako u predchoziho- sla by pro to udelat specialni metoda
			 * kvuli DRY
			 */

			break;

			//------------------------ MSG_WORK_NOWORK ------------------------
		case MSG_WORK_NOWORK: // odmitnuti zadosti o praci
			MPI_Recv(NULL, 0, MPI_PACKED, status.MPI_SOURCE, MSG_WORK_NOWORK,
					MPI_COMM_WORLD, &status);
			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE: tag = MSG_WORK_NOWORK, source = "<<status.MPI_SOURCE);
			noworkTime=MPI_Wtime();
			INFO(*(mpProcInfo->mpLogger), "Time between work request and no work come "<<noworkTime-outgoingTime);
			noworkTime=0;
			outgoingTime=0;
			mRefusedWorkReqs++;
			if (mRefusedWorkReqs > mpProcInfo->mNumProcesses / 2) {//pokud nas odmitlo vice nez polovina procesu
				DEBUG(*(mpProcInfo->mpLogger), "mState = PASSIVE");
				INFO(*(mpProcInfo)->mpLogger,"starting sending token");
				mState = PASSIVE;
				if (mpProcInfo->mRank == ROOT_PROCESS_RANK) { //root vzdy odesila bileho peska
					DEBUG(*(mpProcInfo->mpLogger), "ROOT: mToken = TOKEN_WHITE");
					mToken = TOKEN_WHITE; //vygeneruje novy token
				}
				if (mToken != TOKEN_NOTOKEN) {
					tok_str = mToken == TOKEN_WHITE ? "TOKEN_WHITE"
							: "TOKEN_BLACK";
					DEBUG(*(mpProcInfo->mpLogger), "Sending token to "<<GetRightNeighbourRank()<<", token color mToken = "<<tok_str);
					// odeslani peska, bud bilyho nebo cernyho
					MPI_Send(&mToken, 1, MPI_INT, GetRightNeighbourRank(),
							MSG_TOKEN, MPI_COMM_WORLD);
					mToken = TOKEN_NOTOKEN;
					mProcessColor = PROC_WHITE; // po odeslani se proces vzdy zbili
				}
			}
			if (mState != PASSIVE) { // po odeslani tokenu jiz neni mozne odesilat zpravy
				mOutgoingQueue.push(NO_WORK); // na nas pozadavek bylo odpovezeno zaporne proto zaradime do fronty novy event na odeslani pozadavku dalsimu sousedovi
			}
			break;

			//------------------------ MSG_SOLUTION ------------------------
		case MSG_SOLUTION:
			position = 0;
			solves++;
			MPI_Recv(mBuffer, mBufferSize, MPI_PACKED, MPI_ANY_SOURCE,
					MSG_SOLUTION, MPI_COMM_WORLD, &status);
			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE: tag = MSG_SOLUTION, source = "<<status.MPI_SOURCE);

			MPI_Unpack(mBuffer, mBufferSize, &position, &mOptimal, 1, MPI_INT,
					MPI_COMM_WORLD);

			DEBUG(*(mpProcInfo->mpLogger), "Setting optimal solution to = "<<mOptimal);

			MPI_Unpack(mBuffer, mBufferSize, &position, m_best_outcome.data(),
					m_graph->GetNodesCount(), MPI_INT, MPI_COMM_WORLD);
			break;

			//------------------------ MSG_TOKEN ------------------------
		case MSG_TOKEN: //ukoncovaci token, prijmout a nasledne preposlat
			// - bily nebo cerny v zavislosti na stavu procesu
			MPI_Recv(&mToken, 1, MPI_INT, MPI_ANY_SOURCE, MSG_TOKEN,
					MPI_COMM_WORLD, &status);

			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE: tag = MSG_TOKEN, source = "<<status.MPI_SOURCE);

			tok_str = mToken == TOKEN_WHITE ? "TOKEN_WHITE" : "TOKEN_BLACK";
			DEBUG(*(mpProcInfo->mpLogger), "Received token from "<<status.MPI_SOURCE<<", token color mToken = "<<tok_str);

			if (mProcessColor == PROC_BLACK) {
				mToken = TOKEN_BLACK; //je-li Pi černý, obarví peška na černo.
			}

			if (mState == PASSIVE) {

				if (mpProcInfo->mRank != ROOT_PROCESS_RANK) {
					DEBUG(*(mpProcInfo->mpLogger), "Passing token to neighbor mToken = "<<tok_str);
					MPI_Send(&mToken, 1, MPI_INT, GetRightNeighbourRank(),
							MSG_TOKEN, MPI_COMM_WORLD);
					mToken = TOKEN_NOTOKEN;
					mProcessColor = PROC_WHITE;
				} else {

					if (mToken == TOKEN_BLACK) {
						DEBUG(*(mpProcInfo->mpLogger), "Starting new end circle with TOKEN_WHITE");
						mToken = TOKEN_WHITE;
						MPI_Send(&mToken, 1, MPI_INT, GetRightNeighbourRank(),
								MSG_TOKEN, MPI_COMM_WORLD);
						mToken = TOKEN_NOTOKEN;
						mProcessColor = PROC_WHITE;
					} else { // mToken == TOKEN_WHITE

						//jsem root a prisel mi bily pesek tak uz nikdo nema praci a muzu rozeslat ukonceni vypoctu
						DEBUG(*(mpProcInfo->mpLogger), "Recvd TOKEN_WHITE on root process - Sendong MSG_FINISH");
						outgoingTime=MPI_Wtime();
						//ted by mely byt vsechny procesy v pasivnim stavu a cekat na FIN_ zpravu
						for (int i = 0; i < mpProcInfo->mNumProcesses; i++) {
							if (i != mpProcInfo->mRank) {//sobe neposilam
								DEBUG(*(mpProcInfo->mpLogger), "OUTGOING MESSAGE, type = MSG_FINISH, dest = "<<i);
								MPI_Send(NULL, 0, MPI_CHAR, i, MSG_FINISH,
										MPI_COMM_WORLD);
							}
						}
						incomingTime=MPI_Wtime();
						INFO(*(mpProcInfo->mpLogger),"Time to send msg finish "<<incomingTime-outgoingTime);
						outgoingTime=MPI_Wtime();
						//posbira vysledky
						for (int i = 0; i < mpProcInfo->mNumProcesses - 1; i++) { //todo nahradit za IProbe a while(true)
							position = 0;
							MPI_Recv(mBuffer, mBufferSize, MPI_PACKED,
									MPI_ANY_SOURCE, MSG_SOLUTION,
									MPI_COMM_WORLD, &status);

							MPI_Unpack(mBuffer, mBufferSize, &position,
									&new_optimal, 1, MPI_INT, MPI_COMM_WORLD);

							if (new_optimal < mOptimal) {

								DEBUG(*(mpProcInfo->mpLogger), "Setting optimal solution to = "<<mOptimal);

								MPI_Unpack(mBuffer, mBufferSize, &position,
										m_best_outcome.data(),
										m_graph->GetNodesCount(), MPI_INT,
										MPI_COMM_WORLD);
							}
						}
						incomingTime=MPI_Wtime();
						INFO(*(mpProcInfo->mpLogger), "time to get results "<<incomingTime-outgoingTime);
						DEBUG(*(mpProcInfo->mpLogger), "TERMINATING");
						INFO(*(mpProcInfo->mpLogger),"User time of this process "<<userTime);
						INFO(*(mpProcInfo->mpLogger),"solves "<<solves);
						mState = FINISHED;

						//todo wtime;
					}
				}
			}
			break;

			//------------------------ MSG_FINISH ------------------------
		case MSG_FINISH:
			// zabali sve nejoptimalnejsi reseni a odesle root procesu
			DEBUG(*(mpProcInfo->mpLogger), "INCOMING MESSAGE, type = MSG_FINISH");
			position = 0;
			MPI_Recv(mBuffer, mBufferSize, MPI_PACKED, MPI_ANY_SOURCE,
					MSG_FINISH, MPI_COMM_WORLD, &status);
			MPI_Pack(&mOptimal, 1, MPI_INT, mBuffer, mBufferSize, &position,
					MPI_COMM_WORLD);
			MPI_Pack(m_best_outcome.data(), m_best_outcome.size(), MPI_INT,
					mBuffer, mBufferSize, &position, MPI_COMM_WORLD);

			MPI_Send(mBuffer, position, MPI_CHAR, ROOT_PROCESS_RANK,
					MSG_SOLUTION, MPI_COMM_WORLD);
			mState = FINISHED;
			DEBUG(*(mpProcInfo->mpLogger), "TERMINATING");
			INFO(*(mpProcInfo->mpLogger),"User time of this process "<<userTime);
			INFO(*(mpProcInfo->mpLogger),"solves "<<solves);
			//todo wtime
			break;
		default:

			break;
		}

	}
}

void Solver::ProcessOutgoingMessages() {
	int position = 0;
	MPI_Request req;
	while (!mOutgoingQueue.empty()) {
		switch (mOutgoingQueue.front()) {

		//------------------------ NO_WORK ------------------------
		case NO_WORK: //do nowork stuff
			DEBUG(*(mpProcInfo->mpLogger), "OUTGOING MESSAGE: type = NO_WORK, dest = "<<mNeighbour);
			outgoingTime=MPI_Wtime();
			MPI_Isend(NULL, 0, MPI_CHAR, mNeighbour, MSG_WORK_REQUEST,
					MPI_COMM_WORLD, &req);
			IncNeighbour();
			break;
			//------------------------ SOLUTION_FOUND ------------------------
		case SOLUTION_FOUND:
			DEBUG(*(mpProcInfo->mpLogger), "OUTGOING MESSAGE: type = SOLUTION_FOUND, optimal = "<<mOptimal);

			// zabali nove optimalnejsi reseni a rozesle vsem procesum
			MPI_Pack(&mOptimal, 1, MPI_INT, mBuffer, mBufferSize, &position,
					MPI_COMM_WORLD);
			MPI_Pack(m_best_outcome.data(), m_best_outcome.size(), MPI_INT,
					mBuffer, mBufferSize, &position, MPI_COMM_WORLD);

			//rozeslu ostatnim procesum
			for (int i = 0; i < mpProcInfo->mNumProcesses; i++) {
				if (i != mpProcInfo->mRank) {//sobe neposilam
					MPI_Send(mBuffer, position, MPI_CHAR, i, MSG_SOLUTION,
							MPI_COMM_WORLD);
				}
			}
			break;
		default:
			ERROR(*(mpProcInfo->mpLogger), "Jestli tuto zpravu vidite tak vam to asi spadlo. ");
		}
		mOutgoingQueue.pop();
	}
}

void Solver::Expand() {
	NodeState cState = m_stack.back();
	m_stack.pop_back();

	m_colors[cState.node] = cState.color; // obravi uzel v poli s aktualnim stavem obarveni
	ClearColorSpace(cState.node + 1); //vsechny uzly ktere jsou hloubeji v zasobniku odbarvi

	TRACE(*(mpProcInfo->mpLogger), "POP " << "NodeState: (" << cState.node << ", " << cState.color
			<< ", " << cState.maxColor << ")");

	if (cState.color >= mOptimal) { // neoptimalni stav //backtrack
		TRACE(*(mpProcInfo->mpLogger), "Neperspektivni stav, provadim backtrack");
		return;
	}

	if (cState.node == m_graph->GetNodesCount() - 1) {//dosly uzly //zaznamenat optimum

		int chroma_num = cState.maxColor;
		if (chroma_num < mOptimal) { //sem dat pro jistotu rovnitko aby se sejfovalo i to nejhorsi optimalni reseni rovny upper bound
			mOptimal = chroma_num;
			m_best_outcome = m_colors;
			mOutgoingQueue.push(SOLUTION_FOUND); // notifikace ze jsme nasli nove reseni // zpracuje se v Incomin msgs
			DEBUG(*(mpProcInfo->mpLogger),
					"Nalezeno nove optimalni reseni, chromatic number = " << chroma_num);
		}

		TRACE(*(mpProcInfo->mpLogger), "Dosazeno dna DFS ve stavu: NodeState: ("
				<< cState.node << ", " << cState.color << ", " << cState.maxColor << ")");

		return;
	}

	// novy obarvovany uzel ma id o jedno vetsi nez jeho rodic
	node_index_t next_node = cState.node + 1;
	int newMaxColor = cState.maxColor;

	// pridavame barvy pouze do velikosti horni meze
	for (color_index_t color = mOptimal; color >= 1; color--) {
		newMaxColor = cState.maxColor;
		if (IsAllowedColoring(next_node, color)) {
			if (color > cState.maxColor) { //nove pridana barva zvysuje chromaticke cislo
				newMaxColor = color;
			}

			m_stack.push_back(NodeState(next_node, color, newMaxColor));

			TRACE(*(mpProcInfo->mpLogger), "PUSH" << " NodeState: " << next_node
					<< "," << color << "," << newMaxColor << ",");
		} else {
			TRACE(*(mpProcInfo->mpLogger), "Coloring " << color
					<< " is not allowed for node " << next_node);
		}
	}
}

void Solver::DistributeData() {

	int buff_size = 0;
	int position = 0;
	int nodes_count = m_graph->GetNodesCount();
	int matrix_size = nodes_count * nodes_count;

	//velikost bufferu se odvadi od velikosti adj_matrix i kdyz ta k odeslani tento buffer NEPOTREBUJE!
	//je to tak jen pro sichr, aby byl buffer dostatecne velky pro pripade nutnosti odeslani tyhle matice coz by v praxi nemelo nikdy nastat
	MPI_Pack_size(matrix_size, MPI_CHAR, MPI_COMM_WORLD, &buff_size);

	mBufferSize = buff_size * BUFFER_MULTIPLIER;

	DEBUG(*mpProcInfo->mpLogger, "Allocating Send Buffer on size = " << mBufferSize);

	mBuffer = new char[mBufferSize];


	MPI_Pack(&nodes_count, 1, MPI_INT, mBuffer, mBufferSize, &position,
			MPI_COMM_WORLD);
	//todo Bcast nahradit za Isend, asi nezvlada velky instance grafu
	INFO(*mpProcInfo->mpLogger, "broadcasting initial state");
	MPI_Bcast(mBuffer, position, MPI_PACKED, ROOT_PROCESS_RANK, MPI_COMM_WORLD);

	position = 0;
	//-----------------------------------

	//rozesle graf
	INFO(*mpProcInfo->mpLogger, "broadcasting graph");
	MPI_Bcast(m_graph->GetMatrix(), matrix_size, MPI_CHAR, ROOT_PROCESS_RANK,
			MPI_COMM_WORLD);

	DEBUG(*(mpProcInfo->mpLogger), "Position is " <<position);

	// ===== expanduje p stavu na zasobnik =====/
	while (!m_stack.empty() && m_stack.size() <= mpProcInfo->mNumProcesses) {
		expandStartTime=MPI_Wtime();
		Expand();
		expandFinishTime=MPI_Wtime();
		userTime+=(expandFinishTime-expandStartTime);
		INFO(*(mpProcInfo->mpLogger),"time to expand p states "<<expandFinishTime-expandStartTime);
	}

	DEBUG(*(mpProcInfo->mpLogger), "Expanded " <<m_stack.size()<<" states!");

	for (unsigned int i = 1; i < mpProcInfo->mNumProcesses; i++) {
		position = 0;

		DEBUG(*(mpProcInfo->mpLogger), "Sending state to client: #"<<i);

		//zapackuje m_coloring
		MPI_Pack(m_colors.data(), m_colors.size(), MPI_INT, mBuffer,
				mBufferSize, &position, MPI_COMM_WORLD);
		NodeState node_state = m_stack.back();

		DEBUG(*(mpProcInfo->mpLogger), "NodeState node="<<node_state.node<<" color="
				<<node_state.color<<" MaxColor="<<node_state.maxColor);

		MPI_Pack(&(node_state.node), 1, MPI_INT, mBuffer, mBufferSize,
				&position, MPI_COMM_WORLD);
		MPI_Pack(&(node_state.color), 1, MPI_INT, mBuffer, mBufferSize,
				&position, MPI_COMM_WORLD);
		MPI_Pack(&(node_state.maxColor), 1, MPI_INT, mBuffer, mBufferSize,
				&position, MPI_COMM_WORLD);

		MPI_Send(mBuffer, position, MPI_PACKED, i, MSG_WORK_BCAST,
				MPI_COMM_WORLD);
		m_stack.pop_back();
	}

}

void Solver::RecvInitData() {

	DEBUG(*mpProcInfo->mpLogger,"RecvInitData!");
	int nodes_count = 0;
	int matrix_size = 0;
	int buff_size = 0;
	int position = 0;
	char tmp_buff[sizeof(int)];

	MPI_Pack_size(1, MPI_INT, MPI_COMM_WORLD, &buff_size);

	MPI_Bcast(tmp_buff, sizeof(int), MPI_PACKED, ROOT_PROCESS_RANK,
			MPI_COMM_WORLD);

	MPI_Unpack(tmp_buff, buff_size, &position, &nodes_count, 1, MPI_INT,
			MPI_COMM_WORLD);

	DEBUG(*mpProcInfo->mpLogger,"Recieved nodes_count = "<< nodes_count);

	m_best_outcome.resize(nodes_count, 0); //iniciuje pole vysledku

	position = 0;
	buff_size = 0;
	//-----------------------------------

	matrix_size = nodes_count * nodes_count;

	//kolik potrebuju na zabaleni m_adj_matrix
	MPI_Pack_size(matrix_size, MPI_CHAR, MPI_COMM_WORLD, &buff_size);

	mBufferSize = buff_size * BUFFER_MULTIPLIER;
	mBuffer = new char[mBufferSize];
	DEBUG(*(mpProcInfo->mpLogger), "Allocating buffer on size = " <<mBufferSize);

	DEBUG(*mpProcInfo->mpLogger,"Recieving graph instance");

	MPI_Bcast(mBuffer, matrix_size, MPI_CHAR, ROOT_PROCESS_RANK, MPI_COMM_WORLD);

	DEBUG(*mpProcInfo->mpLogger,"Graph instance recieved" );

	m_graph = new Graph();
	m_graph->LoadFromArray((bool *) mBuffer, nodes_count);
	mOptimal = m_graph->GetNodesDegree() + 1;

	DEBUG(*mpProcInfo->mpLogger,"Recieved graph with nodes_count = "
			<<m_graph->GetNodesCount()<<", k = "<<m_graph->GetNodesDegree());

	//=============================================================================
	DEBUG(*(mpProcInfo->mpLogger), "Recieving state from root");

	MPI_Status status;

	MPI_Recv(mBuffer, mBufferSize, MPI_PACKED, ROOT_PROCESS_RANK,
			MSG_WORK_BCAST, MPI_COMM_WORLD, &status);

	m_colors.resize(nodes_count, UNDEF_COLOR);
	position = 0;
	MPI_Unpack(mBuffer, mBufferSize, &position, m_colors.data(), nodes_count,
			MPI_INT, MPI_COMM_WORLD);

	NodeState node_state;

	MPI_Unpack(mBuffer, mBufferSize, &position, &(node_state.node), 1, MPI_INT,
			MPI_COMM_WORLD);
	MPI_Unpack(mBuffer, mBufferSize, &position, &(node_state.color), 1,
			MPI_INT, MPI_COMM_WORLD);
	MPI_Unpack(mBuffer, mBufferSize, &position, &(node_state.maxColor), 1,
			MPI_INT, MPI_COMM_WORLD);

	DEBUG(*(mpProcInfo->mpLogger), "Recieved NodeState node="<<node_state.node<<" color="
			<<node_state.color<<" MaxColor="<<node_state.maxColor);

	m_stack.push_back(node_state); //ulozi prijaty stav na zasobnik
}

void Solver::IncNeighbour() {

	mNeighbour = (mNeighbour + 1) % mpProcInfo->mNumProcesses;
	if (mNeighbour == mpProcInfo->mRank) {
		mNeighbour++;
		mNeighbour = mNeighbour % mpProcInfo->mNumProcesses;
	}

}

Solver::NodeState * Solver::SplitStack(int * stack_size) {

	if (m_stack.empty() || m_stack.size() < 2) {// neni dostatek stavu ktere by sly preposlat
		TRACE(*(mpProcInfo->mpLogger), "Splitting stack, nothing to split!");
		return NULL;
	}

	// spocita kolik je uzlu na nejvyssi hladine Top-level nodes
	int tl_nodes_count = 0;
	node_index_t tl_node = m_stack.front().node;

	for (unsigned int i = 0; i < m_stack.size() && m_stack[i].node == tl_node; i++) {
		tl_nodes_count++;
	}

	//vzit pocet, podelit dvema zaokrouhlit nahoru a tolik odeslat
	int split_stack_size = tl_nodes_count / 2;
	if (tl_nodes_count % 2 != 0) {
		++split_stack_size;
	}

	DEBUG(*(mpProcInfo->mpLogger), "Splitting stack, splitted stack size = "<<split_stack_size);

	NodeState * split_stack = new NodeState[split_stack_size];

	for (int i = 0; i < split_stack_size; i++) {
		split_stack[i] = m_stack.front();
		m_stack.erase(m_stack.begin()); // dela pop_front, proste ze dna zasobniku vyhazi ty stavy ktery se budou odesilat
	}
	*stack_size = split_stack_size;
	return split_stack;
}

void Solver::ClearColorSpace(node_index_t starting_node) {
	for (unsigned int i = starting_node; i < m_colors.size(); i++) {
		m_colors[i] = UNDEF_COLOR;
	}
}

bool Solver::IsAllowedColoring(node_index_t node, color_index_t color) const {

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

void Solver::SetGraphInstance(Graph * graph) {
	m_graph = graph;
	mOptimal = m_graph->GetNodesDegree() + 1; // upper bound is k + 1
}

int Solver::GetChromaticNumber() const {
	return mOptimal;
}
