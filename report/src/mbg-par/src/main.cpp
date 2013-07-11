//============================================================================
// Name        : mbg-seq.cpp
// Author      : 
// Version     :
// Copyright   : 
// Description : MBG, Ansi-style
//============================================================================

#include <iostream>
#include <string>
#include <sstream>
#include <stdlib.h>
#include <unistd.h>
#include <fstream>

#include "mpi.h"

#include "Graph.h"
#include "Solver.h"
#include "ProcessInfo.h"
#include "utils.h"

#define LOG_FORMAT "%p %t [%l] %T %n [%f@%L] - "

using namespace std;

struct Config {
	int nodes_count;
	int edges_count;
	string in_file;
	string log_file_prefix;
};

void finalize() {
	MPI_Finalize();
}

//tahle metoda vytahne z parametru nazev souboru se vstupy
Config parse_opts(int argc, char * argv[]) {
	struct Config conf = { 0, 0, "", "" };
	char opt = 0;
	while ((opt = getopt(argc, argv, "v:e:i:l:")) != -1) {
		switch (opt) {
		case 'i': // param udavajici cestu k vstupnimu souboru
			conf.in_file = optarg;
			break;
		case 'l': // param udavajici prefix logovaciho souboru ze ktereho se pak generujou nazvy lgofajlu pr vs procaky
			conf.log_file_prefix = optarg;
			break;
		case 'v': // param udavajici pocet uzlu,
			//pouziti pouze pokud bychom chteli volat generator primoz tohoto programu napr. pomoci exec()
			conf.nodes_count = atoi(optarg);
			break;
		case 'e': // param udavajici pocet hran
			conf.edges_count = atoi(optarg);
			break;
		default: /* '?' */
			cerr << "Usage: " << argv[0] << " -i IN_FILE" << endl;
			cerr << "Usage: " << argv[0] << " -v VERTICES_COUNT -e EDGES_COUNT"
					<< endl;
			finalize();
			exit(EXIT_FAILURE);
		}
	}
	return conf;
}

void init_loggers(string log_filename, int processor_number, int processor_rank) {
	cout << "logger filename: " << log_filename << endl;
	Logger *logger_test = 0;

	filebuf *obuf = new filebuf();
	obuf->open(log_filename.c_str(), ios_base::out | ios_base::trunc);
	if (obuf->is_open() == false) {
		cerr << "could not open file: " << log_filename << endl;
		exit(1);
	}

	logger_test = Logger::get_logger("MBG", LOG_FORMAT, obuf);
	logger_test->set_processor_count(processor_number);
	logger_test->set_processor_number(processor_rank);
	ProcessInfo * p_proc_info = ProcessInfo::GetInfo();
	p_proc_info->mpLogger = logger_test;
}

/* we want to log on the standart output ...
 */
void init_loggers(int processor_number, int processor_rank) {
	Logger *logger_test = 0;
	logger_test = Logger::get_logger("MBG", LOG_FORMAT);
	logger_test->set_processor_count(processor_number);
	logger_test->set_processor_number(processor_rank);

	ProcessInfo * p_proc_info = ProcessInfo::GetInfo();
	p_proc_info->mpLogger = logger_test;
}

string get_filename(string prefix) {
	ProcessInfo * p_proc_info = ProcessInfo::GetInfo();
	stringstream ss1;

	ss1 << "log_mbg-par-";
	ss1 << prefix;
	ss1 << "-p";
	ss1 << p_proc_info->mRank;
	ss1 << "of";
	ss1 << p_proc_info->mNumProcesses;
	ss1 << ".log";

	string log_filename = ss1.str();
	return log_filename;
}

int main(int argc, char * argv[]) {

	// ============== MPI INIT ====================//
	MPI_Init(&argc, &argv);

	ProcessInfo * p_proc_info = ProcessInfo::GetInfo();

	// ulozi cislo aktualniho procesu do singletonu
	MPI_Comm_rank(MPI_COMM_WORLD, (int *) &(p_proc_info->mRank));

	//ulozi celkovy pocet procesu do sing.
	MPI_Comm_size(MPI_COMM_WORLD, (int *) &(p_proc_info->mNumProcesses));

	// =============== LOAD CONFIG ==============//
	Config conf = parse_opts(argc, argv);
	Graph * g = NULL;

	// ==================== LOGGING INIT========================//

	//init_loggers(p_proc_info->mNumProcesses, p_proc_info->mRank);
	//INFO(*(p_proc_info->mpLogger), "Logging into stdout.");
	init_loggers(get_filename(conf.log_file_prefix),
			p_proc_info->mNumProcesses, p_proc_info->mRank);
	INFO(*(p_proc_info->mpLogger), "Logging into file: "<<get_filename(conf.log_file_prefix));

	DEBUG(*(p_proc_info->mpLogger),
			"Logging succesfully initialized: Rank = "<<p_proc_info->mRank);
	//=========================================================//

	// =============== LOAD DATA ================//
	if (p_proc_info->mRank == ROOT_PROCESS_RANK) {
		ifstream in_file(conf.in_file.c_str(), fstream::in);

		if (in_file.fail()) {
			cerr << "Input file " << conf.in_file << " doesn't exist!" << endl;
			finalize();
			return 1;
		}

		g = new Graph();
		g->LoadFromFile(in_file);
	}

	// ================ MBG SLOVER INIT ==========//
	Solver * sol = new Solver();

	if (p_proc_info->mRank == ROOT_PROCESS_RANK) {
		sol->SetGraphInstance(g);
	}

	double start_time, end_time;

	MPI_Barrier( MPI_COMM_WORLD);
	start_time = MPI_Wtime();

	sol->Solve();

	MPI_Barrier(MPI_COMM_WORLD);
	end_time = MPI_Wtime();


	if (p_proc_info->mRank == ROOT_PROCESS_RANK) {
		cout<<"total time "<<end_time - start_time<<endl;
		int time=end_time - start_time;
		INFO(*(p_proc_info->mpLogger), "Chromatic number is: " << sol->GetChromaticNumber());
		INFO(*(p_proc_info->mpLogger), "Total run time is: " << time << " sec.");
		sol->PrintOutcome(cout);
	}
	delete sol;

	finalize();
	return 0;
}
