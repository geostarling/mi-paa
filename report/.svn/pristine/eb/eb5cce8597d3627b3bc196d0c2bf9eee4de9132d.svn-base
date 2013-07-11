//============================================================================
// Name        : mbg-seq.cpp
// Author      : 
// Version     :
// Copyright   : 
// Description : MBG, Ansi-style
//============================================================================

#include <iostream>
#include <string>
#include <stdlib.h>
#include <unistd.h>
#include <fstream>
#include "Graph.h"
#include "Solver.h"
#include "logger/logger.hpp"

#define LOG_FORMAT "%p %t [%l] %T %n [%f@%L] - "

#include <time.h>

//#define DEBUG

using namespace std;

Logger *logger_test = 0;

struct Config {
	int nodes_count;
	int edges_count;
	string in_file;
};

//tahle metoda vytahne z parametru nazev souboru se vstupy
Config parse_opts(int argc, char * argv[]) {
	struct Config conf = { 0, 0, "" };
	char opt = 0;
	while ((opt = getopt(argc, argv, "n:e:i:")) != -1) {
		switch (opt) {
		case 'i': // param udavajici cestu k vstupnimu souboru
			conf.in_file = optarg;
			break;
		case 'n': // param udavajici pocet uzlu,
			//pouziti pouze pokud bychom chteli volat generator primoz tohoto programu napr. pomoci exec()
			conf.nodes_count = atoi(optarg);
			break;
		case 'e': // param udavajici pocet hran
			conf.edges_count = atoi(optarg);
			break;
		default: /* '?' */
			cerr << "Usage: " << argv[0] << " -i IN_FILE" << endl;
			cerr << "Usage: " << argv[0] << " -n NODES_COUNT -e EDGES_COUNT"
					<< endl;
			exit(EXIT_FAILURE);
		}
	}
	return conf;
}

void init_loggers(string log_filename, int processor_number, int processor_rank) {
	cout << "logger filename: " << log_filename << endl;

	filebuf *obuf = new filebuf();
	obuf->open(log_filename.c_str(), ios_base::out | ios_base::trunc);
	if (obuf->is_open() == false) {
		cerr << "could not open file: " << log_filename << endl;
		exit(1);
	}

	logger_test = Logger::get_logger("MBG", LOG_FORMAT, obuf);
	logger_test->set_processor_count(processor_number);
	logger_test->set_processor_number(processor_rank);

}

/* we want to log on the standart output ...
 */
void init_loggers(int processor_number, int processor_rank) {

	logger_test = Logger::get_logger("MBG", LOG_FORMAT);
	logger_test->set_processor_count(processor_number);
	logger_test->set_processor_number(processor_rank);

}

string get_filename(int n, int k) {

	stringstream ss1;
	ss1 << "log_mbg-seq-n";
	ss1 << n;
	ss1 << "k";
	ss1 << k;
	ss1 << ".log";
	// log filename looks like: log_mbg-par-p<num-of-proc>-<processor-rank>
	string log_filename = ss1.str();
	return log_filename;
}

int main(int argc, char * argv[]) {
	Config conf = parse_opts(argc, argv);

	ifstream in_file(conf.in_file.c_str(), fstream::in);

	if (in_file.fail()) {
		cerr << "Input file " << conf.in_file << " doesn't exist!" << endl;
		return 1;
	}

	//=========================================================//

	Graph * g = new Graph();
	g->LoadFromFile(in_file);

	// ==================== LOGGING INIT========================//

	//init_loggers(0, 0);
	//INFO(*logger_test, "Logging into stdout.");
	init_loggers(get_filename(g->GetNodesCount(), g->GetNodesDegree()), 0, 0);
	INFO(*logger_test, "Logging into file: ");

	// Vypise matici sousendosti na konzoli //zakomentovat
#ifdef DEBUGGER
	g->Printself(cout);
#endif

	time_t startTime = time(NULL);
	Solver * sol = new Solver();
	INFO(*logger_test, "Chroma nuber = " << sol->Solve(g));
	time_t endTime = time(NULL);

	INFO(*logger_test, "Total run time is: " << (endTime - startTime) << " sec.");

	sol->PrintOutcome(cout);

	delete sol;
	delete g;
	return 0;
}
