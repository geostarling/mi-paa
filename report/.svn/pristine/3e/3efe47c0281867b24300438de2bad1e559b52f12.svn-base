/*
 * ProcessInfo.h
 *
 *  Created on: Nov 6, 2010
 *      Author: watanabe
 */
#ifndef PROCESSINFO_H_
#define PROCESSINFO_H_

#include "logger/logger.hpp"

class ProcessInfo {
public:
	static ProcessInfo * GetInfo();
	virtual ~ProcessInfo();

	int mRank;
	unsigned int mNumProcesses;

	Logger * mpLogger;

private:
	ProcessInfo();
};

#endif /* PROCESSINFO_H_ */
