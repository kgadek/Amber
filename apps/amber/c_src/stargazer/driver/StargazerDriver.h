/*
 * StargazerDriver.h
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */

#ifndef STARGAZERDRIVER_H_
#define STARGAZERDRIVER_H_

#include <boost/interprocess/sync/interprocess_condition.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <log4cxx/logger.h>

#include "StargazerCommon.h"

#define UART_PORT "/dev/ttyO0"
#define UART_SPEED B115200

#define LINE_BUFFER_LEN 32
#define LINE_DELIMITER '`'

class StargazerDriver {

public:
	StargazerDriver(StargazerConfiguration *configuration);
	virtual ~StargazerDriver();

	StargazerDataStruct *getDataStruct();

	void operator()();

	boost::interprocess::interprocess_mutex driverReadyMutex;
	boost::interprocess::interprocess_condition driverIsNotReady;
	boost::interprocess::interprocess_mutex dataMutex;
	boost::interprocess::interprocess_condition dataNotReady;

	bool dataReady;
	bool driverReady;

private:
	StargazerDataStruct _dataStruct;
	int _fd;
	FILE *_file;

	boost::system_time _driverStartTime;

	StargazerConfiguration *_configuration;

	static log4cxx::LoggerPtr _logger;
	void driverLoop();
	void initializeDriver();
};

#endif /* STARGAZERDRIVER_H_ */
