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
#include <boost/interprocess/managed_shared_memory.hpp>
#include <log4cxx/logger.h>

#include "RoboclawCommon.h"

#define UART_SPEED B38400
#define ROBOCLAW_PORT "/dev/ttyO3"

class RoboclawDriver {

public:
	RoboclawDriver(RoboclawConfiguration *configuration);
	virtual ~RoboclawDriver();

	void initializeDriver();
	void readCurrentSpeed(__u8 roboclawAddress, CurrentSpeedStruct *currentSpeedStruct);
	void sendMotorsEncoderCommand(__u8 roboclawAddress, MotorCommandStruct *m1, MotorCommandStruct *m2);
	void stopMotors();

	boost::interprocess::interprocess_mutex driverReadyMutex;
	boost::interprocess::interprocess_condition driverIsNotReady;
	boost::interprocess::interprocess_mutex serialPortMutex;

	bool driverReady;

private:

	static log4cxx::LoggerPtr _logger;

	int _fd;
	RoboclawConfiguration *_configuration;



};

#endif /* STARGAZERDRIVER_H_ */
