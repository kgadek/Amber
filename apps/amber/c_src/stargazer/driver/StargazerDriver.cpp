/*
 * StargazerDriver.cpp
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */
#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <linux/types.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <log4cxx/logger.h>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>


#include "StargazerCommon.h"
#include "StargazerDriver.h"
#include "uart/uart.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace log4cxx;

LoggerPtr StargazerDriver::_logger (Logger::getLogger("Stargazer.Driver"));

StargazerDriver::StargazerDriver(StargazerConfiguration *configuration):
		dataReady(false), driverReady(false), _configuration(configuration) {

}

StargazerDriver::~StargazerDriver() {
#ifndef MOCK
	fclose(_file);
	uart_close(_fd);
#endif
}

StargazerDataStruct *StargazerDriver::getDataStruct() {
	return &_dataStruct;
}

void StargazerDriver::operator()() {
	LOG4CXX_INFO(_logger, "Driver thread started.");

	// initialize driver
	{
		scoped_lock<interprocess_mutex> lock(driverReadyMutex);

		// TODO: initialization exception
		initializeDriver();

		LOG4CXX_INFO(_logger, "Driver process ready");
		driverReady = true;
		driverIsNotReady.notify_all();
	}

	driverLoop();
}

void StargazerDriver::initializeDriver() {

#ifndef MOCK

	speed_t uart_speed;
		switch(_configuration->uart_speed) {

		case 19200:
			uart_speed = B19200;
			break;

		case 57600:
			uart_speed = B19200;
			break;

		case 115200:
			uart_speed = B115200;
			break;

		default:
			LOG4CXX_FATAL(_logger, "Unknown uart speed: " << _configuration->uart_speed << ". Aborting.");
			return;
		}

	LOG4CXX_INFO(_logger, "Initializing driver, port: " << _configuration->uart_port.c_str()
				<< ", baud: " << uart_speed);

	_fd = uart_open(_configuration->uart_port.c_str());

	if (_fd == -1) {
		LOG4CXX_FATAL(_logger, "Cannot open uart port. Aborting.");
		return;
	}


	uart_init(_fd, _configuration->uart_speed);
	_file = fdopen(_fd, "r");

	if (_file == NULL) {
		LOG4CXX_FATAL(_logger, "Failed to fdopen");
		return;
	}

#endif
}

void StargazerDriver::driverLoop() {

#ifdef MOCK
	srand(time(NULL));

	while (1) {

		thread::sleep(get_system_time() + posix_time::milliseconds((rand() % 50) + 50));

		{
			scoped_lock<interprocess_mutex> lock(dataMutex);

			_dataStruct.x_pos = ((float)((rand() % 20000) - 100))/100.000;
			_dataStruct.y_pos = ((float)((rand() % 20000) - 100))/100.000;
			_dataStruct.z_pos = ((float)((rand() % 20000) - 100))/100.000;
			_dataStruct.angle = ((float)((rand() % 36000) - 180))/100.000;

			// data ready
			dataNotReady.notify_all();
		}
	}

#else
	char *line = (char *) malloc(sizeof(char) * LINE_BUFFER_LEN);
	size_t line_len = LINE_BUFFER_LEN;
	ssize_t received;

	while (1) {
		received = getdelim(&line, &line_len, LINE_DELIMITER, _file);
		if (received == -1) {
			continue;
		} else if (line_len > 0) {
			scoped_lock<interprocess_mutex> lock(dataMutex);

			if (sscanf(line, "~^I%i|%f|%f|%f|%f`",
					&_dataStruct.marker_id, &_dataStruct.angle, &_dataStruct.x_pos, &_dataStruct.y_pos, &_dataStruct.z_pos) == 5) {
			}

			// data ready
			dataNotReady.notify_all();
		}
	}

#endif


}

