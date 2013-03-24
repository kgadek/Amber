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
#include "boost/date_time/posix_time/posix_time.hpp"
#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>

#include "RoboclawCommon.h"
#include "RoboclawDriver.h"
#include "roboclaw_lib/roboclaw_lib.h"
#include "roboclaw_lib/uart.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace log4cxx;
using namespace boost::posix_time;
LoggerPtr RoboclawDriver::_logger (Logger::getLogger("Roboclaw.Driver"));

RoboclawDriver::RoboclawDriver(RoboclawConfiguration *configuration):
		driverReady(false), _configuration(configuration) {

}

RoboclawDriver::~RoboclawDriver() {
	uart_close(_fd);
}

void RoboclawDriver::initializeDriver() {
	scoped_lock<interprocess_mutex> lock(driverReadyMutex);

	_fd = uart_open(_configuration->uart_port.c_str());
	if (_fd == -1) {
		LOG4CXX_FATAL(_logger, "Unable to open uart port: " << _configuration->uart_port);
		return;
	}

	speed_t uart_speed;
	switch(_configuration->uart_speed) {

	case 2400:
		uart_speed = B2400;
		break;

	case 9600:
		uart_speed = B9600;
		break;

	case 19200:
		uart_speed = B19200;
		break;

	case 38400:
		uart_speed = B38400;
		break;

	default:
		LOG4CXX_FATAL(_logger, "Unknown uart speed: " << _configuration->uart_speed << ". Aborting.");
		return;
	}

	LOG4CXX_INFO(_logger, "Initializing driver, port: " << _configuration->uart_port.c_str()
			<< ", baud: " << uart_speed);
	uart_init(_fd, uart_speed);

	// Setting motors constants
	rc_set_pid_consts_m1(_fd, _configuration->front_rc_address, _configuration->motors_d_const,
			_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);
	rc_set_pid_consts_m2(_fd, _configuration->front_rc_address, _configuration->motors_d_const,
				_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);

	rc_set_pid_consts_m1(_fd, _configuration->rear_rc_address, _configuration->motors_d_const,
				_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);
	rc_set_pid_consts_m2(_fd, _configuration->rear_rc_address, _configuration->motors_d_const,
				_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);

	driverReady = true;
	driverIsNotReady.notify_all();
}

void RoboclawDriver::readCurrentSpeed(__u8 roboclawAddress, CurrentSpeedStruct *currentSpeedStruct) {
	scoped_lock<interprocess_mutex> lock(serialPortMutex);

	__u8 m1_direction, m2_direction;

	rc_read_speed_m1(_fd, roboclawAddress, &currentSpeedStruct->m1_speed, &m1_direction);
	currentSpeedStruct->m1_direction = m1_direction == 0 ? false : true;

	rc_read_speed_m2(_fd, roboclawAddress, &currentSpeedStruct->m2_speed, &m2_direction);
	currentSpeedStruct->m2_direction = m2_direction == 0 ? false : true;

}

void RoboclawDriver::stopMotors() {
	LOG4CXX_INFO(_logger, "Stopping motors.");
	rc_drive_forward(_fd, _configuration->front_rc_address, 0);
	rc_drive_forward(_fd, _configuration->rear_rc_address, 0);

}

void RoboclawDriver::sendMotorsEncoderCommand(MotorsCommandStruct *mc) {
	scoped_lock<interprocess_mutex> lock(serialPortMutex);


	if (mc != NULL) {
		rc_drive_speed(_fd, _configuration->front_rc_address, mc->frontLeftSpeed, mc->frontRightSpeed);
		rc_drive_speed(_fd, _configuration->rear_rc_address, mc->rearLeftSpeed, mc->rearRightSpeed);
		LOG4CXX_DEBUG(_logger, "rc_drive_speed, fl: " << mc->frontLeftSpeed << ", fr: " << mc->frontRightSpeed << ", rl: " << mc->rearLeftSpeed << ", rr: " << mc->rearRightSpeed);
		return;
	}

}
