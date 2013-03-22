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
	rc_set_pid_consts_m1(_fd, _configuration->motor1_address, _configuration->motors_d_const,
			_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);
	rc_set_pid_consts_m2(_fd, _configuration->motor1_address, _configuration->motors_d_const,
				_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);

	rc_set_pid_consts_m1(_fd, _configuration->motor2_address, _configuration->motors_d_const,
				_configuration->motors_p_const, _configuration->motors_i_const, _configuration->motors_max_qpps);
	rc_set_pid_consts_m2(_fd, _configuration->motor2_address, _configuration->motors_d_const,
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
	rc_drive_forward(_fd, _configuration->motor1_address, 0);
	rc_drive_forward(_fd, _configuration->motor2_address, 0);

}

void RoboclawDriver::sendMotorsEncoderCommand(__u8 roboclawAddress, MotorCommandStruct *m1, MotorCommandStruct *m2) {
	scoped_lock<interprocess_mutex> lock(serialPortMutex);

	// speed only set for both motors
	if (m1 != NULL && !m1->accel_set && !m1->distance_set
			&& m2 != NULL && !m2->accel_set && !m2->distance_set) {

		rc_drive_speed(_fd, roboclawAddress, m1->speed, m2->speed);
		LOG4CXX_DEBUG(_logger, "rc_drive_speed, roboclawAddress: " << (int)roboclawAddress << ", m1_s: " << m1->speed << ", m2_s: " << m2->speed);
		return;
	}

	if (m1 != NULL) {

		//no accel or distance
		if (!m1->accel_set && !m1->distance_set) {
			rc_drive_m1_speed(_fd, roboclawAddress, m1->speed);
			LOG4CXX_DEBUG(_logger, "rc_drive_m1_speed:: " << m1->speed);
		}


		// accel only set
		else if (m1->accel_set && !m1->distance_set) {
			rc_drive_m1_speed_accel(_fd, roboclawAddress, m1->speed, m1->accel);
			LOG4CXX_DEBUG(_logger, "rc_drive_m1_speed_accel"
					", s: " << m1->speed << ", a: " << m1->accel);
		}

		// distance only set
		else if (m1->distance_set && !m1->accel_set) {
			rc_buffered_m1_drive_speed_dist(_fd, roboclawAddress, m1->speed, m1->distance, m1->buffered);
			LOG4CXX_DEBUG(_logger, "rc_buffered_m1_drive_speed_dist"
					", s: " << m1->speed << ", d: " << m1->distance << ", buff: " << m1->distance);
		}

		// accel and distace set
		else if (m1->accel_set && m1->distance_set) {
			rc_buffered_m1_drive_speed_accel_dist(_fd, roboclawAddress, m1->accel, m1->speed, m1->distance, m1->buffered);
			LOG4CXX_DEBUG(_logger, "rc_buffered_m1_drive_speed_accel_dist" <<
					", a: " << m1->accel << ", s: " << m1->speed << ", d: " << m1->distance << ", buff: " << m1->buffered);
		}

	}

	if (m2 != NULL) {

		//no accel or distance
		if (!m2->accel_set && !m2->distance_set) {
			rc_drive_m2_speed(_fd, roboclawAddress, m2->speed);
			LOG4CXX_DEBUG(_logger, "rc_drive_m2_speed:: " << m2->speed);
		}


		// accel only set
		else if (m2->accel_set && !m2->distance_set) {
			rc_drive_m2_speed_accel(_fd, roboclawAddress, m2->speed, m2->accel);
			LOG4CXX_DEBUG(_logger, "rc_drive_m2_speed_accel"
					", s: " << m2->speed << ", a: " << m2->accel);
		}

		// distance only set
		else if (m2->distance_set && !m2->accel_set) {
			rc_buffered_m2_drive_speed_dist(_fd, roboclawAddress, m2->speed, m2->distance, m2->buffered);
			LOG4CXX_DEBUG(_logger, "rc_buffered_m2_drive_speed_dist"
					", s: " << m2->speed << ", d: " << m2->distance << ", buff: " << m2->distance);
		}

		// accel and distace set
		else if (m2->accel_set && m2->distance_set) {
			rc_buffered_m2_drive_speed_accel_dist(_fd, roboclawAddress, m2->accel, m2->speed, m2->distance, m2->buffered);
			LOG4CXX_DEBUG(_logger, "rc_buffered_m2_drive_speed_accel_dist" <<
					", a: " << m2->accel << ", s: " << m2->speed << ", d: " << m2->distance << ", buff: " << m2->buffered);
		}

	}

}
