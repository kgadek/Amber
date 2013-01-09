/*
 * NinedofDriver.cpp
 *
 *  Created on: 29-10-2012
 *      Author: michal
 */

#include <cstdio>
#include <cstdlib>
#include <cerrno>
#include <linux/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <log4cxx/logger.h>

#include "NinedofCommon.h"
#include "NinedofDriver.h"
#include "i2c/i2c.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace log4cxx;

LoggerPtr NinedofDriver::logger (Logger::getLogger("Ninedof.Driver"));


NinedofDriver::NinedofDriver(NinedofConfiguration *configuration):
	driverReady(false), dataReady(false), needToGetData(false), _configuration(configuration) {
}

NinedofDriver::~NinedofDriver() {

}

NinedofDataStruct *NinedofDriver::getDataStruct() {
	return &_dataStruct;
}

void NinedofDriver::operator()() {
	LOG4CXX_INFO(logger, "Driver thread started.");

	// initialize driver
	{
		scoped_lock<interprocess_mutex> lock(driverReadyMutex);

		initializeDriver();

		LOG4CXX_INFO(logger, "Driver process ready");
		driverReady = true;
		driverIsNotReady.notify_all();
	}


	driverLoop();
}

void NinedofDriver::lockUntilDriverReady() {
	scoped_lock<interprocess_mutex> lock(driverReadyMutex);
	driverIsNotReady.wait(lock);
}


void NinedofDriver::initializeDriver() {

#ifndef MOCK

	_fd = i2c_open(_configuration->i2c_port.c_str());
	if (_fd == -1) {
		LOG4CXX_FATAL(logger, "Unable to open i2c bus, port: " << _configuration->i2c_port);
		return;
	}

	LOG4CXX_INFO(logger, "Opened I2C bus.");

	__u8 tmp;

	/*
	* Accel configuration
	*/

	/* CTRL_REG1_A: normal (1.344 kHz), all axes enabled */
	tmp = 0x97;
	if(i2c_write(_fd, ACCEL_ADDRESS, ACCEL_CTRL_REG1_A, 1, &tmp) != 1) {
		LOG4CXX_FATAL(logger, "Unable to write CTRL_REG1_A to accel");
	}


	/*
	* Compass configuration
	*/

	/* CRA_REG_M update rate set to ... */
	tmp = 0x00;
	if(i2c_write(_fd, MAGNET_ADDRESS, MAGNET_M_REG_M, 1, &tmp) != 1) {
		LOG4CXX_FATAL(logger, "Unable to write CRA_REG_M to magnet");
	}

	/*
	* Gyro configuration
	*/

	/* CRA_REG_M update rate set to 800Hz, 110 cut-off */
	tmp = 0xFF;
	if(i2c_write(_fd, GYRO_ADDRESS, GYRO_CTRL_REG1, 1, &tmp) != 1) {
		LOG4CXX_FATAL(logger, "Unable to write CTRL_REG1_A to gyro");
	}

#endif
}

void NinedofDriver::driverLoop() {

#ifdef MOCK
	srand(time(NULL));
#else
	__u8 accel_axes[6];
	__u8 magnet_axes[6];
	__u8 gyro_axes[6];
#endif

	while (1) {
		scoped_lock<interprocess_mutex> lock(dataMutex);

		while (!needToGetData) {
			noNeedToGetData.wait(lock);
		}

#ifdef MOCK

		LOG4CXX_DEBUG(logger, "Randomizing new data, accel x_axis: " << _dataStruct.accel.x_axis);

		_dataStruct.accel.x_axis = (rand() % 2000) - 1000;
		_dataStruct.accel.y_axis = (rand() % 2000) - 1000;
		_dataStruct.accel.z_axis = (rand() % 2000) - 1000;

		_dataStruct.gyro.x_axis = (rand() % 2000) - 1000;
		_dataStruct.gyro.y_axis = (rand() % 2000) - 1000;
		_dataStruct.gyro.z_axis = (rand() % 2000) - 1000;

		_dataStruct.magnet.x_axis = (rand() % 2000) - 1000;
		_dataStruct.magnet.y_axis = (rand() % 2000) - 1000;
		_dataStruct.magnet.z_axis = (rand() % 2000) - 1000;

#else

		LOG4CXX_DEBUG(logger, "Reading data from 9dof sensor");

		if(i2c_read(_fd, ACCEL_ADDRESS, ACCEL_AXES_REG, 6, accel_axes) != 6) {
			LOG4CXX_ERROR(logger, "Unable to read from accel device");
		} else {
			_dataStruct.accel.x_axis = accel_axes[1]<<8 | accel_axes[0];
			_dataStruct.accel.y_axis = accel_axes[3]<<8 | accel_axes[2];
			_dataStruct.accel.z_axis = accel_axes[5]<<8 | accel_axes[4];
		}

		if(i2c_read(_fd, GYRO_ADDRESS, GYRO_AXES_REG, 6, gyro_axes) != 6) {
			LOG4CXX_ERROR(logger, "Unable to read from gyro device");
		} else {
			_dataStruct.gyro.x_axis = gyro_axes[1]<<8 | gyro_axes[0];
			_dataStruct.gyro.y_axis = gyro_axes[3]<<8 | gyro_axes[2];
			_dataStruct.gyro.z_axis = gyro_axes[5]<<8 | gyro_axes[4];
		}


		if(i2c_read(_fd, MAGNET_ADDRESS, MAGNET_AXES_REG, 6, magnet_axes) != 6) {
			LOG4CXX_ERROR(logger, "Unable to read from magnet device");
		} else {
			_dataStruct.magnet.x_axis = magnet_axes[1]<<8 | magnet_axes[0];
			_dataStruct.magnet.y_axis = magnet_axes[3]<<8 | magnet_axes[2];
			_dataStruct.magnet.z_axis = magnet_axes[5]<<8 | magnet_axes[4];
		}

#endif


		// data ready
		needToGetData = false;
		dataNotReady.notify_all();
	}
}
