/*
 * NinedofDriver.h
 *
 *  Created on: 29-10-2012
 *      Author: michal
 */

#ifndef NINEDOFDRIVER_H_
#define NINEDOFDRIVER_H_

#include <boost/interprocess/sync/interprocess_condition.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <log4cxx/logger.h>

#include "NinedofCommon.h"

#define I2C_PORT "/dev/i2c-4"

#define ACCEL_ADDRESS 0x19
#define ACCEL_CTRL_REG1_A 0x20
#define ACCEL_AXES_REG 0x28

#define MAGNET_ADDRESS 0x1E
#define MAGNET_M_REG_M 0x02
#define MAGNET_AXES_REG 0x03

#define GYRO_ADDRESS 0x6B
#define GYRO_CTRL_REG1 0x20
#define GYRO_AXES_REG 0x28


class NinedofDriver {

public:
	NinedofDriver(NinedofConfiguration *_configuration);
	virtual ~NinedofDriver();

	NinedofDataStruct *getDataStruct();

	void operator()();
	void lockUntilDriverReady();

	boost::interprocess::interprocess_mutex driverReadyMutex;
	boost::interprocess::interprocess_condition driverIsNotReady;
	boost::interprocess::interprocess_mutex dataMutex;
	boost::interprocess::interprocess_condition noNeedToGetData;
	boost::interprocess::interprocess_condition dataNotReady;

	bool needToGetData;
	bool dataReady;
	bool driverReady;

private:
	NinedofDataStruct _dataStruct;
	NinedofConfiguration *_configuration;
	int _fd;

	static log4cxx::LoggerPtr logger;

	void driverLoop();
	void initializeDriver();

	//TODO: remove?
	friend class DataRequestScopedLock;
};

#endif /* NINEDOFDRIVER_H_ */

