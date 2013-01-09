/*
 * NinedofController.h
 *
 *  Created on: 12-11-2012
 *      Author: michal
 */
#ifndef NINEDOFCONTROLLER_H_
#define NINEDOFCONTROLLER_H_

#include <log4cxx/logger.h>
#include <boost/thread.hpp>
#include <boost/ref.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

#include "AmberScheduler.h"
#include "AmberPipes.h"
#include "NinedofDriver.h"
#include "drivermsg.pb.h"
#include "ninedof.pb.h"

struct NinedofSchedulerEntry {
	const bool accel;
	const bool gyro;
	const bool magnet;

	NinedofSchedulerEntry(): accel(false), gyro(false), magnet(false) {
	}

	NinedofSchedulerEntry(bool accel, bool gyro, bool magnet): accel(accel), gyro(gyro), magnet(magnet) {}
};


class NinedofController: public AmberSchedulerListener<NinedofSchedulerEntry>, MessageHandler {
public:
	NinedofController(int pipeInFd, int pipeOutFd, const char *confFilename);
	virtual ~NinedofController();

	void sendSensorDataMsg(int receiver, int ackNum, bool accel, bool gyro, bool magnet);
	void handleDataRequestMsg(int sender, int synNum, amber::ninedof_proto::DataRequest *dataRequest);
	void handleSubscribeActionMsg(int sender, amber::ninedof_proto::SubscribeAction *subscribeAction);
	void handleSchedulerEvent(int clientId, NinedofSchedulerEntry *entry);
	void handleDataMsg(amber::DriverHdr *driverHdr, amber::DriverMsg *driverMsg);
	void handleClientDiedMsg(int clientID);
	void operator()();

private:
	NinedofDataStruct *_dataStruct;
	AmberScheduler<NinedofSchedulerEntry> *_amberScheduler;
	NinedofDriver *_ninedofDriver;
	AmberPipes *_amberPipes;

	NinedofConfiguration *_configuration;

	boost::thread *_schedulerThread;
	boost::thread *_driverThread;

	static log4cxx::LoggerPtr _logger;

	amber::DriverMsg *buildSensorDataMsg(bool accel, bool gyro, bool magnet);
	void parseConfigurationFile(const char *filename);
};

#endif /* NINEDOFCONTROLLER_H_ */
