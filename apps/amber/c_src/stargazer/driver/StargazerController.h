/*
 * StargazerController.h
 *
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */

#ifndef STARTGAZERCONTROLLER_H_
#define STARGAZERCONTROLLER_H_

#include <log4cxx/logger.h>
#include <boost/thread.hpp>
#include <boost/ref.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <set>

#include "AmberScheduler.h"
#include "AmberPipes.h"
#include "StargazerDriver.h"
#include "drivermsg.pb.h"
#include "stargazer.pb.h"

struct StargazerSchedulerEntry {

};

class StargazerController: public MessageHandler, AmberSchedulerListener<StargazerSchedulerEntry>  {
public:
	StargazerController(int pipeInFd, int pipeOutFd, const char *confFilename);
	virtual ~StargazerController();

	void handleDataMsg(amber::DriverHdr *driverHdr, amber::DriverMsg *driverMsg);
	void handleClientDiedMsg(int clientID);
	void operator()();

private:
	StargazerDataStruct *_dataStruct;
	StargazerDriver *_stargazerDriver;
	AmberPipes *_amberPipes;
	AmberScheduler<StargazerSchedulerEntry> *_amberScheduler;

	StargazerConfiguration *_configuration;

	std::set<int> _realtimeListeners;
	boost::interprocess::interprocess_mutex _realtimeListenersMutex;

	boost::thread *_schedulerThread;
	boost::thread *_driverThread;
	boost::thread *_pipesThread;

	static log4cxx::LoggerPtr _logger;

	amber::DriverMsg *buildLocalizationDataMsg();
	void sendLocalizationDataMsg(int receiver, int ackNum);
	void handleDataRequestMsg(int sender, int synNum, amber::stargazer_proto::DataRequest *dataRequest);
	void handleSubscribeActionMsg(int sender, amber::stargazer_proto::SubscribeAction *subscribeAction);
	void handleSchedulerEvent(int clientId, StargazerSchedulerEntry *entry);
	void parseConfigurationFile(const char *filename);

};


#endif /* STARGAZERCONTROLLER_H_ */
