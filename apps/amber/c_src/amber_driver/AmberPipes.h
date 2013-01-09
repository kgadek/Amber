/*
 * NinedofMain.h
 *
 *  Created on: 02-11-2012
 *      Author: michal
 */

#ifndef AMBERPIPES_H_
#define AMBERPIPES_H_

#include <exception>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include <log4cxx/logger.h>
#include "drivermsg.pb.h"

#define BUF_SIZE 512
#define MAX_EVENTS 2

class PipeException: public std::exception {};

class MessageHandler {
public:
	virtual ~MessageHandler() {};
	virtual void handleDataMsg(amber::DriverHdr *driverHdr, amber::DriverMsg *driverMsg) = 0;
	virtual void handleClientDiedMsg(int clientID) = 0;
};

class AmberPipes {
public:
	AmberPipes(MessageHandler *messageHandler, int pipeInFd, int pipeOutFd);
	virtual ~AmberPipes();

	void operator()();
	void readMsgFromPipe();
	void writeMsgToPipe(amber::DriverHdr *driverMsgHeader, amber::DriverMsg *driverMsg);
	void handlePingMsg(amber::DriverHdr *driverMsgHeader, amber::DriverMsg *driverMsg);

private:
	MessageHandler *_messageHandler;

	int _pipeInFd, _pipeOutFd;
	unsigned char _pipeInBuffer[BUF_SIZE];
	unsigned char _pipeOutBuffer[BUF_SIZE];

	boost::interprocess::interprocess_mutex _pipeWriteMutex;

	static log4cxx::LoggerPtr _logger;

	void runProcess();

	int readExact(int len);
	int writeExact(int len);
};


#endif /* AMBERPIPES_H_ */
