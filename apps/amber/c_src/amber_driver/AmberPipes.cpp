/*
 * NinedofMain.cpp
 *
 *  Created on: 02-11-2012
 *      Author: michal
 */

#include <cstdio>
#include <sys/epoll.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>
#include "boost/date_time/posix_time/posix_time.hpp"

#include <log4cxx/logger.h>
#include <log4cxx/basicconfigurator.h>

#include "AmberPipes.h"
#include "drivermsg.pb.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::posix_time;
using namespace log4cxx;
using namespace amber;

LoggerPtr AmberPipes::_logger (Logger::getLogger("Amber.Pipes"));

AmberPipes::AmberPipes(MessageHandler *receiver, int pipeInFd, int pipeOutFd):
		_messageHandler(receiver), _pipeInFd(pipeInFd), _pipeOutFd(pipeOutFd) {

}

AmberPipes::~AmberPipes() {

}


void AmberPipes::operator()() {
	LOG4CXX_INFO(_logger, "Pipes thread started.");
	runProcess();
}

void AmberPipes::runProcess() {

	int epollfd, nfds;
	struct epoll_event ev, events[MAX_EVENTS];

	epollfd = epoll_create(MAX_EVENTS);
	if (epollfd == -1) {
		LOG4CXX_FATAL(_logger, "Cannot create epoll instance.");
		return;
	}

	// Add pipe to epoll set
	ev.events = EPOLLIN;
	ev.data.fd = _pipeInFd;
	if (epoll_ctl(epollfd, EPOLL_CTL_ADD, _pipeInFd, &ev) == -1) {
		LOG4CXX_FATAL(_logger, "Cannot manipulate epoll instance.");
		return;
	}

	int read_count = 0;
	ptime lastTime, actTime;
	ptime newMessageTime;
	time_duration time_diff;
	lastTime = boost::get_system_time();

	// TODO: exiting
	while (1) {
		nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);

		if (nfds == -1) {
			if (errno == EINTR) {
				continue;
			}

			LOG4CXX_FATAL(_logger, "Error occured in epoll_wait.");
			return;
		}

		for (int n = 0; n < nfds; n++) {

			// If there is data in pipe
			if (events[n].data.fd == _pipeInFd) {
				try {
					//LOG4CXX_DEBUG(_logger, "Reading data from pipe.");
					newMessageTime = boost::get_system_time();

					readMsgFromPipe();
					//printf("after readMsgFromPipe(): %lld us\n", (get_system_time() - newMessageTime).total_microseconds());

					read_count++;
					if (read_count == 100){
						actTime = boost::get_system_time();
						time_diff = actTime - lastTime;
						lastTime = actTime;

						//printf("read_count: %lld us\n", time_diff.total_microseconds());
						read_count = 0;
					}
				} catch (PipeException &e) {
					continue;
				}
			}
		}
	}
}

int AmberPipes::readExact(int len) {
	int in, got = 0;

	do {
		in = read(_pipeInFd, _pipeInBuffer + got, len - got);
		if (in <= 0) {
			return got;
		}

		got += in;

	} while (got < len);

	return len;
}

int AmberPipes::writeExact(int len) {

	LOG4CXX_DEBUG(_logger, "Writing " << len << " bytes to pipe.");

	int out, written = 0;

	do {
		out = write(_pipeOutFd, _pipeOutBuffer + written, len - written);
	    if (out <= 0) {
	    	return written;
	    }
	    written += out;

	} while (written < len);

	return len;
}

void AmberPipes::readMsgFromPipe() {
	int len;

	if (readExact(2) != 2) {
		//LOG4CXX_ERROR(_logger, "Cannot read header length from pipe.");
		throw PipeException();
	}

	len = (_pipeInBuffer[0] << 8) | _pipeInBuffer[1];

	LOG4CXX_DEBUG(_logger, "Header length: " << len);

	if (readExact(len) != len) {
		//LOG4CXX_ERROR(_logger, "Cannot read header from pipe.");
		throw PipeException();
	}

	DriverHdr header;
	if (!header.ParseFromArray(_pipeInBuffer, len)) {
		LOG4CXX_ERROR(_logger, "Cannot deserialize the header.");
		throw PipeException();
	}

	if (readExact(2) != 2) {
		LOG4CXX_ERROR(_logger, "Cannot read message length from pipe.");
		throw PipeException();
	}

	len = (_pipeInBuffer[0] << 8) | _pipeInBuffer[1];

	if (readExact(len) != len) {
		LOG4CXX_ERROR(_logger, "Cannot read message from pipe.");
		throw PipeException();
	}

	DriverMsg message;
	if (!message.ParseFromArray(_pipeInBuffer, len)) {
		LOG4CXX_ERROR(_logger, "Cannot deserialize the message.");
		throw PipeException();
	}

	switch (message.type()) {

	case DriverMsg_MsgType_DATA:
		_messageHandler->handleDataMsg(&header, &message);
		break;

	case DriverMsg_MsgType_CLIENT_DIED:
		if (header.clientids_size() != 1) {
			LOG4CXX_WARN(_logger, "CLIENT_DIED message came, but clientID not set, ignoring.");
			break;
		}

		_messageHandler->handleClientDiedMsg(header.clientids(0));
		break;

	default:
		LOG4CXX_WARN(_logger, "Unsupported message type, ignoring.");
		break;
	}
}

void AmberPipes::writeMsgToPipe(DriverHdr *header, DriverMsg *message) {
	scoped_lock<interprocess_mutex> lock(_pipeWriteMutex);

	int len, act = 0;

	// Header length
	len = header->ByteSize();
	_pipeOutBuffer[act] = (len >> 8) & 0xff;
	_pipeOutBuffer[act + 1] = len & 0xff;
	act += 2;

	// Serialize the header
	if (!header->SerializeToArray(_pipeOutBuffer + act, BUF_SIZE - act)) {
		LOG4CXX_ERROR(_logger, "Cannot serialize the header.");
		throw PipeException();
	}
	act += len;

	// Message length
	len = message->ByteSize();
	_pipeOutBuffer[act] = (len >> 8) & 0xff;
	_pipeOutBuffer[act + 1] = len & 0xff;
	act += 2;

	LOG4CXX_DEBUG(_logger, "Message is " << len << " bytes long.");

	// Serialize the message
	if (!message->SerializeToArray(_pipeOutBuffer + act, BUF_SIZE - act)) {
		LOG4CXX_ERROR(_logger, "Cannot serialize the message.");
		throw PipeException();
	}
	act += len;

	// Write whole packet to pipe
	if (writeExact(act) != act) {
		LOG4CXX_ERROR(_logger, "Cannot write the packet to pipe.");
		throw PipeException();
	}
}

void AmberPipes::handlePingMsg(DriverHdr *header, DriverMsg *message) {

	if (!message->has_synnum()) {
		LOG4CXX_WARN(_logger, "PING message came, but synNum not set, ignoring.");
		return;
	}

	DriverMsg pongMessage;
	pongMessage.set_type(DriverMsg_MsgType_PONG);
	pongMessage.set_acknum(message->synnum());

	DriverHdr pongHeader;
	pongHeader.add_clientids(header->clientids(0));

	LOG4CXX_DEBUG(_logger, "Sending PONG message");
	writeMsgToPipe(&pongHeader, &pongMessage);
}
