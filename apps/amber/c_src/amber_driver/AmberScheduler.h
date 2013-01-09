/*
 * AmberScheduler.h
 *
 *  Created on: 30-10-2012
 *      Author: michal
 */

#ifndef AMBERSCHEDULER_H_
#define AMBERSCHEDULER_H_

#include <log4cxx/logger.h>
#include <boost/interprocess/sync/interprocess_condition.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>

#include <functional>
#include <utility>
#include <queue>
#include <map>
#include <vector>

using namespace log4cxx;

#define NSEC_PER_SEC 1000 * 1000 * 1000
#define NSEC_PER_MILISEC 1000 * 1000



template <class T>
class AmberSchedulerEntry {
public:
	int clientId;
	int freq;
	boost::system_time nextTime;
	bool outdated;
	T *details;

	AmberSchedulerEntry(int clientId, int freq): clientId(clientId), freq(freq), outdated(false) {};

};


template <class T>
struct PrioQueueComparator: public std::binary_function<AmberSchedulerEntry<T>*, AmberSchedulerEntry<T>*, bool> {
	bool operator() (AmberSchedulerEntry<T> *a, AmberSchedulerEntry<T> *b) {
		return a->nextTime > b->nextTime;
	}
};

template <class T>
class AmberSchedulerListener {
public:
	virtual ~AmberSchedulerListener() {};
	virtual void handleSchedulerEvent(int clientId, T *event) = 0;
};

template <class T>
class AmberScheduler {
public:
	AmberScheduler(AmberSchedulerListener<T> *listener);
	virtual ~AmberScheduler();

	void operator()();
	void addClient(int clientId, int freq, T *details);
	void editClient(int clientId, int newFreq);
	void removeClient(int clientId);

private:
	void runScheduler();

	std::priority_queue<AmberSchedulerEntry<T>*, std::vector<AmberSchedulerEntry<T>*>, PrioQueueComparator<T> > _schedulerQueue;
	std::map<int, AmberSchedulerEntry<T>* > _schedulerMap;
	//NinedofSchedulerSet _schedulerSet;
	AmberSchedulerListener<T> *_listener;
	boost::interprocess::interprocess_mutex _schedulerMutex;
	boost::interprocess::interprocess_condition _noClient;

	static log4cxx::LoggerPtr _logger;
};

template <class T>
LoggerPtr AmberScheduler<T>::_logger (Logger::getLogger("Amber.Scheduler"));

template <class T>
AmberScheduler<T>::AmberScheduler(AmberSchedulerListener<T> *listener): _listener(listener) {
	//_logger->setLevel(Level::getOff());
}

template <class T>
AmberScheduler<T>::~AmberScheduler() {

}

template <class T>
void AmberScheduler<T>::addClient(int clientId, int freq, T *details) {
	boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lock(_schedulerMutex);

	if (_schedulerMap.count(clientId) != 0) {
		return;
	}

	AmberSchedulerEntry<T> *entry = new AmberSchedulerEntry<T>(clientId, freq);
	entry->nextTime = boost::get_system_time();
	entry->nextTime += boost::posix_time::milliseconds(freq);
	entry->details = details;
	
	_schedulerMap.insert(std::pair<int, AmberSchedulerEntry<T>* >(clientId, entry));
	_schedulerQueue.push(entry);

	LOG4CXX_DEBUG(_logger, "Adding client: " << clientId << " with time: " << entry->nextTime.time_of_day().total_milliseconds())

	_noClient.notify_all();
}

template <class T>
void AmberScheduler<T>::removeClient(int clientId) {
	boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lock(_schedulerMutex);

	if (_schedulerMap.count(clientId) > 0) {
		_schedulerMap[clientId]->outdated = true;
	}
}

template <class T>
void AmberScheduler<T>::operator()() {
	runScheduler();
}

// TODO: editClientFreq
template <class T>
void AmberScheduler<T>::runScheduler() {
	LOG4CXX_INFO(_logger, "Scheduler thread started.");

	boost::system_time actTime;

	while (1) {
		{
			boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lock(_schedulerMutex);

			//LOG4CXX_DEBUG(_logger, "Next loop.");

			while (_schedulerQueue.size() == 0) {
				_noClient.wait(lock);
			}

			actTime = boost::get_system_time();
			//LOG4CXX_DEBUG(_logger, "actTime: " << actTime.time_of_day().total_milliseconds() << " " << actTime.time_of_day().total_microseconds());
			while (1) {


				//LOG4CXX_DEBUG(_logger, "top: " << _schedulerQueue.top()->clientId << " millis: " << _schedulerQueue.top()->nextTime.time_of_day().total_milliseconds() << " " << actTime.time_of_day().total_microseconds());
				if (_schedulerQueue.empty() || _schedulerQueue.top()->nextTime > actTime) {
					//LOG4CXX_DEBUG(_logger, "break");
					break;
				}

				//LOG4CXX_DEBUG(_logger, "notbreak");

				AmberSchedulerEntry<T> *entry = _schedulerQueue.top();
				_schedulerQueue.pop();

				if (entry->outdated) {
					//LOG4CXX_DEBUG(_logger, "Outdated: " << entry->clientId);
					_schedulerMap.erase(entry->clientId);
					//LOG4CXX_DEBUG(_logger, "After utdated: " << entry->clientId);
					delete entry;
				} else {
					_listener->handleSchedulerEvent(entry->clientId, entry->details);
					entry->nextTime += boost::posix_time::milliseconds(entry->freq);
					//LOG4CXX_DEBUG(_logger, "Adding back: " << entry->clientId << " millis: " << entry->nextTime.time_of_day().total_milliseconds());
					_schedulerQueue.push(entry);
				}
			}
		}


		//LOG4CXX_DEBUG(_logger, "Going sleep: ");
		boost::thread::sleep(_schedulerQueue.top()->nextTime);
	}
}

#endif /* AMBERSCHEDULER_H_ */
