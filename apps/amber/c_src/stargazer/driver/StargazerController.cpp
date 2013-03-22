/*
 * StargazerController.cpp
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */

#include <log4cxx/propertyconfigurator.h>
#include <boost/program_options.hpp>

#include "StargazerController.h"
#include "StargazerCommon.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::program_options;
using namespace log4cxx;
using namespace amber;

LoggerPtr StargazerController::_logger (Logger::getLogger("Stargazer.Controller"));

StargazerController::StargazerController(int pipeInFd, int pipeOutFd, const char *confFilename) {

	parseConfigurationFile(confFilename);

	_stargazerDriver = new StargazerDriver(_configuration);
	_amberPipes = new AmberPipes(this, pipeInFd, pipeOutFd);
	_amberScheduler = new AmberScheduler<StargazerSchedulerEntry>(this);
	_dataStruct = _stargazerDriver->getDataStruct();

	_schedulerThread = new boost::thread(boost::ref(*_amberScheduler));
	_driverThread = new boost::thread(boost::ref(*_stargazerDriver));
	_pipesThread = new boost::thread(boost::ref(*_amberPipes));

}

StargazerController::~StargazerController() {
	delete _stargazerDriver;
	delete _amberPipes;
}

void StargazerController::handleDataMsg(amber::DriverHdr *driverHdr, amber::DriverMsg *driverMsg) {

	LOG4CXX_DEBUG(_logger, "Message came");

	// TODO: hack for now
	int clientId = driverHdr->clientids_size() > 0 ? driverHdr->clientids(0) : 0;

	// DataRequest
	if (driverMsg->HasExtension(amber::stargazer_proto::dataRequest)) {
		if (!driverMsg->has_synnum()) {
			LOG4CXX_WARN(_logger, "Got DataRequest, but SynNum not set, ignoring.");
			return;
		}

		amber::stargazer_proto::DataRequest *dataRequest = driverMsg->MutableExtension(amber::stargazer_proto::dataRequest);
		handleDataRequestMsg(clientId, driverMsg->synnum(), dataRequest);
	}

	if (driverMsg->HasExtension(amber::stargazer_proto::subscribeAction)) {

		amber::stargazer_proto::SubscribeAction *subscribeAction = driverMsg->MutableExtension(amber::stargazer_proto::subscribeAction);
		handleSubscribeActionMsg(clientId, subscribeAction);
	}

}

void StargazerController::handleClientDiedMsg(int clientID) {
	LOG4CXX_INFO(_logger, "Client " << clientID << " died, removing from scheduler and realtime listeners");

	if (_amberScheduler->hasClient(clientID)) {
		_amberScheduler->removeClient(clientID);
	}

	if (_realtimeListeners.count(clientID) > 0) {
		_realtimeListeners.erase(clientID);
	}
}

void StargazerController::operator()() {

	while (1) {
		{
			scoped_lock<interprocess_mutex> lock(_stargazerDriver->dataMutex);

			_stargazerDriver->dataNotReady.wait(lock);
		}

		{
			scoped_lock<interprocess_mutex> local(_realtimeListenersMutex);

			for(set<int>::iterator it = _realtimeListeners.begin(); it != _realtimeListeners.end(); it++) {
				LOG4CXX_DEBUG(_logger, "Sending realtime data for client: " << *it);
				sendLocalizationDataMsg(*it, 0);
			}
		}
	}
}

amber::DriverMsg *StargazerController::buildLocalizationDataMsg() {
	scoped_lock<interprocess_mutex> lock(_stargazerDriver->dataMutex);

	LOG4CXX_DEBUG(_logger, "buildLocalizationDataMsg");

	//_stargazerDriver->dataNotReady.wait(lock);

	amber::DriverMsg *message = new amber::DriverMsg();
	message->set_type(amber::DriverMsg_MsgType_DATA);

	amber::stargazer_proto::LocalizationData *localizationData = message->MutableExtension(amber::stargazer_proto::localizationData);
	localizationData->set_xpos(_dataStruct->x_pos);
	localizationData->set_ypos(_dataStruct->y_pos);
	localizationData->set_zpos(_dataStruct->z_pos);
	localizationData->set_angle(_dataStruct->angle);
	localizationData->set_markerid(_dataStruct->marker_id);
	localizationData->set_timestamp(_dataStruct->timestamp);

	return message;
}


void StargazerController::sendLocalizationDataMsg(int receiver, int ackNum) {
	amber::DriverMsg *localizationDataMsg = buildLocalizationDataMsg();
	localizationDataMsg->set_acknum(ackNum);
	amber::DriverHdr *header = new amber::DriverHdr();

	header->add_clientids(receiver);

	_amberPipes->writeMsgToPipe(header, localizationDataMsg);

	delete localizationDataMsg;
	delete header;
}


void StargazerController::handleDataRequestMsg(int sender, int synNum, amber::stargazer_proto::DataRequest *dataRequest) {
	LOG4CXX_INFO(_logger, "Got DataRequest message");

	LOG4CXX_INFO(_logger, "Sending LocalizationData message");
	sendLocalizationDataMsg(sender, synNum);
}

void StargazerController::handleSubscribeActionMsg(int sender, amber::stargazer_proto::SubscribeAction *subscribeAction) {
	LOG4CXX_INFO(_logger, "Got SubscribeAction message");

	if (subscribeAction->action() == stargazer_proto::SubscribeAction_ACTION_SUBSCRIBE) {

		if (subscribeAction->freq() == 0) {
			LOG4CXX_INFO(_logger, "Subscribing realtime listener: " << sender << ", freq: " << subscribeAction->freq());
			{
				scoped_lock<interprocess_mutex> local(_realtimeListenersMutex);

				_realtimeListeners.insert(sender);
			}
		} else {
			LOG4CXX_INFO(_logger, "Subscribing listener, client_id: " << sender << ", freq: " << subscribeAction->freq());
			_amberScheduler->addClient(sender, subscribeAction->freq(), new StargazerSchedulerEntry());
		}

	} else if (subscribeAction->action() == stargazer_proto::SubscribeAction_ACTION_UNSUBSCRIBE) {

		if (subscribeAction->freq() == 0) {
			LOG4CXX_INFO(_logger, "Removing real time listener, client_id: " << sender);
			{
				scoped_lock<interprocess_mutex> local(_realtimeListenersMutex);

				if (_realtimeListeners.count(sender) == 1) {
					_realtimeListeners.erase(sender);
				}
			}

		} else {
			LOG4CXX_INFO(_logger, "Removing listener, client_id: " << sender);
			_amberScheduler->removeClient(sender);
		}
	}
}

void StargazerController::handleSchedulerEvent(int clientId, StargazerSchedulerEntry *entry) {
	LOG4CXX_DEBUG(_logger, "Handling scheduler event, clientId: "<< clientId);

	sendLocalizationDataMsg(clientId, 0);
}

void StargazerController::parseConfigurationFile(const char *filename) {

	LOG4CXX_INFO(_logger, "Parsing configuration file: " << filename);

	_configuration = new StargazerConfiguration();

	options_description desc("Stargazer options");
	desc.add_options()
			("stargazer.uart_port", value<string>(&_configuration->uart_port)->default_value("/dev/ttyO0"))
			("stargazer.uart_speed", value<unsigned int>(&_configuration->uart_speed)->default_value(115200))
	;

	variables_map vm;

	try {
		store(parse_config_file<char>(filename, desc), vm);
		notify(vm);

	} catch (std::exception& e) {
		LOG4CXX_ERROR(_logger, "Error in parsing configuration file: " << e.what());
	}

}

int main(int argc, char *argv[]) {

	PropertyConfigurator::configure("log.config");

	LoggerPtr logger (Logger::getLogger("main"));

	if (argc != 3) {
		LOG4CXX_FATAL(logger, "Wrong number of parameters: ");
		return 1;
	}

	const char *confFile;
	if (argc == 4) {
		confFile = argv[3];
	} else {
		confFile = "stargazer.conf";
	}


	LOG4CXX_INFO(logger, "Creating controller");

	StargazerController controller(atoi(argv[1]), atoi(argv[2]), confFile);
	controller();
}
