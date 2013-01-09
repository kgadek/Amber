/*
 * RoboclawController.cpp
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */

#include <log4cxx/propertyconfigurator.h>

#include "RoboclawController.h"
#include "RoboclawCommon.h"

#include <boost/program_options.hpp>
#include <string>

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::program_options;
using namespace log4cxx;
using namespace amber;

LoggerPtr RoboclawController::_logger (Logger::getLogger("Roboclaw.Controller"));

RoboclawController::RoboclawController(int pipeInFd, int pipeOutFd, const char *confFilename) {

	parseConfigurationFile(confFilename);

	_roboclawDriver = new RoboclawDriver(_configuration);
	_amberPipes = new AmberPipes(this, pipeInFd, pipeOutFd);

	_roboclawDriver->initializeDriver();

	//_logger->setLevel(Level::getOff());
}

RoboclawController::~RoboclawController() {
	delete _roboclawDriver;
	delete _amberPipes;
}

void RoboclawController::handleDataMsg(amber::DriverHdr *driverHdr, amber::DriverMsg *driverMsg) {

	LOG4CXX_DEBUG(_logger, "Message came");

	// TODO: hack for now
	int clientId = driverHdr->clientids_size() > 0 ? driverHdr->clientids(0) : 0;

	// DataRequest
	if (driverMsg->ExtensionSize(roboclaw_proto::currentSpeedRequests) > 0) {

		if (!driverMsg->has_synnum()) {
			LOG4CXX_WARN(_logger, "Got CurrentSpeedRequest, but syn num not set. Ignoring.");
			return;
		}

		for (int i = 0; i < driverMsg->ExtensionSize(roboclaw_proto::currentSpeedRequests); i++) {
			amber::roboclaw_proto::CurrentSpeedRequest *currentSpeedRequest = driverMsg->MutableExtension(roboclaw_proto::currentSpeedRequests, i);
			handleCurrentSpeedRequest(clientId, driverMsg->synnum(), currentSpeedRequest);
		}

	} else if (driverMsg->ExtensionSize(roboclaw_proto::motorsCommands) > 0) {

		for (int i = 0; i < driverMsg->ExtensionSize(roboclaw_proto::motorsCommands); i++) {
			roboclaw_proto::MotorsCommand *motorsCommand = driverMsg->MutableExtension(roboclaw_proto::motorsCommands, i);
			handleMotorsEncoderCommand(clientId, driverMsg->synnum(), motorsCommand);
		}
	}
}

void RoboclawController::handleClientDiedMsg(int clientID) {

}

void RoboclawController::operator()() {
	_amberPipes->operator ()();
}

amber::DriverMsg *RoboclawController::buildCurrentSpeedMsg() {
	scoped_lock<interprocess_mutex> lock(_roboclawDriver->serialPortMutex);

	//_stargazerDriver->dataNotReady.wait(lock);

	amber::DriverMsg *message = new amber::DriverMsg();
	message->set_type(amber::DriverMsg_MsgType_DATA);

	// TODO: building current speed message

	return message;
}


void RoboclawController::sendCurrentSpeedMsg(int receiver, int ackNum) {
	amber::DriverMsg *currentSpeedMsg = buildCurrentSpeedMsg();
	currentSpeedMsg->set_acknum(ackNum);
	amber::DriverHdr *header = new amber::DriverHdr();
	header->add_clientids(receiver);

	_amberPipes->writeMsgToPipe(header, currentSpeedMsg);

	delete currentSpeedMsg;
	delete header;
}


void RoboclawController::handleCurrentSpeedRequest(int sender, int synNum, amber::roboclaw_proto::CurrentSpeedRequest *currentSpeedRequest) {
	LOG4CXX_INFO(_logger, "Got DataRequest message");

	LOG4CXX_INFO(_logger, "Sending LocalizationData message");
	sendCurrentSpeedMsg(sender, synNum);
}

void RoboclawController::handleMotorsEncoderCommand(int sender, int synNum, amber::roboclaw_proto::MotorsCommand *motorsCommand) {

	MotorCommandStruct m1, m2;

	// m1
	m1.speed = motorsCommand->m1speed();

	m1.accel_set = motorsCommand->has_m1accel();
	if (m1.accel_set) {
		m1.accel = motorsCommand->m1accel();
	}

	m1.distance_set = motorsCommand->has_m1distance();
	if (m1.distance_set) {
		m1.distance = motorsCommand->m1distance();
	}

	// m2
	m2.speed = motorsCommand->m2speed();

	m2.accel_set = motorsCommand->has_m2accel();
	if (m2.accel_set) {
		m2.accel = motorsCommand->m2accel();
	}

	m2.distance_set = motorsCommand->has_m2distance();
	if (m2.distance_set) {
		m2.distance = motorsCommand->m2distance();
	}

	_roboclawDriver->sendMotorsEncoderCommand(motorsCommand->address(), &m1, &m2);
}

void RoboclawController::parseConfigurationFile(const char *filename) {

	LOG4CXX_INFO(_logger, "Parsing configuration file: " << filename);

	_configuration = new RoboclawConfiguration();

	options_description desc("Roboclaw options");
	desc.add_options()
			("roboclaw.uart_port", value<string>(&_configuration->uart_port)->default_value("/dev/ttyO3"))
			("roboclaw.uart_speed", value<unsigned int>(&_configuration->uart_speed)->default_value(38400))
			("roboclaw.motor1_address", value<unsigned int>(&_configuration->motor1_address)->default_value(128))
			("roboclaw.motor2_address", value<unsigned int>(&_configuration->motor2_address)->default_value(129))
			("roboclaw.motors_max_qpps", value<unsigned int>(&_configuration->motors_max_qpps)->default_value(13800))
			("roboclaw.motors_p_const", value<unsigned int>(&_configuration->motors_p_const)->default_value(65536))
			("roboclaw.motors_i_const", value<unsigned int>(&_configuration->motors_i_const)->default_value(32768))
			("roboclaw.motors_d_const", value<unsigned int>(&_configuration->motors_d_const)->default_value(16384))
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

	//BasicConfigurator::configure();
	PropertyConfigurator::configure("log.config");

	// STDIN_FD = 0, STDOUT_FD = 1
	// pipe_in_fd = 0, pipe_out_fd = 1
	LoggerPtr logger (Logger::getLogger("main"));

	LOG4CXX_INFO(logger, "-------------");
	LOG4CXX_INFO(logger, "Creating controller, pipe_in_fd: " << argv[1] << ", pipe_out_fd: " << argv[2]);

	if (argc < 3) {
		LOG4CXX_FATAL(logger, "Wrong number of parameters: ");
		return 1;
	}

	const char *confFile;
	if (argc == 4) {
		confFile = argv[3];
	} else {
		confFile = "roboclaw.conf";
	}

	RoboclawController controller(atoi(argv[1]), atoi(argv[2]), confFile);

	controller();
}
