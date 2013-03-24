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
#include <cmath>

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
	if (driverMsg->HasExtension(roboclaw_proto::currentSpeedRequest)) {

		if (!driverMsg->has_synnum()) {
			LOG4CXX_WARN(_logger, "Got CurrentSpeedRequest, but syn num not set. Ignoring.");
			return;
		}

		handleCurrentSpeedRequest(clientId, driverMsg->synnum(), driverMsg->MutableExtension(roboclaw_proto::currentSpeedRequest));

	} else if (driverMsg->HasExtension(roboclaw_proto::motorsCommand)) {
		handleMotorsEncoderCommand(clientId, driverMsg->synnum(), driverMsg->MutableExtension(roboclaw_proto::motorsCommand));
	}
}

void RoboclawController::handleClientDiedMsg(int clientID) {
	LOG4CXX_INFO(_logger, "Client " << clientID << " died");

	_roboclawDriver->stopMotors();
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

void RoboclawController::handleMotorsEncoderCommand(int sender, int synNum, amber::roboclaw_proto::MotorsQuadCommand *motorsCommand) {

	MotorsCommandStruct mc;

	// m1
	mc.frontLeftSpeed = toQpps(motorsCommand->frontleftspeed());
	mc.frontRightSpeed = toQpps(motorsCommand->frontrightspeed());
	mc.rearLeftSpeed = toQpps(motorsCommand->rearleftspeed());
	mc.rearRightSpeed = toQpps(motorsCommand->rearrightspeed());

	_roboclawDriver->sendMotorsEncoderCommand(&mc);
}

int RoboclawController::toQpps(int in) {

	double rpm = in / (double)(_configuration->wheel_radius * M_PI * 2);

	int out = (int)(rpm * _configuration->pulses_per_revolution);

	LOG4CXX_DEBUG(_logger, "toOpps: " << in << ", " <<  out);

	return out;
}


void RoboclawController::parseConfigurationFile(const char *filename) {

	LOG4CXX_INFO(_logger, "Parsing configuration file: " << filename);

	_configuration = new RoboclawConfiguration();

	options_description desc("Roboclaw options");
	desc.add_options()
			("roboclaw.uart_port", value<string>(&_configuration->uart_port)->default_value("/dev/ttyO3"))
			("roboclaw.uart_speed", value<unsigned int>(&_configuration->uart_speed)->default_value(38400))
			("roboclaw.front_rc_address", value<unsigned int>(&_configuration->front_rc_address)->default_value(128))
			("roboclaw.rear_rc_address", value<unsigned int>(&_configuration->rear_rc_address)->default_value(129))
			("roboclaw.motors_max_qpps", value<unsigned int>(&_configuration->motors_max_qpps)->default_value(13800))
			("roboclaw.motors_p_const", value<unsigned int>(&_configuration->motors_p_const)->default_value(65536))
			("roboclaw.motors_i_const", value<unsigned int>(&_configuration->motors_i_const)->default_value(32768))
			("roboclaw.motors_d_const", value<unsigned int>(&_configuration->motors_d_const)->default_value(16384))
			("roboclaw.pulses_per_revolution", value<unsigned int>(&_configuration->pulses_per_revolution)->default_value(1865))
			("roboclaw.wheel_radius", value<unsigned int>(&_configuration->wheel_radius)->default_value(60));


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
