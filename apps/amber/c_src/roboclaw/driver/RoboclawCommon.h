/*
 * RoboclawCommon.h
 *
 *  Created on: 02-12-2012
 *      Author: michal
 */

#ifndef ROBOCLAWCOMMON_H_
#define ROBOCLAWCOMMON_H_

#include <linux/types.h>
#include <string>

struct CurrentSpeedStruct {
	int m1_speed;
	bool m1_direction;
	int m2_speed;
	bool m2_direction;

	CurrentSpeedStruct(): m1_speed(0), m2_speed(0) {}
};

struct MotorsCommandStruct {

	int frontLeftSpeed;
	int frontRightSpeed;
	int rearLeftSpeed;
	int rearRightSpeed;

};

struct RoboclawConfiguration {

	std::string uart_port;
	unsigned int uart_speed;

	unsigned int front_rc_address;
	unsigned int rear_rc_address;

	unsigned int motors_max_qpps;
	unsigned int motors_p_const;
	unsigned int motors_i_const;
	unsigned int motors_d_const;

	unsigned int pulses_per_revolution;
	unsigned int wheel_radius;
};


#endif /* ROBOCLAWCOMMON_H_ */
