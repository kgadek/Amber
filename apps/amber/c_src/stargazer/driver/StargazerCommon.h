/*
 * StargazerCommon.h
 *
 *  Created on: 30-11-2012
 *      Author: michal
 */

#ifndef STARGAZERCOMMON_H_
#define STARGAZERCOMMON_H_

#include <linux/types.h>
#include <boost/thread/thread_time.hpp>

struct StargazerDataStruct {
	float x_pos;
	float y_pos;
	float z_pos;
	float angle;
	__u32 marker_id;
	unsigned int timestamp;


	StargazerDataStruct(): x_pos(0.0), y_pos(0.0), z_pos(0.0), angle(0), marker_id(0), timestamp(0) {}
};

struct StargazerConfiguration {

	std::string uart_port;
	unsigned int uart_speed;

};


#endif /* STARGAZERCOMMON_H_ */
