/*
 * NinedofCommin.h
 *
 *  Created on: 02-11-2012
 *      Author: michal
 */

#ifndef NINEDOFCOMMIN_H_
#define NINEDOFCOMMIN_H_

#include <linux/types.h>

struct axes_data {
	__s16 x_axis;
	__s16 y_axis;
	__s16 z_axis;

	axes_data(): x_axis(0), y_axis(0), z_axis(0) {}

};

struct NinedofDataStruct {
	struct axes_data accel;
	struct axes_data gyro;
	struct axes_data magnet;
};

struct NinedofConfiguration {

	std::string i2c_port;

};

#endif /* NINEDOFCOMMIN_H_ */
