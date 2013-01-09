#include <cstdio>
#include <unistd.h>
#include <linux/types.h>

#include "uart.h"
#include "roboclaw_lib.h"


inline void fill_crc(__u8 buf[], int size) {
	__u8 sum = 0;

	for (int i = 0; i < size - 1; i++) {
		sum += buf[i];
	}

	buf[size - 1] = sum & 0x7F;
}

void rc_drive_forward_m1(int fd, __u8 rc_address, __u8 speed) {
    __u8 buffer[4] = {
    		rc_address,
    		DRIVE_FORWARD_M1,
    		speed,
    		0x00, // to be filled with CRC
    };
    fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
} 

void rc_drive_backwards_m1(int fd, __u8 rc_address, __u8 speed) {
	__u8 buffer[4] = {
    		rc_address,
    		DRIVE_BACKWARDS_M1,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_set_minimum_main_voltage(int fd, __u8 rc_address, __u8 voltage) {
	__u8 buffer[4] = {
			rc_address,
			SET_MINIMUM_MAIN_VOLTAGE,
			voltage,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

	uart_write(fd, 4, buffer);
}

void rc_set_maximum_main_voltage(int fd, __u8 rc_address, __u8 voltage) {
	__u8 buffer[4] = {
			rc_address,
			SET_MAXIMUM_MAIN_VOLTAGE,
			voltage,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

	uart_write(fd, 4, buffer);
}

void rc_drive_forward_m2(int fd, __u8 rc_address, __u8 speed) {
    __u8 buffer[4] = {
    		rc_address,
    		DRIVE_FORWARD_M2,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_drive_backwards_m2(int fd, __u8 rc_address, __u8 speed) {
	__u8 buffer[4] = {
    		rc_address,
    		DRIVE_BACKWARDS_M2,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_drive_m1(int fd, __u8 rc_address, __u8 speed) {
	__u8 buffer[4] = {
    		rc_address,
    		DRIVE_M1,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_drive_m2(int fd, __u8 rc_address, __u8 speed) {
	__u8 buffer[4] = {
    		rc_address,
    		DRIVE_M2,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_drive_forward(int fd, __u8 rc_address, __u8 speed) {
    __u8 buffer[4] = {
    		rc_address,
    		DRIVE_FORWARD,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_drive_backwards(int fd, __u8 rc_address, __u8 speed) {
    __u8 buffer[4] = {
    		rc_address,
    		DRIVE_BACKWARDS,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_turn_right(int fd, __u8 rc_address, __u8 value) {
    __u8 buffer[4] = {
    		rc_address,
    		TURN_RIGHT,
    		value,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_turn_left(int fd, __u8 rc_address, __u8 value) {
    __u8 buffer[4] = {
    		rc_address,
    		TURN_LEFT,
    		value,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}


void rc_drive(int fd, __u8 rc_address, __u8 speed) {
    __u8 buffer[4] = {
    		rc_address,
    		DRIVE_FORWARD_OR_BACKWARD,
    		speed,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_turn(int fd, __u8 rc_address, __u8 value) {
    __u8 buffer[4] = {
    		rc_address,
    		TURN_LEFT_OR_RIGHT,
    		value,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

    uart_write(fd, 4, buffer);
}

void rc_read_firmware_version(int fd, __u8 rc_address, __u8 *str) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_FIRMWARE_VERSION};

    uart_write(fd, 2, buffer);

    read(fd, str, 32);
}

void rc_read_main_battery_voltage_level(int fd, __u8 rc_address, __u16 *value) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_MAIN_BATTEY_VOLTAGE_LEVEL};

    uart_write(fd, 2, buffer);

    __u8 in_buffer[3];
    uart_read(fd, 3, in_buffer);

    *value = (in_buffer[0] << 8) & in_buffer[1];
}

void rc_read_main_logic_voltage_level(int fd, __u8 rc_address, __u8 *value) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_LOGIC_BATTERY_VOLTAGE_LEVEL};

    uart_write(fd, 2, buffer);

    __u8 in_buffer[3];
    uart_read(fd, 3, in_buffer);

    *value = (in_buffer[0] << 8) & in_buffer[1];
}

void rc_set_minimum_logic_voltage_level(int fd, __u8 rc_address, __u8 voltage) {
	__u8 buffer[4] = {
			rc_address,
			SET_MINIMUM_LOGIC_VOLTAGE_LEVEL,
			voltage,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

	uart_write(fd, 4, buffer);
}

void rc_set_maximum_logic_voltage_level(int fd, __u8 rc_address, __u8 voltage) {
	__u8 buffer[4] = {
			rc_address,
			SET_MAXIMUM_LOGIC_VOLTAGE_LEVEL,
			voltage,
    		0x00, // to be filled with CRC
	};
	fill_crc(buffer, 4);

	uart_write(fd, 4, buffer);
}

void rc_read_encoder_register_m1(int fd, __u8 rc_address, __u32 *value, __u8 *status) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_QUADRATURE_ENCODER_REGISTER_M1};

    uart_write(fd, 2, buffer);

    __u8 in_buffer[6];
    uart_read(fd, 6, in_buffer);

    *value = (in_buffer[0] << 24) & (in_buffer[1] << 16) & (in_buffer[2] << 8) & in_buffer[3];
    *status = in_buffer[4];
}

void rc_read_encoder_register_m2(int fd, __u8 rc_address, __u8 *value, __u8 *status) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_QUADRATURE_ENCODER_REGISTER_M2};

    uart_write(fd, 2, buffer);

    __u8 in_buffer[6];
	uart_read(fd, 6, in_buffer);

	*value = (in_buffer[0] << 24) & (in_buffer[1] << 16) & (in_buffer[2] << 8) & in_buffer[3];
	*status = in_buffer[4];
}

void rc_read_speed_m1(int fd, __u8 rc_address, __s32 *value, __u8 *direction) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_SPEED_M1};

    uart_read_flush(fd);
    uart_write(fd, 2, buffer);

    usleep(200000);
    __u8 in_buffer[15];
	uart_read(fd, 15, in_buffer);

	*value = (in_buffer[0] << 24) | (in_buffer[1] << 16) | (in_buffer[2] << 8) | in_buffer[3];
	*direction = in_buffer[4];
}


void rc_read_speed_m2(int fd, __u8 rc_address, __s32 *value, __u8 *direction) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_SPEED_M2};

    uart_read_flush(fd);
    uart_write(fd, 2, buffer);


    usleep(200000);
    __u8 in_buffer[15];
	uart_read(fd, 15, in_buffer);

	*value = (in_buffer[0] << 24) | (in_buffer[1] << 16) | (in_buffer[2] << 8) | in_buffer[3];
	*direction = in_buffer[4];
}

void rc_set_pid_consts_m1(int fd, __u8 rc_address, __u32 d, __u32 p, __u32 i, __u32 qpps) {

     __u8 buffer[19] = {
	     rc_address,
	     SET_PID_CONSTANTS_M1,

	     BYTE(d, 3),
	     BYTE(d, 2),
	     BYTE(d, 1),
	     BYTE(d, 0),

	     BYTE(p, 3),
	     BYTE(p, 2),
	     BYTE(p, 1),
	     BYTE(p, 0),

	     BYTE(i, 3),
	     BYTE(i, 2),
	     BYTE(i, 1),
	     BYTE(i, 0),

	     BYTE(qpps, 3),
	     BYTE(qpps, 2),
	     BYTE(qpps, 1),
	     BYTE(qpps, 0),

	     0x00 // to be filled with CRC
	};
	fill_crc(buffer, 19);

	uart_write(fd, 19, buffer);
}

void rc_set_pid_consts_m2(int fd, __u8 rc_address, __u32 d, __u32 p, __u32 i, __u32 qpps) {

     __u8 buffer[19] = {
	     rc_address,
	     SET_PID_CONSTANTS_M2,

	     BYTE(d, 3),
	     BYTE(d, 2),
	     BYTE(d, 1),
	     BYTE(d, 0),

	     BYTE(p, 3),
	     BYTE(p, 2),
	     BYTE(p, 1),
	     BYTE(p, 0),

	     BYTE(i, 3),
	     BYTE(i, 2),
	     BYTE(i, 1),
	     BYTE(i, 0),

	     BYTE(qpps, 3),
	     BYTE(qpps, 2),
	     BYTE(qpps, 1),
	     BYTE(qpps, 0),

	     0x00 // to be filled with CRC
	};
	fill_crc(buffer, 19);

	uart_write(fd, 19, buffer);
}

void rc_reset_encoder_counters(int fd, __u8 rc_address) {
    __u8 buffer[2] = {
    		rc_address,
    		RESET_QUADRATURE_ENCODER_COUNTERS};

    uart_write(fd, 2, buffer);
}


void rc_read_speed125_m1(int fd, __u8 rc_address, __u32 *value) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_CURRENT_SPEED_M1};
    uart_write(fd, 2, buffer);

    __u8 in_buffer[4];
	uart_read(fd, 4, in_buffer);

	*value = (in_buffer[0] << 24) & (in_buffer[1] << 16) & (in_buffer[2] << 8) & in_buffer[3];
}


void rc_read_speed125_m2(int fd, __u8 rc_address, __u32 *value) {
    __u8 buffer[2] = {
    		rc_address,
    		READ_CURRENT_SPEED_M2};
    uart_write(fd, 2, buffer);

    __u8 in_buffer[4];
	uart_read(fd, 4, in_buffer);

	*value = (in_buffer[0] << 24) & (in_buffer[1] << 16) & (in_buffer[2] << 8) & in_buffer[3];
}


void rc_drive_m1_speed(int fd, __u8 rc_address, __s32 speed_m) {

	__u8 buffer[7] = {
		rc_address,
		DRIVE_M1_SPEED,

		BYTE(speed_m, 3),
		BYTE(speed_m, 2),
		BYTE(speed_m, 1),
		BYTE(speed_m, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 7);

	uart_write(fd, 7, buffer);
}

void rc_drive_m2_speed(int fd, __u8 rc_address, __s32 speed_m) {

	__u8 buffer[7] = {
		rc_address,
		DRIVE_M2_SPEED,

		BYTE(speed_m, 3),
		BYTE(speed_m, 2),
		BYTE(speed_m, 1),
		BYTE(speed_m, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 7);

	uart_write(fd, 7, buffer);
}


void rc_drive_speed(int fd, __u8 rc_address, __s32 speed_m1, __s32 speed_m2) {

	__u8 buffer[11] = {
		rc_address,
		MIX_MODE_DRIVE_SPEED,

		BYTE(speed_m1, 3),
		BYTE(speed_m1, 2),
		BYTE(speed_m1, 1),
		BYTE(speed_m1, 0),

		BYTE(speed_m2, 3),
		BYTE(speed_m2, 2),
		BYTE(speed_m2, 1),
		BYTE(speed_m2, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 11);

	//for (int i = 0; i < 11; i++) {
	//	printf("%d ", buffer[i]);
	//}
	//printf("\n");
	uart_write(fd, 11, buffer);
}


void rc_drive_m1_speed_accel(int fd, __u8 rc_address, __u32 accel, __s32 speed) {

	__u8 buffer[11] = {
		rc_address,
		DRIVE_M1_SPEED_ACCEL,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 11);

	uart_write(fd, 11, buffer);
}


void rc_drive_m2_speed_accel(int fd, __u8 rc_address, __u32 accel, __s32 speed) {

	__u8 buffer[11] = {
		rc_address,
		DRIVE_M2_SPEED_ACCEL,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 11);

	uart_write(fd, 11, buffer);
}


void rc_drive_speed_accel(int fd, __u8 rc_address, __u32 accel, __s32 speed_m1, __s32 speed_m2) {

	__u8 buffer[15] = {
		rc_address,
		MIX_MODE_DRIVE_SPEED_ACCEL,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed_m1, 3),
		BYTE(speed_m1, 2),
		BYTE(speed_m1, 1),
		BYTE(speed_m1, 0),

		BYTE(speed_m2, 3),
		BYTE(speed_m2, 2),
		BYTE(speed_m2, 1),
		BYTE(speed_m2, 0),

		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 15);

	uart_write(fd, 15, buffer);
}

void rc_buffered_m1_drive_speed_dist(int fd, __u8 rc_address,__s32 speed, __u32 dist, __u8 now) {

	__u8 buffer[12] = {
		rc_address,
		BUFFERED_M1_DRIVE_SPEED_DIST,

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		BYTE(dist, 3),
		BYTE(dist, 2),
		BYTE(dist, 1),
		BYTE(dist, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 12);

	uart_write(fd, 12, buffer);
}

void rc_buffered_m2_drive_speed_dist(int fd, __u8 rc_address,__s32 speed, __u32 dist, __u8 now) {

	__u8 buffer[12] = {
		rc_address,
		BUFFERED_M2_DRIVE_SPEED_DIST,

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		BYTE(dist, 3),
		BYTE(dist, 2),
		BYTE(dist, 1),
		BYTE(dist, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 12);

	uart_write(fd, 12, buffer);
}

void rc_buffered_drive_speed_dist(int fd, __u8 rc_address, __s32 speed_m1, __u32 dist_m1, __s32 speed_m2, __u32 dist_m2, __u8 now) {

	__u8 buffer[20] = {
		rc_address,
		BUFFERED_MIX_MODE_DRIVE_SPEED_DIST,

		BYTE(speed_m1, 3),
		BYTE(speed_m1, 2),
		BYTE(speed_m1, 1),
		BYTE(speed_m1, 0),

		BYTE(dist_m1, 3),
		BYTE(dist_m1, 2),
		BYTE(dist_m1, 1),
		BYTE(dist_m1, 0),

		BYTE(speed_m2, 3),
		BYTE(speed_m2, 2),
		BYTE(speed_m2, 1),
		BYTE(speed_m2, 0),

		BYTE(dist_m2, 3),
		BYTE(dist_m2, 2),
		BYTE(dist_m2, 1),
		BYTE(dist_m2, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 20);

	uart_write(fd, 20, buffer);
}


void rc_buffered_m1_drive_speed_accel_dist(int fd, __u8 rc_address, __u32 accel, __s32 speed, __u32 dist, __u8 now){

	__u8 buffer[16] = {
		rc_address,
		BUFFERED_M1_DRIVE_SPEED_ACCEL_DIST,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		BYTE(dist, 3),
		BYTE(dist, 2),
		BYTE(dist, 1),
		BYTE(dist, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 16);

	uart_write(fd, 16, buffer);
}

void rc_buffered_m2_drive_speed_accel_dist(int fd, __u8 rc_address, __u32 accel, __s32 speed, __u32 dist, __u8 now){

	__u8 buffer[16] = {
		rc_address,
		BUFFERED_M2_DRIVE_SPEED_ACCEL_DIST,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed, 3),
		BYTE(speed, 2),
		BYTE(speed, 1),
		BYTE(speed, 0),

		BYTE(dist, 3),
		BYTE(dist, 2),
		BYTE(dist, 1),
		BYTE(dist, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 16);

	uart_write(fd, 16, buffer);
}


void rc_buffered_drive_speed_accel_dist(int fd, __u8 rc_address, __u32 accel, __s32 speed_m1, __u32 dist_m1, __s32 speed_m2, __u32 dist_m2, __u8 now) {

	__u8 buffer[24] = {
		rc_address,
		BUFFERED_MIX_MODE_SPEED_ACCEL_DISTANCE,

		BYTE(accel, 3),
		BYTE(accel, 2),
		BYTE(accel, 1),
		BYTE(accel, 0),

		BYTE(speed_m1, 3),
		BYTE(speed_m1, 2),
		BYTE(speed_m1, 1),
		BYTE(speed_m1, 0),

		BYTE(dist_m1, 3),
		BYTE(dist_m1, 2),
		BYTE(dist_m1, 1),
		BYTE(dist_m1, 0),

		BYTE(speed_m2, 3),
		BYTE(speed_m2, 2),
		BYTE(speed_m2, 1),
		BYTE(speed_m2, 0),

		BYTE(dist_m2, 3),
		BYTE(dist_m2, 2),
		BYTE(dist_m2, 1),
		BYTE(dist_m2, 0),

		now,
		0x00 // to be filled with CRC
	};
	fill_crc(buffer, 24);

	uart_write(fd, 24, buffer);
}
