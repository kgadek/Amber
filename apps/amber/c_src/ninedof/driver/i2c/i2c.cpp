/* 
* Funkcje obsługujące magistralę I2C
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <linux/types.h>
#include <linux/i2c-dev.h>
#include <sys/ioctl.h>
#include <string.h>
#include "i2c.h"

int i2c_open(const char *i2c_bus) {
	
	int file;

	file = open(i2c_bus, O_RDWR);
	if (file == -1) {
		return -1;
	}

	return file;
}

int i2c_read(int file, __u8 slave_address, __u8 register_address, int bytes, __u8 *buf) {
		
	// Set LSB to 0 to write register_address to slave. 
	if (ioctl(file, I2C_SLAVE, slave_address) == -1) {
		//perror("i2c ioctl");
		return -1;
	}

	// Set MSB to 1 to read multiple registers.
	register_address |= 0x80;

	// Writing register_address to slave.
	if (write(file, &register_address, 1) == -1) {
		//perror("i2c write");
		return -1;
	}

	// Not seting register_addres back to read, beacuse then reading is timeouting. Strange.
	int read_bytes;

	// Reading registers from slave.
	read_bytes = read(file, buf, bytes);
	if (read_bytes == -1) {
		//perror("i2c read");
		return -1;
	}

	return read_bytes;	
}

int i2c_write(int file, __u8 slave_address, __u8 register_address, int bytes, __u8 *buf) {
	
	// Set LSB to 0 to write register_address to slave. 
	if (ioctl(file, I2C_SLAVE, slave_address) == -1) {
		return -1;
	}

	// Set MSB to 1 to write multiple registers.
	register_address |= 0x80;
	
	// Copy register_address and value buffer to one array. Write needs to be called only once.
	__u8 write_buf[bytes+1];
	write_buf[0] = register_address;
	memcpy(write_buf+1, buf, bytes); 	

	int written_bytes;
	
	// Writing address and values to slave
	written_bytes = write(file, &write_buf, bytes+1);
	if (written_bytes <= 0) {
		return -1;
	}

	return written_bytes - 1;
}

int i2c_close(int file) {
	
	if (close(file) != 0) {;
		return -1;
	}

	return 0; 
}
