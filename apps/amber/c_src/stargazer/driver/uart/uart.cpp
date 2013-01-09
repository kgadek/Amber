#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <linux/types.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

#include "uart.h"

int uart_open(const char *filename) {
    int fd;

    fd = open(filename, O_RDWR);
    if (fd < 0) {
        return -1;
    }

    return fd;
}

void uart_init(int fd, speed_t speed)
{
    struct termios termios;
    int res;

    res = tcgetattr(fd, &termios);
    if (res < 0) {
        //perror("tcgetattr");
    }

    termios.c_iflag &= ~(IGNPAR | IXON | IXOFF);
    termios.c_iflag |= IGNPAR;

    termios.c_cflag &= ~(CSIZE | CSTOPB | CREAD | CLOCAL);
    termios.c_cflag |= CS8;
    termios.c_cflag |= CREAD;
    termios.c_cflag |= CLOCAL;

    termios.c_lflag &= ~(ICANON);
    termios.c_cc[VMIN] = 1;
    termios.c_cc[VTIME] = 0;

    cfsetispeed(&termios, speed);
    cfsetospeed(&termios, speed);

    res = tcsetattr(fd, TCSANOW, &termios);
    if (res < 0) {
       // perror("tcsetattr");
    }
}

void uart_close(int fd) {
    int res;
    
    res = close(fd);
    if (res < 0) {
       // perror("close");
    }
}

int uart_write(int fd, int bytes, __u8 *buf) {
    int res;
    
    res = write(fd, buf, bytes);
    if (res < 0) {
        //perror("write");
        return -1;
    }

    //tcdrain(fd);
	//printf("write");

    return res;
}

int uart_read(int fd, int bytes, __u8 *buf) {
    int received = 0;
    
 
    while (received < 4) {
        //printf("receive...\n");
        received += read(fd, buf + received, bytes);
        if (received < 0) {
            //perror("write");
            return -1; 
        }
    }    
    
    return received;
}
