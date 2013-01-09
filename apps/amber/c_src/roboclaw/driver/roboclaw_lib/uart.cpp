#include <cstdlib>
#include <termios.h>
#include <unistd.h>
#include <linux/types.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <cstdio>

#include "uart.h"

#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>

int uart_open(const char *filename) {
    int fd;

    fd = open(filename, O_RDWR | O_NONBLOCK);
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
       //perror("tcsetattr");
    }
}

void uart_close(int fd) {
    int res;
    
    res = close(fd);
    if (res < 0) {
        //perror("close");
    }
}

int uart_write(int fd, int bytes, __u8 *buf) {
    int res;

    #ifdef DEBUG
    printf("write\n");
#endif
    
    res = write(fd, buf, bytes);
    if (res < 0) {
        //perror("write");
        return -1;
    }

    // wait for buffer to empty
    while (1) {
    	boost::this_thread::sleep(boost::posix_time::milliseconds(500));
    	ioctl (fd, TIOCOUTQ, &chars_in_tx_queue);
    } while (chars_in_tx_queue > 0);

    //tcdrain(fd);

#ifdef DEBUG

    int i;
    for (i = 0; i < bytes; i++) {
    	printf("%x ", buf[i]);
    }

	printf("\n");

#endif

    return res;
}

void uart_read_flush(int fd) {

	__u8 tmp[10];

	while (read(fd, tmp, 10) > 0) {
#ifdef DEBUG
		printf("flush\n");
#endif
	}


}

int uart_read(int fd, int bytes, __u8 *buf) {
    int received;
    
    received = read(fd, buf, bytes);

#ifdef DEBUG
    int i;
	for (i = 0; i < received; i++) {
		printf("%x ", buf[i]);
	}

	printf("\n");
    printf("read %i\n", received);
#endif
    
    return received;
}
