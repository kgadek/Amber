#ifndef UART_H_
#define UART_H_

#include <termios.h>
#include <linux/types.h>

int uart_open(const char *filename);
void uart_init(int fd, speed_t speed);
void uart_close(int fd);
int uart_write(int fd, int bytes, __u8 *buf);
int uart_read(int fd, int bytes, __u8 *buf);

#endif 
