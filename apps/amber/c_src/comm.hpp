#ifndef __COMM_HPP
#define __COMM_HPP

#include <unistd.h>

typedef unsigned char uchar;

int read_exact(uchar*,int);
int write_exact(uchar*,int);
int read_cmd(uchar*);
int write_cmd(uchar*,int);

int write_cmd(uchar *buff, int len) {
  uchar bl[] = { (len>>8) & 0xff, len & 0xff };
  write_exact(bl, 2);
  return write_exact(buff, len);
}

int read_cmd(uchar *buff) {
  int len;
  if(read_exact(buff, 2) < 2)
    return -1; // TODO: throw ?
  len = (buff[0] << 8) | buff[1];
  return read_exact(buff, len);
}

int write_exact(uchar *buff, int len) {
  int out, wrote = 0;
  do {
    out = write(STDOUT_FILENO, buff + wrote, len - wrote);
    if(out <= 0)
      return wrote;
    wrote += out;
  } while(wrote < len);
  return len;
}

int read_exact(uchar *buff,int len) {
  int in, got = 0;
  do {
    in = read(STDIN_FILENO, buff + got, len - got);
    if(in <= 0)
      return got;
    got += in;
  } while(got < len);
  return len;
}

#endif

