#include <iostream>
#include <cstdio>
#include "comm.hpp"
#include "ex.hpp"

#define BUFFSIZE 1000

using namespace std;
typedef unsigned char uchar;

int main() {
  int result, inplen;
  uchar buff[BUFFSIZE];
  uchar buff2[] = {10, 3, 1, 3, 5}; // driverhdr: receivers=1,3,5
  uchar buff3[] = {16, 6};          // drivermsg: type=HELLO

  while((inplen = read_cmd(buff)) > 0) {
    switch(buff[0]) {
      case 1: // call twice
        result = twice(buff[1]);
        break;
      case 2: // call DriverMsg
        result = receiveMsg(inplen, buff);
        break;
      default:
        result = 0;
    }
    buff[0] = result;
    write_cmd(buff, 1);

    write_cmd(buff2, sizeof(buff2) / sizeof(uchar));
    write_cmd(buff3, sizeof(buff3) / sizeof(uchar));
  }

  return 0;
}

