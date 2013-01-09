#ifndef MOCKMSG_H_
#define MOCKMSG_H_

struct msg {
     int type;
     int data_size;
     char data[];
};


#endif
