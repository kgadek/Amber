#include <cstdio>
#include <cstring>
#include <sys/types.h>
#include <sys/socket.h>
#include <strings.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <unistd.h>

#include "drivermsg.pb.h"

#include "9dof.h"
#include "ninedof_client.hpp"



#define MAX_BUF 256

int main(int argc, char *argv[]) {
     struct sockaddr_in serv_addr, tmp_addr, cli_addr;
     int fd, len;
     unsigned int serv_len, tmp_len;
     char buf[MAX_BUF];

     fd = socket(AF_INET, SOCK_DGRAM, 0);

     bzero((char*)&serv_addr, sizeof(serv_addr));
     serv_addr.sin_family = AF_INET;
     serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
     serv_addr.sin_port = htons(12345);

     bzero((char*)&cli_addr, sizeof(cli_addr));
     cli_addr.sin_family = AF_INET;
     cli_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
     cli_addr.sin_port = htons(0);

     bind(fd, (struct sockaddr*)&cli_addr, sizeof(cli_addr));
     
     DriverMsg driverMsg;
     int msgByteSize;

     driverMsg.set_type(DriverMsg::HELLO);
     msgByteSize = driverMsg.ByteSize();

     if (!driverMsg.SerializeToArray(buf, msgByteSize)) {
    	 fprintf(stderr, "Unable to serialize a driverMsg");
    	 exit(1);
     }

     sendto(fd, buf, msgByteSize, 0, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

     driverMsg.set_type(DriverMsg::MEASURMENT);
     msgByteSize = driverMsg.ByteSize();


	if (!driverMsg.SerializeToArray(buf, msgByteSize)) {
	 fprintf(stderr, "Unable to serialize a driverMsg");
	 exit(1);
	}

	sendto(fd, buf, msgByteSize, 0, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

	driverMsg.set_type(DriverMsg::CONTROL);
	driverMsg.set_sender(0);
	msgByteSize = driverMsg.ByteSize();

	if (!driverMsg.SerializeToArray(buf, msgByteSize)) {
		fprintf(stderr, "Unable to serialize a driverMsg");
		exit(1);
	}

	   sendto(fd, buf, msgByteSize, 0, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

	   while(1) {
		   len = recvfrom(fd, buf, MAX_BUF, 0, (struct sockaddr*)&tmp_addr, &tmp_len);

		   DriverMsg driverMsg;
		   if (!driverMsg.ParseFromArray(buf, len)) {
			   fprintf(stderr, "Cannot parse the incoming driver message.\n");
			   continue;
		   }
		   handleIncommingMsg(driverMsg);

	   }

     close(fd);

     return 0;

}

void handleIncommingMsg(DriverMsg driverMsg) {

	switch (driverMsg.type()) {

	case DriverMsg::HELLO:
		fprintf(stderr, "Message type: HELLO\n");
		break;

	case DriverMsg::MEASURMENT:
		fprintf(stderr, "Message type: MEASURMENT\n");
		break;

	case DriverMsg::CONTROL:
		fprintf(stderr, "Message type: CONTROL\n");
		break;

	default:
		fprintf(stderr, "Message type: other\n");
		break;
	}
}




