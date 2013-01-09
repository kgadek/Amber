#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <cassert>
#include <poll.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/types.h>

#define MAX_BUF 256

typedef unsigned int uint;

int c2s[2];
int s2c[2];
int c;

int main(int argc, char *argv[]) {

     int rc;
     __u32 len;

     if (argc != 2) {
          fprintf(stderr, "%s: [proces do spawnowania]\n", argv[0]);
          exit(1);     
     }
     
	if( (pipe(c2s) == -1) || (pipe(s2c) == -1)) {
		fprintf(stderr, "LOL\n");
		exit(1);
	}

	c = fork();
	if(c == -1) {
		fprintf(stderr, "KTHXBYE\n");
		exit(2);
	} else if( c == 0) { //_____________________________________________
		/*dbg*/printf("CHLD c=%d\n",c);
		
          close(STDOUT_FILENO);
	     dup2(c2s[1], STDOUT_FILENO); 
			        
	     close(STDIN_FILENO);
	     dup2(s2c[0], STDIN_FILENO);

		close(s2c[1]);
		close(c2s[0]);

		char pipe_out[5], pipe_in[5];
		sprintf(pipe_out, "%d", c2s[1]);
		sprintf(pipe_in, "%d", s2c[0]);

         
		// out in
          execlp(argv[1], argv[1], pipe_in, pipe_out, (char *) NULL);
		perror("No exec");
		exit(1);

	} else { //_________________________________________________________
		/*dbg*/printf("PAREN c=%d\n",c);
		close(s2c[0]);
		close(c2s[1]);
         
          char buf[MAX_BUF + sizeof(__u16)];
          char *bufin = buf + sizeof(__u16);

          // UDP server
          struct sockaddr_in serv_addr, cli_addr;
          int fd, ret, len;
          bool client = false;
          socklen_t cli_len = sizeof(struct sockaddr);
          fd = socket(AF_INET, SOCK_DGRAM, 0);
          
          bzero((char*)&serv_addr, sizeof(serv_addr));
          bzero((char*)&cli_addr, sizeof(cli_addr));          

          serv_addr.sin_family = AF_INET;
          serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
          serv_addr.sin_port = htons(12345);

          bind(fd, (struct sockaddr*) &serv_addr, sizeof(serv_addr));

          struct pollfd fdset[2];
          int nfds = 2;
          
		while (1) {
		     memset((void*)fdset, 0, sizeof(fdset));
               
               fdset[0].fd = c2s[0];
               fdset[0].events = POLLIN;

               fdset[1].fd = fd;
               fdset[1].events = POLLIN | POLLPRI;

               rc = poll(fdset, nfds, -1);
               if (rc < 0) {
			     return -1;
               }

               if (fdset[0].revents & POLLIN) {         

            	   	__u16 *msg_len;
            	   	char *act_buf;

            	   	len = read(fdset[0].fd, buf, MAX_BUF);
            	   	if (len == -1) {
            	   		perror("read");
            	   		continue;
            	   	}

            	   	int read = 0;

            	   	while (read < len) {
            	   		int header_len = (buf[read] << 8) | buf[read+1];

            	   		int msg_len = (buf[read + 2 + header_len] << 8) | buf[read + 2 + header_len + 1];

            	   		if (client) {
            	   			printf("wysyłam przez udp msg_len: %d\n", msg_len);

							ret = sendto(fd, buf, 2 + header_len + 2 + msg_len, 0x00, (struct sockaddr*)&cli_addr, cli_len);

							if (ret < 0) {
								 perror("sendto");
							}
						}
            	   		read += 2 + header_len + 2 + msg_len;
            	   	}


               } else if (fdset[1].revents & POLLIN) {
                    len = recvfrom(fdset[1].fd, buf, MAX_BUF, 0, (struct sockaddr*)&cli_addr, &cli_len);
                    
                    if (len < 0) {
                         perror("recvfrom");
                    } else {

                         printf("przyszło: %i\n", len);
                         client = true;

                         int header_len = (buf[0] << 8) | buf[1];
                         int msg_len = (buf[2 + header_len] << 8) | buf[2 + header_len + 1];
                         printf("msg_len: %d\n", msg_len);

                         int sent = write(s2c[1], buf, len);
                         printf("wysłano: %d\n", sent);
                    }

                    
               }                    
          }    
	}
	return 0;
}

/*
   if (readExact(2) < 2) {
		LOG4CXX_ERROR(logger, "Cannot read header length from pipe.");
		throw PipeException();
	}

	len = (pipeInBuffer[0] << 8) | pipeInBuffer[1];

	if (readExact(len) != len) {
		LOG4CXX_ERROR(logger, "Cannot read header from pipe.");
		throw PipeException();

	}

	DriverHdr header;
	if (!header.ParseFromArray(pipeInBuffer, len)) {
		throw PipeException();
		LOG4CXX_ERROR(logger, "Cannot deserialize the header.");
	}
 */
