#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#include "network.h"


int min(int a, int b) {
    return a < b ? a : b;
}

int szcopy(char *dst, char *src, size_t dst_size) {
    strncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}


int c_get_network_interfaces(struct network_interface *ns, int max_ns) {
    int sockfd;
    int io;
    int i, count;
	char buffer[1024];
	struct ifconf conf;
	struct ifreq *req, *req_end;
    
    sockfd = socket(AF_INET, SOCK_DGRAM, 0);   
	if (sockfd < 0) {
		perror("socket");
		return 0;
	}

	conf.ifc_len = sizeof(buffer);
	conf.ifc_buf = buffer;
    io = ioctl(sockfd, SIOCGIFCONF, &conf);
	if (io < 0) {
		perror("ioctl(SIOCGIFCONF)");
        close(sockfd);
		return 0;
	}

    req = conf.ifc_req;
    count = min(max_ns, conf.ifc_len / sizeof(struct ifreq));

    for (i = 0; i < count; i++) {
        szcopy(ns[i].name, req[i].ifr_name, NAME_SIZE);
        szcopy(ns[i].inet, inet_ntoa(((struct sockaddr_in *)&req[i].ifr_addr)->sin_addr), IP_SIZE);

        io = ioctl(sockfd, SIOCGIFBRDADDR, &req[i]);
        if (io >= 0) {
            szcopy(ns[i].bcast, inet_ntoa(((struct sockaddr_in *)&req[i].ifr_broadaddr)->sin_addr), IP_SIZE);
        }

        io = ioctl(sockfd, SIOCGIFHWADDR, &req[i]);
        if (io >= 0) {
            memcpy(ns[i].mac, req[i].ifr_hwaddr.sa_data, MAC_SIZE);
        }
    }

    close(sockfd);
    return count;
}
