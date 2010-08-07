#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#include "network.h"
#include "common.h"


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
        mbswszcopy(ns[i].name, req[i].ifr_name, NAME_SIZE);
        ipv4copy(&ns[i].ip_address, &req[i].ifr_addr);

        io = ioctl(sockfd, SIOCGIFHWADDR, &req[i]);
        if (io >= 0) {
            memcpy(&ns[i].mac_address, req[i].ifr_hwaddr.sa_data, MAC_SIZE);
        }
    }

    close(sockfd);
    return count;
}
