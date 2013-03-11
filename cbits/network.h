#ifndef _NETWORK_h
#define _NETWORK_h

#ifdef mingw32_HOST_OS
#include <winsock2.h>
#else
#include <netinet/in.h>
#endif

#include <wchar.h>

#define NAME_SIZE (128+4)
#define MAC_SIZE 6

struct addr_list {
    struct addr_list* next;
    struct sockaddr_storage* payload;
};


struct network_interface {
    wchar_t name[NAME_SIZE];
    struct addr_list *addresses;
    unsigned char mac_address[MAC_SIZE];
};


int c_get_network_interfaces(struct network_interface *ns, int max_ns);

#endif
