#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include <ifaddrs.h>
#include <netdb.h>
#include <netpacket/packet.h>

#include "network.h"
#include "common.h"


void maccopy(unsigned char *dst, struct sockaddr *addr)
{
    /* TODO check that sll_halen is equal to 6 (MAC_SIZE) */
    memcpy(dst, ((struct sockaddr_ll *)addr)->sll_addr, MAC_SIZE);
}

struct network_interface *add_interface(struct network_interface *ns, const wchar_t *name, int max_ns)
{
    int i;
    for (i = 0; i < max_ns; i++) {
        if (wcsempty(ns[i].name)) {
            wszcopy(ns[i].name, name, NAME_SIZE);
            return &ns[i];
        } else if (wcscmp(ns[i].name, name) == 0) {
            return &ns[i];
        }
    }
    return NULL;
}

int count_interfaces(struct network_interface *ns, int max_ns)
{
    int i;
    for (i = 0; i < max_ns; i++) {
        if (wcsempty(ns[i].name)) {
            break;
        }
    }
    return i;
}

int c_get_network_interfaces(struct network_interface *ns, int max_ns)
{
    struct network_interface *n;
    struct ifaddrs *ifaddr, *ifa;
    struct sockaddr *addr;
    wchar_t name[NAME_SIZE];
    int family, error;
   
    error = getifaddrs(&ifaddr);
    if (error != 0) {
        perror("getifaddrs");
        return 0;
    }

    memset(ns, 0, sizeof(struct network_interface) * max_ns);

    for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
        mbswszcopy(name, ifa->ifa_name, NAME_SIZE);
        addr = ifa->ifa_addr;
        family = addr->sa_family;

        n = add_interface(ns, name, max_ns);

        if (family == AF_INET) {
            ipv4copy(&n->ip_address, addr);
        } else if (family == AF_INET6) {
            ipv6copy(&n->ip6_address, addr);
        } else if (family == AF_PACKET) {
            maccopy(n->mac_address, addr);
        }
    }

    freeifaddrs(ifaddr);
    return count_interfaces(ns, max_ns);
}
