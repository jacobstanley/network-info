#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include <sys/types.h>
#include <ifaddrs.h>
#include <netdb.h>

#ifdef __linux__
#   include <netpacket/packet.h>
#else
#   include <sys/socket.h>
#   include <net/if.h>
#   include <net/if_dl.h>
#   ifndef AF_PACKET
#       define AF_PACKET AF_LINK
#   endif
#endif

#ifdef __FreeBSD__
#   include <net/pfvar.h>
#endif

#include "network.h"
#include "common.h"


void maccopy(unsigned char *dst, struct sockaddr *addr)
{
#ifdef __linux__
    /* TODO check that sll_halen is equal to 6 (MAC_SIZE) */
    memcpy(dst, ((struct sockaddr_ll *)addr)->sll_addr, MAC_SIZE);
#else
    /* TODO check that sdl_alen is equal to 6 (MAC_SIZE) */
    struct sockaddr_dl *sdl = (struct sockaddr_dl *)addr;
    memcpy(dst, sdl->sdl_data + sdl->sdl_nlen, MAC_SIZE);
#endif
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
        /* TODO printing the error to stderr is not a very nice thing for
         * TODO a library to do, but i've never seen this happen and its
         * TODO probably better than failing silently. */
        perror("getifaddrs");
        return 0;
    }

    memset(ns, 0, sizeof(struct network_interface) * max_ns);

    for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
        /* check we actually have an address in this item */
        addr = ifa->ifa_addr;
        if (addr == NULL)
            continue;

        /* convert the interface name to wide characters */
        mbswszcopy(name, ifa->ifa_name, NAME_SIZE);

        /* lookup or add a new interface with the given name */
        n = add_interface(ns, name, max_ns);

        /* extract the address from this item */
        family = addr->sa_family;
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
