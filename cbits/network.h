#ifdef OS_WINDOWS
#include <winsock2.h>
#else
#include <netinet/in.h>
#endif

#define NAME_SIZE (128+4)
#define MAC_SIZE 6

struct network_interface {
    wchar_t name[NAME_SIZE];
    struct sockaddr_in ip_address;
    struct sockaddr_in6 ip6_address;
    unsigned char mac_address[MAC_SIZE];
};

int c_get_network_interfaces(struct network_interface *ns, int max_ns);
