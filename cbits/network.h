#ifdef mingw32_HOST_OS
#include <winsock2.h>
#else
#include <netinet/in.h>
#endif

#define NAME_SIZE (128+4)
#define MAC_SIZE 6

struct sockaddr_list {
    struct sockaddr_list *next;
    struct sockaddr_storage addr;
};

struct network_interface {
    struct network_interface *next;
    wchar_t name[NAME_SIZE];
    struct sockaddr_list *addresses;
    unsigned char mac_address[MAC_SIZE];
};

struct network_interface* networkinfo_get_interfaces();

void networkinfo_free_interfaces(struct network_interface *ns);
