#define NAME_SIZE (128+4)
#define MAC_SIZE 6

typedef long ipv4;

struct network_interface {
    char name[NAME_SIZE];
    ipv4 address;
    ipv4 netmask;
    unsigned char mac_address[MAC_SIZE];
};

int c_get_network_interfaces(int sockfd, struct network_interface *ns, int max_ns);
