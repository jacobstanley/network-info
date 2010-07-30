
#define NAME_SIZE 16
#define IP_SIZE 16
#define MAC_SIZE 6

struct network_interface {
    char name[NAME_SIZE];
    char inet[IP_SIZE];
    char bcast[IP_SIZE];
    unsigned char mac[MAC_SIZE];
};

int c_get_network_interfaces(struct network_interface *ns, int max_ns);
