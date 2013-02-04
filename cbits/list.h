#ifndef _LIST_H
#define _LIST_H
#include "network.h"

void prepend_address(struct addr_list **list,struct sockaddr *pl);
void list_free(struct addr_list* head);
void print_addrs(struct network_interface* iface);

#endif
