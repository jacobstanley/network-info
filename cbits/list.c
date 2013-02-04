#include "list.h"
#include <stdlib.h>
#include <string.h>

#ifdef OS_WINDOWS
#include <ws2tcpip.h>
#endif

void prepend_address(struct addr_list **list, struct sockaddr *pl) {
  struct sockaddr_storage *buf = malloc(sizeof(struct sockaddr_storage));
  memset(buf,0 ,sizeof(struct sockaddr_storage));
  if (pl->sa_family == AF_INET) {
      memcpy(buf, pl, sizeof(struct sockaddr_in));
  }
  else if (pl->sa_family == AF_INET6) {
    memcpy(buf, pl, sizeof(struct sockaddr_in6));
  }
  else {
    return;
  }
  struct addr_list *head = malloc(sizeof(struct addr_list));
  memset(head,0 ,sizeof(struct addr_list));
  head->payload = buf;
  head->next = *list;
  *list = head;
}

void list_free(struct addr_list* head) {
  struct addr_list *cur = head;
  struct addr_list *tmp = NULL;
  while (cur != NULL){
    free(cur->payload);
    tmp = cur;
    cur = cur->next;
    free(tmp);
  }
}
