/* Treat this as a C file which is part of network-unix.c and network-windows.c */

static inline int wcsempty(const wchar_t *str)
{
    return wcslen(str) == 0;
}

static inline void wszcopy(wchar_t *dst, const wchar_t *src, size_t dst_size)
{
    wcsncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}

static inline void mbswszcopy(wchar_t *dst, const char *src, size_t dst_size)
{
    mbstowcs(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}

static void sockaddr_prepend_empty(struct sockaddr_list **list)
{
    struct sockaddr_list *head = malloc(sizeof(struct sockaddr_list));
    memset(head, 0, sizeof(struct sockaddr_list));

    head->next = *list;
    *list = head;
}

static void sockaddr_prepend(struct sockaddr_list **list, struct sockaddr *addr)
{
    if (addr->sa_family == AF_INET) {
        sockaddr_prepend_empty(list);
        memcpy(&(*list)->addr, addr, sizeof(struct sockaddr_in));
    } else if (addr->sa_family == AF_INET6) {
        sockaddr_prepend_empty(list);
        memcpy(&(*list)->addr, addr, sizeof(struct sockaddr_in6));
    }
}

static void sockaddr_free(struct sockaddr_list *head)
{
    struct sockaddr_list *cur = head;
    struct sockaddr_list *tmp = NULL;

    while (cur != NULL) {
        tmp = cur;
        cur = cur->next;
        free(tmp);
    }
}

static void prepend_empty(struct network_interface **list)
{
    struct network_interface *head = malloc(sizeof(struct network_interface));
    memset(head, 0, sizeof(struct network_interface));

    head->next = *list;
    *list = head;
}

void networkinfo_free_interfaces(struct network_interface *head)
{
    struct network_interface *cur = head;
    struct network_interface *tmp = NULL;

    while (cur != NULL) {
        tmp = cur;
        cur = cur->next;

        sockaddr_free(tmp->addresses);
        free(tmp);
    }

}
