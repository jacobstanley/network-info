inline int ipv4copy(ipv4 *dst, struct sockaddr *addr) {
    *dst = ((struct sockaddr_in *)addr)->sin_addr.s_addr;
}

inline int ipv6copy(ipv6 *dst, struct sockaddr *addr) {
    memcpy(dst, ((struct sockaddr_in6 *)addr)->sin6_addr.s6_addr, sizeof(ipv6));
}

inline int wszcopy(wchar_t *dst, wchar_t *src, size_t dst_size) {
    wcsncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}
