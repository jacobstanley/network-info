inline void ipv4copy(struct sockaddr_in *dst, struct sockaddr *addr)
{
    memcpy(dst, addr, sizeof(struct sockaddr_in));
}

inline void ipv6copy(struct sockaddr_in6 *dst, struct sockaddr *addr)
{
  memcpy(dst, addr, sizeof(struct sockaddr_in6));
}

inline int wcsempty(const wchar_t *str)
{
    return wcslen(str) == 0;
}

inline void wszcopy(wchar_t *dst, const wchar_t *src, size_t dst_size)
{
    wcsncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}

inline void mbswszcopy(wchar_t *dst, const char *src, size_t dst_size)
{
    mbstowcs(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}
