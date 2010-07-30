#define _WIN32_WINNT 0x0501

#include <stdio.h>
#include <windows.h>
#include <iphlpapi.h>
#include <ws2tcpip.h>
#include <winsock2.h>

#include "network.h"


#define INET_ADDRSTRLEN 16
#define INET6_ADDRSTRLEN 46

void print_error(const wchar_t *operation, ULONG error) {
    DWORD length;
    LPVOID message;

    if (error == ERROR_ADDRESS_NOT_ASSOCIATED) {
        wprintf(L"%s failed: ERROR_ADDRESS_NOT_ASSOCIATED\n", operation);
        return;
    } else if (error == ERROR_BUFFER_OVERFLOW) {
        wprintf(L"%s failed: ERROR_BUFFER_OVERFLOW\n", operation);
        return;
    } else if (error == ERROR_INVALID_PARAMETER) {
        wprintf(L"%s failed: ERROR_INVALID_PARAMETER\n", operation);
        return;
    } else if (error == ERROR_NOT_ENOUGH_MEMORY) {
        wprintf(L"%s failed: ERROR_NOT_ENOUGH_MEMORY\n", operation);
        return;
    } else if (error == ERROR_NO_DATA) {
        wprintf(L"%s failed: ERROR_NO_DATA\n", operation);
        return;
    } else {
        length = FormatMessageW(
                FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                error,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPWSTR) &message,
                0,
                NULL);

        if (length != 0) {
            wprintf(L"%s failed: error %d: %s", operation, error, message);
            LocalFree(message);
        } else {
            wprintf(L"%s failed: error %d\n", operation, error);
        }
    }
}

const char *inet_ntop(int af, const void *src, char *dst, int cnt) {
    if (af == AF_INET) {
        struct sockaddr_in in;
        memset(&in, 0, sizeof(in));
        in.sin_family = AF_INET;
        memcpy(&in.sin_addr, src, sizeof(struct in_addr));
        getnameinfo((struct sockaddr *)&in, sizeof(struct sockaddr_in), dst, cnt, NULL, 0, NI_NUMERICHOST);
        return dst;
    } else if (af == AF_INET6) {
        struct sockaddr_in6 in;
        memset(&in, 0, sizeof(in));
        in.sin6_family = AF_INET6;
        memcpy(&in.sin6_addr, src, sizeof(struct in_addr6));
        getnameinfo((struct sockaddr *)&in, sizeof(struct sockaddr_in6), dst, cnt, NULL, 0, NI_NUMERICHOST);
        return dst;
    }
    return NULL;
}

int c_get_network_interfaces(struct network_interface *ns, int max_ns) {
    ULONG error;
    ULONG size;
    IP_ADAPTER_ADDRESSES *addresses, *current;

    error = GetAdaptersAddresses(AF_UNSPEC, 0, NULL, NULL, &size);
    if (error != ERROR_BUFFER_OVERFLOW) {
        print_error(L"GetAdaptersAddresses", error);
        return 0;
    }

    addresses = malloc(size);
    error = GetAdaptersAddresses(AF_UNSPEC, 0, NULL, addresses, &size);
    if (error != NO_ERROR) {
        print_error(L"GetAdaptersAddresses", error);
        return 0;
    }

    current = addresses;
    while (current) {
        wprintf(L"Adapter: %s\n", current->FriendlyName);

        IP_ADAPTER_UNICAST_ADDRESS *unicast = current->FirstUnicastAddress;
        struct sockaddr *addr = unicast->Address.lpSockaddr;
        struct sockaddr_in *inet = (struct sockaddr_in *)addr;
        struct sockaddr_in6 *inet6 = (struct sockaddr_in6 *)addr;

        char str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &inet->sin_addr, str, INET_ADDRSTRLEN);
        printf("IPv4: %s\n", str);
        
        char str6[INET6_ADDRSTRLEN];
        inet_ntop(AF_INET6, &inet6->sin6_addr, str6, INET6_ADDRSTRLEN);
        printf("IPv6: %s\n", str6);
        
        wprintf(L"\n");
        current = current->Next;
    }
    
    return 0;
}


int main() {
    c_get_network_interfaces(NULL, 0);
}
