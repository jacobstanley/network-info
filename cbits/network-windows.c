#define _WIN32_WINNT 0x0501

#include <stdio.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <iptypes.h>
#include <iphlpapi.h>

#include "network.h"


int szcopy(char *dst, char *src, size_t dst_size) {
    strncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}

int c_get_network_interfaces(int sockfd, struct network_interface *ns, int max_ns) {
    ULONG buffer_size;
    PIP_ADAPTER_INFO adapters;
    DWORD error;
    int i = 0;

    /* make an initial call to GetAdaptersInfo to get
       the necessary size into the buffer_size variable */
    error = GetAdaptersInfo(NULL, &buffer_size);

    if (error != ERROR_BUFFER_OVERFLOW) {
        /* if we didn't get ERROR_BUFFER_OVERFLOW
           then buffer_size was not set */
        return 0;
    }

    adapters = malloc(buffer_size);
    error = GetAdaptersInfo(adapters, &buffer_size);

    if (error == NO_ERROR) {
        IP_ADAPTER_INFO *adapter = adapters;

        while (i < max_ns && adapter) {
            printf("address: %s\n", adapter->IpAddressList.IpAddress.String);
            printf("netmask: %s\n", adapter->IpAddressList.IpMask.String);
            
            szcopy(ns[i].name, adapter->Description, NAME_SIZE);
            memcpy(ns[i].mac_address, adapter->Address, MAC_SIZE);

            i++;
            adapter = adapter->Next;
        }
    }

    free(adapters);
    return i;
}

/*
int c_get_network_interfaces(int sockfd, struct network_interface *ns, int max_ns) {
    int error;
    int i;
    int count;
    unsigned long size;
    INTERFACE_INFO ifaces[20];

    error = WSAIoctl(sockfd, SIO_GET_INTERFACE_LIST, 0, 0, &ifaces, sizeof(ifaces), &size, 0, 0);
    if (error == SOCKET_ERROR) {
        printf("WSAIoctl failed: error %d\n", WSAGetLastError());
        return 0;
    }

    count = min(max_ns, size / sizeof(INTERFACE_INFO));
    for (i = 0; i < count; ++i) {
        ns[i].address = ((struct sockaddr_in *) &ifaces[i].iiAddress)->sin_addr.s_addr;
        ns[i].netmask = ((struct sockaddr_in *) &ifaces[i].iiNetmask)->sin_addr.s_addr;
    }

    return count;
}*/
