#define _WIN32_WINNT 0x0501

#include <stdio.h>
#include <winsock2.h>
#include <ws2tcpip.h>

#include "network.h"


int c_get_network_interfaces(struct network_interface *ns, int max_ns) {
    printf("getting WSASocket\n");

    SOCKET sd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sd == SOCKET_ERROR) {
        printf("WSASocket failed: Error %d\n", WSAGetLastError());
        return 0;
    }

    printf("got socket\n");

    INTERFACE_INFO InterfaceList[20];
    unsigned long nBytesReturned;
    if (WSAIoctl(sd, SIO_GET_INTERFACE_LIST, 0, 0, &InterfaceList,
                sizeof(InterfaceList), &nBytesReturned, 0, 0) == SOCKET_ERROR) {
        printf("WSAIoctl failed: error %d\n", WSAGetLastError());
        return 0;
    }

    printf("got SIO_GET_INTERFACE_LIST\n");

    int nNumInterfaces = nBytesReturned / sizeof(INTERFACE_INFO);
    printf("There are %d interfaces:\n", nNumInterfaces);

    int i;
    for (i = 0; i < nNumInterfaces; ++i) {
        printf("\n");

        struct sockaddr_in *pAddress;
        pAddress = (struct sockaddr_in *) & (InterfaceList[i].iiAddress);
        printf(" %s", inet_ntoa(pAddress->sin_addr));

        pAddress = (struct sockaddr_in *) & (InterfaceList[i].iiBroadcastAddress);
        printf(" has bcast %s", inet_ntoa(pAddress->sin_addr));

        pAddress = (struct sockaddr_in *) & (InterfaceList[i].iiNetmask);
        printf(" and netmask %s\n", inet_ntoa(pAddress->sin_addr));

        printf(" Iface is ");
        u_long nFlags = InterfaceList[i].iiFlags;
        if (nFlags & IFF_UP) printf("up");
        else                 printf("down");
        if (nFlags & IFF_POINTTOPOINT) printf(", is point-to-point");
        if (nFlags & IFF_LOOPBACK)     printf(", is a loopback iface");
        printf(", and can do: ");
        if (nFlags & IFF_BROADCAST) printf("bcast ");
        if (nFlags & IFF_MULTICAST) printf("multicast ");
        printf("\n");
    }

    return 0;
}
