#define _WIN32_WINNT 0x0501

#include <stdio.h>

#include <winsock2.h>
#include <ws2tcpip.h>
#include <iptypes.h>
#include <iphlpapi.h>

#include "network.h"

#include "common.h"

static int get_adapters_addresses(IP_ADAPTER_ADDRESSES *adapters, ULONG *size)
{
    return GetAdaptersAddresses(AF_UNSPEC, 0, 0, adapters, size);
}

struct network_interface* networkinfo_get_interfaces()
{
    IP_ADAPTER_ADDRESSES *adapters, *adapter;
    IP_ADAPTER_UNICAST_ADDRESS *unicast;
    ULONG buffer_size;
    DWORD error;
    struct network_interface *current;

    /* make an initial call to get the necessary
     * size into the buffer_size variable */
    error = get_adapters_addresses(NULL, &buffer_size);

    if (error != ERROR_BUFFER_OVERFLOW) {
        /* if we didn't get ERROR_BUFFER_OVERFLOW
         * then buffer_size was not set */
        return 0;
    }

    adapters = malloc(buffer_size);
    error = get_adapters_addresses(adapters, &buffer_size);
    current = NULL;

    if (error == NO_ERROR) {
        adapter = adapters;

        while (adapter) {
            prepend_empty(&current);

            wszcopy(current->name, adapter->FriendlyName, NAME_SIZE);
            memcpy(current->mac_address, adapter->PhysicalAddress, MAC_SIZE);

            for (unicast = adapter->FirstUnicastAddress; unicast; unicast = unicast->Next) {
                sockaddr_prepend(&(current->addresses), unicast->Address.lpSockaddr);
            }

            adapter = adapter->Next;
        }
    }

    free(adapters);
    return current;
}
