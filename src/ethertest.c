#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <assert.h>

#define BIGEND16(num) \
    (uint16_t)(((num) >> 8) | ((num) << 8))

#define ETH_HW_ADDR_LEN 6
#define ETH_PAYLOAD_LEN 64
#define ETH_CRC_LEN 4

typedef struct {
    uint8_t dst[ETH_HW_ADDR_LEN];
    uint8_t src[ETH_HW_ADDR_LEN];

    uint16_t ethertype;
//    uint32_t tag;
} ethernet_header_t;

#define ETH_HEADER_LEN (sizeof (ethernet_header_t))
#define p_eth_payload(buff) \
    ((buff) + ETH_HEADER_LEN)
#define p_eth_checksum(buff) \
    ((buff) + ETH_HEADER_LEN + ETH_PAYLOAD_LEN)

#define ETH_FRAME_LEN \
    (ETH_HEADER_LEN + ETH_PAYLOAD_LEN + ETH_CRC_LEN)

typedef void (*Payload) (void *pay_load_buffer, void *opaque);

uint32_t ethernet_crc (const void *frame, size_t frame_len) {
    //TODO: implement
    return 0;
}

void *ethernet_frame ( const ethernet_header_t *header, Payload payload, void *opaque)
{
    void *frame = NULL;
    frame = malloc ( ETH_FRAME_LEN);
    memset (frame,0, ETH_FRAME_LEN );
    if ( (header != NULL) && (header != frame) ) {
        memcpy (frame, header, ETH_HEADER_LEN );
    };
    if (payload != NULL ) {
        payload ( p_eth_payload(frame), opaque);
    }
    *((uint32_t *)p_eth_checksum(frame)) = ethernet_crc (frame, ETH_FRAME_LEN);
    return frame;
}

#define IP_ADDR_LEN 4

typedef struct {
    uint8_t     ver_ihl;
    uint8_t     dscp_ecn;
    uint16_t    totlen;
    uint16_t	id;
    uint16_t	flag_off;
    uint8_t 	ttl;
    uint8_t 	protocol;
    uint16_t	checksum;
    uint8_t	    src[IP_ADDR_LEN];
    uint8_t 	dst[IP_ADDR_LEN];
} ip_header_t; 

#define IP_HEADER_LEN \
    sizeof (ip_header_t)
#define p_ip_payload(buff) \
    ((buff)+IP_HEADER_LEN)
#define IP_PAYLOAD_LEN \
    ETH_PAYLOAD_LEN - IP_HEADER_LEN

uint16_t ip_checksum (const void *buffer, size_t len) {
    register long sum = 0;
    const unsigned short *addr = (const unsigned short*) (buffer);

    while(len > 1){
	    sum += *addr++;
	    len -= 2;
    }

    if (len & 1) {
	    sum += (*addr & 0xff);
    }
    sum = (sum & 65535) + (sum >> 16);
    return ~sum;
}

void ip_packet ( const ip_header_t *header, void *ip_packet_buffer,
        size_t ip_packet_buffer_len, Payload payload, void *opaque) 
{
    ip_header_t *res_header;

    assert (ip_packet_buffer != NULL);
    assert (ip_packet_buffer_len > IP_HEADER_LEN);

    res_header = (ip_header_t *)ip_packet_buffer;

    if (header != NULL) {
        *res_header = *header;
    }
    res_header->checksum = 0;
 
    res_header->checksum = ip_checksum (res_header, IP_HEADER_LEN);
    if (payload != NULL) {
        payload(p_ip_payload(ip_packet_buffer), opaque);
    }

}

struct s_ip_header_opaque {
    ip_header_t *header;
    void *opaque;
    Payload payload;
};

void ip_eth_payload (void *payload, void *opaque) {
    struct s_ip_header_opaque *header_opaque;

    if (opaque != NULL) {
        header_opaque = (struct s_ip_header_opaque *)opaque;
        ip_packet (header_opaque->header,
            payload, ETH_PAYLOAD_LEN, 
            header_opaque->payload, header_opaque->opaque);
    } else {
        ip_packet (NULL,NULL,ETH_PAYLOAD_LEN, NULL, opaque);
    }
}

void zero_payload (void *payload, void *opaque)
{
    size_t len = *(size_t *)opaque;
    memset (payload,0, len);
}

#define PACKETS_NUM 1000000
//        1024*1024/ETH_FRAME_LEN //1 Mb
int main (void) {
    int i;
    void *frame;
    static ip_payload_len = IP_PAYLOAD_LEN;
    ethernet_header_t eth_header = {
        .src = {0x10,0x12,0xA3,0x04,0x05,0x06},
        .dst = {0x07,0x08,0x09,0x0A,0x0B,0x0C},
//        .tag = 0x0000,
        .ethertype = BIGEND16(0x0800), 
    };
    ip_header_t ip_header = {
        .ver_ihl    = 0x45,
        .totlen     = BIGEND16(ETH_PAYLOAD_LEN),
        .id         = 0,
        .flag_off   = 0,
        .ttl        = 1,
        .protocol   = 0x05,
        .src        = {127,0,0,1},
        .dst        = {255,255,255,255},
    };
    struct s_ip_header_opaque ip_header_opaque = {
        .header     = &ip_header,
        .payload    = zero_payload,
        .opaque     = &ip_payload_len,
    };

    for (i=0; i<PACKETS_NUM; ++i) {
        frame = ethernet_frame (&eth_header, ip_eth_payload ,&ip_header_opaque);
        fwrite (frame,ETH_FRAME_LEN,1,stdout);
        free (frame);
    }
}
