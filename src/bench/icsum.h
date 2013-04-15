#ifndef _ICSUM_H_
#define _ICSUM_H_

#include <string.h>

/* function gets previous icsum result and calculate
 * crc sum of data mount of length = len * 2 bytes;
 */
unsigned int 
c_icsum16 ( unsigned int prev, void *data, 
        size_t len /* words16! not bytes */ );

#endif //_ICSUM_H_
