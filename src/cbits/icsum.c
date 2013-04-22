#include "icsum.h"

unsigned int c_icsum16 (unsigned int acc, void* data, size_t len)
{
    unsigned short *vector = data;
    size_t i;
    
    for (i=0; i<len; ++i, ++vector) {
        acc += *vector;
    }
    return acc;
}
