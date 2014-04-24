#include <sys/types.h>
#include <sys/socket.h>

struct cmsghdr *cmsg_firsthdr(struct msghdr *msgh)
{
    return CMSG_FIRSTHDR(msgh);
}

struct cmsghdr *cmsg_nexthdr(struct msghdr *msgh, struct cmsghdr *cmsg)
{
    return CMSG_NXTHDR(msgh,cmsg);
}

void *cmsg_data(struct cmsghdr *cmsg)
{
    return (void*)CMSG_DATA(cmsg);
}

size_t cmsg_space(size_t len)
{
    return CMSG_SPACE(len);
}

size_t cmsg_len(size_t len)
{
    return CMSG_LEN(len);
}
