
void null(void);
int my_open(char *, int);
int my_getfd(int);
int my_sendfd(int fd, int wfd);
int my_receivefd(int fd);

ssize_t writevfd(int fd, const struct iovec *iov, int iovcnt, int wfd);
ssize_t writefd(int fd, int wfd);
ssize_t readvfd(int fd, const struct iovec *iov, int iovcnt, int *rfdp);
ssize_t readfd(int fd, int *rfdp);
