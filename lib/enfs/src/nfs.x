/* Copy and pasted from RFC 1094, with some repairs since the bastards
   didn't include one that worked ;-) */

const MAXDATA = 8192;
const MAXPATHLEN = 1024;
const MAXNAMLEN = 255;
const COOKIESIZE  = 4;
const FHSIZE = 32;

enum stat {
  NFS_OK = 0,
  NFSERR_PERM=1,
  NFSERR_NOENT=2,
  NFSERR_IO=5,
  NFSERR_NXIO=6,
  NFSERR_ACCES=13,
  NFSERR_EXIST=17,
  NFSERR_NODEV=19,
  NFSERR_NOTDIR=20,
  NFSERR_ISDIR=21,
  NFSERR_FBIG=27,
  NFSERR_NOSPC=28,
  NFSERR_ROFS=30,
  NFSERR_NAMETOOLONG=63,
  NFSERR_NOTEMPTY=66,
  NFSERR_DQUOT=69,
  NFSERR_STALE=70,
  NFSERR_WFLUSH=99
};

enum ftype {
  NFNON = 0,
  NFREG = 1,
  NFDIR = 2,
  NFBLK = 3,
  NFCHR = 4,
  NFLNK = 5
};

typedef opaque nfscookie[COOKIESIZE];

typedef opaque fhandle[FHSIZE];

typedef string filename<MAXNAMLEN>;

typedef string path<MAXPATHLEN>;

struct linkargs {
  fhandle fromfh;
  fhandle tofh;
  filename toname;
};

struct timeval {
  unsigned int seconds;
  unsigned int useconds;
};

struct fattr {
  ftype        type;
  unsigned int mode;
  unsigned int nlink;
  unsigned int uid;
  unsigned int gid;
  unsigned int size;
  unsigned int blocksize;
  unsigned int rdev;
  unsigned int blocks;
  unsigned int fsid;
  unsigned int fileid;
  timeval      atime;
  timeval      mtime;
  timeval      ctime;
};

struct sattr {
  unsigned int mode;
  unsigned int uid;
  unsigned int gid;
  unsigned int size;
  timeval      atime;
  timeval      mtime;
};


struct symlinkargs {
  fhandle fromfh;
  filename fromname;
  filename topath;
  sattr sattr;
};

/*
struct nfsdata {
  fhandle file;
  sattr attributes;
};
*/
typedef opaque nfsdata<8192>;

struct sattrargs {
  fhandle file;
  sattr attributes;
};

union readlinkres switch (stat status) {
 case NFS_OK:
   path data;
 default:
   void;
};

struct readargs {
  fhandle file;
  unsigned offset;
  unsigned count;
  unsigned totalcount;
};

union readres switch (stat status) {
 case NFS_OK:
   struct {
     fattr attributes;
     nfsdata data;
   } readres1;
 default:
   void;
};

struct writeargs {
  fhandle file;
  unsigned beginoffset;
  unsigned offset;
  unsigned totalcount;
  nfsdata data;
};

struct diropargs {
  fhandle  dir;
  filename name;
};

struct createargs {
  diropargs where;
  sattr attributes;
};

struct renameargs {
  diropargs from;
  diropargs to;
};

struct readdirargs {
  fhandle dir;
  nfscookie cookie;
  unsigned count;
};

struct entry {
  unsigned fileid;
  filename name;
  nfscookie cookie;
  entry *nextentry;
};

union readdirres switch (stat status) {
 case NFS_OK:
   struct {
     entry *entries;
     bool eof;
   } readdirok;
 default:
   void;
};

union statfsres switch (stat status) {
 case NFS_OK:
   struct {
     unsigned tsize;
     unsigned bsize;
     unsigned blocks;
     unsigned bfree;
     unsigned bavail;
   } info;
 default:
   void;
};

union attrstat switch (stat status) {
 case NFS_OK:
   fattr attributes;
 default:
   void;
};

union diropres switch (stat status) {
 case NFS_OK:
   struct {
     fhandle file;
     fattr   attributes;
   } diropok;
 default:
   void;
};

/*
 * Remote file service routines
 */
program NFS_PROGRAM {
  version NFS_VERSION {
    void
      NFSPROC_NULL(void)              = 0;

    attrstat
      NFSPROC_GETATTR(fhandle)        = 1;

    attrstat
      NFSPROC_SETATTR(sattrargs)      = 2;

    void
      NFSPROC_ROOT(void)              = 3;

    diropres
      NFSPROC_LOOKUP(diropargs)       = 4;

    readlinkres
      NFSPROC_READLINK(fhandle)       = 5;

    readres
      NFSPROC_READ(readargs)          = 6;

    void
      NFSPROC_WRITECACHE(void)        = 7;

    attrstat
      NFSPROC_WRITE(writeargs)        = 8;

    diropres
      NFSPROC_CREATE(createargs)      = 9;

    stat
      NFSPROC_REMOVE(diropargs)       = 10;

    stat
      NFSPROC_RENAME(renameargs)      = 11;

    stat
      NFSPROC_LINK(linkargs)          = 12;

    stat
      NFSPROC_SYMLINK(symlinkargs)    = 13;

    diropres
      NFSPROC_MKDIR(createargs)       = 14;

    stat
      NFSPROC_RMDIR(diropargs)        = 15;

    readdirres
      NFSPROC_READDIR(readdirargs)    = 16;

    statfsres
      NFSPROC_STATFS(fhandle)         = 17;
  } = 2;
} = 100003;

/* MOUNT */

const MNTPATHLEN = 1024;
const MNTNAMELEN = 255;

union fhstatus switch (unsigned status) {
 case 0:
   fhandle directory;
 default:
   void;
};

typedef string dirpath<MNTPATHLEN>;
typedef string name<MNTNAMELEN>;
	
struct *mountlist {
  name hostname;
  dirpath directory;
  mountlist nextentry;
};

struct *groups {
  name grname;
  groups grnext;
};

struct *exportlist {
  dirpath filesys;
  groups groups;
  exportlist next;
};

program MOUNTPROG {
  version MOUNTVERS {
    void
      MOUNTPROC_NULL(void) = 0;

    fhstatus
      MOUNTPROC_MNT(dirpath) = 1;

    mountlist
      MOUNTPROC_DUMP(void) = 2;

    void
      MOUNTPROC_UMNT(dirpath) = 3;

    void
      MOUNTPROC_UMNTALL(void) = 4;

    exportlist
      MOUNTPROC_EXPORT(void) = 5;

  } = 1;
} = 100005;

