Minimal NFS v2 server in Erlang
-------------------------------

This is a basic extensible NFS server. One example filesystem
implementation is given, it's like "/proc" but for erlang processes.

Quick guide to running the "proc filesystem":

  (cd src; make)           # compile the code
  bin/run_procfs.sh        # start the NFS server
  ## in another shell, as root:
  bin/mount_procfs.sh mnt  # mount the server in ./mnt

Then you're away.

--------

Notes:

If you restart the server, you have to remount the file system.

nfs_server is the main module; nfs_procfs is the erlang clone of
"/proc". If you want to make your own filesystem, nfs_procfs is the
template.

This package is based on the rpc-1.1 contribution from erlang.org. For
convenience a precompiled version of that program is included.

-- luke@bluetail.com

