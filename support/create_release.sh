#!/bin/sh
#
# Function: Run it from the jungerl/lib/$App directory
#           in order to create a new release tar-ball.
#

CUR=`pwd`
APP=`basename ${CUR}`
VSN=`cat vsn.mk | sed -e 's/.*=//'`
DIR=release/${APP}-${VSN}

mkdir -p ${DIR}
mkdir -p ${DIR}/ebin

for i in `ls -d * | sed -e 's/release//g'`
do
  [ -d ${i} ] && cp -Rp ${i} ${DIR} && rm -rf  ${DIR}/${i}/CVS
  [ -f ${i} ] && cp -p ${i} ${DIR} 
done
rm -rf ${DIR}/CVS
rm -rf ${DIR}/ebin/*.beam

(cd release; tar cvzf ${APP}-${VSN}.tar.gz ${APP}-${VSN}; rm -rf ${APP}-${VSN})
