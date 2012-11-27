# package up the binary extempore executable

BIN_DIR=extempore-$(date "+%Y%m%d")

echo "Creating extempore binary archive in ${BIN_DIR}"

mkdir $BIN_DIR

cp extempore $BIN_DIR

echo 'see http://benswift.me/2012-10-30-downloading-and-installing-extempore.html 
for instructions on how to get up and running with Extempore.' > $BIN_DIR/README.md

tar -cvzf ${BIN_DIR}.tar.gz $BIN_DIR

rm -r $BIN_DIR
