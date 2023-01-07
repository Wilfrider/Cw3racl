funShellCmdErrCheck(){
    if [ "${?}" -ne "$2" ];then
        echo "Cur $1 LINE At File:$(basename $0) error"
        exit 1
    fi
}

error()
{
    echo "$*. Exiting"
    exit 1
}

CS_P=~/DepLib809.so
g++ -Wno-deprecated-declarations -O3 -m64 -fPIC -std=c++11 -c Deps.cpp -o /tmp/DepswAFEAd809.o
funShellCmdErrCheck $LINENO 0


g++ -shared -o $CS_P /tmp/DepswAFEAd809.o  -lpthread -lz -lm -ldl -lrt  -lcurl
funShellCmdErrCheck $LINENO 0

strip --strip-all $CS_P
funShellCmdErrCheck $LINENO 0

ls -al $CS_P

echo "compiled OK!!"
