#!/bin/bash

# Instructions:

# 1. run this script with 1 argument: either (A or B)
#    where the assumption is that the two ends of the connection
#    will decide between themselves who will be A and who will be B

# 2. Startup Extempore and connect to 'local' Extempore server as normal

# 3. connect to 'remote' Extempore on remote machine through local port
#    {A,B}_EXTEMPORE_TUNNEL_PORT (use the one with *your* host (A or B)
#    In other words, if you are host A then connect to remote on port
#    A_EXTEMPORE_TUNNEL_PORT

# 4. create an empty extempore-mode buffer in emacs, then turn on the
#    extempore-sb-mode, listening on port EXTEMPORE_SLAVE_BUFFER_PORT

# 5. any buffer you 'push' with
#    extempore-sb-push-current-buffer to
#    localhost:{A,B}_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT will
#    appear as a read-only 'slave' buffer on the other person's box

# 6. To stop receiving updates to the slave buffer, toggle
#    extempore-sb-mode off

# this is the intermediary for the ssh tunnel
SERVER=user@host.name.com

# local machine ports

EXTEMPORE_PRIMARY_PORT=7099
EXTEMPORE_UTILITY_PORT=7098
EXTEMPORE_SLAVE_BUFFER_PORT=8420

# tunneling ports

A_EXTEMPORE_PRIMARY_TUNNEL_PORT=27099
A_EXTEMPORE_UTILITY_TUNNEL_PORT=27098
A_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT=28420

B_EXTEMPORE_PRIMARY_TUNNEL_PORT=37099
B_EXTEMPORE_UTILITY_TUNNEL_PORT=37098
B_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT=38420

if [ "$1" == 'B' ]; then

    echo -e "Setting up the tunneling from Host B's end via $SERVER\n"

    echo "Mapping:"
    echo "local port $B_EXTEMPORE_PRIMARY_TUNNEL_PORT -> remote port $EXTEMPORE_PRIMARY_PORT"
    echo "local port $B_EXTEMPORE_UTILITY_TUNNEL_PORT -> remote port $EXTEMPORE_UTILITY_PORT"
    echo "local port $B_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT -> remote port $EXTEMPORE_SLAVE_BUFFER_PORT"

    # add printout for which ports are mapped to which

    ssh -R $A_EXTEMPORE_PRIMARY_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_PRIMARY_PORT \
        -R $A_EXTEMPORE_UTILITY_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_UTILITY_PORT \
        -R $A_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_SLAVE_BUFFER_PORT \
        -L $B_EXTEMPORE_PRIMARY_TUNNEL_PORT:127.0.0.1:$B_EXTEMPORE_PRIMARY_TUNNEL_PORT \
        -L $B_EXTEMPORE_UTILITY_TUNNEL_PORT:127.0.0.1:$B_EXTEMPORE_UTILITY_TUNNEL_PORT \
        -L $B_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT:127.0.0.1:$B_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT \
        -N $SERVER

elif [ "$1" == 'A' ]; then

    echo -e "Setting up the tunneling from Host A's end via $SERVER\n"

    echo "Mapping:"
    echo "local port $A_EXTEMPORE_PRIMARY_TUNNEL_PORT -> remote port $EXTEMPORE_PRIMARY_PORT"
    echo "local port $A_EXTEMPORE_UTILITY_TUNNEL_PORT -> remote port $EXTEMPORE_UTILITY_PORT"
    echo "local port $A_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT -> remote port $EXTEMPORE_SLAVE_BUFFER_PORT"

    ssh -R $B_EXTEMPORE_PRIMARY_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_PRIMARY_PORT \
        -R $B_EXTEMPORE_UTILITY_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_UTILITY_PORT \
        -R $B_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT:127.0.0.1:$EXTEMPORE_SLAVE_BUFFER_PORT \
        -L $A_EXTEMPORE_PRIMARY_TUNNEL_PORT:127.0.0.1:$A_EXTEMPORE_PRIMARY_TUNNEL_PORT \
        -L $A_EXTEMPORE_UTILITY_TUNNEL_PORT:127.0.0.1:$A_EXTEMPORE_UTILITY_TUNNEL_PORT \
        -L $A_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT:127.0.0.1:$A_EXTEMPORE_SLAVE_BUFFER_TUNNEL_PORT \
        -N $SERVER

else
    echo "You must provide an host argument (either A or B)"
fi

# Local Variables:
# mode: sh
# End:
