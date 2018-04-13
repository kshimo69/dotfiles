#!/usr/bin/env zsh

pushd $OSDK_DIR/RendezVous/Core/setup/RendezVousServer; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Core/setup/RendezVousSwig/; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/MatchMaking; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/Jugem/Matchmake; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/Jugem/MatchmakeReferee; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/Jugem/DataStore/; python setup.py install; popd
if [ -d $OSDK_DIR/RendezVous/Services/Jugem/Plugin/Calico ]
then
    pushd $OSDK_DIR/RendezVous/Services/Jugem/Plugin/Calico/; python setup.py install; popd
fi
if [ -d $OSDK_DIR/RendezVous/Services/Jugem/Plugin/IdConverter ]
then
    pushd $OSDK_DIR/RendezVous/Services/Jugem/Plugin/IdConverter/; python setup.py install; popd
fi
if [ -d $OSDK_DIR/RendezVous/Services/Jugem/Plugin/RedisConnection ]
then
    pushd $OSDK_DIR/RendezVous/Services/Jugem/Plugin/RedisConnection/; python setup.py install; popd
fi
