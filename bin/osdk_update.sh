#!/bin/bash

pushd $OSDK_DIR/RendezVous/Core/setup/RendezVousServer; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Core/setup/RendezVousSwig/; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/MatchMaking; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/Jugem/Matchmake; python setup.py install; popd
pushd $OSDK_DIR/RendezVous/Services/Jugem/DataStore/; python setup.py install; popd
