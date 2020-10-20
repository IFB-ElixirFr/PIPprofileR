#!/bin/bash

docker pull tdenecker/pip-profiler

BASEDIR=$(pwd)
echo "$BASEDIR"

echo '#!/bin/bash' > $BASEDIR/PIPprofileR.sh
echo 'docker run --rm -p 3838:3838 -v' $BASEDIR':/srv/shiny-server -v' $BASEDIR'/log/:/var/log/shiny-server --name PIPprofileR tdenecker/pip-profiler' >> $BASEDIR/PIPprofileR.sh

chmod +x $BASEDIR/PIPprofileR.sh
