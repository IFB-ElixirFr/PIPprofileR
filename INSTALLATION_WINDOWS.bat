docker pull tdenecker/pip-profiler
echo docker stop PIPprofileR >> PIPprofileR.bat
echo docker run --rm -p 3838:3838 -v %CD%:/srv/shiny-server -v %CD%\log:/var/log/shiny-server --name PIPprofileR tdenecker/pip-profiler> PIPprofileR.bat
