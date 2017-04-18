ariaRacer
=========
A dockerized Aria robots race manager with a web UI

Racer Homepage 
--------------
To access a racer home page use the following web address:

	http://<ip address of web server>/racer-id/<racer-id number here>/home

For example a racer named Bob who has a racer-id of 2. The ip address of the web
server is 10.0.0.10.31. Bob will upload his code to

	http://10.0.0.10.31/racer-id/2/home

Once on the racer home page you can upload new c++ source code files to build and race on
the server. **THESE ARE NOT VISUAL STUDIOS PROJECT FILES!!!!. ONLY .cpp files
should be uploaded**. Note: your entire robot code must be in a source code
file. Multi-file applications will not work.

Selecting Race To build 
-----------------------

Each build uploaded shows the time the build was uploaded and the fastest race
time the build gave. Select the name of the build to race with that build
