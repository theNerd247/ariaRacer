/**
 * @file main.cpp 
 *
 * Test program for the Aria library
 *
 * @author Noah Harvey (nharvey@spsu.edu)
 *
 */

#include <Aria.h>

int main(int argc, char *argv[])
{
	//initialize Aria
	Aria::init(); 

	//handle cmd arguments
	ArArgumentParser argParser(&argc, argv);
	argParser.loadDefaultArguments();

	//create new robot
	ArRobot robot("blarg"); 

	//connect to the robot
	ArRobotConnector robotConnector(&argParser, &robot);
	if(!robotConnector.connectRobot()) 
		ArLog::log(ArLog::Terse, "Could not connect to the robot.");
	else
		ArLog::log(ArLog::Terse, "Connection to robot sucessful.");

	/* Shutdown Aria and exit */
	Aria::shutdown();
	Aria::exit(0);

	return 0;
}
