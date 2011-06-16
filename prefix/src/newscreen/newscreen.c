/*
* main.c
* newscreen
*
* Created by Jeffrey Osterman on 10/30/07.
* Copyright 2007 Jeffrey Osterman. All rights reserved. 
* PROVIDED AS IS AND WITH NO WARRANTIES WHATSOEVER
*/

#include <ApplicationServices/ApplicationServices.h>

void MyDisplaySwitchToMode (CGDirectDisplayID display, CFDictionaryRef mode);

int main (int argc, const char * argv[]) {

int	h; //horizontal resolution
int v; //vertical resolution
CFDictionaryRef switchMode; //mode to swich to
CGDirectDisplayID mainDisplay; //ID of main display


if (argc != 5)
{
printf("Expected 4 inputs, but only received %i\n", argc-1);
return 0;
}

if (!strcmp(argv[1],"-h"))
h=strtol(argv[2], NULL, 0);
else if (!strcmp(argv[1],"-v"))
v=strtol(argv[2], NULL, 0);
else {
printf("Error! Expected first argument to be -h or -v\n");
return 0;
}


if (!strcmp(argv[3],"-h"))
h=strtol(argv[4], NULL, 0);
else if (!strcmp(argv[3],"-v"))
v=strtol(argv[4], NULL, 0);
else {
printf("Error! Expected third argument to be -h or -v");
return 0;
}

mainDisplay = CGMainDisplayID();
switchMode = CGDisplayBestModeForParameters(mainDisplay, 32, h, v, NULL);
MyDisplaySwitchToMode (mainDisplay, switchMode);
return 0;

}



void MyDisplaySwitchToMode (CGDirectDisplayID display, CFDictionaryRef mode)
{
CGDisplayConfigRef config; // 1
CGBeginDisplayConfiguration (&config); // 2
CGConfigureDisplayMode (config, display, mode); // 3

CGCompleteDisplayConfiguration (config, kCGConfigureForSession ); // 5
}

