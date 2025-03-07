/*
 * read_input.h
 * 
 * This header file declares the functions for reading input data for the tournament scheduling system.
 * 
 * The functions in this file are responsible for reading tournament-related data from an input file. 
 * They handle loading the tournament configuration and participant information into the appropriate data structures.
 * 
 * Functions:
 * - read_input: Reads the input data from the provided input file and populates the `Tournament` structure with the details such as participants, match length, number of days, and start/end times.
 * - get_input_file: Returns the name of the input file that contains the tournament configuration.
 * 
 */

#ifndef INPUT_H
#define INPUT_H

#include <fstream>
#include <string>
#include <iostream>
#include "data_structure.h"
using namespace std;

void read_input(ifstream &in, Tournament &tournament);
string get_input_file();

#endif
