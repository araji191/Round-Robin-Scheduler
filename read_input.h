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