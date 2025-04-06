/*
 * File name: read_input.h
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * This header file declares functions for reading and validating tournament input data.
 * The functions handle file input, data parsing, and validation of all tournament parameters.
 * 
 * Functions:
 * - read_input: Main function for reading tournament configuration
 * - validate_tournament_type: Checks tournament type validity
 * - read_participants: Reads and validates participant list
 * - validate_time: Validates time format
 * - count_tournament_participants: Returns number of participants
 * - generate_matchups: Creates all required match pairings
 */

#ifndef INPUT_H
#define INPUT_H

#include <fstream>
#include <string>
#include <iostream>
#include "data_structure.h"
using namespace std;

/**
 * @brief Validates tournament type value
 * @param tournament Tournament object to validate
 * @return True if type is valid
 */
bool validate_tournament_type(Tournament &tournament);

/**
 * @brief Reads participant list from input file
 * @param in Input file stream
 * @param tournament Tournament object to populate
 * @return True if participants were read successfully
 */
bool read_participants(ifstream &in, Tournament &tournament);

/**
 * @brief Validates time format (HH:MM)
 * @param time Time object to validate
 * @param timeType Description of time being validated
 * @return True if time is valid
 */
bool validate_time(Time &time, const string &timeType);

/**
 * @brief Main function for reading tournament input
 * @param in Input file stream
 * @param tournament Tournament object to populate
 * @return True if input was read successfully
 */
bool read_input(ifstream &in, Tournament &tournament);

/**
 * @brief Counts number of participants in tournament
 * @param tournament Tournament object to examine
 * @return Number of participants
 */
int count_tournament_participants(Tournament &tournament);

/**
 * @brief Generates all possible match pairings
 * @param tournament Tournament configuration
 * @param matches Array to store generated matches
 * @param num_participants Number of participants
 * @param match_index Current match index counter
 */
void generate_matchups(Tournament &tournament, Match matches[], int num_participants, int &match_index);

#endif