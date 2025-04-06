/*
 * File name: print_output.h
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * This header file declares functions for formatting and displaying the tournament schedule.
 * The functions handle sorting and printing match information in a user-friendly format.
 * 
 * Functions:
 * - print_schedule: Formats and displays the complete tournament schedule
 * - compare_matches: Comparison function for sorting matches by day, venue, and time
 */

#ifndef OUTPUT_H
#define OUTPUT_H

#include "data_structure.h"
#include <iostream>
#include <algorithm>
using namespace std;

/**
 * @brief Prints the tournament schedule to console
 * @param matches Array of Match objects containing scheduled matches
 * @param total_matchups Total number of matches to display
 * 
 * Output format:
 * - Organized by day and venue
 * - Each match shows time, participants, and venue
 */
void print_schedule(Match matches[], int total_matchups);

/**
 * @brief Comparison function for sorting matches
 * @param a First Match object to compare
 * @param b Second Match object to compare
 * @return True if a should come before b in schedule
 * 
 * Sort order:
 * 1. By day (ascending)
 * 2. By venue (ascending)
 * 3. By start time (ascending)
 */
bool compare_matches(const Match &a, const Match &b);

#endif